import scipy
import numpy as np
import pandas as pd
import pymc3 as pm
import theano.tensor as tt
from plotnine import *
import arviz as az
from statsmodels.stats.outliers_influence import variance_inflation_factor
import matplotlib
matplotlib.rcParams['pdf.fonttype'] = 42
matplotlib.rcParams['ps.fonttype'] = 42

# import data
df = pd.read_csv('all_national_indicators.csv')
df.columns = df.columns.str.replace('.', '_')

# remove small territories
df = df[df.mean_population >= 1000]

# prepare data for analysis
## response variable
y = df['percent_animal_protein_fish']

# scale
# y -= y.min()
y /= y.max()
y = y[~y.isnull()].copy()

## predictor variables of inetrest
cov_names = ['mean_wage_gap_all_sectors', 'female_particip_ssf', 'mean_women_parl_perc',
    'sat_model_est_pov', 'mean_educ',
    'cultural_hegemony', 'language_diversity', 'prop_pop_l1_inst',
    'age_dep_ratio', 'mean_voice_account']

cov_names2 = ['Gender wealth gap', 'Women in fisheries', 'Women in leadership',
    'Poverty', 'Education',
    'Cultural hegemony', 'Language diversity', 'Institutional language',
    'Age dependency', 'Voice accountability']

## predictor variables
x_cov = df[cov_names].copy()

## control variables
x_control = pd.DataFrame()
x_control['fish_relative_caloric_price'] = df['fish_relative_caloric_price']

## merge
X = x_cov.merge(x_control, left_index=True, right_index=True)
X = X.loc[y.index, :].copy()

# interactions
# X['mean_wage_gap_all_sectors:sat_model_est_pov'] = X['mean_wage_gap_all_sectors'] * X['sat_model_est_pov']
# X['mean_educ:mean_wage_gap_all_sectors'] = X['mean_educ'] * X['mean_wage_gap_all_sectors']
# X['cultural_hegemony:sat_model_est_pov'] = X['cultural_hegemony'] * X['sat_model_est_pov']

# standardize all
def standardize(x):
    return (x-np.mean(x))/np.std(x)

X = X.apply(standardize, axis=0)

# variance inflation factor
X_ = X[cov_names].copy()
X_ = X_.dropna(how='any')
vif = pd.DataFrame()
vif['features'] = X_.columns
vif['VIF'] = [variance_inflation_factor(X_.values, i) for i in range(X_.shape[1])]
vif['R2'] = 1 - 1/vif.VIF
#vif

# mask NA
X_masked = np.ma.masked_invalid(X)

# model
with pm.Model() as model:
    # priors
    intercept = pm.Normal('intercept', mu=0., sigma=100.)
    beta = pm.Normal('beta', mu=0., sigma=100., shape=(X_masked.shape[1],))
    sigma = pm.HalfCauchy('alpha', beta=5.)

    # impute missing X
    chol, stds, corr = pm.LKJCholeskyCov('chol', n=X_masked.shape[1], eta=2., sd_dist=pm.Exponential.dist(1.), compute_corr=True)
    cov = pm.Deterministic('cov', chol.dot(chol.T))
    X_mu = pm.Normal('X_mu', mu=0., sigma=100., shape=X_masked.shape[1], testval=X_masked.mean(axis=0))
    X_modeled = pm.MvNormal('X', mu=X_mu, chol=chol, observed=X_masked)

    # observation
    mu_ = intercept + tt.dot(X_modeled, beta)

    # likelihood
    mu = mu_
    likelihood = pm.Normal('y', mu=mu, sigma=sigma, observed=y)

    # sample
    trace = pm.sample(3000, tune=1000, chains=2)

# check fit
#int =  np.quantile(trace.intercept, axis=0, q=0.5)
#coeff = np.quantile(trace.beta, axis=0, q=0.5)
#pred = int + np.dot(X, coeff)
#import matplotlib.pyplot as plt
#plt.hist(pred - y, bins=50)

# summarize results
summary_coeff = np.quantile(trace.beta, axis=0, q=[0.5, 0.025, 0.25, 0.75, 0.975])
summary_coeff = pd.DataFrame(np.transpose(summary_coeff))
summary_coeff.index = X.columns
summary_coeff.columns = ['median', 'lower95', 'lower50', 'upper50', 'upper95']
summary_coeff['P(x > 0)'] = [(trace.beta[:,i] > 0).sum()/trace.beta.shape[0] for i in range(trace.beta.shape[1])]
summary_coeff['rhat'] = az.rhat(trace).beta
summary_coeff = summary_coeff.drop(index=x_control.columns)

# az.plot_trace(trace, var_names=['intercept', 'beta', 'alpha'])

# plot
summary_coeff['var_name'] = cov_names2
summary_coeff = summary_coeff[::-1]
summary_coeff['var_name'] = pd.Categorical(summary_coeff['var_name'], categories=summary_coeff['var_name'])

min_val = summary_coeff.lower95.min()
max_val = summary_coeff.upper95.max()
min_range = min_val - (max_val - min_val) * 0.1
max_range = max_val + (max_val - min_val) * 0.1

p = ggplot(aes(x='var_name', y='median'), data=summary_coeff) + \
    geom_hline(yintercept=0, colour='#cccccc', size=0.3) + \
    geom_errorbar(aes(ymin='lower95', ymax='upper95', size=1, width=0)) + \
    geom_errorbar(aes(ymin='lower50', ymax='upper50', size=2, width=0)) + \
    scale_size_continuous(range=[0.3,1]) + \
    geom_point(size=1.5) + \
    ylim([min_range, max_range]) + \
    labs(x='', y='Estimate') + \
    coord_flip() + \
    theme_classic() + \
    theme(axis_text=element_text(size=6, colour='black', family='Helvetica'),
        axis_title=element_text(size=8, colour='black', family='Helvetica'),
        axis_line=element_line(color='black'),
        axis_ticks=element_line(color='black'),
        legend_position='none')

ggsave(p, 'plots/reliance.pdf', width=3, height=3)
