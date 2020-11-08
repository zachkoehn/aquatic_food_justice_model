from scipy import stats
import numpy as np
import pandas as pd
import pymc3 as pm
import theano.tensor as tt
from plotnine import *
import arviz as az
from statsmodels.stats.outliers_influence import variance_inflation_factor
import geopandas
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
y = df['mean_total_production'] * (df['female_particip_ssf'] + 1e-6) / df['direct_w_esitimated_ssf']

# scale
# y -= y.min()
y /= y.max()
y = y[~y.isnull()].copy()

## predictor variables of inetrest
cov = ['mean_educ', 'sat_model_est_pov', 'language_diversity', 'cultural_hegemony',
    'mean_voice_account', 'mean_sigi', 'age_dep_ratio_sat_mean']

cov_name = ['Education', 'Poverty', 'Language diversity', 'Cultural hegemony',
    'Voice accountability', 'Gender discrimination', 'Age dependency']

## predictor variables
x_cov = df[cov].copy()

## control variables
control = ['eez_total', 'inland_water_max', 'pp_eez_weighted']
x_control = df[control].copy()

## merge
X = x_cov.merge(x_control, left_index=True, right_index=True)
X = X.loc[y.index, :].copy()

## transform highly skewed variables
X['eez_total'] = np.log(X['eez_total'] + 1)
X['inland_water_max'] = np.log(X['inland_water_max'] + 1)

# standardize all
def standardize(x):
    return (x-np.mean(x))/np.std(x)

X = X.apply(standardize, axis=0)

# variance inflation factor
# X_ = X[cov_names].copy()
# X_ = X_.dropna(how='any')
# vif = pd.DataFrame()
# vif['features'] = X_.columns
# vif['VIF'] = [variance_inflation_factor(X_.values, i) for i in range(X_.shape[1])]
# vif['R2'] = 1 - 1/vif.VIF
# vif

# mask NA
X_masked = np.ma.masked_invalid(X)

# model
with pm.Model() as model:
    # priors
    intercept = pm.Normal('intercept', mu=0, sigma=100)
    beta = pm.Normal('beta', mu=0, sigma=100, shape=X_masked.shape[1])
    alpha = pm.HalfCauchy('alpha', beta=5) # common skewness

    # impute missing X
    chol, stds, corr = pm.LKJCholeskyCov('chol', n=X_masked.shape[1], eta=2, sd_dist=pm.Exponential.dist(1), compute_corr=True)
    cov = pm.Deterministic('cov', chol.dot(chol.T))
    X_mu = pm.Normal('X_mu', mu=0, sigma=100, shape=X_masked.shape[1], testval=X_masked.mean(axis=0))
    X_modeled = pm.MvNormal('X', mu=X_mu, chol=chol, observed=X_masked)

    # observation
    mu_ = intercept + tt.dot(X_modeled, beta)

    # likelihood
    mu = tt.exp(mu_)
    likelihood = pm.Gamma('y', alpha=alpha, beta=alpha/mu, observed=y)

    # sample
    trace = pm.sample(4000, tune=1000, chains=2)


## plot posterior
# az.plot_trace(trace, var_names=['intercept', 'beta', 'alpha'])

## posterior predictive distribution
# with model:
#     ppc = pm.sample_posterior_predictive(trace, var_names=['y'])

# import matplotlib.pyplot as plt
# _, ax = plt.subplots(figsize=(12, 6))
# plt.hist([i.mean() for i in ppc['y']], bins=50, alpha=0.5)
# ax.axvline(y.mean())

# summarize results
summary_coef = np.quantile(trace.beta, axis=0, q=[0.5, 0.025, 0.25, 0.75, 0.975])
summary_coef = pd.DataFrame(np.transpose(summary_coef))
summary_coef.index = X.columns
summary_coef.columns = ['median', 'lower95', 'lower50', 'upper50', 'upper95']
summary_coef['P(x > 0)'] = [(trace.beta[:,i] > 0).sum()/trace.beta.shape[0] for i in range(trace.beta.shape[1])]
summary_coef['rhat'] = az.rhat(trace).beta
summary_coef = summary_coef.drop(index=x_control.columns)
summary_coef
# plot
summary_coef['var_name'] = cov_name
summary_coef = summary_coef[::-1]
summary_coef['var_name'] = pd.Categorical(summary_coef['var_name'], categories=summary_coef['var_name'])

min_val = summary_coef.lower95.min()
max_val = summary_coef.upper95.max()
min_range = min_val - (max_val - min_val) * 0.1
max_range = max_val + (max_val - min_val) * 0.1

p = ggplot(aes(x='var_name', y='median'), data=summary_coef) + \
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


ggsave(p, 'plots/national/women_production.pdf', width=1.5, height=3)
