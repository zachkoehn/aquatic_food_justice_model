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

## transform highly skewed variables
df['eez_total'] = np.log(x_control['eez_total'])
df['inland_water_max'] = np.log(x_control['inland_water_max'] + 1)
df['pp_eez_weighted'] = np.log(x_control['pp_eez_weighted'])

# prepare data for analysis
# predictor & control variables of inetrest
cov_names = ['mean_wage_gap_all_sectors', 'female_particip_ssf', 'mean_women_parl_perc',
    'sat_model_est_pov', 'mean_educ',
    'cultural_hegemony', 'language_diversity', 'prop_pop_l1_inst',
    'age_dep_ratio', 'mean_voice_account']
control_names = ['eez_total', 'inland_water_max', 'pp_eez_weighted']

# response variable
y = df['mean_total_production'] / df['direct_w_esitimated_ssf']

# scale
# y -= y.min()
y /= y.max()
# y = y[y > 0].copy()

## predictor variables
x_cov = df[cov_names].copy()

# interactions
x_cov['mean_wage_gap_all_sectors:sat_model_est_pov'] = x_cov['mean_wage_gap_all_sectors'] * x_cov['sat_model_est_pov']
x_cov['mean_educ:mean_wage_gap_all_sectors'] = x_cov['mean_educ'] * x_cov['mean_wage_gap_all_sectors']
x_cov['cultural_hegemony:sat_model_est_pov'] = x_cov['cultural_hegemony'] * x_cov['sat_model_est_pov']

## control variables
x_control = df[control_names].copy()

## merge
X = x_cov.merge(x_control, left_index=True, right_index=True)
X = X.loc[y.index, :].copy()

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
# vif

# mask NA
X_masked = np.ma.masked_invalid(X)

# model
with pm.Model() as model:
    # priors
    intercept = pm.Normal('intercept', mu=0., sigma=100.)
    beta = pm.Normal('beta', mu=0., sigma=100., shape=(X_masked.shape[1],))
    alpha = pm.HalfCauchy('alpha', beta=5.)

    # impute missing X
    chol, stds, corr = pm.LKJCholeskyCov('chol', n=X_masked.shape[1], eta=2., sd_dist=pm.Exponential.dist(1.), compute_corr=True)
    cov = pm.Deterministic('cov', chol.dot(chol.T))
    X_mu = pm.Normal('X_mu', mu=0., sigma=100., shape=X_masked.shape[1], testval=X_masked.mean(axis=0))
    X_modeled = pm.MvNormal('X', mu=X_mu, chol=chol, observed=X_masked)

    # observation
    mu_ = intercept + tt.dot(X_modeled, beta)

    # likelihood
    mu = tt.exp(mu_)
    likelihood = pm.Gamma('y', alpha=alpha, beta=alpha/mu, observed=y)

    # sample
    trace = pm.sample(3000, tune=1000, chains=2)

# summarize results
summary_coeff = np.quantile(trace.beta, axis=0, q=[0.5, 0.025, 0.975])
summary_coeff = pd.DataFrame(np.transpose(summary_coeff))
summary_coeff.index = X.columns
summary_coeff.columns = ['median', 'lower', 'upper']
summary_coeff['significance'] = ['*' if np.logical_or(x1 > 0, x2 < 0) else '' for (x1, x2) in zip(summary_coeff.lower, summary_coeff.upper)]
summary_coeff['rhat'] = az.rhat(trace3).beta
summary_coeff = summary.drop(index=control_names)

# plot
summary_coeff.reset_index(inplace=True)
summary_coeff = summary_coeff[::-1]
summary_coeff['index'] = pd.Categorical(summary_coeff['index'], categories=summary_coeff['index'])

p = ggplot(aes(x='index', y='median'), data=summary_coeff) + \
    geom_hline(yintercept=0, colour='#cccccc') + \
    geom_point() + \
    geom_errorbar(aes(ymin='lower', ymax='upper', width=0)) + \
    ylim([-1.5, 2.2]) + \
    labs(x='', y='Estimate') + \
    coord_flip() + \
    theme_classic() + \
    theme(axis_text=element_text(size=6, colour='black', family='Helvetica'), \
        axis_title=element_text(size=8, colour='black', family='Helvetica'), \
        axis_line=element_line(color='black'),
        axis_ticks=element_line(color='black'))

# ggsave(p, 'total_production.pdf', width=5, height=5)
