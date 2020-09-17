import scipy
import numpy as np
import pandas as pd
import pymc3 as pm
import theano.tensor as tt
from plotnine import *
import arviz as az
from statsmodels.stats.outliers_influence import variance_inflation_factor


# import data
df = pd.read_csv('all_national_indicators.csv')
df.columns = df.columns.str.replace('.', '_')

# remove duplicated countries
df = df[~df.iso3c.duplicated()]


# change distributions to reduce leverage
df['mean_gdp'] = np.log(df['mean_gdp'] + 1)
df['nb_languages_established'] = np.log(df['nb_languages_established'] + 1)
df['eez_total'] = np.log(df['eez_total'] + 1)
df['total_renewable_water'] = np.log(df['total_renewable_water'] + 1)
df['inland_water_min'] = np.log(df['inland_water_min'] + 1)
df['inland_water_max'] = np.log(df['inland_water_max'] + 1)
df['sat_model_est_pov'] = np.log(df['sat_model_est_pov'])

# prepare data
## response variable
y = df['mean_total_production'] / df['mean_population']
# scale
y -= y.min()
y /= y.max()
y = y[y > 0].copy()

## predictor variables
x_cov = df[['mean_wage_gap_all_sectors', 'female_particip_ssf', 'mean_women_parl_perc',
    'mean_pov_prop', 'mean_educ', 'mean_gdp',
    'nb_languages_established', 'language_diversity', 'prop_pop_l1_inst',
    'age_dep_ratio', 'mean_voice_account']]

# drop variables with high VIF
X.drop(columns=['mean_gdp', 'nb_languages_established', 'mean_wage_gap_all_sectors', 'prop_pop_l1_inst', 'age_dep_ratio'], inplace=True)

# check variance inflation factor
X_ = X.dropna(how='any').copy()
vif = pd.DataFrame()
vif['feature'] = X_.columns
vif['VIF'] = [variance_inflation_factor(X_.values, i) for i in range(X_.shape[1])]


## control variables
x_control = df[['eez_total', 'inland_water_max']]

## merge
X = x_cov.merge(x_control, left_index=True, right_index=True)
X = X.loc[y.index, :].copy()

# standardize all
def standardize(x):
    return (x-np.mean(x))/np.std(x)

X = X.apply(standardize, axis=0)

# mask NA
X_masked = np.ma.masked_invalid(X)

# model
with pm.Model() as model:
    # priors
    intercept = pm.Normal('intercept', mu=0., sigma=100.)
    beta = pm.Normal('beta', mu=0., sigma=10., shape=(X_masked.shape[1],))
    alpha = pm.HalfCauchy('alpha', beta=5.)

    # impute missing X
    #X_mu = pm.Normal('X_mu', 0., 10., shape=X_masked.shape[1])
    #X_modeled = pm.Normal('X', mu=X_mu, sigma=1., observed=X_masked)

    # impute missing X
    chol, stds, corr = pm.LKJCholeskyCov('chol', n=X_masked.shape[1], eta=2., sd_dist=pm.Exponential.dist(1.), compute_corr=True)
    cov = pm.Deterministic('cov', chol.dot(chol.T))
    X_mu = pm.Normal('X_mu', 0., 10., shape=X_masked.shape[1], testval=X_masked.mean(axis=0))
    X_modeled = pm.MvNormal('X', mu=X_mu, chol=chol, observed=X_masked)

    # observation
    mu_ = intercept + tt.dot(X_modeled, beta)

    # likelihood
    mu = tt.exp(mu_)
    likelihood = pm.Gamma('y', alpha=alpha, beta=alpha/mu, observed=y)

# sample
with model:
    trace = pm.sample(1000, tune=1000)

# check convergence
az.plot_trace(trace, var_names=('intercept', 'beta', 'alpha'))

# summarize coefficients
summary_coeff = np.quantile(trace.beta, axis=0, q=[0.025, 0.5, 0.975])
summary_coeff = pd.DataFrame(np.transpose(summary_coeff))
summary_coeff.index = X.columns
summary_coeff.columns = ['lower', 'median', 'upper']
summary_coeff['significance'] = ['*' if np.logical_or(x1 > 0, x2 < 0) else '' for (x1, x2) in zip(summary_coeff.lower, summary_coeff.upper)]
summary_coeff['rhat'] = az.rhat(trace).beta
summary_coeff['ess'] = az.ess(trace).beta

summary_coeff
