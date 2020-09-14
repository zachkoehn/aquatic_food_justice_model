import scipy
import numpy as np
import pandas as pd
import pymc3 as pm
import theano.tensor as tt
import matplotlib.pyplot as plt
import seaborn as sns
from plotnine import *


# function to standardize
def standardize(x):
    return (x-np.mean(x))/np.std(x)

# function to summarize samples
def summary_coeff(x):
    foo = np.quantile(x, axis=0, q=[0.025, 0.5, 0.975])
    foo = pd.DataFrame(np.transpose(foo))
    foo.index = X.columns
    foo.columns = ['lower', 'median', 'upper']
    foo['significance'] = ['*' if np.logical_or(x1 > 0, x2 < 0) else '' for (x1, x2) in zip(foo.lower, foo.upper)]

    return(foo)



# import data
df = pd.read_csv('all_national_indicators.csv')
df.columns = df.columns.str.replace('.', '_')

# remove duplicated countries
df = df[~df.iso3c.duplicated()]


# prepare data
y = df['mean_total_production'] / df['mean_population']
y = y[y > 0].copy()

x_cov = df[['mean_wage_gap_all_sectors', 'female_particip_ssf', 'mean_women_parl_perc',
    'mean_pov_prop', 'mean_educ', 'mean_gdp',
    'nb_languages_established', 'language_diversity', 'prop_pop_l1_inst',
    'age_dep_ratio', 'mean_voice_account']]

x_control = df[['eez_total', 'total_renewable_water', 'inland_water_min', 'inland_water_max']]

X = x_cov.merge(x_control, left_index=True, right_index=True)
X = X.loc[y.index, :].copy()

# check distributions
# sns.pairplot(X)

# change distributions
X[['mean_gdp', 'nb_languages_established', 'eez_total', 'total_renewable_water', 'inland_water_min', 'inland_water_max']] = \
    X[['mean_gdp', 'nb_languages_established', 'eez_total', 'total_renewable_water', 'inland_water_min', 'inland_water_max']].apply(np.log, axis=0)

# check correlations
cmap = sns.diverging_palette(230, 20, as_cmap=True)
plt.figure(figsize=(12,8))
sns.heatmap(X.corr(),vmin=-1, vmax=1, annot=True, square=True, fmt='.2f', cmap=cmap)
plt.savefig('correlations.pdf')

# remove highly correlated variables
X.drop(columns=['mean_pov_prop', 'mean_gdp', 'total_renewable_water', 'inland_water_min'], inplace=True)

# standardize
X = X.apply(standardize, axis=0)


# mask NA
X_masked = np.ma.masked_invalid(X)


with pm.Model() as model:
    # priors
    intercept = pm.Normal('intercept', mu=0, sigma=100, testval=-4)
    beta = pm.Normal('beta', mu=0, sigma=10, shape=(X_masked.shape[1],))
    eps = pm.HalfCauchy('eps', beta=5, testval=2)

    # impute missing X
    X_mu = pm.Normal('X_mu', mu=0, sigma=10, shape=(1,X_masked.shape[1]))
    X_modeled = pm.Normal('X', mu=X_mu, sigma=1, observed=X_masked)

    # observation
    mu = intercept + tt.dot(X_modeled, beta)

    # likelihood
    likelihood = pm.Lognormal('y', mu=mu, sigma=eps, observed=y)


with model:
    trace = pm.sample(4000, tune=2000)


summary_coeff(trace.beta)

# pm.traceplot(trace)
# pm.summary(trace)
