import numpy as np
import pandas as pd
import pymc3 as pm
import theano.tensor as tt
from plotnine import *


# function to standardize
def standardize(x):
    return (x-np.mean(x))/np.std(x)

# import data
df = pd.read_csv('all_national_indicators.csv')
df.columns = df.columns.str.replace('.', '_')

# remove duplicated countries
df = df[~df.iso3c.duplicated()]

# variable names of interest
x_name = ['mean_wage_gap_all_sectors', 'female_particip_ssf', 'mean_women_parl_perc']
x_control = ['eez_total']
y_name = ['mean_total_production']

# prepare data
X = df[x_name].copy()

# drop rows where all predictors are missing
X.dropna(how='all', inplace=True)

# add control variables (log)
control = df[x_control].apply(np.log, axis=0)
X = X.merge(control, right_index=True, left_index=True)

# standardize all
X = X.apply(standardize, axis=0)

# masked data for missing values
X_masked = np.ma.masked_invalid(X)

# observed values
y = df.loc[X.index, y_var].values
y = y + 1
#y_masked = np.ma.masked_invalid(y)


with pm.Model() as model:
    # priors
    intercept = pm.Normal('intercept', mu=0, sigma=10)
    beta = pm.Normal('beta', mu=0, sigma=10, shape=(X_masked.shape[1],))
    eps = pm.HalfCauchy('eps', beta=5)
    # eps = pm.Uniform('esp', lower=0, upper=1000)

    # impute missing X
    X_mu = pm.Normal('X_mu', mu=0, sigma=10, shape=(1,X_masked.shape[1]))
    X = pm.Normal('X', mu=X_mu, sigma=1, observed=X_masked)

    # observation
    mu = intercept + tt.dot(X, beta)

    # likelihood
    likelihood = pm.Lognormal('y', mu=mu, sigma=eps, observed=y)


with model:
    trace = pm.sample(1000)


pm.traceplot(trace)
pm.summary(trace)


foo = pm.summary(trace)
beta = foo[['beta' in _ for _ in foo.index]]
beta
