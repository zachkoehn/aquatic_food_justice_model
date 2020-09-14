import numpy as np
import pandas as pd
import pymc3 as pm
import theano.tensor as tt
from plotnine import *
import tqdm

# function to standardize
def standardize(x):
    return (x-np.mean(x))/np.std(x)

# import data
df = pd.read_csv('all_national_indicators.csv')
df.columns = df.columns.str.replace('.', '_')

# remove duplicated countries
df = df[~df.iso3c.duplicated()]

# variable names of interest
x_control = ['eez_total']
y_name = ['mean_total_production']

# prepare data
X = df.eez_total
X.dropna(inplace=True)
y = df.loc[X.index, 'mean_total_production']

#
y = df.mean_total_production + 1
y.dropna(inplace=True)
X = df.loc[y.index, ['mean_wage_gap_all_sectors', 'female_particip_ssf', 'mean_women_parl_perc', 'eez_total']].copy()
X.eez_total = X.eez_total.apply(np.log)

X = X.apply(standardize, axis=0)

X_masked = np.ma.masked_invalid(X)


with pm.Model() as model:
    # priors
    intercept = pm.Normal('intercept', mu=0, sigma=100, testval=10)
    beta = pm.Normal('beta', mu=0, sigma=10, testval=[-1,0,0,2], shape=(X_masked.shape[1],))
    eps = pm.HalfCauchy('eps', beta=5, testval=2)

    # impute missing X
    X_mu = pm.Normal('X_mu', mu=0, sigma=10, shape=(1,X_masked.shape[1]))
    X_modeled = pm.Normal('X', mu=X_mu, sigma=1, observed=X_masked)

    # observation
    mu = intercept + tt.dot(X_modeled, beta)

    # likelihood
    likelihood = pm.Lognormal('y', mu=mu, sigma=eps, observed=y)


with model:
    trace = pm.sample(1000)


def summary_coeff(x):
    foo = np.quantile(x, axis=0, q=[0.025, 0.5, 0.975])
    foo = pd.DataFrame(np.transpose(foo))
    foo.index = X.columns
    foo.columns = ['lower', 'median', 'upper']
    foo['significance'] = ['*' if np.logical_or(x1 > 0, x2 < 0) else '' for (x1, x2) in zip(foo.lower, foo.upper)]

    return(foo)

summary_coeff(trace.beta)

pm.traceplot(trace)
pm.summary(trace)
