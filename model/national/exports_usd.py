from scipy import stats
import geopandas
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
y = df['mean_exports_USD1000']/(df['mean_gdp'] * df['mean_population'])

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
x_control = pd.DataFrame()
x_control['mean_capture_production'] = df['mean_capture_production']
x_control['mean_aquaculture_production'] = df[['mean_aquaculture_production_freshwater','mean_aquaculture_production_marine','mean_aquaculture_production_brackish']].sum(axis=1)

## merge
X = x_cov.merge(x_control, left_index=True, right_index=True)
X = X.loc[y.index, :].copy()

## transform highly skewed variables
X['mean_capture_production'] = np.log(X['mean_capture_production'] + 1)
X['mean_aquaculture_production'] = np.log(X['mean_aquaculture_production'] + 1)

# standardize all
def standardize(x):
    return (x-np.mean(x))/np.std(x)

X = X.apply(standardize, axis=0)

# mask NA
X_masked = np.ma.masked_invalid(X)

# model
with pm.Model() as model:
    # priors
    intercept = pm.Normal('intercept', mu=0, sigma=100)
    beta = pm.Normal('beta', mu=0, sigma=100, shape=X_masked.shape[1])
    alpha = pm.HalfCauchy('alpha', beta=5)

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

# summarize results
summary_coef = np.quantile(trace.beta, axis=0, q=[0.5, 0.025, 0.25, 0.75, 0.975])
summary_coef = pd.DataFrame(np.transpose(summary_coef))
summary_coef.index = X.columns
summary_coef.columns = ['median', 'lower95', 'lower50', 'upper50', 'upper95']
summary_coef['P(x > 0)'] = [(trace.beta[:,i] > 0).sum()/trace.beta.shape[0] for i in range(trace.beta.shape[1])]
summary_coef['rhat'] = az.rhat(trace).beta
summary_coef = summary_coef.drop(index=x_control.columns)

# plot
summary_coef['var_name'] = cov_name
summary_coef = summary_coef[::-1]
summary_coef['var_name'] = pd.Categorical(summary_coef['var_name'], categories=summary_coef['var_name'])

min_val = summary_coef.lower95.min()
max_val = summary_coef.upper95.max()
min_range = min_val - (max_val - min_val) * 0.1
max_range = max_val + (max_val - min_val) * 0.1

# point color
foo = zip(summary_coef.lower95 * summary_coef.upper95, summary_coef.lower50 * summary_coef.upper50)
col = ['#000000' if x1 > 0 else '#aaaaaa' if x2 > 0 else '#ffffff' for (x1, x2) in foo]

p = ggplot(aes(x='var_name', y='median'), data=summary_coef) + \
    geom_hline(yintercept=0, colour='#cccccc', size=0.3) + \
    geom_errorbar(aes(ymin='lower95', ymax='upper95', size=1, width=0)) + \
    geom_errorbar(aes(ymin='lower50', ymax='upper50', size=2, width=0)) + \
    scale_size_continuous(range=[0.3,1]) + \
    geom_point(size=1.5, fill=col) + \
    ylim([min_range, max_range]) + \
    labs(x='', y='Estimate') + \
    coord_flip() + \
    theme_classic() + \
    theme(axis_text=element_text(size=6, colour='black', family='Helvetica'),
        axis_title=element_text(size=8, colour='black', family='Helvetica'),
        axis_line=element_line(color='black'),
        axis_ticks=element_line(color='black'),
        legend_position='none')

ggsave(p, 'plots/national/exports_usd.pdf', width=1.5, height=2.5)
