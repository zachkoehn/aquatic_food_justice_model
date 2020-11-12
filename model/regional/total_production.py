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

# region table
region = pd.DataFrame()
region['un_subregion_name'] = df.un_subregion_name.dropna().unique()
region['region_index'] = range(region.shape[0])

# add region index
df = df.merge(region, how='left', on='un_subregion_name').copy()

# remove small territories
df = df[df.mean_population >= 1000]

## response variable
y = df['mean_total_production'] / df['direct_w_esitimated_ssf']

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

# mask NA
X_masked = np.ma.masked_invalid(X)

# convert region index to integer
df.region_index = df.region_index.astype('int32')
region_index = df.loc[y.index, 'region_index']

# model
with pm.Model() as model:

    # hyperpriors for intercept
    mu_0 = pm.Normal('mu_0', mu=0, sigma=100)
    sigma_0 = pm.HalfNormal('sigma_0', 5)

    # priors
    beta = pm.Normal('beta', mu=0, sigma=100, shape=X_masked.shape[1])
    intercept = pm.Normal('intercept', mu=mu_0, sigma=sigma_0, shape=region.shape[0])
    alpha = pm.HalfCauchy('alpha', beta=5) # common skewness

    # impute missing X
    chol, stds, corr = pm.LKJCholeskyCov('chol', n=X_masked.shape[1], eta=2, sd_dist=pm.Exponential.dist(1), compute_corr=True)
    cov = pm.Deterministic('cov', chol.dot(chol.T))
    X_mu = pm.Normal('X_mu', mu=0, sigma=100, shape=X_masked.shape[1], testval=X_masked.mean(axis=0))
    X_modeled = pm.MvNormal('X', mu=X_mu, chol=chol, observed=X_masked)

    # observation
    mu_ = intercept[region_index] + tt.dot(X_modeled, beta)

    # likelihood
    mu = tt.exp(mu_)
    likelihood = pm.Gamma('y', alpha=alpha, beta=alpha/mu, observed=y)

    # sample
    trace = pm.sample(1000, tune=1000, chains=2)


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

#_______________________________
# bright/dark spots

# fill in posterior estmates of the missing predictors
X_missing = np.quantile(trace.X_missing, axis=0, q=0.5)
idx = np.where(X_masked.mask)
X_imputed = X.copy()
for i in range(X_missing.shape[0]):
    X_imputed.iloc[idx[0][i], idx[1][i]] = X_missing[i]

# predicted values
int = np.quantile(trace.intercept, q=0.5)
coef = np.quantile(trace.beta, axis=0, q=0.5)
mu = np.exp(int + np.dot(X_imputed, coef))

# quantiles
alpha = np.quantile(trace.alpha, axis=0, q=0.5)
quantile = stats.gamma.cdf(np.asarray(y), a=alpha, scale=mu/alpha)

map = pd.DataFrame(index=y.index)
map['country'] = df.country_name_en
map['iso3'] = df.iso3c
map['quantile'] = quantile
map['n_missing'] = X.isnull().sum(axis=1)
map['obs'] = df['mean_total_production'] / df['direct_w_esitimated_ssf']

# plot quantile vs. n_missing
p = ggplot(aes(x='quantile', y='n_missing'), data=map) + geom_point() + \
    labs(title='Production/worker (t)', x='Quantile', y='Number of missing predictors') + \
    scale_y_continuous(breaks=[0,2,4,6,8,10], labels=[0,2,4,6,8,10], limits=[0,10]) + \
    theme(plot_title=element_text(face=2, size=8, colour='black', family='Helvetica'),
    axis_title=element_text(size=8, colour='black', family='Helvetica'),
    axis_text=element_text(size=6, colour='black', family='Helvetica'))

# plot observed vs quantile
p = ggplot(aes(x='obs', y='quantile'), data=map) + geom_point() + \
    labs(x='Production/worker (t)', y='Quantile') + \
    scale_y_continuous(limits=[0,1]) + \
    scale_x_continuous(trans='log') + \
    theme(plot_title=element_text(face=2, size=8, colour='black', family='Helvetica'),
    axis_title=element_text(size=8, colour='black', family='Helvetica'),
    axis_text=element_text(size=6, colour='black', family='Helvetica'))

p
# plot map
world = geopandas.read_file(geopandas.datasets.get_path('naturalearth_lowres'))
world = world.merge(map, how='left', left_on='iso_a3', right_on='iso3')
world.drop(world[world.iso_a3=='ATA'].index, inplace=True)

p = ggplot() + \
    geom_map(aes(fill='quantile'), world, stroke=0, size=0) + \
    coord_equal() + \
    scale_x_continuous(limits=[-180, 180], expand=[0,0]) + \
    scale_y_continuous(limits=[-70, 90], expand=[0,0]) + \
    scale_fill_continuous(name='Quantile') + \
    guides(fill=guide_colourbar(barwidth=3, barheight=6)) + \
    labs(title='Production/worker (t)') + \
    theme(plot_title=element_text(hjust=0, face=2, size=8, colour='black', family='Helvetica'),
        legend_title=element_text(size=6, colour='black', family='Helvetica'),
        legend_text=element_text(size=6, colour='black', family='Helvetica'),
        panel_grid_major=element_blank(),
        panel_grid_minor=element_blank(),
        axis_ticks=element_blank(),
        axis_text=element_blank(),
        axis_title=element_blank(),
        panel_background=element_rect(color='none', fill='none'))
