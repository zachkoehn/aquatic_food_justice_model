from scipy import stats
import geopandas
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
y = df['fish_relative_caloric_price']

# scale
# y -= y.min()
y /= y.max()
y = y[~y.isnull()].copy()

## predictor variables of inetrest
cov_names = ['mean_wage_gap_all_sectors', 'female_particip_ssf', 'mean_women_parl_perc',
    'sat_model_est_pov', 'mean_educ',
    'cultural_hegemony', 'language_diversity', 'prop_pop_l1_inst',
    'age_dep_ratio_sat_mean', 'mean_voice_account']

cov_names2 = ['Gender wealth gap', 'Women in fisheries', 'Women in leadership',
    'Poverty', 'Education',
    'Cultural hegemony', 'Language diversity', 'Institutional language',
    'Age dependency', 'Voice accountability']

## predictor variables
x_cov = df[cov_names].copy()

## control variables
x_control = pd.DataFrame()
x_control['mean_capture_production'] = df['mean_capture_production']
x_control['mean_aquaculture_production'] = df[['mean_aquaculture_production_freshwater','mean_aquaculture_production_marine','mean_aquaculture_production_brackish']].sum(axis=1)
x_control['total_gdp'] = df['mean_gdp'] * df['mean_population']
x_control['unit_exports'] = df['mean_exports_USD1000'] / df['mean_exports_tonnes']
x_control['unit_imports'] = df['mean_imports_USD1000'] / df['mean_imports_tonnes']

## merge
X = x_cov.merge(x_control, left_index=True, right_index=True)
X = X.loc[y.index, :].copy()

## transform highly skewed variables
X['mean_capture_production'] = np.log(X['mean_capture_production'] + 1)
X['mean_aquaculture_production'] = np.log(X['mean_aquaculture_production'] + 1)
X['total_gdp'] = np.log(X['total_gdp']  + 1)
X['unit_exports'] = np.log(X['unit_exports']  + 1)
X['unit_imports'] = np.log(X['unit_imports']  + 1)

# interactions
# X['mean_wage_gap_all_sectors:sat_model_est_pov'] = X['mean_wage_gap_all_sectors'] * X['sat_model_est_pov']
# X['mean_educ:mean_wage_gap_all_sectors'] = X['mean_educ'] * X['mean_wage_gap_all_sectors']
# X['cultural_hegemony:sat_model_est_pov'] = X['cultural_hegemony'] * X['sat_model_est_pov']

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

ggsave(p, 'plots/affordability.pdf', width=3, height=3)


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
scale = alpha/mu
quantile = stats.gamma.cdf(np.asarray(y), a=alpha, scale=mu/alpha)

map = pd.DataFrame(index=y.index)
map['country'] = df.country_name_en
map['iso3'] = df.iso3c
map['quantile'] = quantile
map['n_missing'] = X.isnull().sum(axis=1)


# plot quantile vs. n_missing
p = ggplot(aes(x='quantile', y='n_missing'), data=map) + geom_point() + \
    labs(title='Affordability', x='Quantile', y='Number of missing predictors') + \
    scale_y_continuous(breaks=[0,2,4,6,8,10], labels=[0,2,4,6,8,10], limits=[0,10]) + \
    theme(plot_title=element_text(face=2, size=8, colour='black', family='Helvetica'),
    axis_title=element_text(size=8, colour='black', family='Helvetica'),
    axis_text=element_text(size=6, colour='black', family='Helvetica'))

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
    labs(title='Affordability') + \
    theme(plot_title=element_text(hjust=0, face=2, size=8, colour='black', family='Helvetica'),
        legend_title=element_text(size=6, colour='black', family='Helvetica'),
        legend_text=element_text(size=6, colour='black', family='Helvetica'),
        panel_grid_major=element_blank(),
        panel_grid_minor=element_blank(),
        axis_ticks=element_blank(),
        axis_text=element_blank(),
        axis_title=element_blank(),
        panel_background=element_rect(color='none', fill='none'))
