import numpy as np
import pandas as pd
import xgboost as xgb
from sklearn.model_selection import KFold
from sklearn.metrics import log_loss
from scipy.interpolate import UnivariateSpline
import statsmodels.api as sm
import matplotlib.pyplot as plt
import collections
import eli5
from eli5.sklearn import PermutationImportance

pd.set_option("display.max_column", 999)

# Data preparation. 

tourney_results = pd.read_csv('/kaggle/input/march-machine-learning-mania-2023/MNCAATourneyDetailedResults.csv')
seeds = pd.read_csv('/kaggle/input/march-machine-learning-mania-2023/MNCAATourneySeeds.csv')
regular_results = pd.read_csv('/kaggle/input/march-machine-learning-mania-2023/MRegularSeasonDetailedResults.csv')
kp = pd.read_csv('/kaggle/input/kenpom2023/kenpom_2023.csv')

regular_results['WEFFG'] = regular_results['WFGM'] / regular_results['WFGA']
regular_results['WEFFG3'] = regular_results['WFGM3'] / regular_results['WFGA3']
regular_results['WDARE'] = regular_results['WFGM3'] / regular_results['WFGM']
regular_results['WTOQUETOQUE'] = regular_results['WAst'] / regular_results['WFGM']

regular_results['LEFFG'] = regular_results['LFGM'] / regular_results['LFGA']
regular_results['LEFFG3'] = regular_results['LFGM3'] / regular_results['LFGA3']
regular_results['LDARE'] = regular_results['LFGM3'] / regular_results['LFGM']
regular_results['LTOQUETOQUE'] = regular_results['LAst'] / regular_results['LFGM']
regular_results.head()

def prepare_data(df):
    dfswap = df[['Season', 'DayNum', 'LTeamID', 'LScore', 'WTeamID', 'WScore', 'WLoc', 'NumOT', 
    'LFGM', 'LFGA', 'LFGM3', 'LFGA3', 'LFTM', 'LFTA', 'LOR', 'LDR', 'LAst', 'LTO', 'LStl', 'LBlk', 'LPF', 
    'WFGM', 'WFGA', 'WFGM3', 'WFGA3', 'WFTM', 'WFTA', 'WOR', 'WDR', 'WAst', 'WTO', 'WStl', 'WBlk', 'WPF']]

    dfswap.loc[df['WLoc'] == 'H', 'WLoc'] = 'A'
    dfswap.loc[df['WLoc'] == 'A', 'WLoc'] = 'H'
    df.columns.values[6] = 'location'
    dfswap.columns.values[6] = 'location'    
      
    df.columns = [x.replace('W','T1_').replace('L','T2_') for x in list(df.columns)]
    dfswap.columns = [x.replace('L','T1_').replace('W','T2_') for x in list(dfswap.columns)]

    output = pd.concat([df, dfswap]).reset_index(drop=True)
    output.loc[output.location=='N','location'] = '0'
    output.loc[output.location=='H','location'] = '1'
    output.loc[output.location=='A','location'] = '-1'
    output.location = output.location.astype(int)
    
    output['PointDiff'] = output['T1_Score'] - output['T2_Score']
    
    return output

regular_data = prepare_data(regular_results)
tourney_data = prepare_data(tourney_results)

# Feature engineering!

regular_data.columns


boxscore_cols = ['T1_Score', 'T2_Score', 
        'T1_FGM', 'T1_FGA', 'T1_FGM3', 'T1_FGA3', 'T1_FTM', 'T1_FTA', 'T1_OR', 'T1_DR', 'T1_Ast', 'T1_TO', 'T1_Stl', 'T1_Blk', 'T1_PF', 
        'T2_FGM', 'T2_FGA', 'T2_FGM3', 'T2_FGA3', 'T2_FTM', 'T2_FTA', 'T2_OR', 'T2_DR', 'T2_Ast', 'T2_TO', 'T2_Stl', 'T2_Blk', 'T2_PF', 
        'PointDiff']

boxscore_cols = [
        'T1_FGM', 'T1_FGA', 'T1_OR', 'T1_Ast', 'T1_TO', 'T1_Stl', 'T1_PF', 'T1_FTM', 'T2_FTM', 'T2_FGM', 'T2_FGA', 
        'T2_OR', 'T2_Ast', 'T2_TO', 'T2_Stl', 'T2_Blk', 'T1_Score', 'T2_Score', 'PointDiff',
        'T1_EFFG', 'T1_EFFG3', 'T1_DARE', 'T1_TOQUETOQUE', 'T2_EFFG', 'T2_EFFG3', 'T2_DARE', 'T2_TOQUETOQUE']

boxscore_cols = ['T1_Score', 'T2_Score', 
        'T1_FGM', 'T1_FGA', 'T1_FGM3', 'T1_FGA3', 'T1_FTM', 'T1_FTA', 'T1_OR', 'T1_DR', 'T1_Ast', 'T1_TO', 'T1_Stl', 'T1_Blk', 'T1_PF', 
        'T2_FGM', 'T2_FGA', 'T2_FGM3', 'T2_FGA3', 'T2_FTM', 'T2_FTA', 'T2_OR', 'T2_DR', 'T2_Ast', 'T2_TO', 'T2_Stl', 'T2_Blk', 'T2_PF', 
        'T1_EFFG', 'T1_EFFG3', 'T1_DARE', 'T1_TOQUETOQUE', 'T2_EFFG', 'T2_EFFG3', 'T2_DARE', 'T2_TOQUETOQUE']

# After my analysis
#boxscore_cols = ['PointDiff', 'T1_Blk', 'T2_Blk', 'T1_Ast', 'T2_Ast', 'T1_Stl', 'T2_Stl', 'T1_FGA', 
#                 'T2_FGA', 'T1_FGM', 'T2_FGM', 'T1_DR', 'T2_DR', 'T1_Score', 'T2_Score']


# Choose a function to aggregate
funcs = [np.mean]


season_statistics = regular_data.groupby(["Season", 'T1_TeamID'])[boxscore_cols].agg(funcs).reset_index()
season_statistics.columns = [''.join(col).strip() for col in season_statistics.columns.values]
#Make two copies of the data
season_statistics_T1 = season_statistics.copy()
season_statistics_T2 = season_statistics.copy()

season_statistics_T1.columns = ["T1_" + x.replace("T1_","").replace("T2_","opponent_") for x in list(season_statistics_T1.columns)]
season_statistics_T2.columns = ["T2_" + x.replace("T1_","").replace("T2_","opponent_") for x in list(season_statistics_T2.columns)]
season_statistics_T1.columns.values[0] = "Season"
season_statistics_T2.columns.values[0] = "Season"


tourney_data = tourney_data[['Season', 'DayNum', 'T1_TeamID', 'T1_Score', 'T2_TeamID' ,'T2_Score']]
tourney_data.head()

tourney_data = pd.merge(tourney_data, season_statistics_T1, on = ['Season', 'T1_TeamID'], how = 'left')
tourney_data = pd.merge(tourney_data, season_statistics_T2, on = ['Season', 'T2_TeamID'], how = 'left')
# Notice that there are Team 1 statistics, team 1 opponent's statistics, team 2 statistics and team 2 opponent statistics
tourney_data.head()

# Cut the opponent columns that I don't want
#opplist = [opp for opp in tourney_data.columns if '_opponent_' in opp]
#todelete = [opp for opp in opplist if 'Blk' not in opp]
#tourney_data.drop(todelete, axis = 1, inplace = True)
#tourney_data.head()


# These statistics are created because in the last 2 weeks some stuff may happen (injuries just before the tournament and such)
#last14days_stats_T1 = regular_data.loc[regular_data.DayNum>118].reset_index(drop=True)
#last14days_stats_T1['win'] = np.where(last14days_stats_T1['PointDiff']>0,1,0)
#last14days_stats_T1 = last14days_stats_T1.groupby(['Season','T1_TeamID'])['win'].mean().reset_index(name='T1_win_ratio_14d')

#last14days_stats_T2 = regular_data.loc[regular_data.DayNum>118].reset_index(drop=True)
#last14days_stats_T2['win'] = np.where(last14days_stats_T2['PointDiff']<0,1,0)
#last14days_stats_T2 = last14days_stats_T2.groupby(['Season','T2_TeamID'])['win'].mean().reset_index(name='T2_win_ratio_14d')

#tourney_data = pd.merge(tourney_data, last14days_stats_T1, on = ['Season', 'T1_TeamID'], how = 'left')
#tourney_data = pd.merge(tourney_data, last14days_stats_T2, on = ['Season', 'T2_TeamID'], how = 'left')

# Extract the teams that make it to the tournament and see how they do with respect to the others
regular_season_effects = regular_data[['Season','T1_TeamID','T2_TeamID','PointDiff']].copy()
regular_season_effects['T1_TeamID'] = regular_season_effects['T1_TeamID'].astype(str)
regular_season_effects['T2_TeamID'] = regular_season_effects['T2_TeamID'].astype(str)
regular_season_effects['win'] = np.where(regular_season_effects['PointDiff']>0,1,0)
march_madness = pd.merge(seeds[['Season','TeamID']],seeds[['Season','TeamID']],on='Season')
march_madness.columns = ['Season', 'T1_TeamID', 'T2_TeamID']
march_madness.T1_TeamID = march_madness.T1_TeamID.astype(str)
march_madness.T2_TeamID = march_madness.T2_TeamID.astype(str)
regular_season_effects = pd.merge(regular_season_effects, march_madness, on = ['Season','T1_TeamID','T2_TeamID'])
regular_season_effects.shape

## Team Quality

def normalize_column(values):
  themean = np.mean(values)
  thestd = np.std(values)
  norm = (values - themean)/(thestd) 
  return(pd.DataFrame(norm))

def team_quality(season):
    formula = 'win~-1+T1_TeamID+T2_TeamID'
    glm = sm.GLM.from_formula(formula=formula, 
                              data=regular_season_effects.loc[regular_season_effects.Season==season,:], 
                              family=sm.families.Binomial()).fit()
    quality = pd.DataFrame(glm.params).reset_index()
    quality.columns = ['TeamID','quality']
    quality['Season'] = season
    quality['quality'] = normalize_column(quality['quality'])
    quality['quality'] = np.exp(quality['quality'])
    quality = quality.loc[quality.TeamID.str.contains('T1_')].reset_index(drop=True)
    quality['TeamID'] = quality['TeamID'].apply(lambda x: x[10:14]).astype(int)
    print(quality['quality'].mean(), quality['quality'].std())
    return quality

# This is metric to measure the team's strength, in this case, this is a logistic regression and we
# the coefficients
glm_quality = pd.concat([team_quality(2003),
                         team_quality(2004),
                         team_quality(2005),
                         team_quality(2006),
                         team_quality(2007),
                         team_quality(2008),
                         team_quality(2009),
                         team_quality(2010),
                         team_quality(2011),
                         team_quality(2012),
                         team_quality(2013),
                         team_quality(2014),
                         team_quality(2015),
                         team_quality(2016),
                         team_quality(2017),
                         team_quality(2018),
                         team_quality(2019),
                         team_quality(2021),
                         team_quality(2022),
                         team_quality(2023)]).reset_index(drop=True)

glm_quality_T1 = glm_quality.copy()
glm_quality_T2 = glm_quality.copy()
glm_quality_T1.columns = ['T1_TeamID','T1_quality','Season']
glm_quality_T2.columns = ['T2_TeamID','T2_quality','Season']

tourney_data = pd.merge(tourney_data, glm_quality_T1, on = ['Season', 'T1_TeamID'], how = 'left')
tourney_data = pd.merge(tourney_data, glm_quality_T2, on = ['Season', 'T2_TeamID'], how = 'left')
tourney_data['T1_quality'].fillna(0.2, inplace = True)
tourney_data['T2_quality'].fillna(0.2, inplace = True)
tourney_data.T2_quality.isnull().sum()

seeds['seed'] = seeds['Seed'].apply(lambda x: int(x[1:3]))
seeds.head()

seeds_T1 = seeds[['Season','TeamID','seed']].copy()
seeds_T2 = seeds[['Season','TeamID','seed']].copy()
seeds_T1.columns = ['Season','T1_TeamID','T1_seed']
seeds_T2.columns = ['Season','T2_TeamID','T2_seed']

tourney_data = pd.merge(tourney_data, seeds_T1, on = ['Season', 'T1_TeamID'], how = 'left')
tourney_data = pd.merge(tourney_data, seeds_T2, on = ['Season', 'T2_TeamID'], how = 'left')
#Optional but not relevant
tourney_data["Seed_diff"] = tourney_data["T1_seed"] - tourney_data["T2_seed"]

massey = pd.read_csv('/kaggle/input/march-machine-learning-mania-2023/MMasseyOrdinals_thru_Season2023_Day128.csv')

# RANKINGS AVAILABLE
massey[massey.RankingDayNum == 128].SystemName.unique()


bagofRanks = dict()
#oldtoconsider = ['WLK']
trafalgars = ['WLK', 'SAG', 'POM', 'COL', 'DOL', 'MOR', 'RTH', 'WOL', 'ATP', 'EMK', 'DWH', 'AP']
for traf in trafalgars:
    bagofRanks[traf] = massey[(massey['SystemName']==traf) & (massey['RankingDayNum']==128)]
    traf_T1 = bagofRanks[traf][['Season','TeamID','OrdinalRank']].copy()
    traf_T2 = bagofRanks[traf][['Season','TeamID','OrdinalRank']].copy()
    traf_T1.columns = ['Season','T1_TeamID','T1_OR_' + traf]
    traf_T2.columns = ['Season','T2_TeamID','T2_OR_' + traf]
    tourney_data = pd.merge(tourney_data, traf_T1, on = ['Season', 'T1_TeamID'], how = 'left')
    tourney_data = pd.merge(tourney_data, traf_T2, on = ['Season', 'T2_TeamID'], how = 'left')
    tourney_data[traf + "_diff"] = tourney_data["T1_OR_" + traf] - tourney_data["T2_OR_" + traf]
    tourney_data.drop(["T2_OR_" + traf], axis = 1, inplace = True)

# Time to build some models!

# The descriptive feature is the score, not the winner
y = tourney_data['T1_Score'] - tourney_data['T2_Score']
y.describe()

# Last chance to drop a couple of features:
tourney_data.drop(['T1_OR_POM', 'T1_OR_RTH', 'T1_OR_WLK', 'T1_OR_COL', 'T1_OR_WOL', 'T1_OR_MOR'], axis = 1, inplace = True)
# Drop own efficiency and OR - Curiously the opponent efficiency IS important. - Because we effectively damage it?
tourney_data.drop(['T1_EFFGmean', 'T2_EFFGmean', 'T1_ORmean', 'T2_ORmean'], axis = 1, inplace = True)
# This opponent data just seems to always be insignificant
tourney_data.drop(['T1_opponent_Stlmean', 'T2_opponent_Stlmean', 'T1_opponent_Astmean', 'T2_opponent_Astmean', 'T1_opponent_Scoremean', 'T2_opponent_Scoremean', 'T1_opponent_FGMmean', 'T2_opponent_FGMmean'], axis = 1, inplace = True)
features = tourney_data.columns[6:]
# Drop the next ones from the features but not from the dataframe
features.drop(['T2_seed'])
len(features)


X = tourney_data[features].values
dtrain = xgb.DMatrix(X, label = y)


# #Run the feature experiment to see their importance
# from sklearn.model_selection import train_test_split 
# from sklearn.ensemble import RandomForestClassifier
# X = tourney_data[features] 
# X['random_1'] = np.random.normal(0.0, 1.0, X.shape[0]) 
# X['random_2'] = np.random.normal(0.0, 1.0, X.shape[0]) 
# X['random_3'] = np.random.normal(0.0, 1.0, X.shape[0]) 
# def imp_df(column_names, importances):
#     df = pd.DataFrame({'feature':column_names, 'feature_importance': importances}).sort_values('feature_importance', ascending = False).reset_index(drop = True)
#     return(df)

# myfeatures = dict() 
# for f in list(features) + ['random_1', 'random_2', 'random_3']: 
#     myfeatures[f] = list()
    
# for md in range(5,9): 
#     for n_estimators in [50, 55, 65, 75, 100]: 
#         for rs in range(6): 
#             clf = RandomForestClassifier(max_depth=md,n_estimators=n_estimators, random_state=rs) 
#             clf.fit(X, y) 
#             perm = PermutationImportance(clf, cv = None, refit = False, n_iter = 10).fit(X, y) 
#             perm_imp_eli5 = imp_df(X.columns, perm.feature_importances_) 
#             for c, f in enumerate([i for i in perm_imp_eli5['feature']]): 
#                 myfeatures[f].append(c) 
#             print('where is:', md, n_estimators, rs) 
#             print([i for i in perm_imp_eli5['feature']])
                    
# for f in list(features) + ['random_1', 'random_2', 'random_3']: 
#     print(f, myfeatures[f], max(myfeatures[f]), np.mean(myfeatures[f]), min(myfeatures[f]))

# Loss function


def cauchyobj(preds, dtrain):
    labels = dtrain.get_label()
    c = 5000 
    x =  preds-labels    
    grad = x / (x**2/c**2+1)
    hess = -c**2*(x**2-c**2)/(x**2+c**2)**2
    return grad, hess

param = {} 
#param['objective'] = 'reg:linear'
param['eval_metric'] =  'mae'
param['booster'] = 'gbtree'
param['eta'] = 0.02 #recommend change to ~0.02 for final run. Higher when debugging.
param['subsample'] = 0.35
param['colsample_bytree'] = 0.7
param['num_parallel_tree'] = 3 #recommend 10. Write 3 for debugging.
param['min_child_weight'] = 40
param['gamma'] = 10
param['max_depth'] =  3
param['silent'] = 1

print(param)

xgb_cv = []
repeat_cv = 4 # recommend 10 for final submission. Smaller for debugging.

for i in range(repeat_cv): 
    print(f"Fold repeater {i}")
    xgb_cv.append(
        xgb.cv(
          params = param,
          dtrain = dtrain,
          obj = cauchyobj,
          num_boost_round = 3000,
          folds = KFold(n_splits = 5, shuffle = True, random_state = i),
          early_stopping_rounds = 25,
          verbose_eval = 50
        )
    )

iteration_counts = [np.argmin(x['test-mae-mean'].values) for x in xgb_cv]
val_mae = [np.min(x['test-mae-mean'].values) for x in xgb_cv]
iteration_counts, val_mae

#This is to get out-of-fold predictions
oof_preds = []
for i in range(repeat_cv):
    print(f"Fold repeater {i}")
    preds = y.copy()
    kfold = KFold(n_splits = 5, shuffle = True, random_state = i)    
    for train_index, val_index in kfold.split(X,y):
        dtrain_i = xgb.DMatrix(X[train_index], label = y[train_index])
        dval_i = xgb.DMatrix(X[val_index], label = y[val_index])  
        model = xgb.train(
              params = param,
              dtrain = dtrain_i,
              num_boost_round = iteration_counts[i],
              verbose_eval = 50
        )
        preds[val_index] = model.predict(dval_i)
    oof_preds.append(np.clip(preds,-19,19))

plot_df = pd.DataFrame({"pred":oof_preds[0], "label":np.where(y>0,1,0)})
plot_df["pred_int"] = plot_df["pred"].astype(int)
plot_df = plot_df.groupby('pred_int')['label'].mean().reset_index(name='average_win_pct')

plt.figure()
plt.plot(plot_df.pred_int,plot_df.average_win_pct)


spline_model = []

for i in range(repeat_cv):
    dat = list(zip(oof_preds[i],np.where(y>0,1,0)))
    dat = sorted(dat, key = lambda x: x[0])
    datdict = {}
    for k in range(len(dat)):
        datdict[dat[k][0]]= dat[k][1]
        
    spline_model.append(UnivariateSpline(list(datdict.keys()), list(datdict.values())))
    spline_fit = spline_model[i](oof_preds[i])
    
    print(f"logloss of cvsplit {i}: {log_loss(np.where(y>0,1,0),spline_fit)}") 

plot_df = pd.DataFrame({"pred":oof_preds[0], "label":np.where(y>0,1,0), "spline":spline_model[0](oof_preds[0])})
plot_df["pred_int"] = (plot_df["pred"]).astype(int)
plot_df = plot_df.groupby('pred_int')['spline','label'].mean().reset_index()

plt.figure()
plt.plot(plot_df.pred_int,plot_df.spline)
plt.plot(plot_df.pred_int,plot_df.label)

spline_model = []

for i in range(repeat_cv):
    dat = list(zip(oof_preds[i],np.where(y>0,1,0)))
    dat = sorted(dat, key = lambda x: x[0])
    datdict = {}
    for k in range(len(dat)):
        datdict[dat[k][0]]= dat[k][1]
    spline_model.append(UnivariateSpline(list(datdict.keys()), list(datdict.values())))
    spline_fit = spline_model[i](oof_preds[i])
    spline_fit = np.clip(spline_fit,0.025,0.975)
    
    print(f"adjusted logloss of cvsplit {i}: {log_loss(np.where(y>0,1,0),spline_fit)}") 

spline_model = []

for i in range(repeat_cv):
    dat = list(zip(oof_preds[i],np.where(y>0,1,0)))
    dat = sorted(dat, key = lambda x: x[0])
    datdict = {}
    for k in range(len(dat)):
        datdict[dat[k][0]]= dat[k][1]
    spline_model.append(UnivariateSpline(list(datdict.keys()), list(datdict.values())))
    spline_fit = spline_model[i](oof_preds[i])
    spline_fit = np.clip(spline_fit,0.02,0.98)
    spline_fit[(tourney_data.T1_seed==1) & (tourney_data.T2_seed==16)] = 1.0
    spline_fit[(tourney_data.T1_seed==16) & (tourney_data.T2_seed==1)] = 0.0
    
    print(f"adjusted logloss of cvsplit {i}: {log_loss(np.where(y>0,1,0),spline_fit)}") 

#looking for upsets
pd.concat(
    [tourney_data[(tourney_data.T1_seed==1) & (tourney_data.T2_seed==16) & (tourney_data.T1_Score < tourney_data.T2_Score)],
     tourney_data[(tourney_data.T1_seed==2) & (tourney_data.T2_seed==15) & (tourney_data.T1_Score < tourney_data.T2_Score)],
     tourney_data[(tourney_data.T1_seed==16) & (tourney_data.T2_seed==1) & (tourney_data.T1_Score > tourney_data.T2_Score)],
     tourney_data[(tourney_data.T1_seed==15) & (tourney_data.T2_seed==2) & (tourney_data.T1_Score > tourney_data.T2_Score)]]
)   

spline_model = []

for i in range(repeat_cv):
    dat = list(zip(oof_preds[i],np.where(y>0,1,0)))
    dat = sorted(dat, key = lambda x: x[0])
    datdict = {}
    for k in range(len(dat)):
        datdict[dat[k][0]]= dat[k][1]
    spline_model.append(UnivariateSpline(list(datdict.keys()), list(datdict.values())))
    spline_fit = spline_model[i](oof_preds[i])
    spline_fit = np.clip(spline_fit,0.025,0.975)
    spline_fit[(tourney_data.T1_seed==1) & (tourney_data.T2_seed==16) & (tourney_data.T1_Score > tourney_data.T2_Score)] = 1.0
    spline_fit[(tourney_data.T1_seed==2) & (tourney_data.T2_seed==15) & (tourney_data.T1_Score > tourney_data.T2_Score)] = 1.0
    spline_fit[(tourney_data.T1_seed==3) & (tourney_data.T2_seed==14) & (tourney_data.T1_Score > tourney_data.T2_Score)] = 1.0
    spline_fit[(tourney_data.T1_seed==16) & (tourney_data.T2_seed==1) & (tourney_data.T1_Score < tourney_data.T2_Score)] = 0.0
    spline_fit[(tourney_data.T1_seed==15) & (tourney_data.T2_seed==2) & (tourney_data.T1_Score < tourney_data.T2_Score)] = 0.0
    spline_fit[(tourney_data.T1_seed==14) & (tourney_data.T2_seed==3) & (tourney_data.T1_Score < tourney_data.T2_Score)] = 0.0
    
    print(f"adjusted logloss of cvsplit {i}: {log_loss(np.where(y>0,1,0),spline_fit)}") 

val_cv = []
spline_model = []

for i in range(repeat_cv):
    dat = list(zip(oof_preds[i],np.where(y>0,1,0)))
    dat = sorted(dat, key = lambda x: x[0])
    datdict = {}
    for k in range(len(dat)):
        datdict[dat[k][0]]= dat[k][1]
    spline_model.append(UnivariateSpline(list(datdict.keys()), list(datdict.values())))
    spline_fit = spline_model[i](oof_preds[i])
    spline_fit = np.clip(spline_fit,0.02,0.98)
    spline_fit[(tourney_data.T1_seed==1) & (tourney_data.T2_seed==16) & (tourney_data.T1_Score > tourney_data.T2_Score)] = 1.0
    spline_fit[(tourney_data.T1_seed==16) & (tourney_data.T2_seed==1) & (tourney_data.T1_Score < tourney_data.T2_Score)] = 0.0
    spline_fit[(tourney_data.T1_seed==2) & (tourney_data.T2_seed==15) & (tourney_data.T1_Score > tourney_data.T2_Score)] = 1.0
    spline_fit[(tourney_data.T1_seed==15) & (tourney_data.T2_seed==2) & (tourney_data.T1_Score < tourney_data.T2_Score)] = 0.0
    
    val_cv.append(pd.DataFrame({"y":np.where(y>0,1,0), "pred":spline_fit, "season":tourney_data.Season}))
    print(f"adjusted logloss of cvsplit {i}: {log_loss(np.where(y>0,1,0),spline_fit)}") 
    
val_cv = pd.concat(val_cv)
val_cv.groupby('season').apply(lambda x: log_loss(x.y, x.pred))

# Submission time!

sub = pd.read_csv('/kaggle/input/march-machine-learning-mania-2023/SampleSubmission2023.csv')
sub.head()

sub.shape

sub["Season"] = sub["ID"].apply(lambda x: x[0:4]).astype(int)
sub["T1_TeamID"] = sub["ID"].apply(lambda x: x[5:9]).astype(int)
sub["T2_TeamID"] = sub["ID"].apply(lambda x: x[10:14]).astype(int)
sub.shape

sub = pd.merge(sub, season_statistics_T1, on = ['Season', 'T1_TeamID'], how = 'left')
sub = pd.merge(sub, season_statistics_T2, on = ['Season', 'T2_TeamID'], how = 'left')
print(sub.shape)
sub = pd.merge(sub, glm_quality_T1, on = ['Season', 'T1_TeamID'], how = 'left') # This is because some teams didn't face off in the regular season
sub = pd.merge(sub, glm_quality_T2, on = ['Season', 'T2_TeamID'], how = 'left')
print(sub.shape)
sub = pd.merge(sub, seeds_T1, on = ['Season', 'T1_TeamID'], how = 'left')
sub = pd.merge(sub, seeds_T2, on = ['Season', 'T2_TeamID'], how = 'left')
print(sub.shape)
#sub = pd.merge(sub, last14days_stats_T1, on = ['Season', 'T1_TeamID'])
#sub = pd.merge(sub, last14days_stats_T2, on = ['Season', 'T2_TeamID'])
#print(sub.shape)
sub["Seed_diff"] = sub["T1_seed"] - sub["T2_seed"]
sub.shape
for traf in trafalgars:
    traf_T1 = bagofRanks[traf][['Season','TeamID','OrdinalRank']].copy()
    traf_T2 = bagofRanks[traf][['Season','TeamID','OrdinalRank']].copy()
    traf_T1.columns = ['Season','T1_TeamID','T1_OR_' + traf]
    traf_T2.columns = ['Season','T2_TeamID','T2_OR_' + traf]
    sub = pd.merge(sub, traf_T1, on = ['Season', 'T1_TeamID'], how = 'left')
    sub = pd.merge(sub, traf_T2, on = ['Season', 'T2_TeamID'], how = 'left')
    sub[traf + "_diff"] = sub["T1_OR_" + traf] - sub["T2_OR_" + traf]
sub.shape

sub.columns

sub.head()
print(sub.T2_quality.isnull().sum())
sub['T1_quality'].fillna(0.2, inplace = True)
sub['T2_quality'].fillna(0.2, inplace = True)
sub.T2_quality.isnull().sum()

Xsub = sub[features].values
dtest = xgb.DMatrix(Xsub)

sub_models = []
for i in range(repeat_cv):
    print(f"Fold repeater {i}")
    sub_models.append(
        xgb.train(
          params = param,
          dtrain = dtrain,
          num_boost_round = int(iteration_counts[i] * 1.05),
          verbose_eval = 50
        )
    )

sub_preds = []
for i in range(repeat_cv):
    sub_preds.append(np.clip(spline_model[i](np.clip(sub_models[i].predict(dtest),-30,30)),0.025,0.975))
    
sub["Pred"] = pd.DataFrame(sub_preds).mean(axis=0)
#sub["Pred"] /= (2*sub["Pred"].mean())
sub.loc[sub['Pred'] > 0.5, "Pred"] *= 1.00 # This is calibrated by trial and error.
sub.loc[sub['Pred'] < 0.5, "Pred"] /= 1.00
print(sub['Pred'].mean())


# Remove this before you submit on the other competition
import statistics
val = statistics.mode(sub.Pred)
sub.loc[val == sub.Pred, 'Pred'] = 0.5
statistics.mode(sub.Pred)

#sub.loc[(sub.T1_seed==2) & (sub.T2_seed==15), 'Pred'] = 0.99
#sub.loc[(sub.T1_seed==3) & (sub.T2_seed==14), 'Pred'] = 0.99
#sub.loc[(sub.T1_seed==15) & (sub.T2_seed==2), 'Pred'] = 0.01
#sub.loc[(sub.T1_seed==14) & (sub.T2_seed==3), 'Pred'] = 0.01
#sub.loc[(sub.T1_seed==1) & (sub.T2_seed==16), 'Pred'] = 0.99
#sub.loc[(sub.T1_seed==16) & (sub.T2_seed==1), 'Pred'] = 0.01
sub[['ID','Pred']].to_csv("MarchMen.csv", index = None)

#tourney_results2018 = pd.read_csv('../input/NCAA_2018_Solution_Womens.csv')
#tourney_results2018 = tourney_results2018[tourney_results2018.Pred!=-1].reset_index(drop=True)
#tourney_results2018.columns = ['ID', 'label']
#tourney_results2018 = pd.merge(tourney_results2018, sub, on = 'ID')
#log_loss(tourney_results2018.label, tourney_results2018.Pred)