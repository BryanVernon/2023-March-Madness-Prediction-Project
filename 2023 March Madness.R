library(readr)
library(dplyr)
library(tidyverse)
library(sjmisc)
library(xgboost)
library(dismo)
library(lme4)

# Import data
mtourney <- read_csv("C:/Users/Bryan/Desktop/2023 March Madness Prediction Project/Data/MNCAATourneyDetailedResults.csv")
mreg <- read_csv("C:/Users/Bryan/Desktop/2023 March Madness Prediction Project/Data/MRegularSeasonDetailedResults.csv")
mseed <- read_csv("C:/Users/Bryan/Desktop/2023 March Madness Prediction Project/Data/MNCAATourneySeeds.csv")
wreg <- read_csv("C:/Users/Bryan/Desktop/2023 March Madness Prediction Project/Data/WRegularSeasonDetailedResults.csv")
wseed <- read_csv("C:/Users/Bryan/Desktop/2023 March Madness Prediction Project/Data/WNCAATourneySeeds.csv")
wtourney <- read_csv("C:/Users/Bryan/Desktop/2023 March Madness Prediction Project/Data/WNCAATourneyDetailedResults.csv")
sub<- read_csv("C:/Users/Bryan/Downloads/SampleSubmission2023.csv (1).zip")

# Double data
mreg2 <- prep(mreg)
mtourney2 <- prep(mtourney)
mtourney2<- dplyr::select(mtourney2,'Season','DayNum', "T1_TeamID", "T2_TeamID", "T1_Score", "T2_Score", "PointDiff")
mseed$Seed = as.numeric(substring(mseed$Seed,2,4))

# Extract march teams

march = dplyr::select(mseed, Season, Team = TeamID)
new =  mreg2 %>% 
  inner_join(march, by = c("Season" = "Season", "T1_TeamID" = "Team")) %>% 
  inner_join(march, by = c("Season" = "Season", "T2_TeamID" = "Team")) %>% 
  dplyr::select(Season, T1_TeamID, T2_TeamID, T1_Score, T2_Score, NumOT) %>% distinct()
new$T1_TeamID = as.factor(new$T1_TeamID)
new$T2_TeamID = as.factor(new$T2_TeamID)

# Fit GLMM on regular season data

quality = list()
for (season in unique(new$Season)) {
  glmm = glmer(I(T1_Score > T2_Score) ~  (1 | T1_TeamID) + (1 | T2_TeamID), data = new[new$Season == season & new$NumOT == 0, ], family = binomial) 
  random_effects = ranef(glmm)$T1_TeamID
  quality[[season]] = data.frame(Season = season, Team_Id = as.numeric(row.names(random_effects)), quality = exp(random_effects[,"(Intercept)"]))
}
quality = do.call(rbind, quality)

stats = 
  mreg2 %>%
  mutate(win14days = ifelse(DayNum > 118 & T1_Score > T2_Score, 1, 0),
         last14days = ifelse(DayNum > 118, 1, 0)) %>% 
  group_by(Season, T1_TeamID) %>%
  summarize(
    WinRatio14d = sum(win14days) / sum(last14days),
    PointsMean = mean(T1_Score),
    PointsMedian = median(T1_Score),
    PointsDiffMean = mean(T1_Score - T2_Score),
    FgaMean = mean(T1_FGA), 
    FgaMedian = median(T1_FGA),
    FgaMin = min(T1_FGA), 
    FgaMax = max(T1_FGA), 
    AstMean = mean(T1_Ast), 
    BlkMean = mean(T1_Blk), 
    OppFgaMean = mean(T2_FGA), 
    OppFgaMin = min(T2_FGA)  
  )

T1_stats = stats
T2_stats = stats
names(T1_stats) = c("Season", "T1_TeamID", paste0("T1_",names(T1_stats)[-c(1,2)]))
names(T2_stats) = c("Season", "T2_TeamID", paste0("T2_",names(T2_stats)[-c(1,2)]))


### Combine all features into a data frame

data_matrix =
  mtourney2 %>% 
  left_join(T1_stats, by = c("Season", "T1_TeamID")) %>% 
  left_join(T2_stats, by = c("Season", "T2_TeamID")) %>%
  left_join(dplyr::select(mseed, Season, T1_TeamID = TeamID, Seed1 = Seed), by = c("Season", "T1_TeamID")) %>% 
  left_join(dplyr::select(mseed, Season, T2_TeamID = TeamID, Seed2 = Seed), by = c("Season", "T2_TeamID")) %>% 
  mutate(SeedDiff = Seed1 - Seed2) %>%
  left_join(dplyr::select(quality, Season, T1_TeamID = Team_Id, quality_march_T1 = quality), by = c("Season", "T1_TeamID")) %>%
  left_join(dplyr::select(quality, Season, T2_TeamID = Team_Id, quality_march_T2 = quality), by = c("Season", "T2_TeamID"))

feature = setdiff(names(data_matrix), c("Season", "DayNum", "T1_TeamID", "T2_TeamID", "T1_Score", "T2_Score", "PointDiff"))
dtrain = xgb.DMatrix(as.matrix(data_matrix[, feature]), label = data_matrix$PointDiff)

xgb_parameters = 
  list(objective = cauchyobj, 
       eval_metric = "mae",
       booster = "gbtree", 
       eta = 0.02,
       subsample = 0.35,
       colsample_bytree = 0.7,
       num_parallel_tree = 10,
       min_child_weight = 40,
       gamma = 10,
       max_depth = 3)

N = nrow(data_matrix)
fold5list = c(
  rep( 1, floor(N/5) ),
  rep( 2, floor(N/5) ),
  rep( 3, floor(N/5) ),
  rep( 4, floor(N/5) ),
  rep( 5, N - 4*floor(N/5) )
)


### Build cross-validation model, repeated 10-times

iteration_count = c()
smooth_model = list()

for (i in 1:10) {
  
  ### Resample fold split
  set.seed(i)
  folds = list()  
  fold_list = sample(fold5list)
  for (k in 1:5) folds[[k]] = which(fold_list == k)
  
  set.seed(120)
  xgb_cv = 
    xgb.cv(
      params = xgb_parameters,
      data = dtrain,
      nrounds = 3000,
      verbose = 0,
      nthread = 12,
      folds = folds,
      early_stopping_rounds = 25,
      maximize = FALSE,
      prediction = TRUE
    )
  iteration_count = c(iteration_count, xgb_cv$best_iteration)
  
  ### Fit a smoothed GAM model on predicted result point differential to get probabilities
  smooth_model[[i]] = smooth.spline(x = xgb_cv$pred, y = ifelse(data_matrix$PointDiff > 0, 1, 0))
  
}


### Build submission models

submission_model = list()

for (i in 1:10) {
  set.seed(i)
  submission_model[[i]] = 
    xgb.train(
      params = xgb_parameters,
      data = dtrain,
      nrounds = round(iteration_count[i]*1.05),
      verbose = 0,
      nthread = 12,
      maximize = FALSE,
      prediction = TRUE
    )
}


### Run predictions

sub$Season = 2023
sub$T1_TeamID = as.numeric(substring(sub$ID,6,9))
sub$T2_TeamID = as.numeric(substring(sub$ID,11,14))

Z = sub %>% 
  left_join(T1_stats, by = c("Season", "T1_TeamID")) %>% 
  left_join(T2_stats, by = c("Season", "T2_TeamID")) %>%
  left_join(dplyr::select(mseed, Season, T1_TeamID = TeamID, Seed1 = Seed), by = c("Season", "T1_TeamID")) %>% 
  left_join(dplyr::select(mseed, Season, T2_TeamID = TeamID, Seed2 = Seed), by = c("Season", "T2_TeamID")) %>% 
  mutate(SeedDiff = Seed1 - Seed2) %>%
  left_join(dplyr::select(quality, Season, T1_TeamID = Team_Id, quality_march_T1 = quality), by = c("Season", "T1_TeamID")) %>%
  left_join(dplyr::select(quality, Season, T2_TeamID = Team_Id, quality_march_T2 = quality), by = c("Season", "T2_TeamID"))

dtest = xgb.DMatrix(as.matrix(Z[, feature]))

probs = list()
for (i in 1:10) {
  preds = predict(submission_model[[i]], dtest)
  probs[[i]] = predict(smooth_model[[i]], preds)$y
}
Z$Pred = Reduce("+", probs) / 10

### Better be safe than sorry
Z$Pred[Z$Pred <= 0.025] = 0.025
Z$Pred[Z$Pred >= 0.975] = 0.975

### Anomaly event happened only once before - be brave
Z$Pred[Z$Seed1 == 16 & Z$Seed2 == 1] = 0
Z$Pred[Z$Seed1 == 15 & Z$Seed2 == 2] = 0
Z$Pred[Z$Seed1 == 14 & Z$Seed2 == 3] = 0
Z$Pred[Z$Seed1 == 13 & Z$Seed2 == 4] = 0
Z$Pred[Z$Seed1 == 1 & Z$Seed2 == 16] = 1
Z$Pred[Z$Seed1 == 2 & Z$Seed2 == 15] = 1
Z$Pred[Z$Seed1 == 3 & Z$Seed2 == 14] = 1
Z$Pred[Z$Seed1 == 4 & Z$Seed2 == 13] = 1

write.csv(dplyr::select(Z, ID, Pred), "My_March_Madness_Probabilities.csv", row.names = FALSE)


