library(readr)
library(dplyr)

# Import data
mtourney <- read_csv("C:/Users/Bryan/Desktop/2023 March Madness Prediction Project/Data/MNCAATourneyDetailedResults.csv")
mreg <- read_csv("C:/Users/Bryan/Desktop/2023 March Madness Prediction Project/Data/MRegularSeasonDetailedResults.csv")
mseed <- read_csv("C:/Users/Bryan/Desktop/2023 March Madness Prediction Project/Data/MNCAATourneySeeds.csv")
wreg <- read_csv("C:/Users/Bryan/Desktop/2023 March Madness Prediction Project/Data/WRegularSeasonDetailedResults.csv")
wseed <- read_csv("C:/Users/Bryan/Desktop/2023 March Madness Prediction Project/Data/WNCAATourneySeeds.csv")
wtourney <- read_csv("C:/Users/Bryan/Desktop/2023 March Madness Prediction Project/Data/WNCAATourneyDetailedResults.csv")

swap <- mreg
colnames(swap)<- c(
  'Season', 'DayNum', 'LTeamID', 'LScore', 'WTeamID', 'WScore', 'WLoc', 'NumOT', 
  'LFGM', 'LFGA', 'LFGM3', 'LFGA3', 'LFTM', 'LFTA', 'LOR', 'LDR', 'LAst', 'LTO', 'LStl', 'LBlk', 'LPF', 
  'WFGM', 'WFGA', 'WFGM3', 'WFGA3', 'WFTM', 'WFTA', 'WOR', 'WDR', 'WAst', 'WTO', 'WStl', 'WBlk', 'WPF')

colnames(mreg)[7] <- 'location'
colnames(swap)[7] <- 'location'

names(swap) <- sub('L', 'T1_', names(swap))
names(swap) <- sub('W', 'T2_', names(swap))
names(mreg) <- sub('L', 'T2_', names(mreg))
names(mreg) <- sub('W', 'T1_', names(mreg))

new <- rbind(mreg, swap)

new$location[new$location == 'H'] <- '1'
new$location[new$location == 'A'] <- '-1'
new$location[new$location == 'N'] <- '0'

# index?

swap2 <- mtourney
colnames(swap2)<- c(
  'Season', 'DayNum', 'LTeamID', 'LScore', 'WTeamID', 'WScore', 'WLoc', 'NumOT', 
  'LFGM', 'LFGA', 'LFGM3', 'LFGA3', 'LFTM', 'LFTA', 'LOR', 'LDR', 'LAst', 'LTO', 'LStl', 'LBlk', 'LPF', 
  'WFGM', 'WFGA', 'WFGM3', 'WFGA3', 'WFTM', 'WFTA', 'WOR', 'WDR', 'WAst', 'WTO', 'WStl', 'WBlk', 'WPF')

colnames(mtourney)[7] <- 'location'
colnames(swap2)[7] <- 'location'

names(swap2) <- sub('L', 'T1_', names(swap2))
names(swap2) <- sub('W', 'T2_', names(swap2))
names(mtourney) <- sub('L', 'T2_', names(mtourney))
names(mtourney) <- sub('W', 'T1_', names(mtourney))

new2 <- rbind(mtourney, swap2)

# Assuming all locations are neutral during tournament

#index

new['PointDiff'] <- new$T1_Score - new$T2_Score 
new2['PointDiff'] <- new2$T1_Score - new2$T2_Score

stats <- aggregate(new, by = list(new$T1_TeamID, new$Season), FUN = 'mean')

stats <- select(stats,"Season", 'T1_TeamID','T1_FGM', 'T1_FGA', 'T1_FGM3', 'T1_FGA3', 'T1_OR', 'T1_Ast', 'T1_TO', 'T1_Stl', 'T1_PF', 
                'T2_FGM', 'T2_FGA', 'T2_FGM3', 'T2_FGA3', 'T2_OR', 'T2_Ast', 'T2_TO', 'T2_Stl', 'T2_Blk',  
                'PointDiff')

T1_stats <- stats
T2_stats <- stats

names(T1_stats) <- sub('T2_', 'T1_opponent_', names(T1_stats))
names(T2_stats) <- sub('T1_', 'T2_opponent_', names(T2_stats))
names(T1_stats) <- sub('PointDiff', 'T1_PointDiff', names(T1_stats))
names(T2_stats) <- sub('PointDiff', 'T2_PointDiff', names(T2_stats))
names(T2_stats) <- sub('T2_opponent_TeamID', 'T2_TeamID', names(T2_stats))

mtourney <- select(mtourney, 'Season', 'DayNum', 'T1_TeamID', 'T1_Score', 'T2_TeamID' ,'T2_Score')

mtourney <- left_join(mtourney, T1_stats, by = c('T1_TeamID', 'Season'))
mtourney <- left_join(mtourney, T2_stats, by = c('T2_TeamID', 'Season'))




