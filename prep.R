prep <- function(df){
  swap <- df
  swap_order<- c(
    'Season', 'DayNum', 'LTeamID', 'LScore', 'WTeamID', 'WScore', 'WLoc', 'NumOT', 
    'LFGM', 'LFGA', 'LFGM3', 'LFGA3', 'LFTM', 'LFTA', 'LOR', 'LDR', 'LAst', 'LTO', 'LStl', 'LBlk', 'LPF', 
    'WFGM', 'WFGA', 'WFGM3', 'WFGA3', 'WFTM', 'WFTA', 'WOR', 'WDR', 'WAst', 'WTO', 'WStl', 'WBlk', 'WPF')
  
  swap <- swap[, swap_order]
  
  swap$WLoc[swap$WLoc == 'H'] <- 'A.'
  swap$WLoc[swap$WLoc == 'A'] <- 'H'
  swap$WLoc[swap$WLoc == 'A.'] <- 'A'
  
  colnames(df)[7] <- 'location'
  colnames(swap)[7] <- 'location'
  
  names(df) <- sub('W', 'T1_', names(df))
  names(df) <- sub('L', 'T2_', names(df))
  names(swap) <- sub('L', 'T1_', names(swap))
  names(swap) <- sub('W', 'T2_', names(swap))
  
  output <- rbind(df, swap)
  
  output$location[output$location == 'H'] <- '1'
  output$location[output$location == 'A'] <- '-1'
  output$location[output$location == 'N'] <- '0'
  
  output$location <- as.integer(output$location)
  
  output$'PointDiff' <- output$'T1_Score' - output$'T2_Score'
  
  return(output)
}