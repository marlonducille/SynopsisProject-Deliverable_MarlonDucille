
setwd("D:/DataAnalysis/Synopsis Project 2/Data/DetailedPrescribingInformation")

library(tidyverse)
library(dplyr)

df_pres_join <- NULL
df_presInfo <- list() # creates a list
df_presInfo_uniq < list()
listcsv <- dir(pattern = "*.csv") # creates the list of all the csv files in the directory
for (k in 1:length(listcsv)){
  df_presInfo[[k]] <- read.csv(listcsv[k], colClasses=c(BNF.Code = 'character', Practice.Name = 'character', PCO.Name = 'character', BNF.Description = 'character'))
  
  
  colnames(df_presInfo[[k]]) <- c('regional_office_name', 'regional_office_code', 'area_team_name', 'area_team_code',      
                                   'PCO_name', 'PCO_code', 'practice_name', 'practice_code', 'BNF_code', 'BNF_description', 'items', 
                                   'quantity', 'ADQ_usage', 'NIC',  'actual_cost' )
  
  filesplit <- strsplit(listcsv[k], ' ')
  month <- filesplit[[1]][1]
  year <- filesplit[[1]][2]
  
  df_presInfo[[k]]$date <- paste0('1 ', month, ' ', year )
  
  df_presInfo[[k]] <- df_presInfo[[k]]  %>% select('area_team_name', 'PCO_name', 'practice_name', 'BNF_code', 'BNF_description', 'date', 'actual_cost')
  
  df_presInfo_uniq[[k]] <- df_presInfo[[k]][!duplicated(df_presInfo[[k]][,c('area_team_name', 'PCO_name', 'practice_name', 'BNF_code', 'BNF_description')]),]

}


for (k in 1:length(listcsv)){
  
  if(k < length(listcsv)){
    df_pres_join <- left_join(df_presInfo_uniq[[k]], df_presInfo_uniq[[k+1]], by = c('area_team_name', 'PCO_name', 'practice_name', 'BNF_code', 'BNF_description'))
  }
  
}

df_pres_join <- na.omit(df_pres_join)

write.csv(df_pres_join, 'pres_final_pros.csv')










