

library(tidyverse)
library(dplyr)

# ------- Reading the data ------- 

df_presInfo_jul18 <- read.csv('Jul 2018 Detailed_Prescribing_Information.csv', colClasses=c(BNF.Code = 'character', Practice.Name = 'character', PCO.Name = 'character', BNF.Description = 'character'), fileEncoding="latin1")
df_presInfo_aug18 <- read.csv('Aug 2018 Detailed_Prescribing_Information.csv', colClasses=c(BNF.Code = 'character', Practice.Name = 'character', PCO.Name = 'character', BNF.Description = 'character'), fileEncoding="latin1")
df_presInfo_sep18 <- read.csv('Sep 2018 Detailed_Prescribing_Information.csv', colClasses=c(BNF.Code = 'character', Practice.Name = 'character', PCO.Name = 'character', BNF.Description = 'character'), fileEncoding="latin1")
df_presInfo_oct18 <- read.csv('Oct 2018 Detailed_Prescribing_Information.csv', colClasses=c(BNF.Code = 'character', Practice.Name = 'character', PCO.Name = 'character', BNF.Description = 'character'), fileEncoding="latin1")
df_presInfo_nov18 <- read.csv('Nov 2018 Detailed_Prescribing_Information.csv', colClasses=c(BNF.Code = 'character', Practice.Name = 'character', PCO.Name = 'character', BNF.Description = 'character'), fileEncoding="latin1")
df_presInfo_dec18 <- read.csv('Dec 2018 Detailed_Prescribing_Information.csv', colClasses=c(BNF.Code = 'character', Practice.Name = 'character', PCO.Name = 'character', BNF.Description = 'character'), fileEncoding="latin1")




colnames(df_presInfo_jul18) <- c('regional_office_name', 'regional_office_code', 'area_team_name', 'area_team_code',      
                           'PCO_name', 'PCO_code', 'practice_name', 'practice_code', 'BNF_code', 'BNF_description', 'items', 
                           'quantity', 'ADQ_usage', 'NIC',  'actual_cost' )

colnames(df_presInfo_aug18) <- c('regional_office_name', 'regional_office_code', 'area_team_name', 'area_team_code',      
                                 'PCO_name', 'PCO_code', 'practice_name', 'practice_code', 'BNF_code', 'BNF_description', 'items', 
                                 'quantity', 'ADQ_usage', 'NIC',  'actual_cost' )

colnames(df_presInfo_sep18) <- c('regional_office_name', 'regional_office_code', 'area_team_name', 'area_team_code',      
                                 'PCO_name', 'PCO_code', 'practice_name', 'practice_code', 'BNF_code', 'BNF_description', 'items', 
                                 'quantity', 'ADQ_usage', 'NIC',  'actual_cost' )

colnames(df_presInfo_oct18) <- c('regional_office_name', 'regional_office_code', 'area_team_name', 'area_team_code',      
                                 'PCO_name', 'PCO_code', 'practice_name', 'practice_code', 'BNF_code', 'BNF_description', 'items', 
                                 'quantity', 'ADQ_usage', 'NIC',  'actual_cost' )

colnames(df_presInfo_nov18) <- c('regional_office_name', 'regional_office_code', 'area_team_name', 'area_team_code',      
                                 'PCO_name', 'PCO_code', 'practice_name', 'practice_code', 'BNF_code', 'BNF_description', 'items', 
                                 'quantity', 'ADQ_usage', 'NIC',  'actual_cost' )

colnames(df_presInfo_dec18) <- c('regional_office_name', 'regional_office_code', 'area_team_name', 'area_team_code',      
                                 'PCO_name', 'PCO_code', 'practice_name', 'practice_code', 'BNF_code', 'BNF_description', 'items', 
                                 'quantity', 'ADQ_usage', 'NIC',  'actual_cost' )

#stored actualCost data in the month field
df_presInfo_jul18$Jul <- df_presInfo_jul18$actual_cost
df_presInfo_jul18$date <- 'Jul 2018'

df_presInfo_aug18$Aug <- df_presInfo_aug18$actual_cost
df_presInfo_aug18$date <- 'Aug 2018'

df_presInfo_sep18$Sep <- df_presInfo_sep18$actual_cost
df_presInfo_sep18$date <- 'Sep 2018'

df_presInfo_oct18$Oct <- df_presInfo_oct18$actual_cost
df_presInfo_oct18$date <- 'Oct 2018'

df_presInfo_nov18$Nov <- df_presInfo_nov18$actual_cost
df_presInfo_nov18$date <- 'Nov 2018'

df_presInfo_dec18$Dec <- df_presInfo_dec18$actual_cost
df_presInfo_dec18$date <- 'Dec 2018'

#--- select a subset of fields

df_presInfo_jul18 <- df_presInfo_jul18  %>% select('area_team_name', 'PCO_name', 'practice_name', 'BNF_code', 'BNF_description', 'Jul')

df_presInfo_aug18 <- df_presInfo_aug18  %>% select('area_team_name', 'PCO_name', 'practice_name', 'BNF_code', 'BNF_description', 'Aug')

df_presInfo_sep18 <- df_presInfo_sep18  %>% select('area_team_name', 'PCO_name', 'practice_name', 'BNF_code', 'BNF_description', 'Sep')

df_presInfo_oct18 <- df_presInfo_oct18  %>% select('area_team_name', 'PCO_name', 'practice_name', 'BNF_code', 'BNF_description', 'Oct')

df_presInfo_nov18 <- df_presInfo_nov18  %>% select('area_team_name', 'PCO_name', 'practice_name', 'BNF_code', 'BNF_description', 'Nov')

df_presInfo_dec18 <- df_presInfo_dec18  %>% select('area_team_name', 'PCO_name', 'practice_name', 'BNF_code', 'BNF_description', 'Dec')


# --- get unique data
df_presInfo_jul18_uniq <- df_presInfo_jul18[!duplicated(df_presInfo_jul18[,c('area_team_name', 'PCO_name', 'practice_name', 'BNF_code', 'BNF_description')]),]
df_presInfo_aug18_uniq <- df_presInfo_aug18[!duplicated(df_presInfo_aug18[,c('area_team_name', 'PCO_name', 'practice_name', 'BNF_code', 'BNF_description')]),]
df_presInfo_sep18_uniq <- df_presInfo_sep18[!duplicated(df_presInfo_sep18[,c('area_team_name', 'PCO_name', 'practice_name', 'BNF_code', 'BNF_description')]),]
df_presInfo_oct18_uniq <- df_presInfo_oct18[!duplicated(df_presInfo_oct18[,c('area_team_name', 'PCO_name', 'practice_name', 'BNF_code', 'BNF_description')]),]
df_presInfo_nov18_uniq <- df_presInfo_nov18[!duplicated(df_presInfo_nov18[,c('area_team_name', 'PCO_name', 'practice_name', 'BNF_code', 'BNF_description')]),]
df_presInfo_dec18_uniq <- df_presInfo_dec18[!duplicated(df_presInfo_dec18[,c('area_team_name', 'PCO_name', 'practice_name', 'BNF_code', 'BNF_description')]),]



# ----- merging datasets

df_pres_join <- left_join(df_presInfo_jul18_uniq, df_presInfo_aug18_uniq, by = c('area_team_name', 'PCO_name', 'practice_name', 'BNF_code', 'BNF_description'))
df_pres_join <- left_join(df_pres_join, df_presInfo_sep18_uniq, by = c('area_team_name', 'PCO_name', 'practice_name', 'BNF_code', 'BNF_description'))
df_pres_join <- left_join(df_pres_join, df_presInfo_oct18_uniq, by = c('area_team_name', 'PCO_name', 'practice_name', 'BNF_code', 'BNF_description'))
df_pres_join <- left_join(df_pres_join, df_presInfo_nov18_uniq, by = c('area_team_name', 'PCO_name', 'practice_name', 'BNF_code', 'BNF_description'))
df_pres_join <- left_join(df_pres_join, df_presInfo_dec18_uniq, by = c('area_team_name', 'PCO_name', 'practice_name', 'BNF_code', 'BNF_description'))

# ---- remove NA ----
df_pres_join <- na.omit(df_pres_join)

df_pres_trans <- transpose(df_pres_join)


## 
write.csv(df_pres_join, 'pres_final.csv')




library(reshape2)
library(reshape)
library(plyr)

pres_final <- read.csv('pres_final.csv')
pres_final_pros <- melt(pres_final, id=c('X','area_team_name', 'PCO_name', 'practice_name', 'BNF_code', 'BNF_description'))


pres_final_pros$variable <- mapvalues(pres_final_pros$variable, from = c('Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'), 
                                      to = c('01/07/2018', '01/08/2018', '01/09/2018', '01/10/2018', '01/11/2018', '01/12//2018'))



write.csv(pres_final_pros, 'pres_final_pros.csv')


