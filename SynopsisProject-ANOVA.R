
library(tidyverse)
library(dplyr)

pres_final_pros <- read.csv('pres_final_pros.csv')

data <- read.csv('actual_cost by practice_name.csv')
colnames(data) <- c('actual_cost', 'practice_name')


lookup <- pres_final_pros %>% filter(practice_name %in% data$practice_name) 
df_aggr <- aggregate((lookup$value), list(lookup$practice_name), mean)
colnames(df_aggr) <- c('practice_name', 'mean')


shapiro.test(df_aggr$mean)


model <- aov(lookup$value ~ lookup$practice_name)
summary(model)




