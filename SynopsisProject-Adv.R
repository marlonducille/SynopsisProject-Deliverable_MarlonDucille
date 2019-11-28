
library(htmlwidgets); 
library(ggplot2)
library(plotly)
library(reshape)
library(tidyverse)
library(dplyr)

pres_final_pros <- read.csv('pres_final_pros.csv', colClasses=c(BNF_description = 'character'))

df_nexus <- pres_final_pros[which(pres_final_pros$practice_name == 'NEXUS HEALTH GROUP' & pres_final_pros$variable == '01/11/2018'),]

df_nexus_agg <- aggregate((df_nexus$value), list(df_nexus$BNF_code, df_nexus$BNF_description), mean)
colnames(df_nexus_agg) <- c('BNF_code', 'drug', 'mean_cost')
df_nexus_agg <- df_nexus_agg[order(df_nexus_agg$mean_cost),]


med_strength <- as.integer(paste0(regmatches(df_nexus_agg$drug, gregexpr("[[:digit:]]+", df_nexus_agg$drug))))
med_strength <- as.data.frame(med_strength)


df_nexus_agg <- cbind(df_nexus_agg, med_strength)
df_nexus_agg <- na.omit(df_nexus_agg)




library(plyr)

count_med_strength <- count(df_nexus_agg, "med_strength")
count_med_strength <- count_med_strength[order(count_med_strength$freq, decreasing = TRUE),]

###

df_nexus_agg_10mg <- df_nexus_agg[which(df_nexus_agg$med_strength == '10'),]

## --- read the bnf code information
df_bnfCodeInfo <- read.csv('BNF_Code_Information.csv', colClasses=c(BNF.Presentation.Code = 'character'), fileEncoding="latin1")
colnames(df_bnfCodeInfo) <- c('BNF_chapter', 'BNF_chapter_code', 'BNF_section', 'BNF_section_code',  'BNF_paragraph',  'BNF_paragraph_code',        
                              'BNF_subparagraph', 'BNF_subparagraph_code',  'BNF_chemical_substance', 'BNF_chemical_substance_code', 
                              'BNF_product', 'BNF_product_code', 'BNF_presentation', 'BNF_code')


df <- left_join(df_nexus_agg_10mg, df_bnfCodeInfo, by = "BNF_code")

df <- df %>% select('BNF_code', 'BNF_section', 'drug', 'mean_cost')


count_bnfsection <- count(df, BNF_section)
count_bnfsection <- count_bnfsection[order(count_bnfsection$n, decreasing = TRUE),]


## --- Analysis on Nit,Calc Block & Other Antianginal Drugs ----

df_drug <- df[which(df$BNF_section == 'Nit,Calc Block & Other Antianginal Drugs' | 
                                  df$BNF_section == 'Analgesics'),]



#### ------- CLUSTER ANALYSIS ---------- ####
library(factoextra)

df_clus <- df_drug %>% select('drug', 'mean_cost')
df_clus <- df_clus[order(df_clus$mean_cost),]

id <- seq.int(nrow(df_clus))
df_clus <- cbind(df_clus, id)


data <- df_clus[c('mean_cost', 'id')] 
row.names(data) <- df_clus$drug

#'''''''''''''''''''''''''''''''''''
#'Determine the optimal value for k
#'''''''''''''''''''''''''''''''''''
#'
set.seed(20)
n <- 10
wss <- numeric(n) #preolocate with zeros

for(k in 1:n) {
  k_means <- kmeans(data, centers = k)
  wss[k] <- sum(k_means$withinss)
  print(k_means)
}

plot(1:10, wss, type='b', xlab = "No of clusters", ylab = "WSS")

## number of clusters is 3

library(cluster)
km <- kmeans(data, centers = 3)

fviz_cluster(km, data = data)

drug_price_category <- cbind(data, km$cluster)

#clusplot(data, km$cluster, lines=0, shade = TRUE, color = TRUE, span = TRUE, 
    # plotchar = TRUE, xlab = "Mean Cost", ylab = "Drugs ID", labels = 3,
    # main = "Categorizing Drug Costs" )

########--------------------------------################


practices <- read.csv('actual_cost_by_practice_name_all.csv')
colnames(practices) <- c('practice_name', 'actual_cost')

practices <- practices[order(practices$actual_cost),]


id <- seq.int(nrow(practices))
practices <- cbind(practices, id)


data <- practices[c('actual_cost', 'id')] 
row.names(data) <- practices$practice_name


set.seed(20)
n <- 10
wss <- numeric(n) #preolocate with zeros

for(k in 1:n) {
  k_means <- kmeans(data, centers = k)
  wss[k] <- sum(k_means$withinss)
  print(k_means)
}

plot(1:10, wss, type='b', xlab = "No of clusters", ylab = "WSS")

library(cluster)
library(factoextra)
km <- kmeans(data, centers = 4)

fviz_cluster(km, data = data)

practice_category <- cbind(data, km$cluster)


