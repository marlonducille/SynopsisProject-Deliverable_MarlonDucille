
library(tidyverse)
library(dplyr)
library(htmlwidgets); 
library(ggplot2)
library(plotly)
library(reshape)
library(leaflet)
library(leaflet.extras) 
library(htmlwidgets)
library(sf)
library(htmltools)



df_presInfo <- read.csv('Detailed_Prescribing_Information_Jun19.csv', colClasses=c(BNF.Code = 'character', Practice.Name = 'character'), fileEncoding="latin1")
df_bnfCodeInfo <- read.csv('BNF_Code_Information_2019.csv', colClasses=c(BNF.Presentation.Code = 'character'), fileEncoding="latin1")


#rename column names
colnames(df_presInfo) <- c('regional_office_name', 'regional_office_code', 'area_team_name', 'area_team_code',      
                           'PCO_name', 'PCO_code', 'practice_name', 'practice_code', 'BNF_code', 'BNF_description', 'items', 
                           'quantity', 'ADQ_usage', 'NIC',  'actual_cost', 'total', 'NIC_per_total' )


colnames(df_bnfCodeInfo) <- c('BNF_chapter', 'BNF_chapter_code', 'BNF_section', 'BNF_section_code',  'BNF_paragraph',  'BNF_paragraph_code',        
                              'BNF_subparagraph', 'BNF_subparagraph_code',  'BNF_chemical_substance', 'BNF_chemical_substance_code', 
                              'BNF_product', 'BNF_product_code', 'BNF_presentation', 'BNF_code')


df <- left_join(df_presInfo, df_bnfCodeInfo, by = "BNF_code")




df  <- df  %>% select('PCO_name', 'PCO_code', 'practice_name', 'BNF_chapter', 'BNF_chapter_code', 'BNF_section', 'BNF_paragraph', 'BNF_subparagraph', 'BNF_description', 
                      'BNF_product', 'BNF_product_code', 'items', 'quantity', 'ADQ_usage', 'NIC',  'actual_cost', 'total', 'NIC_per_total')


bnf_chapters <- levels(factor(df$BNF_chapter)); bnf_chapters

df_trafford <- df[which(df$PCO_code == '02A00'),]
df_trafford_ant_muc <- df_trafford[which(df_trafford$BNF_section == 'Antisecretory Drugs+Mucosal Protectants'),]

df_agg <- aggregate(as.numeric(as.character(df_trafford_ant_muc[, 18])) , list(df_trafford_ant_muc$practice_name, df_trafford_ant_muc$BNF_description), mean)
df_agg <- df_agg[order(df_agg$Group.1),]


med_strength <-  gsub(pattern = "\\d+",replacement = "_",x = df_agg$Group.2)
med_strength <- paste0(regmatches(df_agg$Group.2,regexpr(pattern = "\\d+",text = df_agg$Group.2)), 'mg')
med_strength <- as.data.frame(med_strength)

df_agg <- cbind(df_agg, med_strength)
df_agg <- data.frame(medical_centre = df_agg$Group.1, drugs = df_agg$Group.2, NIC_per_Total = df_agg$x, med_strength = med_strength)


df_agg <- df_agg[order(df_agg$NIC_per_Total),]

df_agg <- df_agg[which(df_agg$med_strength == '10mg' | df_agg$med_strength == '20mg' | df_agg$med_strength == '40mg' ),]

df_agg <- df_agg %>% select('drugs', 'NIC_per_Total')

#get unique drug name and their NIC/total
df_agg <- aggregate(as.numeric(as.character(df_agg[, 2])) , list(df_agg$drugs), mean)
df_agg <- df_agg[order(df_agg$NIC_per_Total),]

colnames(df_agg) <- c('drugs', 'NIC_per_Total')

## prepare for k-means

library(factoextra)

id <- seq.int(nrow(df_agg))
df_agg <- cbind(df_agg, id)

data <- df_agg[c('NIC_per_Total', 'id')] 
row.names(data) <- df_agg$drugs


#Before k-means clustering, we can compute some descriptive statistics:
  # http://www.sthda.com/english/wiki/print.php?id=236

  desc_stats <- data.frame(
    Min = apply(data, 2, min), # minimum
    Med = apply(data, 2, median), # median
    Mean = apply(data, 2, mean), # mean
    SD = apply(data, 2, sd), # Standard deviation
    Max = apply(data, 2, max) # Maximum
  )
desc_stats <- round(desc_stats, 1)
head(desc_stats)

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

# use 3 clusters

library(cluster)
km <- kmeans(data, centers = 4)


fviz_cluster(km, data = data)



#group_name <- c('expensive', 'cheap', 'reasonable' )

#clusplot(data, group_name[km$cluster], lines=0, shade = TRUE, color = TRUE, span = TRUE, 
    #     plotchar = TRUE, xlab = "NIC per Total", ylab = "Drugs ID", labels = 4,
    #     main = "Categorizing Drug Costs" )

drug_price_category <- cbind(df_agg[3], km$cluster)
drug_price_category$group_name <- group_name[km$cluster]


