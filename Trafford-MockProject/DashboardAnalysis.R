
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
#df_trafford_ant_muc <- df_trafford[which(df_trafford$BNF_product == 'Lansoprazole' | df_trafford$BNF_product == 'Omeprazole' | df_trafford$BNF_product == 'Rabeprazole Sod'),]
df_trafford_ant_muc <- df_trafford[which(df_trafford$BNF_section == 'Antisecretory Drugs+Mucosal Protectants'),]
#df_trafford_ant_muc <- df_trafford[which(df_trafford$BNF_product == 'Lansoprazole'),]

#df_trafford_ant_muc_AllPractice <- df_trafford_ant_muc
#df_trafford_ant_muc <- df_trafford_ant_muc[which(df_trafford_ant_muc$practice_name == 'PARK MEDICAL PRACTICE' | df_trafford_ant_muc$practice_name == 'ALTRINCHAM MEDICAL PRACTICE' |
# df_trafford_ant_muc$practice_name == 'FIRSWAY HEALTH CENTRE'),]


df_agg <- aggregate(as.numeric(as.character(df_trafford_ant_muc[, 18])) , list(df_trafford_ant_muc$practice_name, df_trafford_ant_muc$BNF_description), mean)
df_agg <- df_agg[order(df_agg$Group.1),]


med_strength <-  gsub(pattern = "\\d+",replacement = "_",x = df_agg$Group.2)
med_strength <- paste0(regmatches(df_agg$Group.2,regexpr(pattern = "\\d+",text = df_agg$Group.2)), 'mg')
med_strength <- as.data.frame(med_strength)

df_agg <- cbind(df_agg, med_strength)
df_agg <- data.frame(medical_centre = df_agg$Group.1, drugs = df_agg$Group.2, NIC_per_Total = df_agg$x, med_strength = med_strength)


df_agg <- df_agg[order(df_agg$med_strength),]

dashboard <- write.csv(df_agg, "dashboard.csv")


