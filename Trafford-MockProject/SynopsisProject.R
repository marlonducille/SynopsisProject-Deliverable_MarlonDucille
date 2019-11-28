
#https://digital.nhs.uk/data-and-information/publications/statistical/prescription-cost-analysis/2018

#http://rmaps.github.io/blog/posts/animated-choropleths/  

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


df_agg <- aggregate(as.numeric(as.character(df_trafford_ant_muc[, 18])) , list(df_trafford_ant_muc$practice_name, 
                                                                               df_trafford_ant_muc$BNF_description), mean)



df_agg <- df_agg[order(df_agg$Group.1),]


med_strength <-  gsub(pattern = "\\d+",replacement = "_",x = df_agg$Group.2)
med_strength <- paste0(regmatches(df_agg$Group.2,regexpr(pattern = "\\d+",text = df_agg$Group.2)), 'mg')
med_strength <- as.data.frame(med_strength)

df_agg <- cbind(df_agg, med_strength)
df_agg <- data.frame(medical_centre = df_agg$Group.1, drugs = df_agg$Group.2, NIC_per_Total = df_agg$x, 
                     med_strength = med_strength)


df_agg <- df_agg[order(df_agg$med_strength),]


df_agg <- df_agg[which(df_agg$med_strength == '10mg'| df_agg$med_strength == '20mg' | df_agg$med_strength == '40mg' ),]

#visualise data
 #ggplot(df_agg, aes(x = drugs, y = NIC_per_Total, group = med_strength,  fill=as.factor(medical_centre))) + 
 # geom_col(position = "dodge", colour = "#757575", size = 0.2, alpha = 0.8) +
 # scale_x_continuous(breaks = seq(1992, 2007, 5), expand = c(0, 0)) +
 # scale_y_continuous(labels = scales::comma, expand = c(0, 0)) +
 # scale_fill_brewer(palette = "Set2") +
 # labs(title = "",
 #      subtitle = "Total population by continent, 1990-2007",
 #      caption = "Source: Gapminder.org  |  @traffordDataLab",
 #      x = NULL,
 #      y = NULL,
 #      fill = NULL) 
 # theme_lab() +
 # theme(panel.grid.major.x = element_blank(),
        #legend.position = "bottom")

df_agg <- df_agg[order(df_agg$NIC_per_Total),]

plot1 <- ggplot(data=df_agg, aes(x=medical_centre, y=NIC_per_Total, fill=drugs)) + 
  geom_bar(stat="identity") + 
  facet_grid(~med_strength) +
  theme(axis.text.x=element_blank())


plot1;

graph1 <- ggplotly(plot1, tooltip = c( 'drugs', 'NIC_per_Total', 'medical_centre')); graph1


#------------------------------------------------------

#http://rgraphgallery.blogspot.com/2013/04/rg-histogram-bar-chart-over-map.html
#https://www.r-bloggers.com/visualize-complex-data-with-subplots/
#https://blog.revolutionanalytics.com/2015/05/digging-up-embedded-plots.html
#https://en.it1352.com/article/0fdb22e6a0dc4fe79a05d5631c8e031a.html

df_practices <- read.csv('trafford_general_practices.csv', colClasses=c(name = 'character'), fileEncoding="latin1")
df_practices <- data.frame(toupper(df_practices$name), df_practices$lat, df_practices$lon)
colnames(df_practices) <- c('medical_centre', 'lat', 'lon')
df_practices$medical_centre <- as.character(df_practices$medical_centre)

df_agg <- left_join(df_agg, df_practices, by = "medical_centre")

bdy <- st_read("boundary.geojson")

sf <- st_as_sf(df_agg, crs = 4326, coords = c("lon", "lat"))
sf <- sf[order(sf$medical_centre),]


#plot graph
library(scales) ; library(mapview) ; library(leafpop) ; library(plainview)

plot <- function(m){ 
  m %>% map(function(m){
    filter(sf, medical_centre == m ) %>%
      ggplot(aes(x = factor(drugs), y = NIC_per_Total, group = medical_centre)) +
      geom_col(color = "#e7298a", size = 1) +
      geom_point(shape = 21, size = 3, colour = "white", fill = "#99998a") +
     # scale_y_continuous(expand = c(0.005, 0.005), limits = c(0, NA), labels = comma) +
      ylim(0, max(sf$NIC_per_Total)) +
      labs(x = NULL, y = "NIc per Total",
           title = "Prescription Comparison", 
           subtitle = m,
           caption = "Source: DfT") +
      theme_minimal() +
      theme(
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(color = "grey50", size = 8, hjust = 1, margin = margin(t = 15)),
        axis.title.x = element_text(size = 9, hjust = 1, margin = margin(t = 10)),
        axis.title.y = element_text(size = 9, angle = 90, hjust = 1, margin = margin(r = 10)),
        axis.text.x = element_text(angle = 90, hjust = 1, margin = margin(t = 0)))
  })
}

plot_10mg <- function(m){
  m %>% map(function(m){
    filter(sf, medical_centre == m & med_strength == "10mg") %>%
      ggplot(aes(x = factor(drugs), y = NIC_per_Total, group = medical_centre)) +
      geom_col(color = "#e7298a", size = 1) +
      geom_point(shape = 21, size = 3, colour = "white", fill = "#99998a") +
      ylim(0, max(sf$NIC_per_Total)) +
      #scale_y_continuous(expand = c(0.005, 0.005), limits = c(0, NA), labels = comma) +
      labs(x = NULL, y = "Number of vehicles",
           title = "Prescription Cost Comparision,
           subtitle = m,
           caption = "") +
      theme_minimal() +
      theme(
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(color = "grey50", size = 8, hjust = 1, margin = margin(t = 15)),
        axis.title.x = element_text(size = 9, hjust = 1, margin = margin(t = 10)),
        axis.title.y = element_text(size = 9, angle = 90, hjust = 1, margin = margin(r = 10)),
        axis.text.x = element_text(angle = 90, hjust = 1, margin = margin(t = 0)))
  })
}

# plot_15mg <- function(m) {
#   m %>% map(function(m){
#     filter(sf, medical_centre == m & med_strength == "15mg") %>%
#       ggplot(aes(x = factor(drugs), y = NIC_per_Total, group = medical_centre)) +
#       geom_col(color = "#e7298a", size = 1) +
#       geom_point(shape = 21, size = 3, colour = "white", fill = "#99998a") +
#      # scale_y_continuous(expand = c(0.005, 0.005), limits = c(0, NA), labels = comma) +
#       ylim(0, max(sf$NIC_per_Total)) +
#       labs(x = NULL, y = "Number of vehicles",
#            title = "Average annual daily flow", 
#            subtitle = m,
#            caption = "Source: DfT") +
#       theme_minimal() +
#       theme(
#         plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
#         panel.grid.major.x = element_blank(),
#         panel.grid.minor = element_blank(),
#         plot.caption = element_text(color = "grey50", size = 8, hjust = 1, margin = margin(t = 15)),
#         axis.title.x = element_text(size = 9, hjust = 1, margin = margin(t = 10)),
#         axis.title.y = element_text(size = 9, angle = 90, hjust = 1, margin = margin(r = 10)),
#         axis.text.x = element_text(angle = 90, hjust = 1, margin = margin(t = 0)))
#   })
# }

plot_20mg <- function(m) {
   m %>% map(function(m){
    filter(sf, medical_centre == m & med_strength == "20mg") %>%
      ggplot(aes(x = factor(drugs), y = NIC_per_Total, group = medical_centre)) +
      geom_col(color = "#e7298a", size = 1) +
      geom_point(shape = 21, size = 3, colour = "white", fill = "#99998a") +
    #  scale_y_continuous(expand = c(0.005, 0.005), limits = c(0, NA), labels = comma) +
       ylim(0, max(sf$NIC_per_Total)) +
      labs(x = NULL, y = "NIc per Total",
           title = "Prescription Cost Comparision", 
           subtitle = m,
           caption = "") +
      theme_minimal() +
      theme(
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(color = "grey50", size = 8, hjust = 1, margin = margin(t = 15)),
        axis.title.x = element_text(size = 9, hjust = 1, margin = margin(t = 10)),
        axis.title.y = element_text(size = 9, angle = 90, hjust = 1, margin = margin(r = 10)),
        axis.text.x = element_text(angle = 90, hjust = 1, margin = margin(t = 0)))
  })
}

# plot_30mg <- function(m) {
#   m %>% map(function(m){
#     filter(sf, medical_centre == m & med_strength == "30mg") %>%
#       ggplot(aes(x = factor(drugs), y = NIC_per_Total, group = medical_centre)) +
#       geom_col(color = "#e7298a", size = 1) +
#       geom_point(shape = 21, size = 3, colour = "white", fill = "#99998a") +
#       #scale_y_continuous(expand = c(0.005, 0.005), limits = c(0, NA), labels = comma) +
#       ylim(0, max(sf$NIC_per_Total)) +
#       labs(x = NULL, y = "Number of vehicles",
#            title = "Average annual daily flow", 
#            subtitle = m,
#            caption = "Source: DfT") +
#       theme_minimal() +
#       theme(
#         plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
#         panel.grid.major.x = element_blank(),
#         panel.grid.minor = element_blank(),
#         plot.caption = element_text(color = "grey50", size = 8, hjust = 1, margin = margin(t = 15)),
#         axis.title.x = element_text(size = 9, hjust = 1, margin = margin(t = 10)),
#         axis.title.y = element_text(size = 9, angle = 90, hjust = 1, margin = margin(r = 10)),
#         axis.text.x = element_text(angle = 90, hjust = 1, margin = margin(t = 0)))
#   })
# }

plot_40mg <- function(m) {
  m %>% map(function(m){
    filter(sf, medical_centre == m & med_strength == "40mg") %>%
      ggplot(aes(x = factor(drugs), y = NIC_per_Total, group = medical_centre)) +
      geom_col(color = "#e7298a", size = 1) +
      geom_point(shape = 21, size = 3, colour = "white", fill = "#99998a") +
     # scale_y_continuous(expand = c(0.005, 0.005), limits = c(0, NA), labels = comma) +
      ylim(0, max(sf$NIC_per_Total)) +
      labs(x = NULL, y = "NIc per Total",
           title = "Prescription Cost Comparision", 
           subtitle = m,
           caption = "") +
      theme_minimal() +
      theme(
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(color = "grey50", size = 8, hjust = 1, margin = margin(t = 15)),
        axis.title.x = element_text(size = 9, hjust = 1, margin = margin(t = 10)),
        axis.title.y = element_text(size = 9, angle = 90, hjust = 1, margin = margin(r = 10)),
        axis.text.x = element_text(angle = 90, hjust = 1, margin = margin(t = 0)))
  })
}

# plot_75mg <- function(m) {
#   m %>% map(function(m){
#     filter(sf, medical_centre == m & med_strength == "75mg") %>%
#       ggplot(aes(x = factor(drugs), y = NIC_per_Total, group = medical_centre)) +
#       geom_col(color = "#e7298a", size = 1) +
#       geom_point(shape = 21, size = 3, colour = "white", fill = "#99998a") +
#      # scale_y_continuous(expand = c(0.005, 0.005), limits = c(0, NA), labels = comma) +
#       ylim(0, max(sf$NIC_per_Total)) +
#       labs(x = NULL, y = "Number of vehicles",
#            title = "Average annual daily flow", 
#            subtitle = m,
#            caption = "Source: DfT") +
#       theme_minimal() +
#       theme(
#         plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
#         panel.grid.major.x = element_blank(),
#         panel.grid.minor = element_blank(),
#         plot.caption = element_text(color = "grey50", size = 8, hjust = 1, margin = margin(t = 15)),
#         axis.title.x = element_text(size = 9, hjust = 1, margin = margin(t = 10)),
#         axis.title.y = element_text(size = 9, angle = 90, hjust = 1, margin = margin(r = 10)),
#         axis.text.x = element_text(angle = 90, hjust = 1, margin = margin(t = 0)))
#   })
# }
# 
# plot_150mg <- function(m) {
#   m %>% map(function(m){
#     filter(sf, medical_centre == m & med_strength == "150mg") %>%
#       ggplot(aes(x = factor(drugs), y = NIC_per_Total, group = medical_centre)) +
#       geom_col(color = "#e7298a", size = 1) +
#       geom_point(shape = 21, size = 3, colour = "white", fill = "#99998a") +
#      # scale_y_continuous(expand = c(0.005, 0.005), limits = c(0, NA), labels = comma) +
#       ylim(0, max(sf$NIC_per_Total)) +
#       labs(x = NULL, y = "Number of vehicles",
#            title = "Average annual daily flow", 
#            subtitle = m,
#            caption = "Source: DfT") +
#       theme_minimal() +
#       theme(
#         plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
#         panel.grid.major.x = element_blank(),
#         panel.grid.minor = element_blank(),
#         plot.caption = element_text(color = "grey50", size = 8, hjust = 1, margin = margin(t = 15)),
#         axis.title.x = element_text(size = 9, hjust = 1, margin = margin(t = 10)),
#         axis.title.y = element_text(size = 9, angle = 90, hjust = 1, margin = margin(r = 10)),
#         axis.text.x = element_text(angle = 90, hjust = 1, margin = margin(t = 0)))
#   })
# }

# plot_300mg <- function(m) {
#   m %>% map(function(m){
#     filter(sf, medical_centre == m & med_strength == "300mg") %>%
#       ggplot(aes(x = factor(drugs), y = NIC_per_Total, group = medical_centre)) +
#       geom_col(color = "#e7298a", size = 1) +
#       geom_point(shape = 21, size = 3, colour = "white", fill = "#99998a") +
#      # scale_y_continuous(expand = c(0.005, 0.005), limits = c(0, NA), labels = comma) +
#       ylim(0, max(sf$NIC_per_Total)) +
#       labs(x = NULL, y = "Number of vehicles",
#            title = "Average annual daily flow", 
#            subtitle = m,
#            caption = "Source: DfT") +
#       theme_minimal() +
#       theme(
#         plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
#         panel.grid.major.x = element_blank(),
#         panel.grid.minor = element_blank(),
#         plot.caption = element_text(color = "grey50", size = 8, hjust = 1, margin = margin(t = 15)),
#         axis.title.x = element_text(size = 9, hjust = 1, margin = margin(t = 10)),
#         axis.title.y = element_text(size = 9, angle = 90, hjust = 1, margin = margin(r = 10)),
#         axis.text.x = element_text(angle = 90, hjust = 1, margin = margin(t = 0)))
#   })
# }



#map_all <- sf %>% mapview(map.types = "CartoDB.Positron",
                        #  col = "#FFFFFF", col.regions = "#e7298a", alpha.regions = 1,
                       #   label = .$medical_centre, 
                        #  popup = popupGraph(plot(.$medical_centre), width = 300, height = 400),
                       #   layer.name = "all", 
                       #   legend = FALSE)

map_10 <- sf %>% mapview(map.types = "CartoDB.Positron",
                       col = "#FFFFFF", col.regions = "#e7298a", alpha.regions = 1,
                       label = .$medical_centre,
                       popup = popupGraph(plot_10mg(.$medical_centre), width = 300, height = 400),
                       layer.name = "10mg",
                       legend = FALSE)

# map_15 <- sf %>% mapview(map.types = "CartoDB.Positron",
#                          col = "#FFFFFF", col.regions = "#e7298a", alpha.regions = 1,
#                          label = .$medical_centre, 
#                          popup = popupGraph(plot_15mg(.$medical_centre), width = 300, height = 400),
#                          layer.name = "20mg", 
#                          legend = FALSE)

map_20 <- sf %>% mapview(map.types = "CartoDB.Positron",
                      col = "#FFFFFF", col.regions = "#e7298a", alpha.regions = 1,
                      label = .$medical_centre, 
                      popup = popupGraph(plot_20mg(.$medical_centre), width = 300, height = 400),
                      layer.name = "20mg", 
                      legend = FALSE)


# map_30 <- sf %>% mapview(map.types = "CartoDB.Positron",
#                          col = "#FFFFFF", col.regions = "#e7298a", alpha.regions = 1,
#                          label = .$medical_centre, 
#                          popup = popupGraph(plot_30mg(.$medical_centre), width = 300, height = 400),
#                          layer.name = "30mg", 
#                          legend = FALSE)

map_40 <- sf %>% mapview(map.types = "CartoDB.Positron",
                         col = "#FFFFFF", col.regions = "#e7298a", alpha.regions = 1,
                         label = .$medical_centre, 
                         popup = popupGraph(plot_40mg(.$medical_centre), width = 300, height = 400),
                         layer.name = "40mg", 
                         legend = FALSE)

# map_75 <- sf %>% mapview(map.types = "CartoDB.Positron",
#                          col = "#FFFFFF", col.regions = "#e7298a", alpha.regions = 1,
#                          label = .$medical_centre, 
#                          popup = popupGraph(plot_75mg(.$medical_centre), width = 300, height = 400),
#                          layer.name = "75mg", 
#                          legend = FALSE)
# 
# map_150 <- sf %>% mapview(map.types = "CartoDB.Positron",
#                          col = "#FFFFFF", col.regions = "#e7298a", alpha.regions = 1,
#                          label = .$medical_centre, 
#                          popup = popupGraph(plot_150mg(.$medical_centre), width = 300, height = 400),
#                          layer.name = "150mg", 
#                          legend = FALSE)
# 
# map_300 <- sf %>% mapview(map.types = "CartoDB.Positron",
#                          col = "#FFFFFF", col.regions = "#e7298a", alpha.regions = 1,
#                          label = .$medical_centre, 
#                          popup = popupGraph(plot_300mg(.$medical_centre), width = 300, height = 400),
#                          layer.name = "300mg", 
#                          legend = FALSE)
# 
# map_10 + map_15 + map_20 + map_30 + map_40 + map_75 + map_150 + map_300 #+ map_all

 map_10 + map_20 + map_40 


























---------------------------
plot2 <- ggplot(data=df_agg, aes(x=lat, y=lon, fill=drugs)) + 
  geom_bar(stat="identity") + 
  theme(axis.text.x=element_blank())


# create popup
popup <- paste0(
  "<div class='popupContainer'>",
  "<h3>", df_practices$name, "</h3>",
  "<table class='popupLayout'>",
  "<tr>",
  "<td>Post code</td>",
  "<td>", df_practices$postcode, "</td>",
  "</tr>",
  "</table>",
  "</div>"
)

popup <-  ggplot(data=df_agg[which(df_agg$medical_centre == 'ALTRINCHAM MEDICAL PRACTICE'),], aes(x=df_agg$drugs, y=df_agg$NIC_per_Total, fill=drugs)) + 
  geom_bar(stat="identity") + 
  theme(axis.text.x=element_blank())


popup <- function(){
  mdata <- df_agg[which(df_agg$medical_centre == 'ALTRINCHAM MEDICAL PRACTICE'),]
  med_plot <- ggplot(data=mdata, aes(x=mdata$drugs, y=mdata$NIC_per_Total, fill=mdata$drugs)) + 
    geom_bar(stat="identity") + 
    theme(axis.text.x=element_blank())
  
  return(med_plot)
}

map <- leaflet(height = "100%", width = "100%") %>% 
  setView(-2.35533522781156, 53.419025498197, zoom = 12) %>% 
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addPolylines(data = bdy, stroke = TRUE, weight = 2, color = "#212121", opacity = 1) %>% 
  #addMarkers(data = df_practices, popup = popup) %>% 
  addControl("<strong>Title</strong><br /><em>Source: </em>", position = 'topright')

# apply CSS
browsable(
  tagList(list(
    tags$head(
      tags$style("
                 html, body {height: 100%;margin: 0;}
                 .leaflet-control-layers-toggle {height: 44; width: 44;}
                 .leaflet-bar a, .leaflet-bar a:hover, .leaflet-touch .leaflet-bar a, .leaflet-touch .leaflet-bar a:hover {height: 34px; width: 34px; line-height: 34px;}
                 .info {width: 300px;}
                 .popupContainer{overflow: scroll;}
                 .popupLayout{width: 100%;}
                 .popupLayout td{vertical-align: top; border-bottom: 1px dotted #ccc; padding: 3px;}
                 .popupLayout td:nth-child(1){width: 1%; font-weight: bold; white-space: nowrap;}
                 ")
    ),
    map
  ))
)

