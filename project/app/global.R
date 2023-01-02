library(shiny)
library(dplyr)
library(DT)
library(shinyWidgets)
library(plotly)
library(ggplot2)
library(leaflet)
library(maps)
library(randomForest)
library(shinythemes)



#import all datasets
skillMigration = read.csv("www/data/skill_migration.csv")
industryMigration = read.csv("www/data/industry_migration.csv")
countryMigration = read.csv("www/data/country_migration.csv")
countryMigrationClean = read.csv("www/data/countryclean.csv")
skillMigrationClean = read.csv("www/data/skillclean.csv")
mapStates = maps::map('world',col="#f2f2f2", fill=TRUE, bg="lightblue", lwd=0.05, mar=rep(0,4),border=0,)


###################### TALENT MIGRATION ########################

talent_graph <- subset(countryMigrationClean, select = c(base_country_name, target_country_name, target_lat, target_long, year, value))


##################### REFERENCES #######################

#https://shiny.rstudio.com/gallery/
#https://github.com/dataprofessor/code/tree/master/shiny
#http://www.sthda.com/english/wiki/ggplot2-colors-how-to-change-colors-automatically-and-manually#:~:text=%2C%20c%20%3D%20100.-,Change%20colors%20manually,()%20for%20lines%20and%20points
#https://cran.r-project.org/web/packages/basictabler/vignettes/v04-styling.html
#https://r-graph-gallery.com/180-change-background-in-leaflet-map.html
#https://www.listendata.com/2021/09/shiny-search-bar-with-suggestions.html
#https://plotly.com/ggplot2/bar-charts/


