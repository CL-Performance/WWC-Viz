rm(list=ls())

setwd("D:/Data Analysis/R/cl-performance/cl-performance")
getwd()

fontFamily <- "Source Sans 3"
fontColour <- "#aaaabb"
backgroundColour <-"#202020"

backgroundLineColour <- "#505055"
foregroundLineColour <- "#707075"

zonecolors<-c("#e02020", "#d02020", "#c02020", "#b02020", "#a02020", "#902020", "#802020", "#702020", "#602020", "#502020", "#402020", "#302020", "#202020" )

competitionName <- "Women's World Cup"
shotSubtitle <- "2023 Shot Assists"
goalSubtitle <- "2023 Goal Assists"


save(fontFamily,fontColour, backgroundColour, backgroundLineColour, foregroundLineColour, zonecolors, competitionName, shotSubtitle, goalSubtitle, file ="data/style.rda")

load(file ="data/style.rda")


