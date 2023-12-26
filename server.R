#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinycssloaders)
library(tidyverse)
library(dplyr)
library(hms)
library(ggrepel)
library(showtext)


#Load Customization variables
# videoLength<-11000
videoLength<-1000
# Add Source Sans 3 Font Family
font_add_google("Source Sans 3")
showtext_auto()
fontFamily <- "Source Sans 3"
fontColour <- "#aaaabb"
backgroundColour <-"#202020"
backgroundLineColour <- "#505055"
foregroundLineColour <- "#707075"

inside.lines<-"#aaaabb"
outside.lines<-"#707080"
pitch.line.size<-0.6

maxXGCeiling <- 5
zoneColours<-c(
  "#be2323",
  "#ba182f",
  "#b30e39",
  "#ac0943",
  "#a30b4b",
  "#981052",
  "#8d1657",
  "#811c5c",
  "#74205e",
  "#672460",
  "#592760",
  "#4c295e",
  "#3f295b",
  "#332956",
  "#272951",
  "#1d274a",
  "#152543",
  "#0f233b",
  "#0c2033",
  "#0b1d2b"
)


#Convert timestamp string to minutes
timestampConvertM <- function(timestamp){
  
  y <-  as.numeric(as_hms(timestamp))/60
  
  
  return(y)
}


# Define server logic required to draw a histogram``
function(input, output, session) {
  

  # setwd("D:/Data Analysis/R/cl-performance/cl-performance") 

  matchesCurrent <- readRDS(file = "data/matchesCurrent.rds")
  teamsCurrent <- readRDS(file = "data/teamsCurrent.rds")
  eventsXG <- readRDS(file = "data/xGTimelineEvents.rds")
  #Passing Events
  eventsPasses <- readRDS(file = "data/eventsPasses.rds")
  eventsCurrent <- readRDS(file = "data/eventsCurrent.rds")





    
#Assist Zone Data
  shotAssistZoneTeam <- readRDS(file = "data/shotAssistZone.rds")
  goalAssistZoneTeam <- readRDS(file = "data/goalAssistZone.rds")
  


  
  xGReactive <- reactive(eventsXG)
  matchReactive <- reactive(matchesCurrent)
  teamsReactive <- reactive(teamsCurrent)
  passesReactive <- reactive(eventsPasses)
  shotAssistZoneReactive <- reactive(shotAssistZone)
  eventsReactive <- reactive(eventsCurrent)

  
  
  

  
  
  
#Preloader hide/main-content show  
  observe({
    delay(videoLength,
     {
       shinyjs::hide("preloader")
       shinyjs::show("main-content")
     }     
)
    
  }) 

    
#teams & matches Tab1  
  observe({
    team <- sort(unique(teamsCurrent$teamName))
    updateSelectInput(session, "team", choices = team)
  })
  observe({
    ids <- sort(matchesCurrent$match_id[matchesCurrent$home_team.home_team_name == paste(input$team,"Women's") | matchesCurrent$away_team.away_team_name == paste(input$team,"Women's")])
    nameList <- vector("list", length = length(ids))
    for (i in 1:length(ids)){
      id <- ids[i]
      teamA <- sub("Women's", "", matchesCurrent$home_team.home_team_name[matchesCurrent$match_id == id])
      teamB <- sub("Women's", "", matchesCurrent$away_team.away_team_name[matchesCurrent$match_id == id])
      stage <-matchesCurrent$competition_stage.name[matchesCurrent$match_id == id]
      name <- paste(teamA, "vs", teamB, "-", stage)
      nameList[i] <- name
    }
    if (length(ids) == length(unlist(nameList))) {
      match_vector <- setNames(ids, unlist(nameList))
      updateSelectInput(session, "match", choices = ids)
      updateSelectInput(session, "match", choices = match_vector)
    } else {
      print("Error: Length mismatch between ids and unlisted nameList")
    }
  })

#teams & Matches tab 4
  observe({
    team <- sort(unique(teamsCurrent$teamName))
    updateSelectInput(session, "team2", choices = team)
  })
  observe({
    ids <- sort(matchesCurrent$match_id[matchesCurrent$home_team.home_team_name == paste(input$team2,"Women's") | matchesCurrent$away_team.away_team_name == paste(input$team2,"Women's")])
    nameList <- vector("list", length = length(ids))
    for (i in 1:length(ids)){
      id <- ids[i]
      teamA <- sub("Women's", "", matchesCurrent$home_team.home_team_name[matchesCurrent$match_id == id])
      teamB <- sub("Women's", "", matchesCurrent$away_team.away_team_name[matchesCurrent$match_id == id])
      stage <-matchesCurrent$competition_stage.name[matchesCurrent$match_id == id]
      name <- paste(teamA, "vs", teamB, "-", stage)
      nameList[i] <- name
    }
    if (length(ids) == length(unlist(nameList))) {
      match_vector <- setNames(ids, unlist(nameList))
      updateSelectInput(session, "match2", choices = ids)
      updateSelectInput(session, "match2", choices = match_vector)
    } else {
      print("Error: Length mismatch between ids and unlisted nameList")
    }
  })  
  
#teams & Matches tab 5
  observe({
    team2 <- sort(unique(teamsCurrent$teamName))
    updateSelectInput(session, "team3", choices = team2)
  })

  
#plots  
  

    output$barPlot <- renderPlot({
      source('scripts/xGbar.R', local = TRUE)$value
      
    })
  
    output$xGPlot <- renderPlot({
      source('scripts/xGtimeline.R', local = TRUE)$value
      
    })
  
    
    
#Shot Assist Zones
    output$totalShotAssistZonePlot <- renderPlot({
      source('scripts/shotAssistZonesTotal.R', local = TRUE)$value
    })
    output$shotAssistZonePlot <- renderPlot({
      source('scripts/shotAssistZonesTeam.R', local = TRUE)$value
    })

#Goal Assist Zones    
    output$totalGoalAssistZonePlot <- renderPlot({
      source('scripts/goalAssistZonesTotal.R', local = TRUE)$value
    })
    output$goalAssistZonePlot <- renderPlot({
      source('scripts/goalAssistZonesTeam.R', local = TRUE)$value 
    })    
    
#Passing Network   
    output$passingBarPlot <- renderPlot({
      source('scripts/passingPercentage.R', local = TRUE)$value  
    })
    output$passingNetworkPlot <- renderPlot({
      source('scripts/passingNetwork.R', local = TRUE)$value  
    })   
    
# Final third entries
    output$finalThirdEntryPlot <- renderPlot({
      source('scripts/finalThirdEntries.R', local = TRUE)$value  
    })   
    output$regainsPlot <- renderPlot({
      source('scripts/regains.R', local = TRUE)$value 
    })
    
        
    
#Server Function End    
}


