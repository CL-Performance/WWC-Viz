  teamsCurrent <- teamsReactive ()
  
  #text size
  axisTitleSize <- 32
  axisTextSize <- 26
  labelSize <- 9
  
  
  
  
  #Find First Half End timestamp
  periodEnd<- xGReactive() %>%
    filter (match_id == input$match & type.id == 34)
  
  if (max(periodEnd$period) > 2){
    
    half1EndTime <- max(timestampConvertM(subset(periodEnd, period == 1)$timestamp))
    half2EndTime <- max(timestampConvertM(subset(periodEnd, period == 2)$timestamp))+half1EndTime
    halfX1EndTime <- max(timestampConvertM(subset(periodEnd, period == 3)$timestamp))+half2EndTime
    matchEndTime <- halfX2EndTime <- max(timestampConvertM(subset(periodEnd, period == 4)$timestamp))+halfX1EndTime
    endTimes <- c(0, half1EndTime, half2EndTime, halfX1EndTime, halfX2EndTime)
    
    
  } else{
    
    half1EndTime <- max(timestampConvertM(subset(periodEnd, period == 1)$timestamp))
    matchEndTime <- half2EndTime <- max(timestampConvertM(subset(periodEnd, period == 2)$timestamp))+half1EndTime
    endTimes <- c(0, half1EndTime, half2EndTime)
    
  }
  startFinishPointId <- 99   
  
  
  
  #Select Events by Match Id and add time for events adding halfend time to 2nd half events
  eventsFiltered <- xGReactive() %>%
    filter (match_id == input$match, type.id == 16 | type.id == 25 | type.id == 19, period < 5) %>%
    mutate (shot.statsbomb_xg = ifelse(!is.na(shot.statsbomb_xg),shot.statsbomb_xg,0)) %>%
    mutate (time = timestampConvertM(timestamp)+(endTimes[period])) %>%
    
    #Arrange, Group and Cumulative Sum XG
    arrange(team.name, time) %>%
    group_by(team.name) %>%
    mutate(XG = cumsum(shot.statsbomb_xg))
  
  #create dataframe to attach to main data
  startFinishPointDf <- data.frame(
    type.id = numeric(0),
    XG = numeric(0),
    time = numeric(0),
    team.name = character(0),
    stringsAsFactors = FALSE  
  )
  
  #create match info variables
  matchFiltered <- matchReactive() %>%  filter (match_id == input$match)
  teamA <- matchFiltered$home_team.home_team_name[1]
  teamB <- matchFiltered$away_team.away_team_name[1]
  
  teamAScore <- matchFiltered$home_score[1]
  teamBScore <- matchFiltered$away_score[1]
  #Find Max XG and Assign start and finish points for each team using 0,0 and Max Time and XG
  maxXGA <- max(eventsFiltered[eventsFiltered$team.name == teamA, "XG"], na.rm = TRUE)
  maxXGB <- max(eventsFiltered[eventsFiltered$team.name == teamB, "XG"], na.rm = TRUE)
  
  #Create Data frame to bind to
  startFinishPointDf <- data.frame(
    type.id = numeric(0),
    XG = numeric(0),
    time = numeric(0),
    team.name = character(0),
    score = numeric(0),
    stringsAsFactors = FALSE
  )
  startFinishPointDf[1,] <- data.frame(type.id = startFinishPointId, XG = ifelse(maxXGA>0, maxXGA, 0), time = matchEndTime, team.name = teamA, score = teamAScore)
  startFinishPointDf[2,] <- data.frame(type.id = startFinishPointId, XG = 0, time = 0, team.name = teamA, score = 0)
  startFinishPointDf[3,] <- data.frame(type.id = startFinishPointId, XG =ifelse(maxXGB>0, maxXGB, 0), time = matchEndTime, team.name = teamB, score = teamBScore)
  startFinishPointDf[4,] <- data.frame(type.id = startFinishPointId, XG = 0, time = 0, team.name = teamB, score = 0)
  
  #add rows from loop    
  eventsFiltered <- rbind(eventsFiltered, startFinishPointDf)
  
  #add short names
  eventsFiltered$short.team.name <- sub(" Women's", "", eventsFiltered$team.name)
  eventsFiltered$short.player.name <- sub("^(\\w)\\w*\\s(?:.+\\s)?(\\w+)$", "\\1. \\2", eventsFiltered$player.name)
  eventsFiltered <- eventsFiltered %>%
    mutate(short.player.name = ifelse(type.id == 25, "(OG)", short.player.name))
  
  
  #Create Title
  chartTitle <- paste(
    "<h2><span style='color:",
    teamsCurrent$colourPrimary[teamsCurrent$teamName==sub(" Women's", "", teamA)], ";'>",
    sub(" Women's", "", teamA),"</span>",
    "vs",
    "<span style='color:",
    teamsCurrent$colourPrimary[teamsCurrent$teamName==sub(" Women's", "", teamB)], ";'>",
    sub(" Women's", "", teamB), "</span></h2>")
  chartSubtitle <-   paste0("<h3>(",round(ifelse(maxXGA>0, maxXGA, 0), digits=2),"xG)  ", teamAScore, " - ", teamBScore,"  (",round(ifelse(maxXGB>0, maxXGB, 0), digits=2),"xG)</h3>") 
  
  
  #Render Chart title
  output$xGPlotTitle<- renderUI({
    HTML(chartTitle,chartSubtitle)
  })
  
  
  
  #plot timeline    
  ggplot(eventsFiltered, aes(x=time, y=XG, group = team.name, color = short.team.name, label=paste0(short.player.name," ",minute,"'"))) +
    geom_step(linewidth=1.5, alpha = .1) +
    geom_step(linewidth=1.5, alpha = .1) +
    geom_step(linewidth=1.5, alpha = .1) +
    geom_step(linewidth=1.5, alpha = .1) +
    geom_step(linewidth=1.5, alpha = .1) +
    geom_step(linewidth=1.5, alpha = .1) +
    geom_step(linewidth=1.5, alpha = .1) +
    geom_step(linewidth=1.5, alpha = .1) +
    geom_step(linewidth=1.5, alpha = .1) +
    
    #create halftime and fulltime lines and x axis by 15's      
    geom_vline(xintercept = endTimes, linetype = "dashed", color = foregroundLineColour, linewidth = 1) +
    scale_x_continuous(breaks = seq(0, max(eventsFiltered$time), by = 15), limits = c(0, max(eventsFiltered$time)+5)) +
    scale_y_continuous(breaks = seq(0, input$xGaxis, by = 1), minor_breaks= seq(0, input$xGaxis, by = 0.5), limits = c(0, input$xGaxis),) +
    

    
    #create dots and name labels for goals      
    geom_point(data = subset(eventsFiltered, shot.outcome.id == 97 | type.id == 25), size = 5, fill=fontColour, pch=21, stroke = 2) +
    geom_point(data = subset(eventsFiltered, type.id == 19), size = 5, color=fontColour, pch=18) +
    geom_text_repel(data = subset(eventsFiltered, shot.outcome.id == 97 | type.id == 25),
                    size=labelSize,
                    hjust = 1.2,
                    vjust = -.8,
                    force = 5,
                    color=fontColour,
                    fontface = "bold",
                    segment.color = fontColour,
                    segment.size = 0.5
    ) +
    geom_text(data = subset(eventsFiltered, time == max(time)),
              aes(label = paste0(score,"G ",round(XG, digits=2),"xG")),
              hjust = -.25,
              color=fontColour,
              fontface = "bold",
              size = labelSize
              
    ) +
    
    #add chart title
    
    labs(
      x = "Minute\n\u25CF Goals — \u25c6 Substitutions",
      y = "xG") +
    
    #create color scale based on primary team colours      
    scale_color_manual(values = setNames(teamsCurrent$colourPrimary, teamsCurrent$team)) +
    
    #adjust theme
    theme(
      text = element_text(family = fontFamily, face = "bold", color = fontColour),
      legend.position = "none",
      panel.grid.major = element_line(linewidth = 0.5, linetype = 'solid',
                                      colour = backgroundLineColour), 
      panel.grid.minor = element_line(linewidth = 0.25, linetype = 'solid',
                                      colour = backgroundLineColour),
      plot.background = element_rect(color = backgroundColour, fill = backgroundColour),
      panel.background = element_rect(color = backgroundColour, fill = backgroundColour),
      axis.title = element_text(size=axisTitleSize),
      axis.text = element_text(size=axisTextSize)
    )
  
