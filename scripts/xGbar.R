  teamsCurrent <- teamsReactive ()
  matchFiltered <- matchReactive() %>%  filter (match_id == input$match)
  
  #text sizing
  axisTitleSize <- 28
  axisTextSize <- 22


  
  
  xGperMatch <- xGReactive() %>% 
    filter(type.id == 16) %>% 
    group_by(team.name) %>% 
    summarise(totalxG=sum(shot.statsbomb_xg), matchNumber=length((unique(match_id))), meanxG = totalxG/matchNumber) %>%
    arrange(meanxG) %>% 
    mutate(team.selected = ifelse(
      team.name == matchFiltered$home_team.home_team_name[1] | team.name == matchFiltered$away_team.away_team_name[1],
      team.name, "None")) 
  
  xGperMatch$team.name <- sub(" Women's", "", xGperMatch$team.name)
  xGperMatch$team.selected <- sub(" Women's", "", xGperMatch$team.selected)
  
  xGperMatch <- xGperMatch %>%
    left_join(teamsCurrent, by = c("team.selected"="teamName"))

  
  
  
  ggplot(xGperMatch, aes(x=meanxG,y=fct_reorder(team.name, meanxG), fill = team.selected)) +
    geom_bar(stat = "identity") +
    
    scale_fill_manual(values = setNames(teamsCurrent$colourPrimary, teamsCurrent$teamName)) +
    
    
    labs(
      x = "xG per Match",
      y = "") +
    
    theme(
      legend.position = "None",
      plot.background = element_rect(color = backgroundColour, fill = backgroundColour),
      panel.background = element_rect(color = backgroundColour, fill = backgroundColour),
      axis.title.x = element_text(size=axisTitleSize),
      axis.text = element_text(size=axisTextSize),
      axis.text.y = element_text(color = ifelse(is.na(xGperMatch$colourPrimary), fontColour, xGperMatch$colourPrimary)),
      panel.grid.major = element_line(linewidth = 0.5, linetype = 'solid',
                                      colour = backgroundLineColour), 
      panel.grid.minor = element_line(linewidth = 0.25, linetype = 'solid',
                                      colour = backgroundLineColour),
      text = element_text(family = fontFamily, face = "bold", color = fontColour)
      
    )
  
  
