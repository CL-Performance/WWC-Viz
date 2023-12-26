filtered_passes <- eventsPasses %>% filter(match_id==input$match2 & team.name == paste(input$team2,"Women's") & type.id==30) %>%
  mutate(pass.success = !is.na(pass.recipient.id) & is.na(pass.outcome.name) | pass.outcome.name !="Out" & pass.outcome.name !="Incomplete" ) %>% 
  group_by(player.id, player.name) %>% 
  summarise(
    successfulPasses = sum(pass.success),
    unsuccessfulPasses = sum(!pass.success),
    totalPasses = successfulPasses + unsuccessfulPasses,
    passCompletionPercent = successfulPasses/totalPasses*100,
    progressiveDistance = sum(ifelse(pass.end_location.x-location.x>0,pass.end_location.x-location.x,0)),
    progressiveDistanceperPass = progressiveDistance/totalPasses
    ) %>%
  mutate(player.initials = sapply(strsplit(as.character(player.name), " "), function(x) paste0(substr(x[1], 1, 1), ". ", x[length(x)])))



ggplot(data = filtered_passes, aes(x = progressiveDistanceperPass, y = passCompletionPercent, label = player.initials,
                                   color = (progressiveDistanceperPass/max(progressiveDistanceperPass))+(passCompletionPercent/max(progressiveDistanceperPass))
                                   )) +

  geom_smooth(method=lm, formula = y ~ x, col='#707080') +
  geom_point(size = 5) +
  geom_label_repel(
    label.size = NA,
    nudge_y = -2,
    size = 8,
    family = fontFamily,
    colour="#bbbbbb",
    fill="#131313",
    alpha= 0.8,
    max.overlaps = getOption("ggrepel.max.overlaps", default = 20),
    
  ) +
  scale_colour_gradient(
    "Pass %",
    labels = scales::percent_format(),
    low = "#a60000",
    high = "#369c00",
    space = "Lab",
    na.value = "grey50",
    guide = "colourbar",
    aesthetics = "colour"
  ) +
  
  labs(title = "",
       x = "Progression per Pass", y = "Passing Percentage") +
  
  theme(axis.title.x = element_text(size = 22, family = fontFamily, face = "bold", color=fontColour),
        axis.text = element_text(size = 16, family = fontFamily, face = "bold", color=fontColour),
        axis.title.y = element_text(size = 22, family = fontFamily, face = "bold", color=fontColour),
        legend.position = "none",
        plot.background = element_rect(fill = backgroundColour),
        panel.background = element_rect(fill = backgroundColour),
        axis.line = element_line(color = "#606080"),
        axis.ticks = element_line(color = "#606080"),
        panel.grid.major = element_line(color = "#606080"),
        panel.grid.minor = element_line(color = "#505060")
  )

