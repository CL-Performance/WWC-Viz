shotXBin <- 80/input$shotXBin
shotYBin <- 120/input$shotYBin

#build heatmap dimensions
heatmap = eventsPasses %>%mutate(location.x = ifelse(location.x>120, 120, location.x),
                                 location.y = ifelse(location.y>80, 80, location.y),
                                 location.x = ifelse(location.x<0, 0, location.x),
                                 location.y = ifelse(location.y<0, 0, location.y)) #1
heatmap$xbin <- cut(heatmap$location.x, breaks = seq(from=0, to=120, by = shotYBin),include.lowest=TRUE )
heatmap$ybin <- cut(heatmap$location.y, breaks = seq(from=0, to=80, by = shotXBin),include.lowest=TRUE) #2

#dataframe for shot assits
shotAssistZoneTeam <- heatmap%>%
  filter(type.id==30 & pass.shot_assist | pass.goal_assist) %>%
  group_by(team.name) %>%
  mutate(total_DA = n()) %>%
  group_by(team.name, xbin, ybin) %>%
  summarise(total_DA = max(total_DA),
            bin_DA = n(),
            bin_pct = bin_DA/total_DA,
            location.x = (ceiling(median(location.x)/shotYBin)*shotYBin)-(shotYBin/2),
            location.y = (ceiling(median(location.y)/shotXBin)*shotXBin)-(shotXBin/2)
  )%>%
  group_by(xbin, ybin) %>%
  mutate(league_ave = mean(bin_pct)) %>%
  group_by(team.name, xbin, ybin) %>%
  mutate(diff_vs_ave = bin_pct - league_ave)







ggplot(data= shotAssistZoneTeam, aes(x = location.y, y = location.x, fill = diff_vs_ave, group = diff_vs_ave)) +
  geom_bin2d(binwidth = c(shotXBin, shotYBin), position = "identity", alpha = 0.9) +
  
  
  #Draw Pitch
  annotate("rect", xmin=0, xmax =80, ymin = 0, ymax =120, fill = NA, colour = outside.lines, size = pitch.line.size)+
  annotate("rect",xmin = 0, xmax = 80, ymin = 0, ymax = 60, fill = NA, colour = outside.lines, size = pitch.line.size) +
  #18yard box fit to bins
  annotate("rect",xmin = 18, xmax = 62, ymin = 18, ymax = 0, fill = NA, colour = inside.lines, size = pitch.line.size) +
  annotate("rect",xmin = 18, xmax = 62, ymin = 102, ymax = 120, fill = NA, colour = inside.lines, size = pitch.line.size) +
  
  annotate("rect",xmin = 30, xmax = 50, ymin = 0, ymax = 6, fill = NA, colour = inside.lines, size = pitch.line.size) +
  annotate("rect",xmin = 30, xmax = 50, ymin = 120, ymax = 114, fill = NA, colour = inside.lines, size = pitch.line.size) +
  annotate("rect",xmin = 36, xmax = 44, ymin =120, ymax = 120.5, fill = NA, colour = outside.lines, size = pitch.line.size) +
  annotate("rect",xmin = 36, xmax = 44, ymin =0, ymax = -0.5, fill = NA, colour = outside.lines, size = pitch.line.size) +
  annotate("segment", x = -0.5, xend = 80.5, y = 60, yend = 60, colour = inside.lines, size = pitch.line.size)+
  annotate("segment", x = 0, xend = 80, y = 0, yend = 0, colour = outside.lines, size = pitch.line.size)+
  annotate("segment", x = 0, xend = 80, y = 120, yend = 120, colour = outside.lines, size = pitch.line.size)+
  annotate("rect", xmin=0, xmax =80, ymin = 0, ymax =120, fill = NA, colour = outside.lines, size = .8) +
  
  #Circles and dots
  theme(rect = element_blank(),
        line = element_blank()) +
  annotate("point", x = 40 , y = 12, colour = inside.lines, size = 1.05) +
  annotate("point", x = 40 , y = 108, colour = inside.lines, size = 1.05) +
  annotate("path", colour = inside.lines, size = pitch.line.size,
           x=40+10*cos(seq(0,2*pi,length.out=2000)),
           y=60+10*sin(seq(0,2*pi,length.out=2000)))+
  annotate("point", x = 40 , y = 60, colour = inside.lines, size = 1.05) +
  
  
  annotate("path", x=40+10*sin(seq(-0.3*pi,0.3*pi,length.out=30)), size = pitch.line.size,
           y=12+10*cos(seq(-0.3*pi,0.3*pi,length.out=30)), col=inside.lines) +
  annotate("path", x=40-10*sin(seq(-0.3*pi,0.3*pi,length.out=30)), size = pitch.line.size,
           y=108-10*cos(seq(-0.3*pi,0.3*pi,length.out=30)), col=inside.lines) +
  
  geom_label(aes(label = bin_DA),
             colour=inside.lines,
             label.size = NA,
             fill="#131313",
             family = fontFamily,
             fontface="bold",
             size= 7,
             alpha=.3)+
  
  theme(axis.text.x=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.caption=element_text(size=20,family="", hjust=0.5, vjust=0.5, face="bold"),
        plot.subtitle = element_text(size = 26, family=fontFamily, lineheight = .35, hjust = 0.5, colour = fontColour,face="bold"),
        axis.text.y=element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size=18,family=fontFamily, hjust=0.5, vjust=0.5, face="bold", colour = fontColour),
        legend.key.size = unit(.75, "cm"),
        plot.title = element_text(
          face="bold",
          size = 32,
          family=fontFamily,
          colour = fontColour,
          hjust = .5,
          vjust=0
        ),
        title = element_text(size = 100),
        legend.direction = "vertical",
        axis.ticks=element_blank(),
        plot.background = element_rect(fill = backgroundColour, color = backgroundColour),
        strip.text.x = element_text(size=10,family=fontFamily),
        plot.margin = margin(0, 30, 0, 40)
  ) +
  
  
  scale_fill_gradientn(colours = zoneColours, trans = "reverse", labels =
                         scales::percent_format(accuracy = 1), limits = c(max(shotAssistZoneTeam$diff_vs_ave), min(shotAssistZoneTeam$diff_vs_ave))) +
  
  
  guides(fill = guide_legend(reverse = TRUE)) +
  
  facet_wrap(~team.name, nrow = 4) +
  theme(strip.text.x = element_text(size = 18, family=fontFamily, color = fontColour))




