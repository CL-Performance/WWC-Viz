library(dbplyr)
library(ggplot2)

  
eventsxGChain <- eventsCurrent %>%
  filter(team.name == paste(input$team3,"Women's"))

xgbyPossession <- eventsxGChain %>% 
  group_by(match_id, possession) %>%
  summarise(possession_xg = sum(shot.statsbomb_xg, na.rm = TRUE))

finalThirdEntries <- eventsxGChain %>% 
  filter(location.x < 80 & pass.end_location.x >80 | location.x < 80 & carry.end_location.x >80 ) %>%
  distinct(match_id, possession, .keep_all = TRUE) %>% 
  select(match_id, player.id, player.name, type.name, location.x, location.y, pass.end_location.x, pass.end_location.y, carry.end_location.x, carry.end_location.y, team.name, possession) %>% 
  mutate(end_location.x = ifelse(!is.na(pass.end_location.x),pass.end_location.x, carry.end_location.x), end_location.y = ifelse(!is.na(pass.end_location.y),pass.end_location.y, carry.end_location.y))


finalThirdEntries_xg <- left_join(finalThirdEntries, xgbyPossession, by = c("match_id", "possession"))


finalThirdEntries_xg <- finalThirdEntries_xg %>% filter(possession_xg > 0)





#build heatmap dimensions
xbinwidth <- 16
ybinwidth <- 20
heatmap = finalThirdEntries_xg %>% mutate(end_location.x = ifelse(end_location.x>120, 120, end_location.x),
                                          end_location.y = ifelse(end_location.y>80, 80, end_location.y),
                                          end_location.x = ifelse(end_location.x<0, 0, end_location.x),
                                          end_location.y = ifelse(end_location.y<0, 0, end_location.y)) #1
heatmap$xbin <- cut(heatmap$end_location.x, breaks = seq(from=0, to=120, by = ybinwidth),include.lowest=TRUE )
heatmap$ybin <- cut(heatmap$end_location.y, breaks = seq(from=0, to=80, by = xbinwidth),include.lowest=TRUE) #2


#dataframe for shot assits
heatmap_finalThirdEntries <- heatmap%>%
  group_by(xbin, ybin) %>%
  summarise(
            location.x = ceiling(median(end_location.x)/ybinwidth)*ybinwidth-(ybinwidth/2),
            location.y = ceiling(median(end_location.y)/xbinwidth)*xbinwidth-(xbinwidth/2),
            sum_bin_xg = sum(possession_xg)
  )





ggplot(finalThirdEntries_xg, aes(x = location.y, y = location.x)) +
  geom_bin2d(data = heatmap_finalThirdEntries, aes(x=location.y, y=location.x, fill = sum_bin_xg), position = "identity", alpha = 0.9, stat = "sum") +
  
  
  
  
  #Draw Pitch
  annotate("rect", xmin=0, xmax =80, ymin = 0, ymax =120, fill = NA, colour = outside.lines, linewidth = pitch.line.size)+
  annotate("rect",xmin = 0, xmax = 80, ymin = 0, ymax = 60, fill = NA, colour = outside.lines, linewidth = pitch.line.size) +
  #18yard box fit to bins
  annotate("rect",xmin = 18, xmax = 62, ymin = 18, ymax = 0, fill = NA, colour = inside.lines, linewidth = pitch.line.size) +
  annotate("rect",xmin = 18, xmax = 62, ymin = 102, ymax = 120, fill = NA, colour = inside.lines, linewidth = pitch.line.size) +
  
  annotate("rect",xmin = 30, xmax = 50, ymin = 0, ymax = 6, fill = NA, colour = inside.lines, linewidth = pitch.line.size) +
  annotate("rect",xmin = 30, xmax = 50, ymin = 120, ymax = 114, fill = NA, colour = inside.lines, linewidth = pitch.line.size) +
  annotate("rect",xmin = 36, xmax = 44, ymin =120, ymax = 120.5, fill = NA, colour = outside.lines, linewidth = pitch.line.size) +
  annotate("rect",xmin = 36, xmax = 44, ymin =0, ymax = -0.5, fill = NA, colour = outside.lines, linewidth = pitch.line.size) +
  annotate("segment", x = -0.5, xend = 80.5, y = 60, yend = 60, colour = inside.lines, linewidth = pitch.line.size)+
  annotate("segment", x = 0, xend = 80, y = 0, yend = 0, colour = outside.lines, linewidth = pitch.line.size)+
  annotate("segment", x = 0, xend = 80, y = 120, yend = 120, colour = outside.lines, linewidth = pitch.line.size)+
  annotate("rect", xmin=0, xmax =80, ymin = 0, ymax =120, fill = NA, colour = outside.lines, linewidth = .8) +
  
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



geom_label(data = heatmap_finalThirdEntries, aes(x = location.y, y = location.x, label = round(sum_bin_xg, digits = 3)),
             colour=inside.lines,
             label.size = NA,
             fill="#131313",
             family = fontFamily,
             fontface="bold",
             size= 12,
             alpha=.3)+
  
  geom_segment(
    aes(x = location.y, y = location.x,
        xend = end_location.y, yend = end_location.x,
        alpha = .8),
    color = fontColour, linetype = "solid",
    size = 1,
    arrow = arrow(
      angle = 30,
      length = unit(0.2, "cm"),
      type = "closed")
    ) +
  
  
  
  
  geom_point(data = subset(finalThirdEntries_xg, type.name == "Carry"),
             aes(size = possession_xg),
             color = fontColour,
             fill = "#c4cc2d",
             shape = 21,  
             stroke = 2) +
  
  geom_point(data = subset(finalThirdEntries_xg, type.name == "Pass"),
             aes(size = possession_xg),
             color = fontColour,
             fill = "#2c74e8",
             shape = 21,  
             stroke = 2) +
  
  
  

  scale_size_continuous(range = c(3, 10)) +
  
  labs(x = "X-axis", y = "Y-axis", title = "Final Third Entries") +



  
  theme(axis.text.x=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.caption=element_text(size=14,family="", hjust=0.5, vjust=0.5, face="bold"),
        plot.subtitle = element_text(size = 26, family=fontFamily, lineheight = .35, hjust = 0.5, colour = backgroundLineColour,face="bold"),
        axis.text.y=element_blank(),
        legend.title = element_blank(),
        legend.position = "none",
        legend.key.size = unit(.5, "cm"),
        plot.title = element_text(
          face="bold",
          size = 48,
          family=fontFamily,
          colour = fontColour,
          hjust = .5,
          vjust=0
        ),
        legend.direction = "vertical",
        axis.ticks=element_blank(),
        plot.background = element_rect(fill = backgroundColour, color = backgroundColour),
        strip.text.x = element_text(size=10,family=fontFamily),
        plot.margin = margin(0, 30, 0, 40),
        strip.text = element_text(size=50,family=fontFamily, color = fontColour)
  ) +
  
  
  scale_fill_gradientn(colours = zoneColours, trans = "reverse") +
  
  guides(fill = guide_legend(reverse = TRUE))


