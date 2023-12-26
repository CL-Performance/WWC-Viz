filtered_data <- eventsPasses %>% filter(match_id==input$match2 & team.name == paste(input$team2,"Women's"))
# filtered_data <- eventsPasses %>% filter(match_id==3893827 & team.name =="Vietnam Women's")

lineups <- eventsPasses %>% filter(match_id==input$match2 & type.id==35)
# lineups <- eventsPasses %>% filter(match_id==3893827 & type.id==35)

#create lineup table


lineup_df <- filtered_data %>% filter (type.id==35)
lineup <- as.data.frame(lineup_df$tactics.lineup)
formation <- lineup_df$tactics.formation
#First Sub Index
subsCur <- filtered_data %>% filter(type.id==19)
subStartIndex <- min(subsCur["index"])



#DataFrame Mask
eventsMatchPassTeam <- filtered_data %>% filter(index<subStartIndex & type.id==30 & play_pattern.id !=4)




#Taking Relevant Coloumns
df_pass_all <- eventsMatchPassTeam %>%
  select(location.x, location.y, pass.end_location.x, pass.end_location.y, player.id, player.name, pass.recipient.id, pass.recipient.name, pass.outcome.name)
df_pass_completed <- eventsMatchPassTeam %>% filter(pass.outcome.name != "Incomplete" & pass.outcome.name != "Out" & pass.outcome.name != "Pass Offside" | is.na(pass.outcome.name) & !is.na(pass.recipient.name) ) %>%
  select(location.x, location.y, pass.end_location.x, pass.end_location.y, player.id, player.name, pass.recipient.id, pass.recipient.name, pass.outcome.name)
df_pass <- eventsMatchPassTeam %>% filter(!is.na(pass.recipient.name)) %>%
  select(location.x, location.y, pass.end_location.x, pass.end_location.y, player.id, player.name, pass.recipient.id, pass.recipient.name)
df_pass$pair.key = paste(df_pass$player.id, "-", df_pass$pass.recipient.id)

#Loop Through Passes
scatter_df <- data.frame()
for (i in 1:length(unique(df_pass$player.id))) {
  id <- unique(df_pass$player.id)[i]
  
  # Location of pass flipped to vertical field
  # Location of pass flipped to vertical field
  
  
  
  playery <- df_pass[df_pass$player.id == id, "location.x"]
  playerx <- df_pass[df_pass$player.id == id, "location.y"]
  if (!is.nan(sapply(playerx, mean))){
    scatter_df[i, "pass.x"] <- sapply(playerx, mean)
    scatter_df[i, "pass.y"] <- sapply(playery, mean)
  }else{
    scatter_df[i, "pass.x"] <- 0
    scatter_df[i, "pass.y"] <- 0
  }
  
  
  playery.end <- df_pass[df_pass$pass.recipient.id == id, "location.x"]
  playerx.end <- df_pass[df_pass$pass.recipient.id == id, "location.y"]
  if (!is.nan(sapply(playerx.end, mean))){
    scatter_df[i, "recipient.x"] <- sapply(playerx.end, mean)
    scatter_df[i, "recipient.y"] <- sapply(playery.end, mean) 
  }else{
    scatter_df[i, "recipient.x"] <- 0
    scatter_df[i, "recipient.y"] <- 0
  }
  
  
  
  
  
  
  
  #adds name and id
  scatter_df[i, "player.id"] <- id
  scatter_df[i, "player.name"] <- sapply(df_pass[df_pass$player.id == id, "player.name"], function(x) tail(strsplit(as.character(x), " ")[[1]], 1))
  scatter_df[i, "player.initials"] <- sapply(df_pass[df_pass$player.id == id, "player.name"], function(x) tail(substr((x), 1,1)  [[1]], 1))
  
  #make sure that x and y location for each circle representing the player is the average of passes and receptions
  
  
  #calculate number of passes & receptions
  scatter_df[i, "Passes"] <- nrow(df_pass[df_pass$player.id == id, ])
  scatter_df[i, "Receptions"] <- nrow(df_pass[df_pass$pass.recipient.id  == id, ])
}

#Calc Average position based on passing and receiving
scatter_df$x <- ((scatter_df$pass.x*scatter_df$Passes)+(scatter_df$recipient.x*scatter_df$Receptions))/ (scatter_df$Passes +   scatter_df$Receptions)

scatter_df$y <- ((scatter_df$pass.y*scatter_df$Passes)+(scatter_df$recipient.y*scatter_df$Receptions))/ (scatter_df$Passes + scatter_df$Receptions)

#Add Jersey Numbers
scatter_df$jersey_number <- lineup$jersey_number[match(scatter_df$player.id, lineup$player.id)]
scatter_df$player.initials <- paste0(scatter_df$player.initials,". ",scatter_df$player.name)

#calculate number completed and incomplete passes
total.passes_df <- data.frame()
for (i in 1:length(unique(df_pass_all$player.id))) {
  id <- unique(df_pass_all$player.id)[i]
  total.passes_df[i, "player.id"] <- id
  total.passes_df[i, "total.passes"] <- nrow(df_pass_all[df_pass_all$player.id == id, ])
  
}
completed.passes_df <- data.frame()
for (i in 1:length(unique(df_pass_completed$player.id))) {
  id <- unique(df_pass_completed$player.id)[i]
  completed.passes_df[i, "player.id"] <- id
  completed.passes_df[i, "completed.passes"] <- nrow(df_pass_completed[df_pass_completed$player.id == id, ])
  
}

#Complete Passing percentage and add it to scatter df
scatter_df$completed.passes <- completed.passes_df$completed.passes[match(scatter_df$player.id, completed.passes_df$player.id)];
scatter_df$total.passes <- total.passes_df$total.passes[match(scatter_df$player.id, total.passes_df$player.id)];
scatter_df$pass.per <- scatter_df$completed.passes/scatter_df$total.passes

#Set Passing Rank
for (i in 1:11){
  
  if (scatter_df$pass.per[i]>0.9)
  { scatter_df[i, "pass.rank"] <- "Green"
  } else if (scatter_df$pass.per[i]>0.7)
  {
    scatter_df[i, "pass.rank"] <- "Yellow"
  }else if (scatter_df$pass.per[i]>0.5){
    scatter_df[i, "pass.rank"] <- "Orange"
  }else if (scatter_df$pass.per[i]<=0.5){
    scatter_df[i, "pass.rank"] <- "Red"
  } else {}
}

#adjust the marker size variable to the total passes and receptions
scatter_df$Touches <- (scatter_df$total.passes + scatter_df$Receptions)







#Prepare pairing dataview
pass_pairing <- df_pass
pairings_df <- data.frame()



#Loop to count passes between pairings
for (i in 1:length(unique(pass_pairing$pair.key))) {
  pair.key <- unique(pass_pairing$pair.key)[i]
  player.id <- unique(pass_pairing$player.id)[i]
  pass.recipient.id <- unique(pass_pairing$pass.recipient.id)[i]
  
  pairings_df[i, "passes.pairing"] <- nrow(pass_pairing[pass_pairing$pair.key == pair.key, ])
  pairings_df[i, "pair.key"] <- pair.key
  
  pairings_df[i, "player.id"] <-  pass_pairing[pass_pairing$pair.key == pair.key, "player.id"]
  pairings_df[i, "pass.recipient.id"] <-  pass_pairing[pass_pairing$pair.key == pair.key, "pass.recipient.id"]
  
  pairings_df[i, "player.name"] <- sapply(pass_pairing[pass_pairing$pair.key == pair.key, "player.name"], function(x) tail(strsplit(as.character(x), " ")[[1]], 1))
  pairings_df[i, "pass.recipient.name"] <- sapply(pass_pairing[pass_pairing$pair.key == pair.key, "pass.recipient.name"], function(x) tail(strsplit(as.character(x), " ")[[1]], 1))
  
}

#Loop to link locations to pairings

pairings_df$jersey_number <- scatter_df$jersey_number[match(pairings_df$player.id, scatter_df$player.id)]


for (i in seq_along(pairings_df)){
  pairings_df$player.initials <- scatter_df$player.initials[match( pairings_df$player.id,scatter_df$player.id)];
  pairings_df$passer.x <- scatter_df$x[match( pairings_df$player.id,scatter_df$player.id)];
  pairings_df$passer.y <- scatter_df$y[match( pairings_df$player.id,scatter_df$player.id)];
  pairings_df$recipient.x <- scatter_df$x[match( pairings_df$pass.recipient.id,scatter_df$player.id)];
  pairings_df$recipient.y <- scatter_df$y[match( pairings_df$pass.recipient.id,scatter_df$player.id)];
  
}


#Offset Variables and Loop to create offsets
offset.dis<-480
offset.dis.start<- offset.dis/120
offset.dis.end<- offset.dis/80
slope.offset<-130
slope.offset.x <- slope.offset/80
slope.offset.y <- slope.offset/240

for (i in seq_along(pairings_df$pair.key)){
  
  #d=√((x_2-x_1)²+(y_2-y_1)²)
  
  dist <- sqrt(
    (pairings_df$recipient.x-pairings_df$passer.x)^2 + (pairings_df$recipient.y-pairings_df$passer.y)^2
    
  )
  
  pairings_df$slope <- (pairings_df$recipient.y - pairings_df$passer.y) / (pairings_df$recipient.x - pairings_df$passer.x)
  
  condition_up_right <- (pairings_df$passer.x <= pairings_df$recipient.x & pairings_df$passer.y <= pairings_df$recipient.y)
  condition_up_left <- (pairings_df$passer.x >= pairings_df$recipient.x & pairings_df$passer.y <= pairings_df$recipient.y)
  condition_down_right <- (pairings_df$passer.x <= pairings_df$recipient.x & pairings_df$passer.y >= pairings_df$recipient.y)
  condition_down_left <- (pairings_df$passer.x >= pairings_df$recipient.x & pairings_df$passer.y >= pairings_df$recipient.y)
  
  
  pairings_df$dif.x <- (pairings_df$passer.x - pairings_df$recipient.x)
  pairings_df$dif.y <- (pairings_df$passer.y - pairings_df$recipient.y)
  pairings_df$dist <- dist
  
  # Update start.x, end.x, direction.x
  pairings_df$start.x[condition_up_right] <- pairings_df$passer.x[condition_up_right] + offset.dis.start / sqrt(1 + (pairings_df$slope[condition_up_right]+slope.offset.x)^2)
  pairings_df$end.x[condition_up_right] <- pairings_df$recipient.x[condition_up_right] - offset.dis.end / sqrt(1 + (pairings_df$slope[condition_up_right]-slope.offset.x)^2)
  pairings_df$start.y[condition_up_right] <- pairings_df$passer.y[condition_up_right] + (pairings_df$slope[condition_up_right]+slope.offset.y) * offset.dis.start / sqrt(1 + (pairings_df$slope[condition_up_right]+slope.offset.y)^2)
  pairings_df$end.y[condition_up_right] <- pairings_df$recipient.y[condition_up_right] - (pairings_df$slope[condition_up_right]-slope.offset.y) * offset.dis.end / sqrt(1 + (pairings_df$slope[condition_up_right]-slope.offset.y)^2)
  pairings_df$direction[condition_up_right] <- "Up-Right"
  
  pairings_df$start.x[condition_up_left] <- pairings_df$passer.x[condition_up_left] - offset.dis.start / sqrt(1 + (pairings_df$slope[condition_up_left]+slope.offset.x)^2)
  pairings_df$end.x[condition_up_left] <- pairings_df$recipient.x[condition_up_left] + offset.dis.end / sqrt(1 + (pairings_df$slope[condition_up_left]-slope.offset.x)^2)
  pairings_df$start.y[condition_up_left] <- pairings_df$passer.y[condition_up_left] - (pairings_df$slope[condition_up_left]+slope.offset.y) * offset.dis.start / sqrt(1 + (pairings_df$slope[condition_up_left]+slope.offset.y)^2)
  pairings_df$end.y[condition_up_left] <- pairings_df$recipient.y[condition_up_left] + (pairings_df$slope[condition_up_left]-slope.offset.y) * offset.dis.end / sqrt(1 + (pairings_df$slope[condition_up_left]-slope.offset.y)^2)
  pairings_df$direction[condition_up_left] <- "Up-Left"
  
  pairings_df$start.x[condition_down_right] <- pairings_df$passer.x[condition_down_right] + offset.dis.start / sqrt(1 + (pairings_df$slope[condition_down_right]+slope.offset.x)^2)
  pairings_df$end.x[condition_down_right] <- pairings_df$recipient.x[condition_down_right] - offset.dis.end / sqrt(1 + (pairings_df$slope[condition_down_right]-slope.offset.x)^2)
  pairings_df$start.y[condition_down_right] <- pairings_df$passer.y[condition_down_right] + (pairings_df$slope[condition_down_right]+slope.offset.y) * offset.dis.start / sqrt(1 + (pairings_df$slope[condition_down_right]+slope.offset.y)^2)
  pairings_df$end.y[condition_down_right] <- pairings_df$recipient.y[condition_down_right] - (pairings_df$slope[condition_down_right]-slope.offset.y) * offset.dis.end / sqrt(1 + (pairings_df$slope[condition_down_right]-slope.offset.y)^2)
  pairings_df$direction[condition_down_right] <- "Down-Right"
  
  pairings_df$start.x[condition_down_left] <- pairings_df$passer.x[condition_down_left] - offset.dis.start / sqrt(1 + (pairings_df$slope[condition_down_left]+slope.offset.x)^2)
  pairings_df$end.x[condition_down_left] <- pairings_df$recipient.x[condition_down_left] + offset.dis.end / sqrt(1 + (pairings_df$slope[condition_down_left]-slope.offset.x)^2)
  pairings_df$start.y[condition_down_left] <- pairings_df$passer.y[condition_down_left] - (pairings_df$slope[condition_down_left]+slope.offset.y) * offset.dis.start / sqrt(1 + (pairings_df$slope[condition_down_left]+slope.offset.y)^2)
  pairings_df$end.y[condition_down_left] <- pairings_df$recipient.y[condition_down_left] + (pairings_df$slope[condition_down_left]-slope.offset.y) * offset.dis.end / sqrt(1 + (pairings_df$slope[condition_down_left]-slope.offset.y)^2)
  pairings_df$direction[condition_down_left] <- "Down-Left"
  
  
}







plot_df <- pairings_df %>% filter (passes.pairing>input$passingNetworkMinPasses)
#GRAPH VARIABLES










ggplot(
  data = scatter_df,
  aes(x=x, y=y,
      label=jersey_number),
) +
  
  #background_image(img) +
  #Drawing Field Lines
  
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
        line = element_blank(),
        legend.key.size = unit(1, "cm")) +
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
  
  
  
  
  
  
  
  
  
  
  


#Passing Network Plotting
geom_segment(
  data = plot_df,
  aes( x=start.x, y=start.y, xend=end.x, yend=end.y,
       linewidth=passes.pairing,
       alpha=passes.pairing
  ),
  arrow = arrow(type="closed", length=unit((scatter_df$Touches/max(scatter_df$Touches)/3), 'cm')),
  colour = "#969696"
  
) +
  
  scale_alpha(guide = "none", range = c(.3, .9)) +
  scale_linewidth("Passes", range = c(1, 3))+
  
  geom_point(
    data = scatter_df,
    aes(size = Touches,
        stroke = Touches/max(Touches)*2,
        colour = pass.per
    ),
    fill="#131313",
    pch=21,
    
  ) +
  
  scale_size(guide ="none", range = c(10,18)) +
  
  
#input$namebol==  
  geom_label(
    label=if(input$passingNetworkNameBol==TRUE){scatter_df$player.initials}else{""},
    label.size = NA,
    nudge_y = -4,
    size = 8,
    family = fontFamily,
    colour="#bbbbbb",
    fill="#131313",
    alpha=if(input$passingNetworkNameBol==TRUE){.5}else{0}
    
  ) +
  
  geom_text(
    position="stack",
    colour="white",
    family = fontFamily,
    fontface="bold",
    size=11
  ) +
  
  expand_limits(x=c(0,80), y=c(0, 120) ) +
  
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
  
  
  
  theme(
    axis.line = element_blank(),
    axis.text.x=element_blank(), axis.text.y=element_blank(),
    axis.ticks.x=element_blank(), axis.ticks.y=element_blank(),
    axis.title.x=element_blank(), axis.title.y=element_blank(),
    plot.background = element_rect(fill = backgroundColour, color = backgroundColour),    
    legend.title =
      element_text(
        color = fontColour,
        family = fontFamily,
        size = 24,
        face = "bold"
      ),
    legend.text = element_text(
      color = fontColour,
      family = fontFamily,
      size = 20,
      face="bold"
    ),   
    plot.margin = unit(c(-1, -1, -1, -1), "mm")
  )      
