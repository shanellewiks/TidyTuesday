###Tidy Tuesday 2021-02-23: BLS####
###Created by: Shanelle Wikramanayake###
###2021-03_16##

###Packages###
library(tidyverse)
library(here)
library(tidytuesdayR)
library(extrafont)
library(lubridate)
library(directlabels)

###Load data###
tuesdata <- tidytuesdayR::tt_load('2021-03-16')
games <- tuesdata$games

view(games)

###Load fonts###
font_import()
loadfonts(device = "win")


game_time <- games %>% 
  mutate(month = factor(month,
                        levels = c("January", "February", "March",
                                   "April", "May", "June",
                                   "July", "August", "September",
                                   "October", "November", "December")),
         date = make_date(year, month)) %>%
  filter(gamename == c("Fallout 3", "Half-Life")) %>% 
  ggplot(mapping = aes(x = date, 
                       y = avg))+
  geom_point(aes(colour = gamename))+
  geom_line(aes(colour = gamename))+
  geom_dl(aes(label = gamename, colour = gamename), method = list("last.points", cex = 1.3))+
  labs(title = 'Average gametime for Halflife and Fallout 3 over time', 
       x= "Date", 
       y = "Avergae number of players at the same time", 
       colour = "Game")+
  scale_colour_discrete(guide = "none")+
  theme(axis.text = element_text(size = 12, 
                                 colour = "grey"),
        axis.title = element_text(size = 15, 
                                  colour = "grey"), 
        plot.background = element_rect(fill = "black"), 
        panel.background = element_rect(fill = "black"), 
        legend.background = element_rect(fill = "black"), 
        legend.text = element_text(size = 12, 
                                   colour = "grey"), 
        legend.title = element_text(size = 13, 
                                    colour = "grey"), 
        plot.title = element_text(size = 17, 
                                  colour = "grey"))+
  ggsave(here("TidyTuesday", "2021_03_16", "Gaming_time.png"), 
         height = 5, 
         width =  8)

game_time

 
  
