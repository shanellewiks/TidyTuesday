###Tidy Tuesday 2021-02-23: BLS####
###Created by: Shanelle Wikramanayake###
###2021-03_02##

###Packages###
library(tidyverse)
library(here)
library(tidytuesdayR)
library(extrafont)
library(scales)
library(ggsci)

###Load data###
tuesdata <- tidytuesdayR::tt_load('2021-03-09')
bechdel <- tuesdata$raw_bechdel
movies <- tuesdata$movies

view(bechdel)
view(movies)

###Load fonts###
font_import()
loadfonts(device = "win")


###Checking on something###

find_tron <- movies %>% 
  filter(title =="Tron Legacy")
view(find_tron) #Trong legacy failed the bechdel test, but I"ll keep the colour scheme 


###Plot###
mov1 <- movies %>% 
  select(test, clean_test, budget, domgross, binary) %>% 
  rename(Test_Results = clean_test, 
         Budget = budget, 
         Domestic_Gross = domgross) %>% 
  ggplot(aes(x = Budget, 
             colour= Test_Results))+
  geom_freqpoly(position = position_dodge(), 
                 bins = 8,
                 size = 1.5)+
  scale_colour_tron(labels = c("Dubious", "Men", "No Talking", "No Women", "Okay"))+
  theme_dark()+
  theme(text = element_text(family = "Courier New", face = "bold"),
        plot.title = element_text(colour = "white", 
                                  size = 18),
        panel.background = element_rect(fill = "#2D2D2D" ),
        plot.background = element_rect(fill = "#2D2D2D"),
        legend.background = element_rect(fill = "#2D2D2D"),
        legend.key = element_rect("#2D2D2D"), 
        legend.text = element_text(colour = "white", 
                                   size = 13),
        legend.title = element_text(colour = "white", 
                                    size = 15),
        axis.text = element_text(color = "white", 
                                 size = 15), 
        axis.title = element_text(colour = "white", 
                                  size = 15))+
  labs(title = "How many high budget films fail the bechdel test?", 
       x = "Budget($)", 
       y = "Number of movies", 
       colour = "Bechdel Test Category")+
  ggsave(here("2021_03_9", "Outputs", "Categories.png" ))

mov1



mov2 <- movies %>% 
  select(budget, domgross, binary) %>% 
  rename(Pass_Fail = binary, 
         Budget = budget, 
         Domestic_Gross = domgross) %>% 
  ggplot(aes(x = Budget, 
             colour= Pass_Fail))+
  geom_freqpoly(position = position_dodge(), 
                 bins = 8,
                 size = 1.5)+
  scale_colour_tron()+
  theme_dark()+
  theme(text = element_text(family = "Courier New", face = "bold"),
        plot.title = element_text(colour = "white", 
                                  size = 18),
        panel.background = element_rect(fill = "#2D2D2D" ),
        plot.background = element_rect(fill = "#2D2D2D"),
        legend.background = element_rect(fill = "#2D2D2D"),
        legend.key = element_rect("#2D2D2D"), 
        legend.text = element_text(colour = "white", 
                                   size = 13),
        legend.title = element_text(colour = "white", 
                                    size = 15),
        axis.text = element_text(color = "white", 
                                 size = 15), 
        axis.title = element_text(colour = "white", 
                                  size = 15))+
  labs(title = "How many high budget films fail the bechdel test?", 
       x = "Budget($)", 
       y = "Number of movies", 
       colour = "Bechdel Test Category")+
  ggsave(here("2021_03_9", "Outputs", "Pass_Fail.png" ))


mov2

