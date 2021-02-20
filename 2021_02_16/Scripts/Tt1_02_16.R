###Tidy Tuesday 2021-02-16: W.E.B Du Bois Challenge####
###Created by: Shanelle Wikramanayake###
###2021-02-19###

###Packages###
library(tidyverse)
library(here)
library(tidytuesdayR)
library(showtext)


###Load data###
tuesdata <- tidytuesdayR::tt_load('2021-02-16')
view(tuesdata$freed_slaves) #taking at data
freed_slaves <- tuesdata$freed_slaves #assigning freed slaves data
view(freed_slaves)

###Load font###

font_add_google("Public Sans")

####plot

freed_slaves %>% 
  pivot_longer(cols = Slave:Free, 
               names_to = "Slave_Freed", #make Slave/Free category in 1 column
               values_to = "Percent") %>% #percent in another column
  filter(Slave_Freed != "Free") %>% #filter for only percentage of slaves
  ggplot(aes(x = Year, #year on x axis
             y = Percent #percentage of slaves on y axis
             ) #y axis representing proportion of slaves
         )+
 
  geom_area(colour = "black")+ #using an area plot and set balck fill colour
  scale_x_continuous(c(1790, 1870), #set axis from 1790-1870
                     breaks = seq(1790, 1870, 10), #specify breaks for years
                     position = "top", 
                     expand = c(0,0)
                     )+ 
  scale_y_continuous(expand = c(0,0))+#set axis to top
  coord_cartesian(clip = "off")+ #allow annotations outside of graph 
  geom_text(aes(x = 1825, 
                y = 55, 
                label = "SLAVES \n\nESCALVES"), #adding label for slaves
            colour = "white", 
            size = 9)+
  geom_text(aes(x = 1825, 
                y = 90, 
                label = "FREE - LIBRE"), #adding label for freemen
            colour = "black", 
            size = 9)+
  geom_text(aes(x = 1790, 
                y = 92, 
                label = "8%"))+
  geom_text(aes(x = 1800, 
                y = 90, 
                label = "11%"))+ #adding percentage freemen
  geom_text(aes(x = 1810, 
                y = 88, 
                label = "13.5%"))+
  geom_text(aes(x = 1820, 
                y = 88, 
                label = "13%"))+
  geom_text(aes(x = 1830, 
                y = 87, 
                label = "14%"))+
  geom_text(aes(x = 1840, 
                y = 88, 
                label = "13%"))+
  geom_text(aes(x = 1850, 
                y = 90, 
                label = "12%"))+
  geom_text(aes(x = 1860, 
                y = 91, 
                label = "11%"))+
  geom_text(aes(x = 1870, 
                y = 90, 
                label = "100%"))+
  labs(title = "PROPORTION OF FREEMEN AND SLAVES AMONG AMERICAN NEGROES.\n\nPROPORTION DES NEGRES LIBRES ET DES ESCLAVES EN AMERIQUE.", #set title
       subtitle = "DONE BY ATLANTA UNIVERSITY")+ #set subtitle
  theme(plot.background = element_rect(colour = NA, 
                                       fill = "#dfd2c6"), #adding background colour for plot
        panel.background = element_rect(colour = "#238455",
                                        fill = "#238455"), #adding background colour for panel
        panel.grid = element_blank(), #removing gridlines
        panel.border = element_rect(fill = NA, 
                                    colour = NA),
        axis.ticks = element_blank(), #remove axis ticks 
        axis.text.y  = element_blank(), #removing axis text
        axis.text.x = element_text(size = 14, 
                                   face = "bold", 
                                   colour = "black"), #change axis text 
        axis.title = element_blank(), #removing axis titles
        plot.title = element_text(face = "bold",
                                  hjust = 0.5, 
                                  size = 14), #chaneg title text 
        plot.title.position = "panel",
        plot.subtitle = element_text(face = "bold",
                                     hjust = 0.5,
                                     size = 12), #change subtitle text

        plot.margin = margin(1, 1, 0.4, 1, "cm"))+ #adjust plot margins
  ggsave(here("2021_02_16", "Outputs", "Free_Slaves.png"), 
         height = 7, width = 5)


  

