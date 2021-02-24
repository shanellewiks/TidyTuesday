###Tidy Tuesday 2021-02-23: BLS####
###Created by: Shanelle Wikramanayake###
###2021-02-23##

###Packages###
library(tidyverse)
library(here)
library(tidytuesdayR)

###Load data###
tuesdata <- tidytuesdayR::tt_load('2021-02-23')

#view data
view(tuesdata$earn)
view(tuesdata$employed)
glimpse(tuesdata$earn)

earn <- tuesdata$earn %>% 
  filter(quarter == 4) %>% #filter for only 4th quarter
  select(sex, race, age, year, median_weekly_earn) %>% #select columns I want
  filter(race != "All Races", sex != "Both Sexes") %>% #remove All races and Both sexes categories
  relocate(age) %>% #bring age to the front
  ggplot(mapping = aes(x = year, #Plot year by median weekly earning 
                       y  = median_weekly_earn, 
                       colour = race, #colour code by race
                       shape = sex) #shape by sex
         )+
  geom_line()+ #add lineplot for year by median weekly earnings
  geom_point()+ #add points for year by median weekly earnings
  facet_wrap(~age, scales = "free")+ #Facet grid by each age group 
  scale_x_continuous(breaks = c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2010))+ #set the scale breaks for each year
  scale_colour_viridis_d()+ #set the colour scheme
  labs(title = "Median Weekly Income By Race for Different Age groups", #set title 
       y = "Median Weekly Income ($)", #set y axis label
       x = "Year", #set x axis label
       colour = "Race", #set legend title
       shape = "Sex")+ #set legend title 
  theme_bw()+ #set theme to bw to better show the partitions
  ggsave(here("2021_02_23", "Outputs", "Median_Weekly_Income.png"), #save my plot
         height = 5,
         width = 15)




