###Tidy Tuesday 2021-02-23: BLS####
###Created by: Shanelle Wikramanayake###
###2021-03_02##

###Packages###
library(tidyverse)
library(here)
library(tidytuesdayR)
library(LaCroixColoR)
library(extrafont)

###Load data###

tuesdata <- tidytuesdayR::tt_load('2021-03-02')

#view data
view(tuesdata)

#fonts
font_import()
loadfonts(device = "win")

#create labels
cats <- c("Animals", "Celebrities", "Danger", "Comedy", "Patriotic", "Sex")



ads <- tuesdata$youtube %>% 
  select(brand, funny, patriotic, 
         celebrity, danger, animals, use_sex) %>% #selecting categories and variables I want to look at 
  group_by(brand) %>% #group by brand
  rename(Animals = animals, #make acceptable column names
         Celebrity = celebrity, 
         Danger = danger, 
         Comedy = funny, 
         Patriotic = patriotic, 
         Sex = use_sex) %>% 
  pivot_longer(cols = Comedy:Sex, #pivot longer do categories in 1 column and sttaus for add for the category in the other
               names_to = "category",
               values_to = "status",
               values_transform = list(status = as.numeric)) %>% #Make the TRUE/FALSE binary instead
  group_by(brand, category) %>%  #group by brand
  filter(status ==1) %>%  #filter for only ads associated for a category
  ggplot(aes(x = brand, #plot by brand
             fill = brand))+ #fill by colour of brand
  geom_bar()+ #bar plot for frequency of brand per cateogry 
  coord_flip()+ #horizontal bar plot
  facet_wrap(~category,
             nrow = 3, #plots by category 
             scales = "free")+ #free scales
  labs(x = "Brand",
       y = "Number of Adds", 
       title = "Brand advertising strategies for the Superbowl", 
       fill = "Brand")+
  scale_fill_manual(values = lacroix_palette("PassionFruit", n= 10, type = "continuous"))+ #Since we're looking at add, I thought this would be a fun palette, because consumerism, right?
  theme(text = element_text(family = "Nunito", face = "bold"), 
        plot.title = element_text(face = "bold", 
                                  size = 23), 
        axis.text.x = element_text(size = 12), #adjust text size for axes
        axis.text.y = element_text(size = 11), 
        axis.title = element_text(size = 14))+
  ggsave(here("2021_03_2", "Outputs", "Superbowl_ads.png"), 
         height = 7, 
         width = 10)

ads
