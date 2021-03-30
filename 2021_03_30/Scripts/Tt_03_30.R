###Tidy Tuesday 2021-03-23: UN Votes####
###Created by: Shanelle Wikramanayake###
###2021-03_28##

###Packages###
library(tidyverse)
library(here)
library(tidytuesdayR)
library(ggridges)
library(cartography)

###Load data###
tuesdata <- tidytuesdayR::tt_load('2021-03-30')

seph <- tuesdata$sephora
ulta <- tuesdata$ulta
numbers <- tuesdata$allNumbers
cats <- tuesdata$allCategories

view(cats)

famous_brands <- c("Bobbi Brown", "Maybelline", "Dior", "ULTA", "Revlon")


colour_scale2 <- cats %>% 
  select(brand, lightness) %>% 
  filter(brand %in% famous_brands)%>%
  ggplot(aes(x = lightness, 
             y = brand))+
  geom_density_ridges_gradient(aes(fill = ..x..), 
                               scale = 1, 
                               size = 0.3)+
  scale_fill_gradientn(colours = carto.pal(pal1 = "brown.pal", 
                                           n1= 20),
                       name = "Lightness")+
  labs(title = "Shades used in five different make-up brands", 
       subtitle = "Shades are represented on a lightness scale of 0 -1", 
       x = "Lightness", 
       y = "Brand")+
  theme_grey()+
  theme(axis.title = element_text(size = 15), 
        axis.text = element_text(size = 13), 
        axis.text.y = element_text(face = "bold"))+
  ggsave(here("2021_03_30", "Outputs", "MakeUp_Shades.png"))



colour_scale2

