###Tidy Tuesday 2021-02-09: Wealth and Income####
###Created by: Shanelle Wikramanayake###
###2021-02-10###

###Packages###
library(tidyverse)
library(here)
library(tidytuesdayR)


###Load data###
tuesdata <- tidytuesdayR::tt_load('2021-02-09')

glimpse(tuesdata)

head(tuesdata$income_aggregate)
view(tuesdata$student_debt)

studept <- tuesdata$student_debt
view(studept)


###Code###
years <- c(studept$year)
years <- c(unique(years))
years <- sort(years, decreasing = FALSE)
years
ggplot(data = studept, 
       mapping = aes(x= year, 
                     y = loan_debt)
       )+
  geom_col(alpha = 0.5)+
  geom_point(data = studept, 
           aes(x = year, 
               y = loan_debt, 
               colour = race)
           )+
  geom_line(data = studept, 
            aes(x = year, 
            y = loan_debt, 
            colour = race))+
  labs(title = "Student Loan Debt by Year for Different Races", 
       x = "Year", 
       y = "Loan Debt ($)",
       colour = "Race")+
  scale_colour_viridis_d()+
  scale_x_continuous(breaks = (years))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))+
  ggsave(here("Outputs", "StudentLoans.png"), 
         width = 7, 
         height = 7)
