###Tidy Tuesday 2021-03-23: UN Votes####
###Created by: Shanelle Wikramanayake###
###2021-03_28##

###Packages###
library(tidyverse)
library(here)
library(tidytuesdayR)
library(gameofthrones)
library(unvotes)

###Load data###
tuesdata <- tidytuesdayR::tt_load('2021-03-23')
unvotes <- tuesdata$unvotes

view(unvotes)

roll_calls <- tuesdata$roll_calls
view(roll_calls)

issues <- tuesdata$issues
view(issues)

#join unvotes and issues

vote_issues <-inner_join(unvotes, issues, by = "rcid") %>% 
  select("country", "vote", "short_name", "issue") %>% 
  ggplot(mapping = aes(x = vote))+
  geom_bar()+
  facet_wrap(~issue)+
  labs(title = "Voting patterns for issues in the UN")
vote_issues



vetos = c("United States", "Russia", "France", "United Kingdom", "China")



vote_issues_vetos <-inner_join(unvotes, issues, by = "rcid") %>% 
  select("country", "vote", "short_name", "issue") %>% 
  filter(country %in% vetos) %>% 
  ggplot(mapping = aes(x = vote, 
                       fill = country))+
  geom_bar()+
  facet_wrap(~issue, 
             scales = "free")+
  scale_x_discrete(labels = c("Abstain", "Yes", "No"))+
  scale_fill_got(discrete = TRUE, 
                 option = "Daenerys")+
  labs(title = "Voting patterns for issues in the UN",
       subtitle = "How have the UNSC veto powers been voting on critical global issues debated in the UN General Assembly",
       x = "Country", 
       y = "Number of Votes",
       fill= "Country",
       caption = "Erik Voeten Data and Analyses of Voting in the UN General Assembly Routledge Handbook of International Organization, edited by Bob Reinalda (published May 27, 2013)")+
  theme_dark()+
  ggsave(here("2021_03_23", "Outputs", "Veto_votes.png" ))




vote_issues_vetos
