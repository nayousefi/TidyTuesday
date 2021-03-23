########## Tidy Tuesday: UN Votes##########
##### Created by: Nikolas Yousefi #############
##### Created on: 2021-03-23 ###############

### Load libraries #####################
library(tidyverse)
library(here)
library(tidytuesdayR)
library(ggeasy)

### Load data ##########################
unvotes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-23/unvotes.csv')

View(unvotes)

issues <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-23/issues.csv')

View(issues)

### Data Analysis #######################

unvoteissues <- left_join(unvotes, issues) # Joining both sets by rcid

unclean <- unvoteissues %>% 
  select(vote, country, issue) %>%# Selecting groups of interest
  filter(vote != "abstain") %>% # Removing any "abstain" votes
  filter(country == c("United States", 
                      "Mexico", 
                      "Canada",
                      "Cuba",
                      "Costa Rica",
                      "Dominican Republic",
                      "Nicaragua",
                      "Guatemala",
                      "Haiti",
                      "Honduras",
                      "Jamaica",
                      "Panama")) %>% # Choosing 12 NA countries with highest populations
  drop_na() %>% # Dropping all NAs
  mutate(vote_2 = ifelse(vote == "yes",1,0)) %>% # Making separate column to count yes/no votes in binary
  group_by(issue, country) %>% # Grouping by aforementioned categories
  summarise(meanvote = mean(vote_2)) # Getting means of the new mutated binary column

View(unclean)

### Graph Data ########################
ggplot(unclean, aes(x = country, 
                    y = issue, 
                    fill = meanvote))+ # Setting up the plot
  geom_tile(color = "white", size = 0.1)+ # Setting up a heatmap/tile plot
  scale_fill_gradient(low = "#89CFF0",
                      high = "#191970", 
                      breaks = c(0, 1), 
                      labels = c("No", "Yes"))+ # Selecting colors, breaks, and labels of the legend
  theme(axis.title = element_blank(), # Removing redundant axes names
        axis.text.x = element_text(angle = 50, hjust = 1), #  Rotating x axis text for easier reading
        axis.text = element_text(size = 12), # Increasing axis element text size
        plot.title = element_text(size = 20), # Increasing title size
        plot.subtitle = element_text(hjust = 0.5))+  # Shifting subtitle to the middle
  labs(title = "Average Votes of Major North American Countries by Issue",
       subtitle = "Top 12 NA countries by population",
       fill = "Majority Votes",
       caption = "data from rfordatascience/tidytuesday and Harvard Dataverse")+ # Labelling title, subtitle, and caption
  ggeasy::easy_center_title()+ # Centering the title
  ggsave(here("TT_2021-03-23", "Output", "TT_UNVotes_2021-03-02.png"),
         width = 14, height = 7) # Saving the output to the desired folder at a specific size
