########## Tidy Tuesday: Employment and Earnings - BLS Article##########
##### Created by: Nikolas Yousefi #############
##### Updated on: 2021-02-23 ###############

### Load libraries #####################
library(tidyverse)
library(here)
library(tidytuesdayR)
library(LaCroixColoR)

### Load data ##########################

earn <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-23/earn.csv')

View(earn)

### Data and Graph ##########

earn_clean <- earn %>%
  filter(!sex == "Both Sexes") %>% # Removing "Both Sexes" category from column
  filter(!race =="All Races") %>%  # Removing "All Races" category from column
  select(sex, race, median_weekly_earn) # Selecting columns for only sex, race, and median weekly earnings

ggplot(earn_clean,
       aes(x = race,
           y = median_weekly_earn,
           fill = sex)) + # Setting up the axes and legend
  geom_boxplot() + # Setting up a boxplot for these data
  geom_jitter(color="black", size=0.1, alpha=0.2) + # Adding each individual point of relevance
  theme_bw() + #Choosing the basic black/white theme
  theme(strip.background = element_rect(fill = "white", color = "white"), # Forcing a white background
        strip.text = element_text(face = "bold", size = 10), # Bolding and increasing text of axes
        plot.title = element_text(size=14)) + # Increasing title font size
  scale_fill_manual(values = lacroix_palette("Berry")[c(5, 1)]) + # Choosing a color palette to fill in the boxplots
  labs(x = "Race", 
       y = "Median Weekly Earning ($)", # Labeling my axes
       title = "Median Weekly Earning by Race and Sex") + # Adding a title for the graph
ggsave(here("TT_2021-02-23","Output","TT_2021-02-23_Graph.png"), 
       width = 9, height = 9) # Saving the graph in the appropraite folder
