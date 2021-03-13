########## Tidy Tuesday: Bechdel Test##########
##### Created by: Nikolas Yousefi #############
##### Created on: 2021-03-09 ###############

### Load libraries #####################
library(tidyverse)
library(here)
library(tidytuesdayR)
library(ggeasy)

### Load data ##########################
raw_bechdel <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-09/raw_bechdel.csv')

View(raw_bechdel)

### Data Analysis ##########################
bechdel <- raw_bechdel %>%
  pivot_wider(names_from = year,
              values_from = rating) %>% # Pivoting wider to allow the years to be their own columns for organization
  pivot_longer(cols = "1888":"2021",
               names_to = "Variables",
               values_to = "Values") %>% # Pivoting longer to condense and rename the years and ratings
  drop_na() %>% # Dropping all NAs from the rows 
  select(Variables, Values) %>% # Selecting which groups to include
  group_by(Variables) %>% # Grouping by "Variables"
  summarise(mean_rating = mean(Values)) # Taking the mean rating of the movies per year

View(bechdel)

### Graphing Plot ##########################
ggplot(bechdel, aes(x=Variables, y=mean_rating, group = 1))+ # Plotting the graph
  geom_point(size = 1.5, color = "red")+ # Using points to denote the average
  geom_line()+ # Connecting the dots via a line to show upwards and downwards trends
  scale_x_discrete(breaks=c(1900, 1920, 1940, 1960, 1980, 2000, 2020))+ # Cleaning up the axes
  theme_bw() + # Choosing the basic black/white theme
  labs(x = "Year", 
       y = "Average Bechdel Rating", 
       title = "Average Bechdel Rating of Movies from 1888 to 2021",
       caption = "data from rfordatascience/tidytuesday and FiveThirtyEight")+ # Labeling axes, adding title and caption
  ggeasy::easy_center_title()+ # Centering the title
  ggsave(here("TT_2021-03-09", "Output", "TT_BechdelTest_2021-03-02.png"),
         width = 14, height = 7) # Saving the output to the desired folder at a specific size
