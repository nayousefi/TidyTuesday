########## Tidy Tuesday: Super Bowl Ads##########
##### Created by: Nikolas Yousefi #############
##### Updated on: 2021-03-02 ###############

### Load libraries #####################
library(tidyverse)
library(here)
library(tidytuesdayR)
library(ggeasy)
library(ghibli)

### Load data ##########################
youtube <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-02/youtube.csv')

View(youtube)

### Data and Graph ##########
youtube_clean <- youtube %>%
  drop_na(like_count) %>% # Dropping all NAs in the like count column
  select(funny, show_product_quickly, patriotic, celebrity, danger, animals, use_sex, like_count) # Choosing my groups of interest

youtube_longer <- youtube_clean %>% 
  rename("Animals" = animals,
         "Celebrity" = celebrity,
         "Danger" = danger,
         "Funny" = funny,
         "Patriotic" = patriotic,
         "Shows product quickly" = show_product_quickly,
         "Uses sex" = use_sex) %>% # Capitalizing and removing underscores from the categories
  pivot_longer(cols = c(1:7), 
               names_to = "variables", 
               values_to = "video_inclusion") %>% # Pivoting longer to make it easier to visualize the data
  filter(video_inclusion == TRUE) %>% # Removing all FALSE results so only TRUE ones remain
  group_by(variables, video_inclusion) %>% # Grouping by aforementioned categories
  summarise(mean_likes = mean(like_count)) # Taking the averages of the like counts per category

ggplot(youtube_longer, aes(x=variables,
        y=mean_likes,
        fill = video_inclusion)) +  # Setting up the axes and fill in of the graph
  geom_col(show.legend = FALSE) + # Setting up a column graph and removing the legend
  coord_flip()+ # Flipping the coordinates so the X variables are on the Y axis and vice versa
  theme_bw() + # Choosing the basic black/white theme
  scale_fill_manual(values = ghibli_palette("MarnieDark2")[c(4)])+ # Selecting my color of choice for the columns
  labs(x = "Video Characteristics", 
       y = "Average Likes", # Labeling my axes
       title = "Average Youtube likes per types of Super Bowl ads",
       caption = "data from rfordatascience/tidytuesday")+ # Labeling the axes and adding a title to the graph
  ggeasy::easy_center_title() + # Centering the title
  ggsave(here("TT_2021-03-02", "Output", "TT_SuperBowl_2021-03-02.png"),
         width = 10, height = 6) # Saving the output to the desired folder at a specific size
