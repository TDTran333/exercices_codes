# Load packages
library(tidyverse)

# Load the CSV data
super_bowls  <-  read_csv("datasets/super_bowls.csv")
tv  <-  read_csv("datasets/tv.csv")
halftime_musicians  <-  read_csv("datasets/halftime_musicians.csv")

# Display the first six rows of each tibble
head(super_bowls)
head(tv)
head(halftime_musicians)

# Summary of the TV data
summary(tv)

# Summary of the halftime musician data 
summary(halftime_musicians)

# Reduce the size of the plots
options(repr.plot.width = 5, repr.plot.height = 4)

# Plot a histogram of combined points
ggplot(super_bowls, aes(combined_pts)) +
  geom_histogram(binwidth = 5) +
  labs(x = "Combined Points", y = "Number of Super Bowls")

# Display the highest- and lowest-scoring Super Bowls
super_bowls  %>% 
  filter(combined_pts > 70 | combined_pts < 25)

# Reduce the size of the plots
options(repr.plot.width = 5, repr.plot.height = 4)

# Plot a histogram of point differences
ggplot(super_bowls, aes(difference_pts)) +
  geom_histogram(binwidth = 2) +
  labs(x = "Point Difference", y = "Number of Super Bowls")

# Display the closest game and largest blow out
super_bowls  %>% 
  filter(difference_pts == min(difference_pts) | difference_pts == max(difference_pts))

# Filter out Super Bowl I and join the game data and TV data
games_tv <- tv  %>% 
  filter(super_bowl != 1)  %>% 
  inner_join(super_bowls, by = "super_bowl")

# Create a scatter plot with a linear regression model
ggplot(games_tv, aes(difference_pts, share_household)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Point Difference", y = "Viewership (household share)")

# Convert the data format for plotting
games_tv_plot  <- games_tv %>% 
  gather(key = "category", value = "value", avg_us_viewers, rating_household, ad_cost)  %>% 
  mutate(cat_name = case_when(category == "avg_us_viewers" ~ "Average number of US viewers",
                              category == "rating_household" ~ "Household rating",
                              category == "ad_cost" ~ "Advertisement cost (USD)",
                              TRUE ~ as.character(category)))

# Plot the data
ggplot(games_tv_plot, aes(super_bowl, value)) +
  geom_line() +
  facet_wrap(~ cat_name, scales = "free", nrow = 3) + 
  labs(x = "Super Bowl", y = "") +
  theme_minimal()

# Filter and diplay halftime musicians before and including Super Bowl XXVII
( pre_MJ  <- halftime_musicians  %>% 
    filter(super_bowl <= 27) )

# Display the musicians who performed more than once
halftime_musicians  %>% 
  count(musician, sort = TRUE)  %>% 
  filter(n > 1) 

# Remove marching bands and data before Super Bowl XX
musicians_songs  <- halftime_musicians  %>% 
  filter(!str_detect(musician, "Marching"),
         !str_detect(musician, "Spirit"),
         super_bowl > 20)

# Plot a histogram of the number of songs per performance
ggplot(musicians_songs, aes(num_songs)) + 
  geom_histogram(binwidth = 1) +
  labs(x = "Number of songs per halftime show", y = "Number of musicians")

# Display the musicians with more than four songs per show
musicians_songs  %>% 
  filter(num_songs > 4)  %>% 
  arrange(desc(num_songs))

