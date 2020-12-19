# Load the packages
library(readr)
library(dplyr)

# Read in the data from the datasets folder
wwc_raw <- read_csv(file = "datasets/2019_WWCFIFA_summary.csv")

# Check the dimensions and structure of the data
glimpse(wwc_raw)
summary(wwc_raw)
str(wwc_raw)

# Read in the data specifying column types
wwc_raw <- read_csv("datasets/2019_WWCFIFA_summary.csv",
                    col_types = cols(
                      Round = col_factor(),
                      Date = col_date(format = "%m/%d/%y"),
                      Venue = col_factor() 
                    )
)

# Look at the summary and structure of the data
glimpse(wwc_raw)
summary(wwc_raw)

# Print the dataset
wwc_raw

# load the package
library(tidyr)

# Remove rows of NA
wwc_1  <- wwc_raw  %>% 
  rename_all(tolower)  %>% 
  filter(!is.na(round))

# Get the dimensions and inspect the first 10 and last 10 rows
dim(wwc_1)
head(wwc_1, n = 10)
tail(wwc_1, n = 10)

# Housekeeping
wwc_2  <- wwc_1

# Find, view, and replace NA in column date
index_date  <- which(is.na(wwc_2$date))
index_date
wwc_2$date[index_date]  <- "2019-06-09"

# Find, view, and replace NA in column venue
index_venue  <- which(is.na(wwc_2$venue))
index_venue
wwc_2$venue[index_venue] <- "Groupama Stadium"

# Separate columns and replace NA (you've got this!)
wwc_3  <- wwc_2  %>% 
  separate(score, into = c("home_score", "away_score"), sep = "-", convert = TRUE) %>% 
  separate(pks, into = c("home_pks", "away_pks"), sep = "-", convert = TRUE) %>%
  mutate(home_pks = replace_na(home_pks, 0),
         away_pks = replace_na(away_pks, 0))

# Print the data
wwc_3

# Housekeeping for plot size
options(repr.plot.width=6, repr.plot.height=4)

# Load the package
library(ggplot2)

# Make a boxplot of attendance by venue and add the point data
ggplot(wwc_3, aes(venue, attendance)) +
  geom_boxplot() +
  # .... YOUR CODE FOR TASK 6 ....
  geom_jitter(color = "red", size = 0.5) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(labels = scales::comma)

# Summarize the number of games, and min and max attendance for each venue
wwc_3  %>% 
  group_by(venue) %>% 
  summarize(games = n(), min = min(attendance), max = max(attendance))

# Correct the outlier
wwc_4  <- wwc_3  %>% 
  mutate(attendance = replace(attendance, which(attendance == max(attendance)), 57900))

# Print an updated summary table 
wwc_venue_summary <- wwc_4  %>% 
  group_by(venue) %>% 
  summarize(games = n(), min = min(attendance), max = max(attendance))

wwc_venue_summary

# Housekeeping for plot size
options(repr.plot.width=6, repr.plot.height=4)

# Prettier boxplot of attendance data by venue
wwc_4  %>% 
  ggplot(aes(x = forcats::fct_reorder(venue, attendance), y = attendance)) +
  geom_boxplot() +
  geom_jitter(color = "red", size = 0.5) +
  coord_flip() +
  theme(axis.text.x = element_text(angle = 45, , hjust = 1)) +
  labs(title = "Distribution of attendance by stadium",
       subtitle = "2019 FIFA Women's World Cup",
       x = "Stadium", 
       y = "Attendance")

# Housekeeping for plot size
options(repr.plot.width=6, repr.plot.height=4)

# Line plot of attendance over time
wwc_4  %>% 
  ggplot(aes(date, attendance, color = venue)) +
  geom_line() +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 8)) +
  guides(col = guide_legend(nrow = 3)) +
  labs(title = "Stadium attendance during the tournament",
       subtitle = "2019 FIFA Women's World Cup",
       x = "Date", 
       y = "Attendance",
       color = "") 

