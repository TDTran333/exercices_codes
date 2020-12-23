# Loading in packages
library(readr)
library(dplyr)
library(ggplot2)

# player_id: a unique player id
# dt: the date
# level: the level number within the episode, from 1 to 15.
# num_attempts: number of level attempts for the player on that level and date.
# num_success: number of level attempts that resulted in a success/win for the player on that level and date.

data <- read_csv("datasets/candy_crush.csv")
head(data, n = 6)

# Count and display the number of unique players
print("Number of players:")
data %>% 
  select(player_id) %>% 
  unique() %>% 
  count()

# Display the date range of the data
print("Period for which we have data:")
data %>%
  summarize(min(dt), max(dt))

# Calculating level difficulty
difficulty <- data %>% 
  group_by(level) %>% 
  summarise(attempts = sum(num_attempts), wins = sum(num_success)) %>%
  mutate(p_win = wins / attempts)

# Printing out the level difficulty
difficulty

# Plotting the level difficulty profile
ggplot(difficulty, aes(level, p_win)) +
  geom_line() +
  scale_x_continuous(breaks = 1:length(difficulty$level)) +
  scale_y_continuous(labels = scales::percent)

# Adding points and a dashed line
ggplot(difficulty, aes(level, p_win)) +
  geom_line() +
  scale_x_continuous(breaks = 1:length(difficulty$level)) +
  scale_y_continuous(labels = scales::percent) +
  geom_point() +
  geom_hline(yintercept = 0.1, lty = 2)

# Computing the standard error of p_win for each level
difficulty <- difficulty %>%
  mutate(error = sqrt(p_win * (1 - p_win) / attempts))

# Adding standard error bars
ggplot(difficulty, aes(level, p_win)) +
  geom_line() +
  scale_x_continuous(breaks = 1:length(difficulty$level)) +
  scale_y_continuous(labels = scales::percent) +
  geom_point() +
  geom_hline(yintercept = 0.1, lty = 2) +
  geom_errorbar(aes(ymin = p_win - error, ymax = p_win + error))

# The probability of completing the episode without losing a single time
p <- difficulty %>% 
  select(p_win) %>% 
  prod()

# Printing it out
p

