library(dplyr)
library(readr)
library(stringr)
library(ggplot2)
library(janitor)
library(lubridate)
library(forcats)
library(scales)


# Load data ---------------------------------------------------------------
data_dir <- here::here("toggl_track")
files <- list.files(path = data_dir,  pattern="\\.csv$")
data <- tibble(filename = files) %>% 
  mutate(file_contents = map(filename, ~ read_csv(file.path(data_dir, .))))


# Tidy Data ---------------------------------------------------------------
summary_report <- data %>% 
  filter(str_detect(filename, "summary")) %>%
  mutate(filename = str_extract(filename, "([0-9]+).*[^.csv]")) %>% 
  separate(filename, c("from", "to"), "_") %>%
  unnest(cols = file_contents) %>% 
  clean_names() %>% 
  select(from, to, project, duration) %>%
  mutate_at(vars(from, to), as.Date)
    

weekly_report <- data %>% 
  filter(str_detect(filename, "weekly")) %>%
  mutate(filename = str_extract(filename, "([0-9]+).*[^.csv]")) %>%
  separate(filename, c("from", "to"), "_") %>% 
  unnest(cols = file_contents) %>%
  select(-c(Client, User), project = Project) %>% 
  pivot_longer(cols = -c(from, to, project), names_to = "date", values_to = "duration") %>%
  filter(!is.na(duration), date != "Total") %>%
  mutate_at(vars(from, to, date), as.Date)

# Summary Data Analysis ---------------------------------------------------
factor_levels <- summary_report %>%
  group_by(project) %>%
  summarize(total = seconds_to_period(sum(duration))) %>%
  arrange(desc(total)) %>%
  select(project) %>%
  unlist() %>% 
  unname()
  
clean_summary_report <- summary_report %>% 
  mutate(project = factor(project, levels = factor_levels)) %>%
  select(project, duration, date = from) %>% 
  group_by(project, date) %>% 
  summarize(time = seconds_to_period(sum(duration))) %>%
  arrange(date) %>% 
  ungroup()

clean_summary_report %>% 
  ggplot(aes(date, time, fill = project, )) +
  geom_col(color = "black") +
  scale_y_time() +
  labs(x = "Week",
       y = "Hours",
       title = "Time Tracking by Week.",
       fill = "")

ggsave(here::here("toggl_track", "weekly_overview.png"))

clean_summary_report %>% 
  ggplot(aes(date, time, fill = project)) +
  geom_col(color = "black") +
  scale_y_time() +
  facet_wrap(~project, scales = "free_y") +
  labs(x = "Week",
       y = "Hours",
       title = "Time Tracking by Category.",
       fill = "") +
  theme(legend.position="none")

ggsave(here::here("toggl_track", "weekly_category.png"))


# Weekly Data Analysis ----------------------------------------------------
clean_weekly_report <- weekly_report %>% 
  mutate(project = factor(project, levels = factor_levels)) %>%
  select(project, date, duration) %>%
  group_by(project, date) %>% 
  summarize(time = seconds_to_period(sum(duration))) %>%
  arrange(date) %>% 
  ungroup()


clean_weekly_report %>%
  ggplot(aes(date, time, fill = project)) +
  geom_col(color = "black", width = 1) +
  scale_y_time() +
  facet_wrap(~ project) +
  labs(x = "Date",
       y = "Hours",
       title = "Time Tracking.",
       fill = "") +
  theme(legend.position="none")

ggsave(here::here("toggl_track", "daily_category.png"))


  