library(tidyverse)
library(lubridate)

dat <- read_csv(here::here("toggl_track", "summary.csv"))

clean <- dat %>%
  janitor::clean_names() %>% 
  mutate(date = dmy(date), project = factor(project, 
                                            levels = c("Sleep", 
                                                       "Productive Time", 
                                                       "Learning", 
                                                       "Exercice", 
                                                       "Personal", 
                                                       "Unproductive Time", 
                                                       "Idle", 
                                                       "Admin"))) %>% 
  select(project, duration, date) %>% 
  group_by(project, date) %>% 
  summarize(time = as.duration(sum(duration))/3600) %>%
  arrange(date) %>% 
  ungroup()

clean %>% 
  ggplot(aes(date, time, fill = project)) +
  geom_col() +
  facet_wrap(~project, scales = "free_y") +
  labs(x = "Week",
       y = "Hours",
       title = "Time Tracking Exercice. Last 7.5 Weeks of 2020.",
       fill = "") +
  theme(legend.position="none")

ggsave(here::here("toggl_track", "time_tracking.png"))
       