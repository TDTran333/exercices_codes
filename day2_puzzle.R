library(tidyverse)

passwords <- read_csv("input2.txt", col_names = "passwords") %>% 
  extract(passwords, c("min", "max", "letter", "password"), regex = "(\\d+)-(\\d+) (\\w): (.*)", convert = TRUE)

test <- read_csv("input2.txt", col_names = "passwords")

unglue::unglue_data(test[[1]], "{min}-{max} {letter}: {password}", convert = TRUE)

# Let's think. How many passwords are valid according to their policies?
# Each line gives the password policy and then the password. 
# The password policy indicates the lowest and highest number 
# of times a given letter must appear for the password to be valid.

passwords %>% 
  mutate(count = map2_int(password, letter, str_count)) %>% 
  filter(count >= min, count <= max)

# 2nd part
# Each policy actually describes two positions in the password, 
# where 1 means the first character, 2 means the second character, and so on.
# Exactly one of these positions must contain the given letter.

passwords %>% 
  mutate(count = (str_sub(password, min, min) == letter) + 
                 (str_sub(password, max, max) == letter)) %>% 
  filter(count == 1)
