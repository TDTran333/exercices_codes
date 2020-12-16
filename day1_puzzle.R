library(microbenchmark)
library(tidyverse)

input <- read_csv("input.txt", col_names = "numbers") %>% as.matrix()

# Let's think. We want to find two random numbers that sum up to 2020 and multiply them.

test <- function() {
for (i in 1:(nrow(input) - 1)) {
  for (j in 2:nrow(input)) {
    if (input[i, ] + input[j, ] == 2020) {
    ans <- (input[i, ] * input[j, ])
    }
  }
}

ans
}

test2 <- function() {
input %>% 
  as_tibble() %>% 
  crossing(numbers1 = ., numbers2 = .) %>% 
  filter(numbers1 + numbers2 == 2020) %>%
  pull() %>% 
  prod()
}

test3 <- function() {
  prod(intersect(input, 2020 - input))
}

microbenchmark(test, test2, test3, test4, test5, times = 10000L)

# Let's think. We now want to find three random numbers that sum up to 2020 and multiply them.

# First try below, incredibly inefficient way to code. Nested Triple loop... 

for (i in 1:(nrow(input) - 2)) {
  for (j in 2:nrow(input)) {
    for (k in 3:nrow(input)) {
      if (input[i, ] + input[j, ] + input[k, ] == 2020) {
      ans <- (input[i, ] * input[j, ] * input[k, ])
      }
    }
  }
}

ans

# 2nd try at a better solution


test4 <- function() {
  prod(intersect(input, 2020 - outer(input, input, "+")))
}


test5 <- function() {
  input %>% 
    as_tibble() %>% 
    crossing(numbers1 = ., numbers2 = ., numbers3 = .) %>% 
    filter(numbers1 + numbers2 + numbers3 == 2020) %>%
    select(1) %>% 
    unique() %>% 
    pull() %>% 
    prod()
}
