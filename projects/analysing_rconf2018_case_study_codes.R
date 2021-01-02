# Print the first element of the list to the console 
rstudioconf[[1]]

# Create a sublist of non-retweets
non_rt <- discard(rstudioconf, "is_retweet")

# Extract the favorite count element of each non_rt sublist
fav_count <- map_dbl(non_rt, "favorite_count")

# Get the median of favorite_count for non_rt
median(fav_count)

# Keep the RT, extract the user_id, remove the duplicate
rt <- keep(rstudioconf, "is_retweet") %>%
  map("user_id") %>% 
  unique()

# Remove the RT, extract the user id, remove the duplicate
non_rt <- discard(rstudioconf, "is_retweet") %>%
  map("user_id") %>% 
  unique()

# Determine the total number of users
union(rt, non_rt) %>% length()

# Determine the number of users who has just retweeted
setdiff(rt, non_rt) %>% length()

# Prefill mean() with na.rm, and round() with digits = 1
mean_na_rm <- partial(mean, na.rm = TRUE)
round_one <- partial(round, digits = 1)

# Compose a rounded_mean function
rounded_mean <- compose(round_one, mean_na_rm)

# Extract the non retweet  
non_rt <- discard(rstudioconf, "is_retweet")

# Extract "favorite_count", and pass it to rounded_mean()
non_rt %>%
  map_dbl("favorite_count") %>%
  rounded_mean()

# Combine as_vector(), compact(), and flatten()
flatten_to_vector <- compose(as_vector, compact, flatten)

# Complete the fonction
extractor <- function(list, what = "mentions_screen_name"){
  map( list , what ) %>%
    flatten_to_vector()
}

# Create six_most, with tail(), sort(), and table()
six_most <- compose(tail, sort, table)

# Run extractor() on rstudioconf
extractor(rstudioconf) %>% 
  six_most()

# Extract the "urls_url" elements, and flatten() the result
urls_clean <- map(rstudioconf, "urls_url") %>%
  flatten()

# Remove the NULL
compact_urls <- compact(urls_clean)

# Create a mapper that detects the patten "github"
has_github <- as_mapper(~ str_detect(.x, "github"))

# Look for the "github" pattern, and sum the result
map_lgl( compact_urls, has_github ) %>%
  sum()

# Complete the function definition
str_prop_detected <- function(string, pattern) {
  string %>%
    # Detect the pattern
    str_detect(pattern) %>%
    # Calculate the mean
    mean()
} 

# Create flatten_and_compact()
flatten_and_compact <- compose(compact, flatten)

# From previous step
str_prop_detected <- function(vec, pattern) {
  vec %>%
    str_detect(pattern) %>%
    mean()
} 
flatten_and_compact <- compose(compact, flatten)

rstudioconf %>%
  # From each element, extract "urls_url"
  map("urls_url") %>%
  # Flatten and compact
  flatten_and_compact() %>% 
  # Get proportion of URLs containing "github"
  str_prop_detected("github")

# Create mean_above, a mapper that tests if .x is over 3.3
mean_above <- as_mapper(~ .x > 3.3)

# Prefil map_at() with "retweet_count", mean_above for above, 
# and mean_above negation for below
above <- partial(map_at, .at = "retweet_count", .f := mean_above )
below <- partial(map_at, .at = "retweet_count", .f := negate(mean_above) )

# Map above() and below() on non_rt, keep the "retweet_count"
ab <- map(non_rt, above) %>% keep("retweet_count")
bl <- map(non_rt, below) %>% keep("retweet_count")

# Compare the size of both elements
length(ab)
length(bl)

# Get the max() of "retweet_count" 
max_rt <- map_dbl(non_rt, "retweet_count") %>% 
  max()

# Prefill map_at() with a mapper testing if .x equal max_rt
max_rt_calc <- partial(map_at, .at = "retweet_count", .f := ~ .x == max_rt )

res <- non_rt %>%
  # Call max_rt_calc() on each element
  map(max_rt_calc) %>% 
  # Keep elements where retweet_count is non-zero
  keep("retweet_count") %>% 
  # Flatten it
  flatten()

# Print the "screen_name" and "text" of the result
res$screen_name
res$text