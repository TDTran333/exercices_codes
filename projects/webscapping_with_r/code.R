library(rvest)
library(tidyverse)

html_excerpt_raw <- '
<html> 
  <body> 
    <h1>Web scraping is cool</h1>
    <p>It involves writing code – be it R or Python.</p>
    <p><a href="https://datacamp.com">DataCamp</a> 
		has courses on it.</p>
  </body> 
</html>'
# Turn the raw excerpt into an HTML document R understands
html_excerpt <- read_html(html_excerpt_raw)
html_excerpt
# Print the HTML excerpt with the xml_structure() function
xml_structure(html_excerpt)

# Read in the corresponding HTML string
list_html <- read_html(list_html_raw)
# Extract the ol node
ol_node <- list_html %>% html_node('ol')
# Extract and print the nodeset of all the children of ol_node
ol_node %>% html_children()

# Extract all the a nodes from the bulleted list
links <- hyperlink_html %>%
  read_html() %>%
  html_nodes('ul') %>%
  html_nodes('a')

# Parse the nodes into a data frame
link_df <- tibble(
  domain = links %>% html_attr('href'),
  name = links %>% html_text()
)

link_df

# Read in the HTML
languages_html <- read_html(languages_raw_html)
# Select the div and p tags and print their text
languages_html %>%
  html_nodes('div, p') %>%
  html_text()

# Extract only the text of the computer languages (without the sub lists)
languages_html %>% 
  html_nodes('ul#languages > li') %>% 
  html_text()

# Extract the actors in the cells having class "actor"
actors <- roles_html %>% 
  html_nodes(xpath = '//table//td[@class = "actor"]') %>%
  html_text()
actors

# Extract the roles in the cells having class "role"
roles <- roles_html %>% 
  html_nodes(xpath = '//table//td[@class = "role"]/em') %>% 
  html_text()
roles

# Extract the functions using the appropriate XPATH function
functions <- roles_html %>% 
  html_nodes(xpath = '//table//td[@class = "role"]/text()') %>%
  html_text(trim = TRUE)
functions

# Define a throttled read_html() function with a delay of 0.5s
read_html_delayed <- slowly(read_html, 
                            rate = rate_delay(0.5))
# Construct a loop that goes over all page urls
for(page_url in mountain_wiki_pages){
  # Read in the html of each URL with a delay of 0.5s
  html <- read_html_delayed(page_url)
  # Extract the name of the peak and its coordinates
  peak <- html %>% 
    html_node("#firstHeading") %>% html_text()
  coords <- html %>% 
    html_node("#coordinates .geo-dms") %>% html_text()
  print(paste(peak, coords, sep = ": "))
}

