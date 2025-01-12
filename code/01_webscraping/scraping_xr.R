# Script Documentation: XR Press Release Scraping and Processing
## Author: Malo Jan

# Overview:
# This script scrapes press releases from the Extinction Rebellion France website 
# (https://extinctionrebellion.fr/actualites/all/). It extracts URLs of individual press releases 
# and collects detailed information from each press release, including the title, publication date, 
# author, main text, and the original URL of the article.
# 
# The script performs the following steps:
# 1. Defines a function to extract URLs of press releases from the main news page.
# 2. Extracts all press release URLs from the given main page.
# 3. Defines another function to extract detailed content for each press release, including:
#    - The title of the press release.
#    - The publication date.
#    - The author's name.
#    - The main body text, cleaned to remove extraneous content.
#    - The URL of the press release.
# 4. Scrapes and processes all press releases by applying the content extraction function to each URL.
# 5. Combines the extracted data into a structured tibble for analysis or storage.

# Dependencies:
# - tidyverse: A collection of R packages for data manipulation and visualization.
# - rvest: A package for web scraping, specifically for reading HTML pages and extracting data.

# Output:
# - The final result (`xr_cp`) is a tibble containing the scraped press release data, ready for further analysis.

# Notes:
# - The script is currently designed to scrape all press releases listed on the main page of the website.
# - Ensure both `tidyverse` and `rvest` packages are installed and loaded before running the script.

# Load required libraries
needs(tidyverse, rvest)


page <- "https://extinctionrebellion.fr/actualites/all/"

extract_urls <- function(x) {
  x |> 
    read_html() |> 
    html_elements("#visible-blog-posts .o-button") |> 
    html_attr("href") %>%
    str_c("https://extinctionrebellion.fr", .)
}

urls <- extract_urls(page)

extract_content <- function(x) {
  page <- read_html(x)
  
  tibble(
    title = page |> html_element(".my-3") |> html_text(),
    date = page |> html_element(".date") |> html_text(),
    author = page |> html_element(".author") |> html_text(),
    text = page |> html_element(".post-content") |> html_text2() |> str_squish() |> str_remove("Lire d'autres articles sur le site web.+"),
    url = x
  )
}

xr_cp <- map_df(urls, extract_content, .progress = T)

xr_cp