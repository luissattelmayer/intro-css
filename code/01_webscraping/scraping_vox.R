# Script Documentation: VOX Press Release Scraping and Processing
## Author : Malo Jan 
# Overview:
# This script scrapes press releases from the VOX Spain website (https://www.voxespana.es/noticias/). 
# It extracts URLs of individual press releases from multiple news pages, collects relevant data 
# from each press release, processes the content (including dates), and saves the results in a CSV file.
# The data includes the press release title, subtitle, publication date, main text, and associated tags.
#
# The script performs the following steps:
# 1. Generates a list of URLs for news pages on the VOX website.
# 2. Scrapes the URLs of individual press releases from the news pages.
# 3. Collects the content of each press release, including title, subtitle, date, text, and tags.
# 4. Converts the Spanish date format to a standard date format (YYYY-MM-DD).
# 5. Saves the extracted and processed content to a CSV file for further analysis or use.

# Dependencies:
# - tidyverse: A collection of R packages for data manipulation and visualization.
# - rvest: A package for web scraping, specifically for reading HTML pages and extracting data.
#


# Load required libraries
needs(tidyverse, rvest)

# Generate a list of URLs for VOX news pages
vox_page_urls <- str_c("https://www.voxespana.es/noticias/page/", 1:1051)

# Function to collect URLs of press releases from a VOX page
collect_vox_pr_urls <- function(url) {
    # Scrape the page for press release links
    urls <- url |>
        read_html() |>
        html_nodes(".title .post-link") |>
        html_attr("href")
    
    return(urls)
}

# Collect press release URLs from the first two pages for demonstration purposes

vox_pr_urls <- map(vox_page_urls[1:2], safely(collect_vox_pr_urls), .progress = TRUE) |> 
    map("result") |> 
    unlist()

# Display the collected press release URLs

vox_pr_urls

# Function to collect the content of a VOX press release
collect_vox_pr_content <- function(url) {
    # Read the press release page
    page <- url |> 
        read_html()
    
    # Extract relevant content fields
    title <- page |> 
        html_element(".entry-title") |> 
        html_text2()
    
    subtitle <- page |> 
        html_element(".entry-subtitle") |> 
        html_text2()
    
    date <- page |> 
        html_element(".entry-date") |> 
        html_text2()
    
    text <- page |> 
        html_element(".article-content") |> 
        html_text2()
    
    tags <- page |> 
        html_elements("#main #primary a") |> 
        html_text2() |> 
        str_c(collapse = "|")
    
    # Combine the fields into a tibble
    tibble(
        party = "VOX",
        title,
        subtitle,
        date,
        text,
        tags
    )
}

# Collect press release content for the first three URLs as a demonstration
vox_pr_content <- map(vox_pr_urls[1:3], safely(collect_vox_pr_content), .progress = TRUE) |>
    map_df("result")

# View the collected press release content
vox_pr_content 

# Function to convert a Spanish date string to a standard date format (YYYY-MM-DD)
convert_date <- function(date) {
    # Dictionary of Spanish to English month translations
    month_translations <- c(
        "enero" = "january",
        "febrero" = "february",
        "marzo" = "march",
        "abril" = "april",
        "mayo" = "may",
        "junio" = "june",
        "julio" = "july",
        "agosto" = "august",
        "septiembre" = "september",
        "octubre" = "october",
        "noviembre" = "november",
        "diciembre" = "december"
    )
    
    # Perform replacements and convert to a Date object
    date <- date |>
        str_replace_all(month_translations) |> # Replace all Spanish months with English
        str_remove(",") |>                     # Remove commas
        str_replace_all(" ", "-") |>           # Replace spaces with dashes
        dmy()                                 # Convert to Date
    
    return(date)
}

# Convert the date column in the press release content to standard format
vox_pr_content <- vox_pr_content |> 
    mutate(date = convert_date(date))

# Display the processed content
vox_pr_content

# Save the collected press release content to a CSV file

write_csv(vox_pr_content, "data/vox_press_releases.csv")
