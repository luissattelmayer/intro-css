## Script Documentation: Scraping UK Government News Articles from the GOV.UK Website
## Author : Malo Jan
# Overview:
# This script scrapes news articles from the GOV.UK website (https://www.gov.uk/). 
# It extracts URLs of individual news articles from search result pages, collects detailed 
# information (such as article type, title, subtitle, publication date, source, body text, and topic) 
# from each article, and saves the results in CSV files for further analysis or use.
# 
# The script performs the following steps:
# 1. Generates a list of URLs for multiple pages of UK government news search results.
# 2. Scrapes the URLs of individual news articles from the search result pages.
# 3. Collects detailed content from each news article, including its type, title, subtitle, date, source, text, and topic.
# 4. Saves the URLs and the detailed content into separate CSV files.
# 
# This script is designed to work with large sets of news results (up to 6591 pages). For demonstration, 
# it collects data from the first 10 pages and 10 articles.
#
# Dependencies:
# - tidyverse: A collection of R packages for data manipulation and visualization.
# - rvest: A package for web scraping, specifically for reading HTML pages and extracting data.


# Load required libraries
needs(tidyverse, rvest)

# Generate a list of URLs for the UK government news pages
# These URLs correspond to multiple pages of search results

gov_uk_news <-
    str_c("https://www.gov.uk/search/news-and-communications?page=",
          1:6591)
# Function to collect the URLs of individual news articles from a given page

collect_gov_uk_news_urls <- function(url) {
    # Scrape the page and extract the href attributes of the news article links
    urls <- url |>
        read_html() |>
        html_elements("#js-results .govuk-link--no-underline") |>  # CSS selector for the article links
        html_attr("href")  # Extract the href attribute, which contains the article URL
    
    # Create a tibble with the full URLs by combining the base URL with the relative paths
    tibble(
        url = str_c("https://www.gov.uk", urls))  # Add the base URL to the relative URLs)
}

# Collect URLs of news articles from the first 10 pages of search results for demonstration

gov_uk_news_urls <-
    map(gov_uk_news[1:10], safely(collect_gov_uk_news_urls), .progress = TRUE) |>
    map_df("result")  # Combine results into a single tibble

# View the collected URLs
gov_uk_news_urls

# Save the collected news URLs to a CSV file for further use
#write_csv(gov_uk_news_urls, "data/gov_uk_news_urls.csv")

# Function to collect detailed content from an individual news article
collect_gov_uk_news_content <- function(url) {
    # Read the content of the article page
    page <- url |>
        read_html()
    
    # Extract relevant content fields
    tibble(
        # Extract the news article's type (context or category)
        type = page |>
            html_element(".gem-c-title__context") |>
            html_text2(),
        
        # Extract the title of the article
        title = page |>
            html_element(".govuk-heading-l") |>
            html_text2(),
        
        # Extract the subtitle of the article (lead paragraph)
        subtitle = page |>
            html_element(".gem-c-lead-paragraph") |>
            html_text2(),
        
        # Extract and convert the publication date (parse to Date object)
        date = page |>
            html_element(
                ".gem-c-metadata__definition~ .gem-c-metadata__definition"
            ) |>
            html_text2() |>
            dmy(),
        # Convert to Date format using the day-month-year format
        
        # Extract the source of the news (e.g., department or author)
        from = page |>
            html_element(".gem-c-metadata__definition .govuk-link") |>
            html_text2(),
        
        # Extract the main body text of the article
        text = page |>
            html_element(".govspeak") |>
            html_text2(),
        
        # Extract the topic or category of the article
        topic = page |>
            html_element(".gem-c-related-navigation__section-link--footer") |>
            html_text2()
    )
}

# Collect detailed content from the first 10 news article URLs for demonstration
gov_uk_news_content <-
    map(gov_uk_news_urls$url[1:10],
        safely(collect_gov_uk_news_content),
        .progress = TRUE) |>
    map_df("result")  # Combine results into a single tibble

gov_uk_news_content

# Save the collected news article content to a CSV file for further use
#write_csv(gov_uk_news_content, "data/gov_uk_news_content.csv")
