
## Script Documentation: Scraping Discursos (Speeches) from the Casa Rosada Website
## Author : Malo Jan 
# Overview:
# This script scrapes speeches (discursos) from the official website of the Argentine government (Casa Rosada). 
# The script extracts the URLs of individual speeches from multiple pages, collects detailed content from each speech,
# and processes the data to include the title, publication date, and main text of each speech.
# 
# The script performs the following steps:
# 1. Generates a list of URLs for pages of speeches, based on the pagination system on the website.
# 2. Scrapes the URLs of individual speeches from the generated page URLs.
# 3. Extracts detailed content from each speech, including the title, date, and the speech's main text.
# 4. Converts the Spanish month names in the date to numerical values and standardizes the date format.
# 
# This script is designed to handle a series of pages containing up to 40 speeches each. For demonstration, 
# the script processes the first 5 pages of speeches, but this can be extended to cover all available pages.
#
# Dependencies:
# - tidyverse: A collection of R packages for data manipulation and visualization.
# - rvest: A package for web scraping, specifically for reading HTML pages and extracting data.
#
# Before running this script, ensure that both of these packages are installed and loaded into the R environment.

# Load required libraries

needs(tidyverse, rvest)

arg_main_page <- "https://www.casarosada.gob.ar/informacion/discursos"

# There are 34 pages of 40 speeches in the 18th of october, the page are ?start=0, ?start=40, ?start=80, etc.

page_nums <-seq(0, 38*40, 40)

arg_pages_urls <- str_c(arg_main_page, "?start=", page_nums)

# Create a function to extract the links

collect_arg_discursos_urls <- function(x) {
    page <- read_html(x)
    tibble(
        url = html_elements(page, "#jm-maincontent > main > div > section > div > div > div.contentboxes > div > div > a") |> html_attr("href") %>% 
            str_c("https://www.casarosada.gob.ar", .),
    ) 
}

arg_discursos_urls  <- map(arg_pages_urls[1:5], safely(collect_arg_discursos_urls), .progress = TRUE) |> 
    map("result") 

arg_discursos_urls <- arg_discursos_urls |> 
    unlist() |> 
    as_tibble()

arg_discursos_urls

#write_csv(arg_discursos_urls, "arg_discursos_urls.csv")
# Extract the content

# Create a function that convert spanish month names to numbers

month_to_number <- c("enero" = "01", "febrero" = "02", "marzo" = "03", "abril" = "04", "mayo" = "05", "junio" = "06", "julio" = "07", "agosto" = "08", "septiembre" = "09", "octubre" = "10", "noviembre" = "11", "diciembre" = "12")

collect_arg_discursos_content <- function(x) {
    page <- read_html(x)
    tibble(
        country = "Argentina",
        title = html_element(page, "h2 strong") |> html_text2(),
        date = html_element(page, "div time") |> html_text2() |> str_extract("\\d.+") |> str_replace_all(" de ", "-") |> str_replace_all(month_to_number) |> dmy(),
        text = html_element(page, ".jm-allpage-in") |> html_text2() |> str_squish()
    )
}

arg_discursos <- map(arg_discursos_urls$value[1:5], safely(collect_arg_discursos_content), .progress = TRUE) |> 
    map_df("result")

arg_discursos
