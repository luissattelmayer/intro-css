needs(tidyverse, purrr, rvest, beepr, lubridate)


base_url <- "https://www.tagesschau.de/archiv?datum="

# Generate the URLs
urls <- expand.grid(year = 2006:2023,
                    month = 1:12,
                    page = 1:15) |>
  arrange(year, month, page) |>
  mutate(
    url = paste0(
      "https://www.tagesschau.de/archiv?datum=",
      year,
      "-",
      str_pad(month, 2, pad = "0"),
      "-01",
      "&pageIndex=",
      page
    )
  ) |>
  pull(url)


scrape_urls <- function(x) {
  read_html(x) |>
    html_nodes("a") |>
    html_attr("href") |>
    as_tibble() |>
    filter(str_detect(
      value,
      regex(
        "/inland/|/ausland/|/wirtschaft/|/wissen/|faktenfinder|investigativ",
        ignore_case = TRUE
      )
    )) |>
    filter(str_detect(value, "\\.html$")) |>
    mutate(value = paste0("https://www.tagesschau.de", value)) |>
    pull(value)
}


# creating the safely function separately enhances the speed later on
safe_scrape_urls <- safely(scrape_urls)

# mapping over our list of urls
results <- purrr::map(urls, safe_scrape_urls, .progress = TRUE)

# create a tibble with the urls (with magrittr pipe for placeholder)
final_urls_tagesschau <- results |>
  map("result") |>
  keep(~ !is_empty(.)) |>
  unlist() |>
  tibble(urls = .)       


# scrape the articles
scrape_articles <- function(x) {
  page_content <- read_html(x)
  
  date <- html_element(page_content, ".metatextline") |>
    html_text() |>
    str_remove_all("Stand: |Uhr|Aktualisiert am |\\s\\d{2}:\\d{2}") |>
    dmy()
  
  title <- html_element(page_content, ".seitenkopf__headline--text") |>
    html_text()
  
  supra_title <- html_element(page_content, ".seitenkopf__topline") |>
    html_text()
  
  content <- html_elements(page_content, ".textabsatz") |>
    html_text() |>
    str_remove("\\n") |>
    str_squish()
  
  # Create tibble and then collapse content
  tagesschau_articles <- tibble(date, title, supra_title, content) |>
    group_by(date, title, supra_title) |>
    summarize(content = paste(content, collapse = " "),
              .groups = 'drop')
  
}


# Wrap the scrape_articles function with safely
safe_scrape_articles <- safely(scrape_articles)

tagesschau_scraped <- final_urls_tagesschau$urls |>
  set_names() |>
  map(safe_scrape_articles, .progress = TRUE) |>
  map_dfr( ~ .x$result, .id = "url") |>
  arrange(date)

tagesschau_scraped_secondhalf <- final_urls_tagesschau$urls[18570:29772] |>
  set_names() |>
  map(safe_scrape_articles, .progress = TRUE) |>
  map_dfr( ~ .x$result, .id = "url") |>
  arrange(date)

tagesschau_scraped_full <- rbind(tagesschau_scraped,
                                 tagesschau_scraped_secondhalf,
                                 tagesschau_missing_articles) |>
  arrange(date)


write_csv(tagesschau_scraped_full, "tagesschau_scraped_full.csv")


# scraping was interrupted; anti_join to keep only missing urls
tagesschau_missing_urls <- final_urls_tagesschau |>
  mutate(url = urls) |>
  anti_join(tagesschau_scraped_full, by = "url") |>
  select(urls)

tagesschau_missing_articles <- tagesschau_missing_urls$urls |>
  set_names() |>
  map(safe_scrape_articles, .progress = TRUE) |>
  map_dfr( ~ .x$result, .id = "url") |>
  arrange(date)

tagesschau_sample <- tagesschau_scraped_full |>
  sample_n(2000)




url_to_search <- "https://www.tagesschau.de/investigativ/swr/umfrage-handwerk-101.html"

# For dataframe df1
index_df1 <- which(final_urls_tagesschau$urls == url_to_search)
if (length(index_df1) > 0) {
  cat("The URL was found at row", index_df1[1], "in df1.\n")
} else {
  cat("The URL was not found in df1.\n")
}


# For dataframe df2
index_df2 <- which(tagesschau_scraped$url == url_to_search)
if (length(index_df2) > 0) {
  cat("The URL was found at row", index_df2[1], "in df2.\n")
} else {
  cat("The URL was not found in df2.\n")
}
