# Load necessary libraries
library(tidyverse)  # For data manipulation and visualization
library(atrrr)      # For accessing Bluesky API data

# Documentation for the atrrr package:
# https://github.com/JBGruber/atrrr

# Authenticate with the Bluesky API using your account.
# This will redirect you to a page where you copy the app password.
#atrrr::auth("malojan.bsky.social") # Here you should replace with your own Bluesky account handle

# Retrieve all posts ("skeets") authored by the account "lesecologistes.fr"
greens_bluesky <- get_skeets_authored_by(actor = "lesecologistes.fr", parse = TRUE)
greens_bluesky  # View the retrieved posts

# Define a list of Bluesky handles representing different political parties
party_list <- c(
    "lesecologistes.fr",             # Greens
    "pcf.bsky.social",               # French Communist Party
    "lfi.bsky.social",               # La France Insoumise
    "partisocialiste.bsky.social"   # Socialist Party
)

# Define a function to retrieve up to 1000 skeets for a given account,
# and add a column identifying the account
get_all_skeets <- function(x) {
    skeets <- get_skeets_authored_by(actor = x, parse = TRUE, limit = 1000) |> 
        mutate(account = x)
    return(skeets)
}

# Apply the function to each party and combine results into a single dataframe
# Then extract date and month from the post timestamp
parties <- map_df(party_list, get_all_skeets) |>
    mutate(
        date = str_sub(indexed_at, 1, 10) |> ymd(),   # Extract full date
        month = str_sub(date, 1, 7) |> ym()           # Extract year-month
    )

# Plot total number of posts per account (horizontal bar chart)
parties |> 
    count(account) |>
    ggplot(aes(account, n, fill = account)) +
    geom_col(position = "dodge2") +
    coord_flip() +
    # Apply custom party colors (Green, Purple, Pink, Red)
    scale_fill_manual(values = c("#00C000", "#A22E89", "#FF3366", "#DD0000")) +
    theme_light()

# Plot number of posts per month (vertical bar chart)
parties |>
    count(month) |>
    ggplot(aes(month, n)) +
    geom_col() +
    theme_light()
