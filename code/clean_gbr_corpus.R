
library(tidyverse)

uk_manifestos <- read_csv(here("data/gbr_manifesto_corpus.csv")) |>
    bind_rows(read_csv(here("data/gbr_manifesto_corpus2.csv"))) |>
    mutate(
        year = str_sub(date, 1, 4) |> as.numeric(),
        doc_id = str_c(party, "_", date),
    ) |>
    rename(party_id = party) |>
    mutate(
        partyname = case_when(
            partyname == "Conservative Party" ~ "Tories",
            partyname == "Labour Party" ~ "Lab",
            partyname == "Liberal Democrats" ~ "Lib Dems",
            partyname == "United Kingdom Independence Party" ~ "UKIP",
            partyname == "Reform UK" ~ "Reform UK",
            partyname == "Green Party of England and Wales" ~ "Green",
            partyname == "Scottish National Party" ~ "SNP",
            partyname == "Plaid Cymru" ~ "PC",
            .default = partyname
        ), 
        party_year = str_c(partyname, "_", year))

write_csv(uk_manifestos, here("data/gbr_manifesto_corpus_clean.csv"))
