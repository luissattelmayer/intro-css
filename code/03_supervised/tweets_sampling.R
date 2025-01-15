
needs(tidyverse)

# Import tweets from US representatives

tweets <- read_csv("https://www.dropbox.com/s/dpu5m3xqz4u4nv7/tweets_house_rep_party.csv?dl=1") |> 
    mutate(id = row_number())

set.seed(123)

sample_tweets <- tweets |> 
    filter(!is.na(party)) |> 
    slice_sample(n = 725)  |> 
    select(id, text)

emails <- c(
    "Niklas.ayris@sciencespo.fr",
    "yasmeen.bintimohdfaisalsyam@sciencespo.fr",
    "mathilde.blanchon@sciencespo.fr",
    "eva.bossuyt@sciencespo.fr",
    "romain.fernex@sciencespo.fr",
    "paulo.gugelmocavalheirodias@sciencespo.fr",
    "bautista.gutierrezguerra@sciencespo.fr",
    "vasundhara.krishnan@sciencespo.fr",
    "lucas.lam@sciencespo.fr",
    "matteo.lanoe@sciencespo.fr",
    "enzo.meneguz@sciencespo.fr",
    "saga.oskarsonkindstrand@sciencespo.fr",
    "marius.perrin@sciencespo.fr",
    "noemie.piolat@sciencespo.fr",
    "prarthana.puthran@sciencespo.fr",
    "qiange.liu@sciencespo.fr",
    "sofia.rua@sciencespo.fr",
    "tanja.schwenkreis@sciencespo.fr",
    "chloe.sibille@sciencespo.fr",
    "madalyn.stewart@sciencespo.fr",
    "matylda.strand@sciencespo.fr",
    "hugo.vacus@sciencespo.fr",
    "marijn.vergunst@sciencespo.fr",
    "maike.zink@sciencespo.fr",
    "sophia.moran@sciencespo.fr",
    "aleksejs.popovs@sciencespo.fr",
    "malo.jan@sciencespo.fr",
    "luis.sattelmayer@sciencespo.fr",
    "chalotte.boucher@sciencespo.fr"
) |> 
    rep(25) |> 
    sort() |> 
    as_tibble() |> 
    rename(email = value)

# Bind the emails to the sample_tweets

sample_tweets <- sample_tweets |> 
    bind_cols(emails) |> 
    select(email, id, text) |> 
    mutate(label = NA) 

write_csv(sample_tweets, "data/sample_to_annotate.csv")

