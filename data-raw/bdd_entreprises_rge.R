library(tidyr)
requete_ademe <- function() {

  # Récupération date de maj des données
  url_meta <- "https://data.ademe.fr/data-fair/api/v1/datasets/liste-des-entreprises-rge-2/"
  res_meta <- httr::GET(url_meta)
  content_meta <- httr::content(res_meta, as = "parsed")
  date_maj <- as.Date(content_meta$dataUpdatedAt)

  # intitialisation
  url_ademe <- "https://data.ademe.fr/data-fair/api/v1/datasets/liste-des-entreprises-rge-2/lines?size=10000"
  res <- httr::GET(url_ademe)
  content <- httr::content(res, as = "parsed")
  df <- data.table::rbindlist(content$results, fill = TRUE)

  # Tant que le requête renvoie un url next on refait une requête
  while (!is.null(url_ademe)) {
    res <- httr::GET(url_ademe)
    content <- httr::content(res, as = "parsed")
    url_ademe <- content$"next"
    df2 <- data.table::rbindlist(content$results, fill = TRUE)

    # Merge pour concaténer les df
    df <- df %>% dplyr::full_join(df2, by = names(df))
  }

  df$meta_domaine <- df$meta_domaine %>%
    forcats::fct_recode(
      "Inconnu" = "Non renseigné",
      "Inconnu" = "anciens domaines avant 2021"
    )

  df <- df %>%
    dplyr::filter(particulier) %>%
    dplyr::select(
      siret, code_qualification, nom_qualification, nom_certificat, domaine,
      meta_domaine, organisme, code_postal, latitude, longitude
    )%>%
    dplyr::distinct(siret,.keep_all = TRUE) #Selectionne les sirets uniques.

  list("date_maj" = date_maj, "data" = df)

}


bdd = requete_ademe()
bdd_entreprises_reg = bdd[["data"]]
save(bdd_entreprises_reg, file = "//donnees.dreal-norm.ad.e2.rie.gouv.fr/services/SMCAP/05_BOeS/5PREL/base_donnees/BDNB_OPEN_SOURCE/bdnb/data/bdd_entreprise_rge.RData")



