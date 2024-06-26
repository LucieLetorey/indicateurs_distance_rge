library(tidyr)

# url EXTRACTION bdd https://bdnb.io/archives_data/bdnb_millesime_2023_01_a/
## Prendre les formats csv

# chargement des bases de données

adresse = read_data_table_csv(
  chemin_dossier = "//donnees.dreal-norm.ad.e2.rie.gouv.fr/services/SMCAP/05_BoEs/5PREL/base_donnees/BDNB_OPEN_SOURCE/bdnb/data-load/adresse"
)

batiment_groupe_dpe_representatif_logement = read_data_table_csv(
  chemin_dossier = "//donnees.dreal-norm.ad.e2.rie.gouv.fr/services/SMCAP/05_BoEs/5PREL/base_donnees/BDNB_OPEN_SOURCE/bdnb/data-load/batiment_groupe_dpe_representatif_logement"
)

batiment_groupe_ffo_bat = read_data_table_csv(
  chemin_dossier = "//donnees.dreal-norm.ad.e2.rie.gouv.fr/services/SMCAP/05_BoEs/5PREL/base_donnees/BDNB_OPEN_SOURCE/bdnb/data-load/batiment_groupe_ffo_bat"
)
rel_batiment_groupe_adresse = read_data_table_csv(
  chemin_dossier = "//donnees.dreal-norm.ad.e2.rie.gouv.fr/services/SMCAP/05_BoEs/5PREL/base_donnees/BDNB_OPEN_SOURCE/bdnb/data-load/rel_batiment_groupe_adresse"
)

batiment_groupe_adresse = read_data_table_csv(
  chemin_dossier = "//donnees.dreal-norm.ad.e2.rie.gouv.fr/services/SMCAP/05_BoEs/5PREL/base_donnees/BDNB_OPEN_SOURCE/bdnb/data-load/batiment_groupe_adresse"
)

batiment_groupe = read_data_table_csv(
  chemin_dossier = "//donnees.dreal-norm.ad.e2.rie.gouv.fr/services/SMCAP/05_BoEs/5PREL/base_donnees/BDNB_OPEN_SOURCE/bdnb/data-load/batiment_groupe"
)

# Pré-traitement des bases

# Base de données contenant les DPE des logements.

# Ici nous n'allons garder que les DPE F et G, qui correspondent aux passoires énergétiques
bdd_dpe_representatif = batiment_groupe_dpe_representatif_logement %>%
  dplyr::select(
    batiment_groupe_id,
    identifiant_dpe,
    arrete_2021,
    classe_bilan_dpe,
    classe_emission_ges,
    classe_conso_energie_arrete_2012,
    conso_5_usages_ep_m2,
    conso_3_usages_ep_m2_arrete_2012,
    emission_ges_3_usages_ep_m2_arrete_2012
  ) %>%
  dplyr::mutate(
    classe_bilan_dpe = ifelse(
      arrete_2021==0,
      classe_conso_energie_arrete_2012,
      classe_bilan_dpe
    ) #,
    # classe_bilan_dpe =  ifelse(classe_bilan_dpe == "N",
    #                            NA,
    #                            classe_bilan_dpe)
  )%>%
dplyr::filter(
  classe_bilan_dpe %in% c("F", "G")
)


bdd_rel_batiment_groupe_adresse = rel_batiment_groupe_adresse%>%
  dplyr::select(
    batiment_groupe_id,
    cle_interop_adr
  ) %>%
  dplyr::distinct(batiment_groupe_id,.keep_all =TRUE)



bdd_batiment_groupe_adresse = batiment_groupe_adresse %>%
  dplyr::select(batiment_groupe_id,cle_interop_adr_principale_ban)



bdd_batiment_groupe_ffo_bat=batiment_groupe_ffo_bat %>%
  dplyr::select(
    batiment_groupe_id,
    nb_log
  )


bdd_adresse = adresse %>%
  dplyr::select(WKT,cle_interop_adr, code_commune_insee)%>%
  dplyr::mutate(code_commune_insee = as.factor(code_commune_insee)) %>%
  COGiter::passer_au_cog_a_jour(code_commune = code_commune_insee,
                                aggrege = FALSE,
                                garder_info_supra = TRUE)  %>%
  dplyr::select(WKT,cle_interop_adr, DEPCOM, EPCI)  %>%
  dplyr::rename("code_commune_insee" = "DEPCOM")

# Définition du système de référence spatial (CRS) pour Lambert-93 et WGS84
crs_lambert <- sp::CRS("+proj=lcc +lat_1=49 +lat_2=44 +lat_0=46.5 +lon_0=3 +x_0=700000 +y_0=6600000 +ellps=GRS80 +units=m +no_defs")
crs_wgs84 <- sp::CRS("+proj=longlat +datum=WGS84")

#Base Batiment groupe completée avec 1ère adresse table bdd_rel_groupe_adresse
bdd_bat_grpe_adresse_complete=bdd_batiment_groupe_adresse %>%
  dplyr::mutate(cle_interop_adr_principale_ban=dplyr::na_if(cle_interop_adr_principale_ban,"")) %>%
  dplyr::left_join(bdd_rel_batiment_groupe_adresse,
                   by = 'batiment_groupe_id') %>%
  dplyr::mutate(
    cle_interop_adr_principale_ban=dplyr::coalesce(cle_interop_adr_principale_ban,cle_interop_adr)) %>%
  dplyr::select(-cle_interop_adr)



bdd_dpe_representatif_rel_batiment_groupe_ffo = bdd_dpe_representatif %>%
  dplyr::left_join(bdd_batiment_groupe_ffo_bat, by='batiment_groupe_id') %>%
  dplyr::filter(nb_log>0) %>%
  dplyr::left_join(bdd_bat_grpe_adresse_complete,by='batiment_groupe_id') %>%
  dplyr::left_join(bdd_adresse, by = c('cle_interop_adr_principale_ban'='cle_interop_adr')) %>%
  dplyr::mutate(
    wkt = gsub("POINT \\(|\\)", "", WKT),
    x_lambert = as.numeric(sub("\\s.*", "", wkt)),
    y_lambert = as.numeric(sub("^[^ ]* ", "", wkt)),
  ) %>%
  dplyr::select(!c(WKT,wkt) ) %>%
  sf::st_as_sf(coords = c("x_lambert", "y_lambert"), crs = crs_lambert) %>%
  sf::st_transform(crs_wgs84) %>%
  dplyr::mutate(code_dep = substr(code_commune_insee,1,2)) %>%
  dplyr::mutate(
    latitude = as.numeric(gsub(".*, (-?\\d+\\.\\d+e?-?\\d*)\\)", "\\1", geometry)),
    longitude = as.numeric(gsub("c\\((-?\\d+\\.\\d+e?-?\\d*), .*", "\\1", geometry))
  ) %>%
  as.data.frame()




save(bdd_dpe_representatif_rel_batiment_groupe_ffo,file="//donnees.dreal-norm.ad.e2.rie.gouv.fr/services/SMCAP/05_BoEs/5PREL/base_donnees/BDNB_OPEN_SOURCE/bdnb/data/bdd_dpe_representatif_rel_batiment_groupe_ffo.RData" )


