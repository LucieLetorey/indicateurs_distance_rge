library(tidyr)

# url EXTRACTION bdd https://bdnb.io/archives_data/bdnb_millesime_2023_01_a/
## Prendre les formats csv

# chargement des bases de données

adresse = read_data_table_csv(
  chemin_dossier = "//donnees.dreal-norm.ad.e2.rie.gouv.fr/services/SMCAP/05_BoEs/5PREL/base_donnees/BDNB_OPEN_SOURCE/bdnb/data-load/adresse"
  )

dpe_logement = read_data_table_csv(
  chemin_dossier = "//donnees.dreal-norm.ad.e2.rie.gouv.fr/services/SMCAP/05_BoEs/5PREL/base_donnees/BDNB_OPEN_SOURCE/bdnb/data-load/dpe_logement"
)

rel_batiment_groupe_dpe_logement = read_data_table_csv(
  chemin_dossier = "//donnees.dreal-norm.ad.e2.rie.gouv.fr/services/SMCAP/05_BoEs/5PREL/base_donnees/BDNB_OPEN_SOURCE/bdnb/data-load/rel_batiment_groupe_dpe_logement"
)


# Pré-traitement des bases

# Base de données contenant les DPE des logements.
# Ici nous n'allons garder que les DPE F et G, qui correspondent aux passoires énergétiques
bdd_dpe_logement = dpe_logement %>%
  dplyr::select(
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
      is.na(classe_bilan_dpe),
      classe_conso_energie_arrete_2012,
      classe_bilan_dpe
      ) #,
    # classe_bilan_dpe =  ifelse(classe_bilan_dpe == "N",
    #                            NA,
    #                            classe_bilan_dpe)
   ) %>%
   dplyr::filter(
   classe_bilan_dpe %in% c("F", "G")
    )


bdd_rel_batiment_groupe_dpe_logement = rel_batiment_groupe_dpe_logement %>%
  dplyr::select(
    batiment_groupe_id,
    identifiant_dpe,
    cle_interop_adr,
    fiabilite_geocodage
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

bdd_dpe_logement_rel_batiment_groupe = bdd_dpe_logement %>%
  dplyr::left_join(bdd_rel_batiment_groupe_dpe_logement, by='identifiant_dpe') %>%
  dplyr::left_join(bdd_adresse, by = 'cle_interop_adr') %>%
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




save(bdd_dpe_logement_rel_batiment_groupe,file="//donnees.dreal-norm.ad.e2.rie.gouv.fr/services/SMCAP/05_BoEs/5PREL/base_donnees/BDNB_OPEN_SOURCE/bdnb/data/bdd_dpe_logement_rel_batiment_groupe_tot.RData" )

# summary(as.factor(bdd_dpe_logement$classe_bilan_dpe))
# prop.table(summary(as.factor(bdd_dpe_logement_rel_batiment_groupe$classe_bilan_dpe)))
# summary(as.factor(bdd_dpe_logement_rel_batiment_groupe$classe_bilan_dpe))



tronc$geometry=sf::st_as_text(tronc$geometry)
tronc=troncons %>%
          #dplyr::select(c(batiment_groupe_id,classe_bilan_dpe,nb_log,cle_interop_adr_principale_ban,EPCI,latitude,longitude)) %>%
          dplyr::slice(1:8) %>%
          as.data.frame()
write.csv(tronc,file="//donnees.dreal-norm.ad.e2.rie.gouv.fr/services/SMCAP/05_BoEs/5PREL/base_donnees/BDNB_OPEN_SOURCE/bdnb/Rapport/troncons.csv")

# epci_geo_simpl_lnglat$geometry=sf::st_as_text(epci_geo_simpl_lnglat$geometry)
# epci_geo_simpl_lnglat=as.data.frame(epci_geo_simpl_lnglat)
# write.csv(epci_geo_simpl_lnglat[1:10,],file="epci.csv")

d_long_csv=d_long_group_by_a_jour %>%
           dplyr::ungroup() %>%
           dplyr::slice(294:301) %>%
           dplyr::select(-nb_inf_60)
write.csv(d_long_csv,file="//donnees.dreal-norm.ad.e2.rie.gouv.fr/services/SMCAP/05_BoEs/5PREL/base_donnees/BDNB_OPEN_SOURCE/bdnb/Rapport/d_long.csv")

res_epci_honfleur=resultat_etape %>%
                  dplyr::select(-nb_inf_60)
write.csv(res_epci_honfleur,file="//donnees.dreal-norm.ad.e2.rie.gouv.fr/services/SMCAP/05_BoEs/5PREL/base_donnees/BDNB_OPEN_SOURCE/bdnb/Rapport/res_epci_honf.csv")
