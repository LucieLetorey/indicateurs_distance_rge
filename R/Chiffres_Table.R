library(tidyr)
batiment_groupe = read_data_table_csv(
  chemin_dossier = "//donnees.dreal-norm.ad.e2.rie.gouv.fr/services/SMCAP/05_BoEs/5PREL/base_donnees/BDNB_OPEN_SOURCE/bdnb/data-load/batiment_groupe"
)

batiment_groupe_ffo_bat = read_data_table_csv(
  chemin_dossier = "//donnees.dreal-norm.ad.e2.rie.gouv.fr/services/SMCAP/05_BoEs/5PREL/base_donnees/BDNB_OPEN_SOURCE/bdnb/data-load/batiment_groupe_ffo_bat"
)
#Récupération nb logements base en Normandie
batiment_groupe=batiment_groupe %>%
                dplyr::filter(code_departement_insee!=13) %>%
                dplyr::select(batiment_groupe_id)

batiment_groupe_ffo_bat=batiment_groupe_ffo_bat %>%
                        dplyr::filter(code_departement_insee!=13) %>%
                        dplyr::select(batiment_groupe_id,nb_log)


batiments_normands=batiment_groupe %>%
                   dplyr::left_join(batiment_groupe_ffo_bat,
                                    by="batiment_groupe_id") %>%
                   na.omit() %>%
                   dplyr::summarise(nb_log=(sum(nb_log,na.rm=TRUE)))

#1887912logements recensés dans base depuis batiment_groupe (parler des logements pas dans base fichiers fonciers)



#Nombre de logements avec DPE

batiment_groupe_dpe_representatif_logement = read_data_table_csv(
  chemin_dossier = "//donnees.dreal-norm.ad.e2.rie.gouv.fr/services/SMCAP/05_BoEs/5PREL/base_donnees/BDNB_OPEN_SOURCE/bdnb/data-load/batiment_groupe_dpe_representatif_logement"
)

bdd_batiment_groupe_dpe_representatif_logement = batiment_groupe_dpe_representatif_logement %>%
  dplyr::filter(code_departement_insee!=13) %>%
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
  dplyr::filter(classe_bilan_dpe!="N")


nb_log_dpe=bdd_batiment_groupe_dpe_representatif_logement %>%
           dplyr::left_join(batiment_groupe_ffo_bat,
                   by="batiment_groupe_id") %>%
           dplyr::filter(nb_log>0) %>%
           dplyr::summarise(nb_log=(sum(nb_log,na.rm=TRUE))) %>%
           dplyr::pull(nb_log)

resume_dpe_repr=bdd_batiment_groupe_dpe_representatif_logement %>%
                dplyr::left_join(batiment_groupe_ffo_bat,
                                 by="batiment_groupe_id") %>%
                dplyr::filter(nb_log>0) %>%
                dplyr::group_by(classe_bilan_dpe) %>%
                dplyr::summarise(nb=sum(nb_log)) %>%
                dplyr::ungroup()

proportion_dpe=resume_dpe_repr %>%
               dplyr::mutate(proportion=(nb/nb_log_dpe)*100,
                             ymax=cumsum(proportion),
                             ymin=c(0,head(ymax,n=-1)),
                             labelPosition=(ymax+ymin)/2,
                             label = paste0(classe_bilan_dpe, "\n", round(proportion, 1), "%")

                             )

# dplyr::filter(
  #   classe_bilan_dpe %in% c("F", "G")
  # )

library(ggplot2)
ggplot(proportion_dpe, aes(ymin = ymin, ymax = ymax, xmin = 3, xmax = 4, fill = classe_bilan_dpe)) +
  geom_rect() +
  geom_text(aes(x = 3.5, y = labelPosition, label = label), size = 5) +
  coord_polar(theta = "y") +
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "none") +
  ggtitle("Diagramme en beignet des étiquettes DPE")

#Récupération taux de passoires thermiques par EPCI.

adresse = read_data_table_csv(
  chemin_dossier = "//donnees.dreal-norm.ad.e2.rie.gouv.fr/services/SMCAP/05_BoEs/5PREL/base_donnees/BDNB_OPEN_SOURCE/bdnb/data-load/adresse"
)

rel_batiment_groupe_adresse = read_data_table_csv(
  chemin_dossier = "//donnees.dreal-norm.ad.e2.rie.gouv.fr/services/SMCAP/05_BoEs/5PREL/base_donnees/BDNB_OPEN_SOURCE/bdnb/data-load/rel_batiment_groupe_adresse"
)

batiment_groupe_adresse = read_data_table_csv(
  chemin_dossier = "//donnees.dreal-norm.ad.e2.rie.gouv.fr/services/SMCAP/05_BoEs/5PREL/base_donnees/BDNB_OPEN_SOURCE/bdnb/data-load/batiment_groupe_adresse"
)

adresse=adresse %>%
  dplyr::filter(code_departement_insee!=13) %>%
  dplyr::select(WKT,cle_interop_adr, code_commune_insee)%>%
  dplyr::mutate(code_commune_insee = as.factor(code_commune_insee)) %>%
  COGiter::passer_au_cog_a_jour(code_commune = code_commune_insee,
                                aggrege = FALSE,
                                garder_info_supra = TRUE)  %>%
  dplyr::select(WKT,cle_interop_adr, DEPCOM, EPCI)  %>%
  dplyr::rename("code_commune_insee" = "DEPCOM")

rel_batiment_groupe_adresse=rel_batiment_groupe_adresse%>%
  dplyr::select(
    batiment_groupe_id,
    cle_interop_adr
  ) %>%
  dplyr::distinct(batiment_groupe_id,.keep_all =TRUE)

batiment_groupe_adresse=batiment_groupe_adresse %>%
  dplyr::filter(code_departement_insee!=13) %>%
  dplyr::mutate(cle_interop_adr_principale_ban=dplyr::na_if(cle_interop_adr_principale_ban,"")) %>%
  dplyr::left_join(rel_batiment_groupe_adresse,
                   by = 'batiment_groupe_id') %>%
  dplyr::mutate(
    cle_interop_adr_principale_ban=dplyr::coalesce(cle_interop_adr_principale_ban,cle_interop_adr)) %>%
  dplyr::select(-cle_interop_adr)

bat_groupe_adresse_EPCI=batiment_groupe_adresse %>%
                        dplyr::left_join(adresse,
                                         by=c('cle_interop_adr_principale_ban'='cle_interop_adr'))

bat_groupe_dpe_repr_epci=bdd_batiment_groupe_dpe_representatif_logement %>%
                         dplyr::left_join(bat_groupe_adresse_EPCI,
                                          by="batiment_groupe_id") %>%
                        dplyr::left_join(batiment_groupe_ffo_bat,
                        by="batiment_groupe_id") %>%
                        dplyr::filter(nb_log>0)

taux_passoire= bat_groupe_dpe_repr_epci %>%
               dplyr::group_by(EPCI) %>%
              dplyr::summarise(
              total_log=sum(nb_log),
              nb_fg=sum(nb_log[classe_bilan_dpe %in% c("F","G")])

              ) %>%
              dplyr::mutate(
              taux_passoire=nb_fg/total_log
              ) %>%
              dplyr::filter(EPCI %in% epci_normand$codezone)

agreg_modif=agreg %>%
            dplyr::filter(CodeZone %in% epci_normand$codezone | TypeZone %in% c("Régions","Départements")) %>%
            dplyr::mutate(dist_min_moy=(dist_min/nb_log)/1000,
                          nb_inf_30_moy=nb_inf_30/nb_log)


plot(agreg_modif[6:74,]$dist_min_moy,agreg_modif[6:74,]$taux_passoire)
plot(agreg_modif[6:74,]$nb_inf_30,agreg_modif[6:74,]$taux_passoire)
reg=lm(agreg_modif[6:74,]$taux_passoire~agreg_modif[6:74,]$dist_min_moy)
summary(reg)
popu=read.csv2("//donnees.dreal-norm.ad.e2.rie.gouv.fr/services/SMCAP/05_BOeS/5PREL/base_donnees/BDNB_OPEN_SOURCE/bdnb/data-load/population.csv",skip=2)
popu2=popu %>%
      dplyr::filter(Code %in% epci_normand$codezone) %>%
      dplyr::mutate(Code = as.character(Code))




##############
dpts_stat=agreg %>%
          dplyr::ungroup() %>%
          dplyr::slice(1:5) %>%
          dplyr::mutate(dist_min_moy=(dist_min/nb_log)/1000,
                        nb_inf_30_moy=nb_inf_30/nb_log)

summary(dpts_stat$dist_min_moy)



dpe <- c('A', 'B', 'C', 'D', 'E', 'F', 'G')
nb <- c(150, 200, 400, 800, 500, 300, 100)

data <- data.frame(dpe = dpe, nb = nb)

dpe_colors =  c("#339a33", "#33cc33","#cbfe32", "#ffff00", "#fdca00","#fe9932","#ff0000")


p = proportion_dpe %>%
  plotly::plot_ly(labels = ~classe_bilan_dpe , values = ~proportion, sort=FALSE) %>%
  plotly::add_pie(hole = .6,
                  marker=list(colors=dpe_colors),
                  textinfo="label+percent",
                  insidetextorientation='outside',
                  direction = "clockwise"
  ) %>%
  plotly::layout(margin=list(l = 50, r = 50, b = 50, t = 50, pad = 4))
p

stats_dpt_regions= agreg %>%
                  dplyr::ungroup() %>%
                  dplyr::slice(c(1:5,77)) %>%
                  dplyr::mutate(dist_min_moy=(dist_min/nb_log)/1000,
                  nb_inf_30_moy=nb_inf_30/nb_log) %>%
                  select(Zone,dist_min_moy,nb_inf_30_moy,nb_log)

write.csv(stats_dpt_regions,file="//donnees.dreal-norm.ad.e2.rie.gouv.fr/services/SMCAP/05_BOeS/5PREL/base_donnees/BDNB_OPEN_SOURCE/bdnb/Rapport/Stats_dpts.csv")
