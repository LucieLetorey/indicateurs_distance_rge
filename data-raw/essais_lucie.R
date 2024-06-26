
load("//donnees.dreal-norm.ad.e2.rie.gouv.fr/services/SMCAP/3_BoEs/5PREL/base_donnees/BDNB_OPEN_SOURCE/bdnb/data/bdd_entreprise_rge.RData")

bdd_entreprises = bdd_entreprises_reg %>%
  dplyr::mutate(code_dep = substr(code_postal,1,2))

i = "14"

for(i in unique(bdd_dpe_logement_rel_batiment_groupe$code_dep) ){


  bdd_batiment_temp = bdd_dpe_logement_rel_batiment_groupe %>%
    dplyr::filter(code_dep == i)

  bdd_entreprises_temp = bdd_entreprises %>%
    dplyr::filter(code_dep %in% liste_dep_rge_retain(code_dep = i)) %>%
    dplyr::select(!code_dep)


  bdd_temp = bdd_batiment_temp %>%
    dplyr::cross_join(bdd_entreprises_temp)



}






