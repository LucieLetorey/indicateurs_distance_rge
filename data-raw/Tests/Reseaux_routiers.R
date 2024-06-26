library(tidyverse)

Sys.time()
print("Chargement données...")
load("//donnees.dreal-norm.ad.e2.rie.gouv.fr/services/SMCAP/05_BOeS/5PREL/base_donnees/BDNB_OPEN_SOURCE/bdnb/data/departements_geo.RData")
#load("//donnees.dreal-norm.ad.e2.rie.gouv.fr/services/SMCAP/05_BOeS/5PREL/base_donnees/BDNB_OPEN_SOURCE/bdnb/data/bdd_entreprise_rge.RData")
#load("//donnees.dreal-norm.ad.e2.rie.gouv.fr/services/SMCAP/05_BOeS/5PREL/base_donnees/BDNB_OPEN_SOURCE/bdnb/data/bdd_dpe_logement_rel_batiment_groupe.RData")
print("Fin de chargement...")
departements_geo_simpl_lnglat_plus=departements_geo_simpl_lnglat%>%
  dplyr::mutate(geometry_plus=sf::st_buffer(geometry,dist=60000))
print("Buffer effectué...")
Sys.time()
#Extraction bordures Buffer
min_longitude <- numeric(length = length(departements_geo_simpl_lnglat[[2]]))
max_longitude <- numeric(length = length(departements_geo_simpl_lnglat[[2]]))# Crée un vecteur numérique vide
min_latitude <- numeric(length = length(departements_geo_simpl_lnglat[[2]]))
max_latitude <- numeric(length = length(departements_geo_simpl_lnglat[[2]]))


for (i in 1:length(departements_geo_simpl_lnglat_plus[[5]])){
  coords_all <- NULL
  if(length(departements_geo_simpl_lnglat_plus[[5]][[i]]) == 1){
    coords_all=departements_geo_simpl_lnglat_plus[[5]][[i]][[1]]
    min_latitude[i]=min(coords_all[,2])
    max_latitude[i]=max(coords_all[,2])
    min_longitude[i]=min(coords_all[,1])
    max_longitude[i]=max(coords_all[,1])
  }

  # else if (length(departements_geo_simpl_lnglat[[2]][[i]]) > 1)
  # {
  #   for (j in 1:length(departements_geo_simpl_lnglat[[2]][[i]])){
  #     if (is.null(coords_all)){
  #       coords_all=departements_geo_simpl_lnglat[[2]][[i]][[j]]
  #     }
  #     else {
  #       coords_all=rbind(coords_all,departements_geo_simpl_lnglat[[2]][[i]][[j]])
  #     }
  #     if (is.list(departements_geo_simpl_lnglat[[2]][[i]][[1]])){
  #       min_latitude[i]=min(coords_all[[1]][,2])
  #       max_latitude[i]=max(coords_all[[1]][,2])
  #       min_longitude[i]=min(coords_all[[1]][,1])
  #       max_longitude[i]=max(coords_all[[1]][,1])
  #     }
  #
  #     else{
  #
  #       min_latitude[i]=min(coords_all[,2])
  #       max_latitude[i]=max(coords_all[,2])
  #       min_longitude[i]=min(coords_all[,1])
  #       max_longitude[i]=max(coords_all[,1])
  #     }
  #   }
  # }
}
bordures=data.frame(code=departements_geo_simpl_lnglat$codezone,
                    min_longitude = min_longitude,
                    max_longitude = max_longitude,
                    min_latitude = min_latitude,
                    max_latitude = max_latitude)
Sys.time()
print("Extraction bordures effectuée...Extraction réseau")

for (i in 1:1){

  bb=bordures[i,2:5]
  box=matrix(c(bb[["min_longitude"]],bb[["max_longitude"]],bb[["min_latitude"]],bb[["max_latitude"]]),byrow=TRUE,ncol=2)
  rownames(box)=c("x","y")
  colnames(box)=c("min","max")

  Sys.time()
  print(paste("Extraction réseau ",i))
  reseau=dodgr::dodgr_streetnet(box) #Extraction réseau
  Sys.time()
  print(paste("Création  Graphe ",i))
  graphe=dodgr::weight_streetnet(reseau,wt_profile="motocar")
  Sys.time()

  save(graphe,file = paste0("//donnees.dreal-norm.ad.e2.rie.gouv.fr/services/SMCAP/05_BOeS/5PREL/base_donnees/BDNB_OPEN_SOURCE/bdnb/data/Graphe_", bordures$code[i], ".RData"))


}











################################################################################################
epci_geo_simpl_lnglat_plus=epci_geo_simpl_lnglat%>%
  dplyr::mutate(geometry_plus=sf::st_buffer(geometry,dist=60000))

min_longitude <- numeric(length = length(epci_geo_simpl_lnglat[[2]]))
max_longitude <- numeric(length = length(epci_geo_simpl_lnglat[[2]]))# Crée un vecteur numérique vide
min_latitude <- numeric(length = length(epci_geo_simpl_lnglat[[2]]))
max_latitude <- numeric(length = length(epci_geo_simpl_lnglat[[2]]))


for (i in 1:length(epci_geo_simpl_lnglat_plus[[5]])){
  coords_all <- NULL
  if(length(epci_geo_simpl_lnglat_plus[[5]][[i]]) == 1){
    coords_all=epci_geo_simpl_lnglat_plus[[5]][[i]][[1]]
    min_latitude[i]=min(coords_all[,2])
    max_latitude[i]=max(coords_all[,2])
    min_longitude[i]=min(coords_all[,1])
    max_longitude[i]=max(coords_all[,1])
  }

}
bordures=data.frame(code=epci_geo_simpl_lnglat_plus$code_unique,
                    min_longitude = min_longitude,
                    max_longitude = max_longitude,
                    min_latitude = min_latitude,
                    max_latitude = max_latitude)
Sys.time()
print("Extraction bordures effectuée...Extraction réseau")



  bb=bordures[1,2:5]
  box=matrix(c(bb[["min_longitude"]],bb[["max_longitude"]],bb[["min_latitude"]],bb[["max_latitude"]]),byrow=TRUE,ncol=2)
  rownames(box)=c("x","y")
  colnames(box)=c("min","max")

  Sys.time()
  #print(paste("Extraction réseau ",i))
  reseau=dodgr::dodgr_streetnet(box) #Extraction réseau
  Sys.time()
  print(paste("Création  Graphe ",i))
  graphe=dodgr::weight_streetnet(reseau,wt_profile="motocar")
  Sys.time()

  save(graphe,file = paste0("//donnees.dreal-norm.ad.e2.rie.gouv.fr/services/SMCAP/05_BOeS/5PREL/base_donnees/BDNB_OPEN_SOURCE/bdnb/data/Graphe_", bordures$code[i], ".RData"))


