#Code qui traite les différents fichiers de tronçons de la BDTOPO pour qu'ils soient lisibles par
#le package R Dodgr. Enregsitre par la suite le résultat en .RData.
crs_lambert = sf::st_crs("+proj=lcc +lat_1=49 +lat_2=44 +lat_0=46.5 +lon_0=3 +x_0=700000 +y_0=6600000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs") #Nécessaires pour
crs_wgs84=sf::st_crs("+proj=longlat +datum=WGS84 +no_defs")
library(tidyr)
for (i in c("22","49","60","62","75","80","91","92","93","94","95")) {
  # Créer un exemple d'objet sf (remplacez par votre objet réel)
  troncons=sf::st_read(paste0("//donnees.dreal-norm.ad.e2.rie.gouv.fr/services/SMCAP/05_BOeS/5PREL/base_donnees/BDNB_OPEN_SOURCE/bdnb/data-load/troncons/TRONCON_DE_ROUTE_",i,"/TRONCON_DE_ROUTE.shp"))%>%
           sf::st_transform(crs=crs_wgs84) %>%
           sf::st_zm()%>%
           dplyr::filter(!(NATURE %in% c("Sentier","Escalier","Bac ou liaison maritime","Piste cyclable")))%>%
           dplyr::filter(SENS!="Sans objet")%>% #Pistes cyclables enregistrées
           dplyr::select(ID,NATURE,SENS,geometry)%>%
           dplyr::mutate(geometry = dplyr::if_else(SENS == "Sens inverse", sf::st_reverse(geometry), geometry), #Inverse geometry des sens unique modélisés en sens inverse
                           SENS = dplyr::if_else(SENS == "Sens inverse", "Sens direct", SENS))%>%      #Attribue valeur "Sens direct" quand valeur initiale était "sens inverse"
           dplyr::mutate(highway = dplyr::recode(NATURE,
                                                   "Route à 1 chaussée" = "secondary",
                                                   "Route empierrée" = "unclassified",
                                                   "Chemin" = "service",
                                                   "Sentier" = "path",
                                                   "Rond-point" = "tertiary",
                                                   "Escalier" = "steps",
                                                   "Route à 2 chaussées" = "trunk",
                                                   "Bretelle" = "trunk_link",
                                                   "Type autoroutier" = "motorway",
                                                   "Bac ou liaison maritime" = "ferry",
                                                   "Piste cyclable"="cycleway"),
                           oneway=dplyr::recode(SENS,
                                                "Double sens"="no",
                                                "Sens direct"="yes",
                                                "Sens inverse"="reverse"))
  # Sauvegarder l'objet avec un nom de fichier unique
  save(troncons, file = paste0("//donnees.dreal-norm.ad.e2.rie.gouv.fr/services/SMCAP/05_BOeS/5PREL/base_donnees/BDNB_OPEN_SOURCE/bdnb/data/troncons_",i, ".RData"))
}

