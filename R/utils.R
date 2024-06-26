dpe = function(energie, ges){
  ifelse(energie <= 70 & ges <= 6, "A",
         ifelse(energie <= 110 & ges <= 11, "B",
                ifelse(energie <= 180 & ges <= 30, "C",
                       ifelse(energie <= 250 & ges <= 50, "D",
                              ifelse(energie <= 330 & ges <= 70, "E",
                                     ifelse(energie <= 420 & ges <= 100, "E",
                                            "G"
                                            )
                                     )
                              )
                       )
                )
         )
}

chemin_dossier = "//donnees.dreal-norm.ad.e2.rie.gouv.fr/services/SMCAP/05_BOeS/5PREL/base_donnees/BDNB_OPEN_SOURCE/bdnb/data-load/rel_batiment_groupe_dpe_logement"
chemin="//donnees.dreal-norm.ad.e2.rie.gouv.fr/services/SMCAP/05_BOeS/5PREL/base_donnees/BDNB_OPEN_SOURCE/bdnb/data/resultat_etape_"
read_data_table_csv = function(chemin_dossier){

  # Obtenir la liste des fichiers CSV dans le dossier
  fichiers_csv <- list.files(path = chemin_dossier, pattern = "\\.csv$", full.names = TRUE)

  # Initialiser une liste pour stocker les données de chaque fichier CSV
  donnees_liste <- list()

  # Lire chaque fichier CSV et le stocker dans la liste
  for (fichier in fichiers_csv) {
    nom_fichier <- basename(fichier)
    donnees_liste[[nom_fichier]] <- read.csv(fichier, sep = ",")
  }

  data = donnees_liste[[1]]

  for(i in  2:length(fichiers_csv)){
    data = data %>%
      rbind(donnees_liste[[i]])
  }

  return(data)

}

#tttttt

liste_dep_rge_retain = function(code_dep){

  switch(code_dep,
         "14" = c("14", "76", "27", "28", "61", "50", "35", "53"),
         "27" = c("76", "60", "14", "28", "61", "72", "78", "95","93","75","92","27"),
         "35" = c("50", "14", "61", "53", "72", "49", "44", "22","56","35"),
         "50" = c("14", "76", "27", "35", "53", "22", "35","50"),
         "53" = c("50", "76", "14", "61", "27", "72", "49", "44","35","22","53"),
         "61" = c("14", "76", "27", "28", "61", "50", "35", "53","41","72","61"),
         "76" = c("14", "76","80","62","60","95","78","61")
           )


}


deg_to_rad=function(val){
res=val*(pi/180)
return(res)
}

distance = function(lat1,long1,lat2,long2){
la1=deg_to_rad(lat1)
lg1=deg_to_rad(long1)
la2=deg_to_rad(lat2)
lg2=deg_to_rad(long2)
d=lg1-lg2
dist=6371*acos(sin(la1)*sin(la2)+cos(la1)*cos(la2)*cos(d))
return (dist)
}



#Filtrage Base Entreprises RGE

data_temp=bdd_dpe_logement_rel_batiment_groupe%>%
  dplyr::filter(code_dep=="14")%>%
  as.data.frame()

buffer=departements_geo_simpl_lnglat_plus %>%
  as.data.frame() %>%
  dplyr::select(!geometry) %>%
  sf::st_as_sf() %>%
  dplyr::filter(codezone=="14")%>%
  dplyr::select(geometry_plus)

liste_dep=liste_dep_rge_retain("14")
bdd_entreprises_reg_filtree=bdd_entreprises_reg%>%
                            dplyr::filter(substr(code_postal,1,2)%in%liste_dep)

bdd_entreprises_reg_filtree=bdd_entreprises_reg_filtree%>%
                            dplyr::rowwise() %>%
                            dplyr::mutate(test_buffer=sf::st_within(sf::st_point(c(longitude,latitude)),buffer))%>%
                            dplyr::filter(length(test_buffer)>0)

lon_lat <- data_temp %>%dplyr::select(longitude, latitude)


distances2 <- geosphere::distm(lon_lat, bdd_entreprises_reg_filtree %>% dplyr::select(longitude,latitude))

nom_entreprises=bdd_entreprises_reg_filtree%>%
                dplyr::pull(siret)
colnames(distances2)=nom_entreprises

id_bat= data_temp%>%
        dplyr::mutate(id_bat=paste(identifiant_dpe,batiment_groupe_id,sep="_"))%>%
        dplyr::pull(id_bat)

rownames(distances2)=id_bat
distances2=as.data.frame(distances2)
d_long=distances2%>%
           dplyr::mutate(ID_Bat = rownames(.))%>%
           pivot_longer(-ID_Bat,
                        names_to = "Entreprise",
                        values_to = "Distance") %>%
          dplyr::mutate(dpe_id = sub("^(.*?)_.*", "\\1", ID_Bat))

data_temp=data_temp %>%
          dplyr::mutate(code_commune_insee = as.character(code_commune_insee))


d_long_with_code_insee <- d_long %>%
  dplyr::left_join(data_temp %>%
            dplyr::distinct(identifiant_dpe, code_commune_insee),
            by = c("dpe_id" = "identifiant_dpe")) %>%
            dplyr::select(-dpe_id) %>%
            dplyr::mutate(inf_30=ifelse(Distance<30000,1,0),
                   inf_60=ifelse(Distance<60000,1,0))


d_long_group_by=d_long_with_code_insee%>%
  dplyr::group_by(code_commune_insee,ID_Bat)%>%
  dplyr::summarise(min_dist=min(Distance),
                   nb_inf_30=sum(inf_30,na.rm=TRUE),
                   nb_inf_60=sum(inf_60,na.rm=TRUE),
                   nb_batiments=1)
str(d_long_group_by)


d_long_group_by_a_jour=d_long_group_by%>%
                       dplyr::ungroup()%>%
                       dplyr::select(!ID_Bat) %>%
                       COGiter::passer_au_cog_a_jour(code_commune = "code_commune_insee",aggrege=FALSE)

cgif=COGiter::cogifier(d_long_group_by_a_jour,communes=FALSE,regions=FALSE,metro=FALSE,na.rm=TRUE)

##########################################################################
##############################################################
COGiter::departements[1]

algo = function (bordure){
  departements_geo_simpl_lnglat_plus=departements_geo_simpl_lnglat%>%
    dplyr::mutate(geometry_plus=sf::st_buffer(geometry,dist=bordure*1000))

  for (i in c("14","27","50","53","35","76","61")){
    data_temp=bdd_dpe_logement_rel_batiment_groupe%>%
      dplyr::filter(code_dep==i)%>%
      as.data.frame()

    buffer_i= departements_geo_simpl_lnglat_plus %>%
      as.data.frame() %>%
      dplyr::select(!geometry) %>%
      sf::st_as_sf() %>%
      dplyr::filter(codezone==i)%>%
      dplyr::select(geometry_plus)

    liste_dep=liste_dep_rge_retain(i)

    bdd_entreprises_reg_filtree=bdd_entreprises_reg%>%
      dplyr::filter(substr(code_postal,1,2)%in%liste_dep)%>%
      dplyr::rowwise() %>%
      dplyr::mutate(test_buffer=sf::st_within(sf::st_point(c(longitude,latitude)),buffer_i))%>%
                      dplyr::filter(length(test_buffer)>0)

    lon_lat <- data_temp %>%dplyr::select(longitude, latitude)

    distances2 <- geosphere::distm(lon_lat, bdd_entreprises_reg_filtree %>% dplyr::select(longitude,latitude))

    nom_entreprises=bdd_entreprises_reg_filtree%>%
                      dplyr::pull(siret)
    colnames(distances2)=nom_entreprises
    id_bat= data_temp%>%
            dplyr::mutate(id_bat=paste(identifiant_dpe,batiment_groupe_id,sep="_"))%>%
            dplyr::pull(id_bat)

    rownames(distances2)=id_bat
    distances2=as.data.frame(distances2)
    d_long=distances2%>%
           dplyr::mutate(ID_Bat = rownames(.))%>%
          pivot_longer(-ID_Bat,
                      names_to = "Entreprise",
                      values_to = "Distance") %>%
                      dplyr::mutate(dpe_id = sub("^(.*?)_.*", "\\1", ID_Bat))

    data_temp=data_temp %>%
              dplyr::mutate(code_commune_insee = as.character(code_commune_insee))


    d_long_with_code_insee <- d_long %>%
                      dplyr::left_join(data_temp %>%
                                         dplyr::distinct(identifiant_dpe, code_commune_insee),
                                         by = c("dpe_id" = "identifiant_dpe")) %>%
                      dplyr::select(-dpe_id) %>%
                      dplyr::mutate(inf_30=ifelse(Distance<30000,1,0),
                                    inf_60=ifelse(Distance<60000,1,0))


    d_long_group_by=d_long_with_code_insee%>%
                      dplyr::group_by(code_commune_insee,ID_Bat)%>%
                      dplyr::summarise(min_dist=min(Distance),
                                       nb_inf_30=sum(inf_30,na.rm=TRUE),
                                       nb_inf_60=sum(inf_60,na.rm=TRUE),
                                       nb_batiments=1)

    d_long_group_by_a_jour=d_long_group_by%>%
                           dplyr::ungroup()%>%
                           dplyr::select(!ID_Bat) %>%
                           COGiter::passer_au_cog_a_jour(code_commune = "code_commune_insee",aggrege=FALSE)

    resultat_etape=COGiter::cogifier(d_long_group_by_a_jour,communes=FALSE,regions=FALSE,metro=FALSE,na.rm=TRUE)
    save(resultat_etape,file = paste0(chemin, i, ".RData"))
  }

}

algo(30)
####################################################################
load("//donnees.dreal-norm.ad.e2.rie.gouv.fr/services/SMCAP/05_BOeS/5PREL/base_donnees/BDNB_OPEN_SOURCE/bdnb/data/departements_geo.RData")
load("//donnees.dreal-norm.ad.e2.rie.gouv.fr/services/SMCAP/05_BOeS/5PREL/base_donnees/BDNB_OPEN_SOURCE/bdnb/data/bdd_entreprise_rge.RData")
load("//donnees.dreal-norm.ad.e2.rie.gouv.fr/services/SMCAP/05_BOeS/5PREL/base_donnees/BDNB_OPEN_SOURCE/bdnb/data/bdd_dpe_logement_rel_batiment_groupe.RData")
departements_geo_simpl_lnglat_plus=departements_geo_simpl_lnglat%>%
    dplyr::mutate(geometry_plus=sf::st_buffer(geometry,dist=30000))

  min_longitude <- numeric(length = length(departements_geo_simpl_lnglat[[2]]))
  max_longitude <- numeric(length = length(departements_geo_simpl_lnglat[[2]]))# Crée un vecteur numérique vide
  min_latitude <- numeric(length = length(departements_geo_simpl_lnglat[[2]]))
  max_latitude <- numeric(length = length(departements_geo_simpl_lnglat[[2]]))


  for (i in 1:length(departements_geo_simpl_lnglat[[2]])){
    coords_all <- NULL
    if(length(departements_geo_simpl_lnglat[[2]][[i]]) == 1){
      coords_all=departements_geo_simpl_lnglat[[2]][[i]][[1]]
      min_latitude[i]=min(coords_all[,2])
      max_latitude[i]=max(coords_all[,2])
      min_longitude[i]=min(coords_all[,1])
      max_longitude[i]=max(coords_all[,1])
    }

    else if (length(departements_geo_simpl_lnglat[[2]][[i]]) > 1)
    {
      for (j in 1:length(departements_geo_simpl_lnglat[[2]][[i]])){
        if (is.null(coords_all)){
          coords_all=departements_geo_simpl_lnglat[[2]][[i]][[j]]
        }
        else {
          coords_all=rbind(coords_all,departements_geo_simpl_lnglat[[2]][[i]][[j]])
        }
        if (is.list(departements_geo_simpl_lnglat[[2]][[i]][[1]])){
          min_latitude[i]=min(coords_all[[1]][,2])
          max_latitude[i]=max(coords_all[[1]][,2])
          min_longitude[i]=min(coords_all[[1]][,1])
          max_longitude[i]=max(coords_all[[1]][,1])
        }

        else{

          min_latitude[i]=min(coords_all[,2])
          max_latitude[i]=max(coords_all[,2])
          min_longitude[i]=min(coords_all[,1])
          max_longitude[i]=max(coords_all[,1])
        }
      }
    }
  }
  bordures=data.frame(code=departements_geo_simpl_lnglat$codezone,
                      min_longitude = min_longitude,
                      max_longitude = max_longitude,
                      min_latitude = min_latitude,
                      max_latitude = max_latitude)

  for (i in 1:95){
    bb=bordures[i,2:5]
    box=matrix(c(bb[["min_longitude"]],bb[["max_longitude"]],bb[["min_latitude"]],bb[["max_latitude"]]),byrow=TRUE,ncol=2)
    rownames(box)=c("x","y")
    colnames(box)=c("min","max")

    Sys.time()
    reseau=dodgr::dodgr_streetnet(box) #Extraction réseau
    Sys.time()
    graphe=dodgr::weight_streetnet(reseau,wt_profile="motocar")
    Sys.time()

    save(graphe,file = paste0("//donnees.dreal-norm.ad.e2.rie.gouv.fr/services/SMCAP/05_BOeS/5PREL/base_donnees/BDNB_OPEN_SOURCE/bdnb/data/Graphe_", bordures$code[i], ".RData"))


  }



































####################################################################################################
#######################################TESTS graph##################################################
####################################################################################################
data_troncons= sf::st_read("//10.14.128.55/dossiers/geobase/REF_EXT/BDTOPO/BDTOPO_ED202306/BDTOPO_3-3_TOUSTHEMES_SHP_D014/TRANSPORT/TRONCON_DE_ROUTE.shp")

data_troncons=data_troncons %>% sf::st_zm()





data_troncons=data_troncons %>%
              dplyr::filter(!(NATURE %in% c("Sentier","Escalier","Bac ou liaison maritime","Piste cyclable")))%>%
              dplyr::select(NATURE,POS_SOL,SENS,VIT_MOY_VL,geometry)
crs_lambert = sf::st_crs("+proj=lcc +lat_1=49 +lat_2=44 +lat_0=46.5 +lon_0=3 +x_0=700000 +y_0=6600000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
crs_wgs84=sf::st_crs("+proj=longlat +datum=WGS84 +no_defs")
data_troncons_transformed = sf::st_transform(data_troncons, crs_wgs84)

data_troncons_transformed = data_troncons_transformed%>%
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
                                            "Piste cyclable"="cycleway"))


Sys.time()
graphe=dodgr::weight_streetnet(data_troncons_transformed,id_col="ID",wt_profile="motorcar")
unique(graphe$highway)
Sys.time()

data_temp=bdd_dpe_logement_rel_batiment_groupe%>%
  dplyr::filter(code_dep=="14")%>%
  as.data.frame()
lon_lat <- data_temp %>%dplyr::select(longitude, latitude)
depart <- c(48.8566, 2.3522)  # Paris
arrivee <- c(51.5074, -0.1278)
Sys.time()
dist=dodgr::dodgr_dists(graphe,from=lon_lat,to=lon_lat)
Sys.time()


lon_lat_entr=bdd_entreprises_reg%>%
  dplyr::filter(substr(code_postal,1,2)%in%"14")%>%
  dplyr::select(longitude,latitude)
####################################################################################################
#######################################TESTS DODGR##################################################
####################################################################################################










hampi=dodgr::dodgr_streetnet("hampi india")
graph <- dodgr::weight_streetnet (hampi, wt_profile = "foot")
dim(hampi)


# Définition des coordonnées des points
point1 <- c(76.47491, 15.34167)
point2 <- c(76.47612, 15.34173)

# Calcul de la distance géodésique entre les deux points
distance <- geosphere::distGeo(point1, point2)
distance

bb <- osmdata::getbb("Copenhagen")


npts=10
xy <- apply(bb, 1, function (i) min (i) + runif (npts) * diff (i))
dodgr::dodgr_streetnet()
dodgr::weight_streetnet(net, wt_profile = "foot")
#Test extraction réseau routier Calvados
bb14=bordures[14,2:5]
box_14=matrix(c(bb14[["min_longitude"]],bb14[["max_longitude"]],bb14[["min_latitude"]],bb14[["max_latitude"]]),byrow=TRUE,ncol=2)

rownames(box_14)=c("x","y")
colnames(box_14)=c("min","max")

Sys.time()
reseau_14=dodgr::dodgr_streetnet(box_14) #Extraction réseau
Sys.time()
weighted_reseau=dodgr::weight_streetnet(reseau_14,wt_profile = "motorcar") #Transformation en graphe
Sys.time()

lon_lat_bdentreprises=bdd_entreprises_reg_filtree %>% dplyr::select(longitude, latitude)

Sys.time()
dist_dogr=dodgr::dodgr_dists(weighted_reseau,from=lon_lat,to=lon_lat_bdentreprises)#Matrice des distances
Sys.time()
dim(dist_dogr)

Sys.time()
plus_court=dodgr::dodgr_paths(weighted_reseau,from=c(-0.475980,48.98162),to=c(0.299288,49.61161))#Test pour visualiser chemin entre 2 points précis
Sys.time()


shortest_path_df <- data.frame(from_id = plus_court)
colnames(shortest_path_df)=c("id")

weighted_reseau_path=weighted_reseau[weighted_reseau$from_id=="26525881",]
for (id in shortest_path_df$id[2:1486]){
weighted_reseau_path=rbind(weighted_reseau_path,weighted_reseau[weighted_reseau$from_id==id,])
}
duplicate_rows <- duplicated(weighted_reseau_path$from_id)
unique_weighted_reseau <- weighted_reseau_path[!duplicate_rows, ]
coords_path=unique_weighted_reseau[,c("from_lon","from_lat")]


points_sf=sf::st_as_sf(coords_path,coords = c("from_lon", "from_lat"),crs=4326)
leaflet::leaflet() %>%
  leaflet::addTiles() %>%
  leaflet::addMarkers(data = points_sf)

network_leaflet <- sf::st_as_sf(weighted_reseau)

# Visualiser avec leaflet
leaflet::leaflet() %>%
  leaflet::addTiles() %>%
  leaflet::addPolylines(data = reseau_14)
###############################################################################
###############################################################################

