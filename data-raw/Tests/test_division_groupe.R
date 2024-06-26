#TEST DIVISION EPCI GROUPE#
Sys.time()
library(tidyr)
load("//donnees.dreal-norm.ad.e2.rie.gouv.fr/services/SMCAP/05_BOeS/5PREL/base_donnees/BDNB_OPEN_SOURCE/bdnb/data/bdd_entreprise_rge.RData")
load("//donnees.dreal-norm.ad.e2.rie.gouv.fr/services/SMCAP/05_BOeS/5PREL/base_donnees/BDNB_OPEN_SOURCE/bdnb/data/bdd_dpe_representatif_rel_batiment_groupe_ffo.RData")
load("//donnees.dreal-norm.ad.e2.rie.gouv.fr/services/SMCAP/05_BOeS/5PREL/base_donnees/BDNB_OPEN_SOURCE/bdnb/data/departements_geo.RData")
load("//donnees.dreal-norm.ad.e2.rie.gouv.fr/services/SMCAP/05_BOeS/5PREL/base_donnees/BDNB_OPEN_SOURCE/bdnb/data/epci_geo.RData")

num_cells=2
#Récupération bordure EPCI
bbox=sf::st_bbox(epci_geo_simpl_lnglat[271,])
#Buffer sur les contours de l'EPCI pour être sûr d'avoir tous les bâtiments. On rajoute 1 km au rectangle bordure grille
bbox_sf=sf::st_as_sfc(bbox)
buffer_contour=sf::st_buffer(bbox_sf,dist=1000)
grille=sf::st_make_grid(buffer_contour,n=num_cells) #Num cellule opti à déterminer, quand 1 seul nb -> nb* cellule en long et nb*cellule en large donc 4 quand =2.
#Récupération géométrie zones EPCI dans chaque cellule grille
petites_zones=sf::st_intersection(epci_geo_simpl_lnglat[271,],grille)

#buffer autour des morceaux de chaque EPCI
petites_zones_plus=petites_zones%>%
  dplyr::mutate(geometry_plus=sf::st_buffer(geometry,dist=30000),
                superficie_buffer=sf::st_area(geometry_plus)) %>% #Colonne Buffer
  as.data.frame() %>%
  dplyr::select(!geometry)%>%
  sf::st_as_sf()
#buffer un peu plus grand pour ne pas oublier tronçons lors agrégation tables tronçons
petites_zones_plus2=petites_zones%>%
  dplyr::mutate(geometry_plus=sf::st_buffer(geometry,dist=31000),
                superficie_buffer=sf::st_area(geometry_plus)) %>% #Colonne Buffer
  as.data.frame() %>%
  dplyr::select(!geometry)%>%
  sf::st_as_sf()

#Base batiment filtree par EPCI
data_temp=bdd_dpe_representatif_rel_batiment_groupe_ffo %>%
  dplyr::filter(EPCI=="200054807") %>%
  dplyr::mutate(longitude_copy = longitude, latitude_copy = latitude) #filter dans boucle enleve long et lat
#Transformation en objet sf pour réaliser jointure
data_temp=sf::st_as_sf(
  data_temp,
  coords = c("longitude", "latitude"),
  crs = 4326
)
#On crée un data frame avec les géométries de chaque celulle, puis on le transforme en objet SF.
grid_sf= sf::st_as_sf(data.frame(geometry = sf::st_geometry(grille)), crs = 4326)
#On attribue à chq cellule un ID.
grid_sf=grid_sf %>%
  dplyr::mutate(cell_id = 1:nrow(grid_sf))

#Jointure qui permet d'associer à chaque ligne data_temp le numéro de cellule auquel il appartient
data_temp=sf::st_join(data_temp, grid_sf, join = sf::st_intersects)

#Récupération liste des départements touchés par buffer morceux EPCi
res=sf::st_intersects(petites_zones_plus,departements_geo_simpl_lnglat)#Obtenir la liste des dpts touchés par chaque zone epci divisée + son Buffer

table_na=data.frame(code = character(),
                    nb_na_bat=integer(),
                    nb_bat_init=integer(),
                    taux_perte_bat=numeric(),
                    nb_na_entr=integer(),
                    nb_entr_init=integer(),
                    taux_perte_entr=numeric())

for (i in 1:length(grid_sf$cell_id)){
  indice_dep=res[[i]] #Indice Départements touchés par buffer + zone EPCI (Falaise = 775)dans table dpt_geo_lngt_lat
  liste_dep=departements_geo_simpl_lnglat%>% #récupération Liste dpts touchés
    dplyr::slice(indice_dep)%>%
    dplyr::pull(codezone)%>%
    as.character()

  data_temp2=data_temp %>%
    dplyr::filter(cell_id==i) %>%
    as.data.frame()%>%
    dplyr::mutate(code_commune_insee = as.character(code_commune_insee)) %>%
    dplyr::rename(longitude=longitude_copy,latitude=latitude_copy)


  bdd_entreprises_reg_filtree=bdd_entreprises_reg%>%
    dplyr::filter(substr(code_postal,1,2)%in%liste_dep) %>% #premier filtrage base entreprises (On ne prend que celles dans dpts touchés par buffer + Epci)
    dplyr::rowwise() %>%
    dplyr::filter(lengths(sf::st_intersects(sf::st_point(c(longitude,latitude)),petites_zones_plus[i,]))>0)

  lon_lat_entr=bdd_entreprises_reg_filtree %>%  #Longitude,latitude base entreprise
    dplyr::select(longitude,latitude)
  lon_lat <- data_temp2 %>% #Longitudes,latitudes batiments. Seulement les valeurs distinctes pour moins de calculs de distance.
    dplyr::select(longitude, latitude)

  nb_l_bat=sum(data_temp2$nb_log)
  nb_l_entr=nrow(bdd_entreprises_reg_filtree)


  Sys.time()
  print("Début Agrégation")
  agreg=NULL
  for (j in  1:length(liste_dep)){
    load(paste0("//donnees.dreal-norm.ad.e2.rie.gouv.fr/services/SMCAP/05_BOeS/5PREL/base_donnees/BDNB_OPEN_SOURCE/bdnb/data/troncons_", liste_dep[j], ".RData")) #On charge tablen tronçons traités dep j
    temp=troncons%>%
      dplyr::filter(lengths(sf::st_intersects(geometry,petites_zones_plus2[i,]))>0) # On ne garde que les Tronçons dans buffer

    agreg=rbind(agreg,temp) #On agrege
  }
  Sys.time()
  agreg=agreg %>% dplyr::distinct(ID,.keep_all=TRUE) #Suppression des tronçons en double qui alourdissent le graphe
  Sys.time()
  print("Graphe")
  graphe=dodgr::weight_streetnet(agreg,id_col="ID",wt_profile="motorcar",keep_cols = "oneway") #Transformtion tronçons en graphe
  nb_lignes=nrow(graphe)
  Sys.time()
  print("Graphe contracté")
  graphe=dodgr::dodgr_contract_graph(graphe)
  #Contraction graphe
  Sys.time()

  Sys.time()

  graphe=graphe[graphe$component==1,]

  print("Calcul distances à partir uniques")
  dist2=dodgr::dodgr_dists(graphe,from = lon_lat, to = lon_lat_entr) %>%
    as.data.frame()#Matrice des distances sur liste des batiments uniques
  Sys.time()



  indices_vide=which(rowSums(is.na(dist2)) == ncol(dist2))
  indices_vide_col=which(colSums(is.na(dist2)) == nrow(dist2))

  if(length(indices_vide)>0){
    graphe=dodgr::add_nodes_to_graph(graphe,lon_lat[indices_vide,])
    dist_vide=dodgr::dodgr_dists(graphe,from=lon_lat[indices_vide,],to=lon_lat_entr)
    dist2[indices_vide,]=dist_vide
  }


  if(length(indices_vide_col)>0){
    graphe=dodgr::add_nodes_to_graph(graphe,lon_lat_entr[indices_vide_col,])
    dist_vide=dodgr::dodgr_dists(graphe,from=lon_lat,to=lon_lat_entr[indices_vide_col,])
    dist2[,indices_vide_col]=dist_vide
  }
  if (length(indices_vide)>0 &&length(indices_vide_col)>0){
    dist_vide=dodgr::dodgr_dists(graphe,from=lon_lat[indices_vide,],to=lon_lat_entr[indices_vide_col,])
    dist2[indices_vide,indices_vide_col]=dist_vide
  }

  indices_vide=which(rowSums(is.na(dist2)) == ncol(dist2))
  indices_vide_col=which(colSums(is.na(dist2)) == nrow(dist2))
  table_na=rbind(table_na,data.frame(code = i,
                                     nb_na_bat=sum(data_temp2[indices_vide,]$nb_log),
                                     nb_bat_init=nb_l_bat,
                                     taux_perte_bat=(sum(data_temp2[indices_vide,]$nb_log)/nb_l_bat)*100,
                                     nb_na_entr=length(indices_vide_col),
                                     nb_entr_init=nb_l_entr,
                                     taux_perte_entr=length(indices_vide_col)/nb_l_entr
  ))

  if(length(indices_vide)>0){
    data_temp2=data_temp2%>%
      dplyr::slice(-indices_vide)
    dist2=dist2%>%
      dplyr::slice(-indices_vide)
  }

  if(length(indices_vide_col)>0){
    bdd_entreprises_reg_filtree=bdd_entreprises_reg_filtree %>%
      dplyr::slice(-indices_vide_col)
    dist2=dist2%>%
      dplyr::select(-indices_vide_col)
  }
  colnames(dist2)=bdd_entreprises_reg_filtree%>% #Nom des colonnes de dist --> siret des entreprises
    dplyr::pull(siret)


  rownames(dist2)=data_temp2%>% #nom des lignes de dist --> concatenation id_dpe et batiment_groupe_id qui donne id unique
    dplyr::pull(batiment_groupe_id)

  d_long_group_by_a_jour=dist2 %>%
    as.data.frame() %>%
    dplyr::mutate(ID_groupe = rownames(.))%>% #Permet d'avoir une colonne Id_Bat qui sera utilisée pour écupérer le id_dpe
    pivot_longer(-ID_groupe, #pivot longer, on obtient table avec n x m lignes où une ligne correspond à la distance entre une coord de bat et une entreprise
                 names_to = "Entreprise",
                 values_to = "Distance") %>%
    dplyr::left_join(data_temp2 %>%
                       dplyr::select(batiment_groupe_id,code_commune_insee),
                     by=c('ID_groupe'='batiment_groupe_id')) %>%
    dplyr::mutate(inf_30=ifelse(Distance<30000,1,0), #On ajoute colonnes pour pouvoir compter le nb de distance inf à une certaine valeur
                  inf_60=ifelse(Distance<60000,1,0)) %>%
    dplyr::group_by(code_commune_insee,ID_groupe)%>%#on groupe par commune et par batiment
    dplyr::summarise(dist_min=min(Distance,na.rm=TRUE),
                     nb_inf_30=sum(inf_30,na.rm=TRUE),
                     nb_inf_60=sum(inf_60,na.rm=TRUE)
    ) %>% #Permet d'avoir le nombre de batiments traités
    dplyr::left_join(data_temp2%>%
                       dplyr::select(batiment_groupe_id,nb_log),
                     by= c("ID_groupe"="batiment_groupe_id")) %>%
    dplyr::ungroup()%>%
    dplyr::mutate(dist_min=nb_log*dist_min,
                  nb_inf_30=nb_log*nb_inf_30,
                  nb_inf_60=nb_log*nb_inf_60) %>%
    dplyr::select(!ID_groupe) %>%
    COGiter::passer_au_cog_a_jour(code_commune = "code_commune_insee",aggrege=FALSE)


  resultat_etape2=COGiter::cogifier(d_long_group_by_a_jour,communes=FALSE,regions=TRUE,metro=FALSE,na.rm=TRUE)
  save(resultat_etape2,file =paste0("//donnees.dreal-norm.ad.e2.rie.gouv.fr/services/SMCAP/05_BOeS/5PREL/base_donnees/BDNB_OPEN_SOURCE/bdnb/data/res_mars_",i,".RData"))
}

Sys.time()
