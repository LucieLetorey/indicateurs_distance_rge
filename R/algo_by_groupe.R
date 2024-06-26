library(tidyverse)
load("//donnees.dreal-norm.ad.e2.rie.gouv.fr/services/SMCAP/05_BOeS/5PREL/base_donnees/BDNB_OPEN_SOURCE/bdnb/data/bdd_entreprise_rge.RData")
load("//donnees.dreal-norm.ad.e2.rie.gouv.fr/services/SMCAP/05_BOeS/5PREL/base_donnees/BDNB_OPEN_SOURCE/bdnb/data/bdd_dpe_logement_rel_batiment_groupe.RData")
load("//donnees.dreal-norm.ad.e2.rie.gouv.fr/services/SMCAP/05_BOeS/5PREL/base_donnees/BDNB_OPEN_SOURCE/bdnb/data/departements_geo.RData")
load("//donnees.dreal-norm.ad.e2.rie.gouv.fr/services/SMCAP/05_BOeS/5PREL/base_donnees/BDNB_OPEN_SOURCE/bdnb/data/epci_geo.RData")


#On récupère liste départements touchés par Buffer
Sys.time()
#Table avec les géométries de chaque ECPI+Buffer de 30kms
epci_geo_plus=epci_geo_simpl_lnglat%>%
  dplyr::mutate(dpt=COGiter::code_dep_of_epci(codezone)) %>% #récupérer départements Epci
  dplyr::filter(!(dpt %in% c("971","972","973","974","975","976"))) %>% #Enlever Epci outre-mer
  dplyr::mutate(geometry_plus=sf::st_buffer(geometry,dist=30000),
                superficie_buffer=sf::st_area(geometry_plus)) %>% #Colonne Buffer
  as.data.frame() %>%
  dplyr::select(!geometry)%>%
  sf::st_as_sf()

epci_geo_plus2=epci_geo_simpl_lnglat%>%
  dplyr::mutate(dpt=COGiter::code_dep_of_epci(codezone)) %>% #récupérer départements Epci
  dplyr::filter(!(dpt %in% c("971","972","973","974","975","976"))) %>% #Enlever Epci outre-mer
  dplyr::mutate(geometry_plus=sf::st_buffer(geometry,dist=31000)) %>% #Colonne Buffer
  as.data.frame() %>%
  dplyr::select(!geometry)%>%
  sf::st_as_sf()

epci_normandie=epci_geo_plus %>%    #Récupérer Indices (dans table epci_geo_plus) Epci qui ont au moins une partie dans Normandie
  #dplyr::mutate(row_number=dplyr::row_number()) %>%
  dplyr::rowwise() %>%
  dplyr::filter(any(dpt %in% c("14","27","50","61","76"))) %>%
  dplyr::pull(code_unique)




res=sf::st_intersects(epci_geo_plus,departements_geo_simpl_lnglat)#Obtenir la liste des dpts touchés par la zone epci+Buffer


print("Fin Buffer + Intersection")
Sys.time()
print("Début Boucle")

results_time = data.frame(
  code = character(),
  time_diff = numeric(),
  nb_lignes = integer(),
  nb_lignes_graphe=integer(),
  nb_lignes_graphe_c=integer(),
  superficie=numeric(),
  stringsAsFactors = FALSE
)

table_na=data.frame(code = character(),
                    nb_na_bat=integer(),
                    nb_bat_init=integer(),
                    taux_perte_bat=numeric(),
                    nb_na_entr=integer(),
                    nb_entr_init=integer(),
                    taux_perte_entr=numeric())
for(code in epci_normandie){
  t=Sys.time()
  i=epci_geo_plus%>%
    dplyr::mutate(row_number=dplyr::row_number()) %>%
    dplyr::filter(code_unique==code)%>%
    pull(row_number)

  indice_dep=res[[i]] #Indice Départements touchés par buffer + zone EPCI (Falaise = 775)dans table dpt_geo_lngt_lat
  liste_dep=departements_geo_simpl_lnglat%>% #récupération Liste dpts touchés
    dplyr::slice(indice_dep)%>%
    dplyr::pull(codezone)%>%
    as.character()





  data_temp=bdd_dpe_logement_rel_batiment_groupe%>% #Base batiment sur laquelle on applique filtre(code Epci)
  dplyr::filter(EPCI==epci_geo_plus$codezone[i])%>%
  as.data.frame()%>%
  mutate(code_commune_insee = as.character(code_commune_insee)) %>%  #utile pour manipulation tables après calcul distance
  dplyr::group_by(batiment_groupe_id,code_commune_insee,cle_interop_adr,latitude,longitude) %>%
  dplyr::summarise(nb=n()) %>%
  dplyr::mutate(id_bat=paste(cle_interop_adr,batiment_groupe_id,sep="_")) %>%
  dplyr::ungroup()

  bdd_entreprises_reg_filtree=bdd_entreprises_reg%>%
    dplyr::filter(substr(code_postal,1,2)%in%liste_dep) %>% #premier filtrage base entreprises (On ne prend que celles dans dpts touchés par buffer + Epci)
    dplyr::rowwise() %>%
    dplyr::filter(lengths(sf::st_intersects(sf::st_point(c(longitude,latitude)),epci_geo_plus[i,]))>0) #Sélectionne les entreprises qui sont dans le buffer+Zone Epci

  lon_lat_entr=bdd_entreprises_reg_filtree %>%  #Longitude,latitude base entreprise
    dplyr::select(longitude,latitude)
  lon_lat <- data_temp %>% #Longitudes,latitudes batiments. Seulement les valeurs distinctes pour moins de calculs de distance.
    dplyr::select(longitude, latitude)

  nb_l_bat=sum(data_temp$nb)
  nb_l_entr=nrow(bdd_entreprises_reg_filtree)
  #Agrégation

  Sys.time()
  print("Début Agrégation")
  agreg=NULL
  for (j in  1:length(liste_dep)){
    load(paste0("//donnees.dreal-norm.ad.e2.rie.gouv.fr/services/SMCAP/05_BOeS/5PREL/base_donnees/BDNB_OPEN_SOURCE/bdnb/data/troncons_", liste_dep[j], ".RData")) #On charge tablen tronçons traités dep j
    temp=troncons%>%
      dplyr::filter(lengths(sf::st_intersects(geometry,epci_geo_plus2[i,]))>0) # On ne garde que les Tronçons dans buffer

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
  # print("Distance")
  # dist=dodgr::dodgr_dists(graphe,from = lon_lat, to = lon_lat_entr)
  # print(dim(dist))
  # Sys.time()

  #Gestion des distances en double


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
  table_na=rbind(table_na,data.frame(code = code,
                                     nb_na_bat=sum(data_temp[indices_vide,]$nb),
                                     nb_bat_init=nb_l_bat,
                                     taux_perte_bat=(sum(data_temp[indices_vide,]$nb)/nb_l_bat)*100,
                                     nb_na_entr=length(indices_vide_col),
                                     nb_entr_init=nb_l_entr,
                                     taux_perte_entr=length(indices_vide_col)/nb_l_entr
  ))
  if(length(indices_vide)>0){
    data_temp=data_temp %>%
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



  # dist=data_temp%>% #On join la base des batiments (dont on ne choisit que lon,lat) avec matrice des distances
  #   dplyr::select(c(longitude,latitude))%>% #Selection lon,lat base bast
  #   dplyr::left_join(cbind(lon_lat,dist2),by=c("longitude","latitude"))%>% #Jointure avec matrice qui est combinaison lon_lat (coordonées uniques) et mat distances
  #   dplyr::select(!c(longitude,latitude)) #on enlève lon et lat pour n'obtenir que mat des distances in fine
  print(dim(dist2))
  Sys.time()

  #Traitement matrice de distances

  print("debut traitement")

  colnames(dist2)=bdd_entreprises_reg_filtree%>% #Nom des colonnes de dist --> siret des entreprises
    dplyr::pull(siret)


  rownames(dist2)=data_temp%>% #nom des lignes de dist --> concatenation id_dpe et batiment_groupe_id qui donne id unique
  dplyr::pull(id_bat)

  d_long_group_by_a_jour=dist2 %>%
    as.data.frame() %>%
    dplyr::mutate(ID_Bat = rownames(.))%>% #Permet d'avoir une colonne Id_Bat qui sera utilisée pour écupérer le id_dpe
    pivot_longer(-ID_Bat, #pivot longer, on obtient table avec n x m lignes où une ligne correspond à la distance entre une coord de bat et une entreprise
                 names_to = "Entreprise",
                 values_to = "Distance") %>%
    dplyr::mutate(code_commune_insee = sub("^(.*?)_.*", "\\1", ID_Bat)) %>% #récupération code INSEE pour jointure future
    # dplyr::left_join(data_temp %>% #jointure sur les dpe_id pour récupérer code commune insee chaque bat
    #                    dplyr::distinct(cle_interop_adr,code_commune_insee),
    #                  by = c("cle" = "cle_interop_adr")) %>%
    # dplyr::select(-cle) %>%
    dplyr::mutate(inf_30=ifelse(Distance<30000,1,0), #On ajoute colonnes pour pouvoir compter le nb de distance inf à une certaine valeur
                  inf_60=ifelse(Distance<60000,1,0)) %>%
    dplyr::group_by(code_commune_insee,ID_Bat)%>%#on groupe par commune et par batiment
    dplyr::summarise(dist_moy=min(Distance,na.rm=TRUE),
                     nb_inf_30=sum(inf_30,na.rm=TRUE),
                     nb_inf_60=sum(inf_60,na.rm=TRUE)
                     ) %>% #Permet d'avoir le nombre de batiments traités
    dplyr::left_join(data_temp %>%
                    dplyr::select(id_bat,nb),
                     by= c("ID_Bat"="id_bat")) %>%
    dplyr::ungroup()%>%
    dplyr::mutate(dist_moy=nb*dist_moy,
                  nb_inf_30=nb*nb_inf_30,
                  nb_inf_60=nb*nb_inf_60) %>%
    dplyr::select(!ID_Bat) %>%
    COGiter::passer_au_cog_a_jour(code_commune = "code_commune_insee",aggrege=FALSE)


  resultat_etape=COGiter::cogifier(d_long_group_by_a_jour,communes=FALSE,regions=TRUE,metro=FALSE,na.rm=TRUE)
  save(resultat_etape,file =paste0("//donnees.dreal-norm.ad.e2.rie.gouv.fr/services/SMCAP/05_BOeS/5PREL/base_donnees/BDNB_OPEN_SOURCE/bdnb/data/res_",code,".RData"))
  t2=Sys.time()

  results_time = rbind(results_time,data.frame(
    code = code,
    time_diff=difftime(t2,t,units="mins"),
    nb_lignes=nrow(agreg),
    nb_lignes_graphe=nb_lignes,
    nb_lignes_graphe_c=nrow(graphe),
    superficie=epci_geo_plus$superficie_buffer[i],
    stringsAsFactors = FALSE))
}
save(results_time,file ="//donnees.dreal-norm.ad.e2.rie.gouv.fr/services/SMCAP/05_BOeS/5PREL/base_donnees/BDNB_OPEN_SOURCE/bdnb/data/df_temps.RData")
save(table_na,file ="//donnees.dreal-norm.ad.e2.rie.gouv.fr/services/SMCAP/05_BOeS/5PREL/base_donnees/BDNB_OPEN_SOURCE/bdnb/data/table_na.RData")

Sys.time()
print("Fin boucle Distances-Agrégation des tables obtenues")



#AGREGATION RESULTATS EPCI#

agreg=NULL
for (code in epci_normandie){

  load(paste0("//donnees.dreal-norm.ad.e2.rie.gouv.fr/services/SMCAP/05_BOeS/5PREL/base_donnees/BDNB_OPEN_SOURCE/bdnb/data/res_",code,".RData"))
  agreg=rbind(agreg,resultat_etape)

}
agreg=agreg %>%
  group_by(TypeZone,Zone,CodeZone)%>%
  summarise(across(.cols = where(is.numeric), sum))
save(agreg,file="//donnees.dreal-norm.ad.e2.rie.gouv.fr/services/SMCAP/05_BOeS/5PREL/base_donnees/BDNB_OPEN_SOURCE/bdnb/data/res_agreges.RData")

print("fin")
Sys.time()


