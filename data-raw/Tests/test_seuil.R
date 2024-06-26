#Tests Pour seuil
results <- data.frame(
  code = character(),
  time_diff = numeric(),
  num_lines = integer(),
  stringsAsFactors = FALSE
)
for(code in epci_normandie){
  Sys.time()
  i=epci_geo_plus%>%
    dplyr::mutate(row_number=dplyr::row_number()) %>%
    dplyr::filter(code_unique==code)%>%
    pull(row_number)
  
  indice_dep=res[[i]] #Indice Départements touchés par buffer + zone EPCI (Falaise = 775)dans table dpt_geo_lngt_lat
  liste_dep=departements_geo_simpl_lnglat%>% #récupération Liste dpts touchés
    dplyr::slice(indice_dep)%>%
    dplyr::pull(codezone)%>%
    as.character()

t=Sys.time()
print("Début Agrégation")
agreg=NULL
for (j in  1:length(liste_dep)){
  load(paste0("//donnees.dreal-norm.ad.e2.rie.gouv.fr/services/SMCAP/05_BOeS/5PREL/base_donnees/BDNB_OPEN_SOURCE/bdnb/data/troncons_", liste_dep[j], ".RData")) #On charge tablen tronçons traités dep j
  temp=troncons%>%
    dplyr::filter(lengths(sf::st_intersects(geometry,epci_geo_plus2[i,]))>0) # On ne garde que les Tronçons dans buffer
  
  agreg=rbind(agreg,temp) #On agrege
}


Sys.time()
agreg=agreg %>% dplyr::distinct(ID,.keep_all=TRUE)
t2=Sys.time()

results = rbind(results,data.frame(
  code = code,
  time_diff=difftime(t2,t,units="mins"),
  nb_lignes=nrow(agreg),
  stringsAsFactors = FALSE
))
}


plot(results$time_diff,results$nb_lignes)
