# Installer et charger le package sf
install.packages("sf")
library(sf)

# Spécifier le chemin vers le fichier shapefile
chemin <- "//10.14.128.55/dossiers/geobase/REF_EXT/BDTOPO/BDTOPO_ED202306/BDTOPO_3-3_TOUSTHEMES_SHP_D014/TRANSPORT/TRONCON_DE_ROUTE.shp"

# Lire le fichier shapefile
donnees_spatiales <- st_read(chemin) %>% 
  st_transform("+proj=longlat +datum=WGS84")

donnees_spatiales = donnees_spatiales %>% 
  sf::st_as_sf() %>% 
  sf::st_zm()

leaflet::leaflet() %>% 
  leaflet::addTiles() %>% 
  leaflet::addPolylines(data = donnees_spatiales)

# Définir la zone géographique d'intérêt
bbox <- matrix(c( 3.95, 4, 45, 46), ncol = 2) 
# Coordonnées GPS de la zone
colnames(bbox) <-  c("longitude", "latitude")
rownames(bbox) <-  c("min", "max")
# Extraire le réseau routier à partir des données OSM
net <- dodgr::dodgr_streetnet(bbox = bbox)
graph = dodgr::weight_streetnet(net, wt_profile = "motorcar")
graph = dodgr::weight_streetnet(donnees_spatiales, wt_profile = "motorcar")



dodgr::dodgr_dists()
