## code to prepare `geo_simpl_lng_lat` dataset goes here

install.packages("rmapshaper")
remotes::install_github("MaelTheuliere/COGiter")

library(tidyr)

regions_geo_simpl_lnglat = COGiter::regions_geo %>%
  dplyr::select(!AREA) %>%
  dplyr::rename_with(tolower) %>%
  rmapshaper::ms_simplify(keep = 0.1,
                          keep_shapes = FALSE) %>%
  sf::st_transform( crs = 4326) %>%
  dplyr::rename("codezone" = "reg") %>%
  dplyr::mutate(code_unique = paste0("R",codezone),
                typezone = 'Régions')


departements_geo_simpl_lnglat = COGiter::departements_geo %>%
  dplyr::select(!AREA) %>%
  dplyr::rename_with(tolower) %>%
  rmapshaper::ms_simplify(keep = 0.1,
                          keep_shapes = FALSE) %>%
  sf::st_transform( crs = 4326) %>%
  dplyr::rename("codezone" = "dep") %>%
  dplyr::mutate(code_unique = paste0("D",codezone),
                typezone = 'Départements')

epci_geo_simpl_lnglat = COGiter::epci_geo %>%
  dplyr::select(!AREA) %>%
  dplyr::rename_with(tolower) %>%
  rmapshaper::ms_simplify(keep = 0.1,
                          keep_shapes = FALSE) %>%
  sf::st_transform( crs = 4326) %>%
  dplyr::rename("codezone" = "epci") %>%
  dplyr::mutate(code_unique = paste0("E",codezone),
                typezone = 'Epci')



geo_simpl_lng_lat = rbind(regions_geo_simpl_lnglat,
                          departements_geo_simpl_lnglat ,
                          epci_geo_simpl_lnglat)


departements_geo_simpl_lnglat_plus %>%
  as.data.frame() %>%
  dplyr::select(!geometry) %>%
  sf::st_as_sf() %>%
  dplyr::filter(code_unique=="D01") %>%
  leaflet::leaflet() %>%
  leaflet::addTiles() %>%
  leaflet::addPolygons()

epci_geo_plus %>%
  # as.data.frame() %>%
  # dplyr::select(!geometry) %>%
  # sf::st_as_sf() %>%
  dplyr::filter(code_unique=="E200042604") %>%
  leaflet::leaflet() %>%
  leaflet::addTiles() %>%
  leaflet::addPolygons()


  dplyr::filter(typezone == 'Epci') %>%
  leaflet::leaflet() %>%
  leaflet::addTiles() %>%
  leaflet::addPolygons()



  agreg%>%
  leaflet::leaflet() %>%
  leaflet::addTiles() %>%
  leaflet::addPolygons()
