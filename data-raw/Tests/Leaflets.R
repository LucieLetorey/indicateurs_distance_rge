
leaflet::leaflet() %>%
leaflet::addTiles() %>%
leaflet::addPolygons(data = epci_geo_plus[775, ],
              color = "blue", # Couleur du contour
              weight = 2, # Épaisseur du contour
              opacity = 1, # Opacité du contour
              fillColor = "blue", # Couleur de remplissage
              fillOpacity = 0.1, # Opacité du remplissage
              group = "epci_geo_plus") %>%
leaflet::addPolygons(data = epci_geo_simpl_lnglat[785, ],
              color = "red", # Couleur du contour
              weight = 2, # Épaisseur du contour
              opacity = 2, # Opacité du contour
              fillColor = "red", # Couleur de remplissage
              fillOpacity = 0.5, # Opacité du remplissage
              group = "epci_geo_simpl_lnglat")


#PATTERN POUR PALETTE COULEUR#
data = departements_geo_simpl_lnglat %>%
  dplyr::mutate(y = runif(n=101,min = 0, max = 1))

bins = c(0, .25,.50,.75, 1)


pal = leaflet::colorBin(palette = "YlOrRd" , domain = data$y, bins = bins)

map = leaflet::leaflet(data = data) %>%
  leaflet::addTiles() %>%
  leaflet::addPolygons(
    fillColor = ~pal(y),
    fillOpacity = 0.7,
    color = "#BDBDC3",
    weight = 1
  )
map
#TEST PATTERN EPCI Normandes#
test_agreg=agreg_modif %>%
           dplyr::left_join(COGiter::epci_geo,
                            by = c("CodeZone"="EPCI")) %>%
            sf::st_as_sf()%>%
            sf::st_transform(crs=4326)
p=(RColorBrewer::brewer.pal(5,"Purples"))


bins=c(0,128,167,228,530)



pal = leaflet::colorBin(palette =RColorBrewer::brewer.pal(4,"Purples")  , domain = test_agreg$nb_inf_30_moy, bins = bins)

map = leaflet::leaflet(data = test_agreg) %>%
  leaflet::addTiles() %>%
  # leaflet::addProviderTiles("OpenStreetMap.France") %>%
  leaflet::addPolygons(
    fillColor = ~pal(nb_inf_30_moy),
    fillOpacity = 0.8,
    color = "black",
    weight = 1
  ) %>%
  leaflet::addLegend(
    pal=pal,
    values = test_agreg$nb_inf_30_moy,
    title = "Nombre d'entreprises RGE moyen<br>à moins de 30kms des passoires",
    position = "bottomright",
    labFormat = leaflet::labelFormat(prefix = ""),
  ) %>%
  leaflet::addPolygons(data = COGiter::departements_geo %>%
                         dplyr::filter(DEP %in%
                                         c("14","27", "50", "76", "61")) %>%
                         sf::st_transform( crs = 4326),
                       fillOpacity = 0,
                       weight = 2)
map

# htmltools::tags$style(htmltools::HTML("
#   .semi-transparent-legend {
#     background-color: #rgba(255, 255, 255, 0.8) !important; /* Blanc avec 80% d'opacité */
#     box-shadow: none !important;
#     border: 1px solid #ccc !important; /* Optionnel : ajouter une bordure pour une meilleure visibilité */
#   }
# ")) %>% htmltools::browsable()





bins2=c(0,1.13,1.44,1.95,4.51)




pal2 = leaflet::colorBin(palette =rev(RColorBrewer::brewer.pal(4,"Purples")) , domain = test_agreg$dist_min_moy, bins = bins2)

map2 = leaflet::leaflet(data = test_agreg) %>%
  leaflet::addTiles() %>%
  # leaflet::addProviderTiles("OpenStreetMap.France") %>%
  leaflet::addPolygons(
    fillColor = ~pal2(dist_min_moy),
    fillOpacity = 0.8,
    color = "black",
    weight = 1
  ) %>%
  leaflet::addLegend(
    pal=pal2,
    values = test_agreg$dist_min_moy,
    title = "Distance minimale moyenne (km)<br> des  passoires aux entreprises RGE",
    position = "bottomright",
    labFormat = leaflet::labelFormat(prefix = ""),
  ) %>%
  leaflet::addPolygons(data = COGiter::departements_geo %>%
                         dplyr::filter(DEP %in%
                                         c("14","27", "50", "76", "61")) %>%
                         sf::st_transform( crs = 4326),
                       fillOpacity = 0,
                       weight=2)

map2
