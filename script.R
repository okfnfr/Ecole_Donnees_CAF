library(widgetframe)
library(tmap)
library(tmaptools)
library(sp)

load("/media/Data/Dropbox/DonneesIGN_ContoursIRIS/IRISCONTOURS-IRIS75.Rdata")

iris <- `IRISCONTOURS-IRIS75`
freresvoisins <- iris[iris@data$NOM_IRIS %in% "JAVEL 1",]
chevaleret <- iris[iris@data$NOM_IRIS %in% c("GARE 15", "GARE 21", "GARE 20", "GARE 19", "GARE 13", "GARE 14"),]
lachapelle <- iris[stringr::str_detect(iris@data$NOM_IRIS, "CHAPELLE"),]

chevaleret <- aggregate_map(chevaleret, by = "DEPCOM")
lachapelle <- aggregate_map(lachapelle, by ="DEPCOM")

arrondissements <- aggregate_map(iris, by = "DEPCOM")
arrondissementsok <- arrondissements[arrondissements@data$DEPCOM %in% c("75113", "75115", "75118"),]

# fonds <- read_osm(iris, zoom = 11, type = "stamen-terrain")

library(leaflet)

freresvoisins <- set_projection(freresvoisins, CRS("+init=epsg:4326"))
chevaleret <- set_projection(chevaleret, CRS("+init=epsg:4326"))
lachapelle <- set_projection(lachapelle, CRS("+init=epsg:4326"))
arrondissementsok <- set_projection(arrondissementsok, CRS("+init=epsg:4326"))

l <- leaflet() %>% 
  addProviderTiles(providers$CartoDB.DarkMatter, group = "Sombre") %>%
  addProviderTiles(providers$CartoDB.Positron, group = "Clair") %>%
  addPolygons(data = arrondissementsok, color = "lightgrey", weight = 3, fill = FALSE) %>% 
  addPolygons(data = freresvoisins, color = "red", weight = 8) %>% 
  addPolygons(data = chevaleret, color = "blue", weight = 8) %>% 
  addPolygons(data = lachapelle, color = "green", weight = 8) %>% 
  addLayersControl(baseGroups=c('Sombre','Clair'))

htmlwidgets::saveWidget(frameableWidget(l),'quartiers.html')
