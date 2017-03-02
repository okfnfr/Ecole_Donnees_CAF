# livrables pour la première expé de données


# carte des EPN

library(sp)
library(spdplyr)
library(tidyverse)
library(tmap)
library(tmaptools)
library(banR)
library(hrbrthemes)

library(sf)
iris <- read_sf("./data/CONTOURS-IRIS75.shp", stringsAsFactors = FALSE)


# utilisation des services en ligne de la CAF

enligne <- read_delim("./data/Inclusion_numerique.csv", 
                      delim = ";",
                      col_types = cols(
                      IRIS = col_character(),
                      CAFFR = col_integer(),
                      APPLI = col_integer(),
                      TX_EMAIL = col_double()
                      ), 
                      locale = locale(decimal_mark = "."),
                      na = ".",
                     )
menages <- read_csv2("./data/menages2013.csv")
config_familiales <- read_csv2("./data/configfamiliale2015.csv")

enligne %>%
  mutate(CODGEO = stringr::str_sub(IRIS, 1, 5)) %>% 
  group_by(CODGEO) %>% 
  summarise(CAFFR = sum(CAFFR), APPLI = sum(APPLI)) %>% 
  left_join(menages %>% filter(CODGEO %in% as.character(75101:75120)), by = "CODGEO") %>% 
  mutate(CAFFR_menage = CAFFR / C13_MEN,
         APPLI_menages = APPLI / C13_MEN) %>% 
  select(CODGEO, LIBGEO, CAFFR_menage, APPLI_menages) %>% 
  filter(!is.na(CAFFR_menage)) %>% 
  arrange(desc(CAFFR_menage)) %>% 
  as.data.frame()
# nb de connexions corrélées au nb d'allocataires ?

png("./livrables/connexions_caffr.png", width = 800, height = 400)
enligne %>% 
  mutate(CODGEO = stringr::str_sub(IRIS, 1, 5)) %>% 
  group_by(CODGEO) %>% 
  summarise(CAFFR = sum(CAFFR), APPLI = sum(APPLI)) %>% 
  left_join(menages %>% filter(CODGEO %in% as.character(75101:75120)), by = "CODGEO") %>%
  left_join(config_familiales, by = c(CODGEO = "Codes_Insee")) %>% 
  mutate(CAFFR_menage = CAFFR / C13_MEN,
         APPLI_menages = APPLI / C13_MEN,
         CAFFR_alloc = CAFFR / NB_allocataires,
         APPLI_alloc = APPLI / NB_allocataires) %>% 
  select(CODGEO, LIBGEO, CAFFR_menage, APPLI_menages, CAFFR_alloc, APPLI_alloc) %>% 
  filter(!is.na(CAFFR_menage)) %>% 
  mutate(color = if_else(CODGEO %in% "75113", "#FFAF9D", "#16307F")) %>% 
  arrange(CAFFR_alloc) %>% 
  mutate(CODGEO = forcats::as_factor(CODGEO)) %>% 
  ggplot(aes(x = CODGEO, y = CAFFR_alloc)) +
   geom_col(aes(fill = color), show.legend = FALSE) +
   scale_fill_manual(values = c("#FFAF9D" = "#FFAF9D", "#16307F" = "#16307F")) +
   geom_text(aes(label = paste0(substr(CODGEO, 4, 5), "e")), hjust = 0, nudge_y = 0.5, size = 5) +
   coord_flip() +
   theme_ipsum(grid = "X", base_size = 25) +
   xlab("") +
   ylab("Nombre de connexions à caf.fr par foyer allocataire") +
   theme(axis.text.y = element_blank()) +
   ylim(c(0, 13))
dev.off()

png("./livrables/connexions_appli.png", width = 800, height = 400)
enligne %>% 
  mutate(CODGEO = stringr::str_sub(IRIS, 1, 5)) %>% 
  group_by(CODGEO) %>% 
  summarise(CAFFR = sum(CAFFR), APPLI = sum(APPLI)) %>% 
  left_join(menages %>% filter(CODGEO %in% as.character(75101:75120)), by = "CODGEO") %>%
  left_join(config_familiales, by = c(CODGEO = "Codes_Insee")) %>% 
  mutate(CAFFR_menage = CAFFR / C13_MEN,
         APPLI_menages = APPLI / C13_MEN,
         CAFFR_alloc = CAFFR / NB_allocataires,
         APPLI_alloc = APPLI / NB_allocataires) %>% 
  select(CODGEO, LIBGEO, CAFFR_menage, APPLI_menages, CAFFR_alloc, APPLI_alloc) %>% 
  filter(!is.na(CAFFR_menage)) %>% 
  mutate(color = if_else(CODGEO %in% "75113", "#FFAF9D", "#16307F")) %>% 
  arrange(APPLI_alloc) %>% 
  mutate(CODGEO = forcats::as_factor(CODGEO)) %>% 
  ggplot(aes(x = CODGEO, y = APPLI_alloc)) +
  geom_col(aes(fill = color), show.legend = FALSE) +
  scale_fill_manual(values = c("#FFAF9D" = "#FFAF9D", "#16307F" = "#16307F")) +
  geom_text(aes(label = paste0(substr(CODGEO, 4, 5), "e")), hjust = 0, nudge_y = 0.5, size = 5) +
  coord_flip() +
  theme_ipsum(grid = "X", base_size = 25) +
  xlab("") +
  ylab("Nombre de connexions à l'appli CAF par foyer allocataire") +
  theme(axis.text.y = element_blank()) +
  ylim(c(0, 13))
dev.off()

## idem à l'IRIS, et comparaison avec autres modes de contact avec la CAF

allocataires_iris <- read_csv2("./data/iris_alloc.csv")
accueil_iris <- read_csv2("./data/iris_accueil.csv")
bornes_iris <- read_csv2("./data/iris_bornes.csv")

contacts <- allocataires_iris %>% 
  left_join(enligne, by = c("no_iris" = "IRIS")) %>% 
  left_join(accueil_iris, by = "no_iris") %>% 
  left_join(bornes_iris, by = "no_iris")

contacts_pc <- contacts %>% 
  mutate_at(vars(APL:RSA), funs(. / Total_alloc * 100)) %>% 
  mutate_at(vars(CAFFR, APPLI, Accueil_physique, Borne), funs(. / Total_alloc))

library(extrafont)
loadfonts("pdf", quiet = TRUE)
loadfonts("postscript", quiet = TRUE)


png("./livrables/contacts.png", width = 16.54, height = 23.39, units = "in", res = 300)
pdf("./livrables/contacts.pdf", width = 16.54, height = 23.39)
contacts %>% 
  filter(!is.na(no_iris)) %>% 
  filter(!no_iris %in% c("Inconnu", "Total")) %>% 
  mutate(CODGEO = stringr::str_sub(no_iris, 1, 5)) %>% 
  group_by(CODGEO) %>% 
  summarise_at(vars(-no_iris, -TX_EMAIL), funs(sum(., na.rm = TRUE))) %>% 
  mutate_at(vars(APL:RSA), funs(. / Total_alloc * 100)) %>% 
  mutate_at(vars(CAFFR, APPLI, Accueil_physique, Borne), funs(. / Total_alloc)) %>% 
  gather(contact, valeur, CAFFR:Borne) %>% 
  mutate(contact = factor(contact, levels = c("CAFFR", "APPLI", "Accueil_physique", "Borne"))) %>% 
  arrange(contact, valeur) %>% 
  mutate(CODGEO = forcats::as_factor(CODGEO)) %>% 
  mutate(color = if_else(CODGEO %in% "75113", "#FFAF9D", "#16307F")) %>% 
  ggplot(aes(x = CODGEO, y = valeur)) +
    geom_col(aes(fill = color), width = 0.85, show.legend = FALSE) +
    scale_fill_manual(values = c("#FFAF9D" = "#FFAF9D", "#16307F" = "#16307F")) +
    geom_text(aes(label = paste0(substr(CODGEO, 4, 5), "e")), hjust = 0, nudge_y = 0.5, size = 5) +
    coord_flip() +
    facet_wrap(~ contact, labeller = labeller(contact = c("Accueil_physique" = "Accueil physique", "APPLI" = "Appli mobile", "Borne" = "Borne en agence", "CAFFR" = "Site caf.fr"))) +
    theme_ipsum(grid = "X", base_size = 25) +
    xlab("") +
    ylab("Nombre de contacts avec la CAF par foyer allocataire, en 2015") +
    labs(title = "La grande majorité des contacts des allocataires avec la CAF se fait de manière dématérialisée", subtitle = "Le nombre de contacts est plus élevé dans les arrondissements les plus populaires.", caption = "Source : CAF de Paris. Réalisation : École des données/OKF pour la CAF.") +
    theme(axis.text.y = element_blank()) +
    ylim(c(0, 13))
dev.off()
dev.off()

# carte à l'iris

pc_iris <- iris %>% 
  left_join(contacts_pc, by = c("DCOMIRIS" = "no_iris")) %>% 
  st_transform(4326)

iris13 <- pc_iris %>% 
  filter(DEPCOM %in% "75113") %>% 
  st_union() %>% 
  st_sf() %>% 
  st_transform(4326)


stbb <- pc_iris %>% 
  filter(DEPCOM %in% "75113") %>% 
  st_bbox()

stbb_tmapbb <- function(x) {
  matrix(c(x["xmin"], x["ymin"], x["xmax"], x["ymax"]), nrow = 2, dimnames = list(c("x", "y"), c("min", "max")))
}

paris13_osm <- read_osm(stbb_tmapbb(stbb), type = "https://cartodb-basemaps-a.global.ssl.fastly.net/light_all/{z}/{x}/{y}.png", minNumTiles = 25)

chevaleret <- pc_iris %>% 
  filter(NOM_IRIS %in% c("GARE 15", "GARE 21", "GARE 20", "GARE 19", "GARE 13", "GARE 14")) %>%
  st_union() %>% 
  st_sf() %>% 
  st_transform(4326)
  

pdf("./livrables/carte_contacts.pdf", width = 16.54, height = 23.39)
png("./livrables/carte_contacts.png", width = 16.54, height = 23.39, units = "in", res = 300)
tm_shape(paris13_osm) +
  tm_raster() +
tm_shape(pc_iris %>% filter(DEPCOM %in% "75113")) +
  tm_fill(col = c("CAFFR", "APPLI", "Accueil_physique", "Borne"), alpha = 0.5, showNA = FALSE, title = "Contacts par\nfoyer allocataire") +
tm_shape(iris13) +
  tm_borders() +
tm_shape(chevaleret) +
  tm_borders(lty = 4, lwd = 2) + 
tm_legend(legend.position = c("right", "bottom"), legend.format = list(text.separator = "à")) +
tm_layout(title = c("Site caf.fr", "Appli mobile", "Accueil physique", "Borne en agence"), scale = 2, attr.outside.position = "bottom", attr.outside = TRUE) +
  tm_credits(text = "Source : CAF de Paris. Réalisation : École des données/OKF pour la CAF. Map tiles by Carto, under CC BY 3.0. Data by OpenStreetMap, under ODbL.")
dev.off()
dev.off()

# carte des EPN

epn <- read_csv("./data/espace-public-numerique-epn.csv")

epn <- epn %>% 
  select(Identifiant, Nom, adresse = `N° et libellé de voie`, CodeInsee = `Code Insee commune`) %>% 
  ban_geocode(adresses = adresse, code_insee = "CodeInsee") %>% 
  filter(!is.na(latitude))

coordinates(epn) <- c("longitude", "latitude")
epn@proj4string <- CRS("+init=epsg:4326")

fonds <- read_osm(bb(stbb_tmapbb(stbb), ext = 1.5), type = "https://cartodb-basemaps-a.global.ssl.fastly.net/dark_all/{z}/{x}/{y}.png", minNumTiles = 36)

png("./livrables/epn.png", width = 16.54, height = 23.39, units = "in", res = 300)
pdf("./livrables/epn.pdf", width = 16.54, height = 23.39)
tm_shape(fonds) +
  tm_raster() +
tm_shape(epn) +
  tm_squares(col = "blue") +
  tm_text(text = "Nom", col = "white", just = c("left", "bottom")) +
tm_shape(iris13) +
  tm_borders(col = "lightgrey") +
tm_shape(chevaleret) +
  tm_borders(col = "lightgrey", lwd = 2, lty = 5) +
tm_layout(title = "Les EPN dans le 13e", scale = 1.5, title.color = "light grey", attr.outside.position = "bottom", attr.outside = TRUE, title.size = 2) +
  tm_credits("Source : NetPublic. Réalisation : École des données/OKF pour la CAF.\nMap tiles by Carto, under CC BY 3.0. Data by OpenStreetMap, under ODbL.")
dev.off()
dev.off()
