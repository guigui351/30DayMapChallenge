# Load library
library(tidyverse)
library(sf)
library(ggtext)
library(showtext)

# Add google fonts
font_add_google("Staatliches", "Staatliches")
font_add_google("Bitter", "Bitter")
showtext_opts(dpi = 320)
showtext_auto(enable = TRUE)
theme_set(theme_void())

# Update theme elements for the map
theme_update(
  panel.background = element_rect(fill = "grey20", 
                                  color = "grey20"),
  plot.background = element_rect(fill = "grey20", 
                                 color = "grey20"),
  plot.margin = margin(5, 25, 5, 25),
  plot.title = element_markdown(family = "Staatliches", 
                                color = "#8fb0b0",
                                size = 35,
                                face = "bold",
                                hjust = 0.5,
                                margin = margin(t = 42, b = 12)),
  plot.subtitle = element_markdown(family = "Staatliches", 
                                   color = "grey50",
                                   size = 19,
                                   hjust = 0.5,
                                   margin = margin(t = 0, b = 0))
)

# Read OSM breizh data
sf_breizh_buildings <- sf::read_sf(dsn = here::here("osm_bretagne_shp", "gis_osm_buildings_a_free_1.shp"), 
                               layer = "gis_osm_buildings_a_free_1")

sf_breizh_roads <- sf::read_sf(dsn = here::here("osm_bretagne_shp", "gis_osm_roads_free_1.shp"), 
                            layer = "gis_osm_roads_free_1")

sf_breizh_water <- sf::read_sf(dsn = here::here("osm_bretagne_shp", "gis_osm_water_a_free_1.shp"), 
                            layer = "gis_osm_water_a_free_1")

sf_breizh_natural <- sf::read_sf(dsn = here::here("osm_bretagne_shp", "gis_osm_natural_a_free_1.shp"), 
                               layer = "gis_osm_natural_a_free_1")

sf_breizh_trafic <- sf::read_sf(dsn = here::here("osm_bretagne_shp", "gis_osm_traffic_a_free_1.shp"), 
                                   layer = "gis_osm_traffic_a_free_1")

sf_breizh_landuse <- sf::read_sf(dsn = here::here("osm_bretagne_shp", "gis_osm_landuse_a_free_1.shp"), 
                                layer = "gis_osm_landuse_a_free_1")

sf_breizh_places <- sf::read_sf(dsn = here::here("osm_bretagne_shp", "gis_osm_places_free_1.shp"), 
                                 layer = "gis_osm_places_free_1")

sf_bretagne <- sf::read_sf(dsn = here::here("recensement-de-la-population-en-bretagne-evolution-de-lemploi", "recensement-de-la-population-en-bretagne-evolution-de-lemploi.shp"), 
                               layer = "recensement-de-la-population-en-bretagne-evolution-de-lemploi")

sf_rennes_metropole <- sf_bretagne %>% 
  filter(nom_geo == "Rennes")

# Intersect OSM breizh data with Rennes coordinates
buildings_rennes = st_intersects(x = sf_breizh_buildings, y = sf_rennes_metropole)
sel_logical = lengths(buildings_rennes) > 0
sf_rennes_buildings = sf_breizh_buildings[sel_logical, ]

natural_rennes = st_intersects(x = sf_breizh_natural, y = sf_rennes_metropole)
sel_logical = lengths(natural_rennes) > 0
sf_rennes_natural = sf_breizh_natural[sel_logical, ]

water_rennes = st_intersects(x = sf_breizh_water, y = sf_rennes_metropole)
sel_logical = lengths(water_rennes) > 0
sf_rennes_water = sf_breizh_water[sel_logical, ]

roads_rennes = st_intersects(x = sf_breizh_roads, y = sf_rennes_metropole)
sel_logical = lengths(roads_rennes) > 0
sf_rennes_roads = sf_breizh_roads[sel_logical, ]

trafic_rennes = st_intersects(x = sf_breizh_trafic, y = sf_rennes_metropole)
sel_logical = lengths(trafic_rennes) > 0
sf_rennes_traffic = sf_breizh_trafic[sel_logical, ]

landuse_rennes = st_intersects(x = sf_breizh_landuse, y = sf_rennes_metropole)
sel_logical = lengths(landuse_rennes) > 0
sf_rennes_landuse = sf_breizh_landuse[sel_logical, ]

places_rennes = st_intersects(x = sf_breizh_places, y = sf_rennes_metropole)
sel_logical = lengths(places_rennes) > 0
sf_rennes_places = sf_breizh_places[sel_logical, ]

pois_rennes = st_intersects(x = sf_breizh_pois, y = sf_rennes_metropole)
sel_logical = lengths(pois_rennes) > 0
sf_rennes_pois = sf_breizh_pois[sel_logical, ]

# Selection of interest 
sf_rennes_landuse <- sf_rennes_landuse %>% 
  filter(fclass %in% c("park") & !is.na(name) & name != "Parc de Saint-Jacques-de-la-Lande")	

sf_rennes_roads_c <- 
  sf_rennes_roads %>% 
  filter(!fclass %in% c("bridleway", "footway", "path", "pedestrian", "steps")) %>% 
  mutate(class = if_else(fclass == "cycleway", "bike", "car")) 

# chose some places in Rennes to annotate
sf_rennes_places_sel <- sf_rennes_places %>% 
  filter(name %in% c("Beaulieu", "Champ de Mars", "Thabor - Paris", "Les Longs Champs", "Centre-Ville",
                     "Les Gayeulles", "Arsenal - Redon", "Villejean", "Saint-Martin", "Cleunay", "Bréquigny", "Les Champs Manceaux", "Jeanne d'Arc", 
                     "Cimetière de l'Est", "La Poterie", "Dinan - Saint-Malo"))

# Define lat and long for plot limits
lat <-c(48.1545, 48.0684)
long <-c(-1.7614, -1.6024)

# create sf map object
map <- ggplot() + 
  ## Rennes base map
  geom_sf(data = sf_rennes_metropole,
          fill = "grey30",
          color = "grey10",
          size = 0.15) +
  ## water
  geom_sf(data = sf_rennes_water,
          fill = "#ccddef",  
          color = "#ccddef",
          size = 0.2) +
  ## Buildings in rennes
  geom_sf(data = sf_rennes_buildings,
          fill = colorspace::darken("#2c6350", 0.8),  
          color = colorspace::darken("#2c6350", 0.8),
          size = 0.15) +
  ## Parks in rennes
  geom_sf(data = sf_rennes_landuse,
          fill = colorspace::lighten("#417026", 0.4),  
          color = colorspace::lighten("#417026", 0.4)) +
  ## roads designated for cars and bike
  geom_sf(data = sf_rennes_roads, 
          color = "#c8d8d8", size = 0.11) +
  # Annotate some important places in text
  geom_sf_text(data = sf_rennes_places_sel %>%  filter(name != "Centre-Ville"), 
               aes(label = name), 
               size=3, 
               fontface = "italic",
               color = "white",
               family="Bitter") +
  geom_sf_text(data = sf_rennes_places_sel %>%  filter(name == "Centre-Ville"), 
               aes(label = name), 
               size=4.5, 
               fontface = "italic",
               color = "white",
               family="Bitter") +
  coord_sf(xlim=long, ylim=lat) +
  # Captin / title
  annotate("text", x = -1.6824, y = 48.0712, 
           hjust = 0.4, vjust = 1,
           label = "Visualization by Guillaume Abgrall  •  Data by OpenStreetMap",
           family = "Staatliches", size = 4, color = "#8fb0b0") + 
  labs(title = "Moving through <span style='font-size:45pt'>Rennes</span> within <b style='color:#7EAB6B'>Parks</b> and <b style='color:#2c6350'>Buildings</b>")


# Save plot
ragg::agg_png(here::here(paste0("Day1", ".png")), res = 320, width = 16, height = 10, units = "in")
map
dev.off()
