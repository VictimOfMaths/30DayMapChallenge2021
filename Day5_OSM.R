rm(list=ls())

library(sf)
library(ragg)
library(extrafont)
library(tidyverse)
library(osmdata)
library(ggtext)

#Get data, queries built with http://overpass-turbo.eu/
PretGBq <- getbb("Great Britain") %>% 
  opq(timeout=25*100) %>% 
  add_osm_feature("brand:wikidata", "Q2109109") 

PretNIq <- getbb("Northern Ireland") %>% 
  opq(timeout=25*100) %>% 
  add_osm_feature("brand:wikidata", "Q2109109") 
  
GreggsGBq <- getbb("Great Britain") %>% 
  opq(timeout=25*100) %>% 
  add_osm_feature("brand:wikidata", "Q3403981")

GreggsNIq <- getbb("Northern Ireland") %>% 
  opq(timeout=25*100) %>% 
  add_osm_feature("brand:wikidata", "Q3403981")

PretGB <- osmdata_sf(PretGBq)
PretNI <- osmdata_sf(PretNIq)
GreggsGB <- osmdata_sf(GreggsGBq)
GreggsNI <- osmdata_sf(GreggsNIq)

agg_png("Day5_OSM.png", units="in", width=8, height=9, res=700)
ggplot()+
  geom_sf(data=GreggsGB$osm_points, aes(geometry=geometry), fill="#2184b4", shape=21, alpha=0.5)+
  geom_sf(data=GreggsNI$osm_points, aes(geometry=geometry), fill="#2184b4", shape=21, alpha=0.5)+
  geom_sf(data=PretGB$osm_points, aes(geometry=geometry), fill="#94042c", shape=21, alpha=0.5)+
  geom_sf(data=PretNI$osm_points, aes(geometry=geometry), fill="#94042c", shape=21, alpha=0.5)+
  xlim(-8, 3)+
  theme_void()+
  theme(text=element_text(family="Segoe UI", colour="Grey30", face="bold"),
        plot.subtitle=element_markdown(size=rel(1.5)), plot.title=element_text(size=rel(3)))+
  labs(title="No escape from the steak bake",
       subtitle="Location of <span style='color:#2184b4;'>Greggs</span> and <span style='color:#94042c;'>Pret a Manger</span> outlets in the UK",
       caption="Data from OpenStreeMap | Plot by @VictimOfMaths")

dev.off()