rm(list=ls())

library(sf)
library(ragg)
library(extrafont)
library(tidyverse)
library(osmdata)
library(ggtext)
library(ggmap)

#Get data, queries built with http://overpass-turbo.eu/
SheffieldLightsq <- getbb("Sheffield") %>% 
  opq(timeout=25*100) %>% 
  add_osm_feature("highway", "traffic_signals") 

SheffieldLights <- osmdata_sf(SheffieldLightsq)

mapdata <- SheffieldLights$osm_points %>% 
  mutate(rand=runif(nrow(.)),
         colour=case_when(
           rand<0.2 ~ "#cc3232",
           rand<0.3 ~ "#e7b416",
           TRUE ~ "#2dc937"))

Sheffmap <- get_map(getbb("Sheffield"), maptype="toner-background")

agg_png("Day7_Green.png", units="in", width=9, height=7, res=700, background="Grey10")
ggmap(Sheffmap)+
  geom_sf(data=mapdata, inherit.aes=FALSE, aes(fill=colour), 
          shape=23, alpha=0.6, colour="White")+
  scale_fill_identity()+
  theme_void()+
  theme(text=element_text(family="Lobster", colour="antiquewhite"), 
        plot.title=element_markdown(size=rel(4)),
        plot.title.position="plot",
        plot.background=element_rect(fill="Grey10", colour="Grey10"),
        plot.subtitle=element_text(size=rel(1.5)), 
        plot.caption=element_text(size=rel(1.2)))+
  labs(title="Pray for <span style='color:#2dc937;'>green",
       subtitle="All of the traffic lights in Sheffield",
       caption="Data from OpenStreetMap | Map by @VictimOfMaths")
dev.off()

