#devtools::install_github("rCarto/osrm")
library(osrm)
library(sf)
library(tidyverse)
library(ggmap)
library(ragg)
library(extrafont)
library(ggtext)

car <- osrmIsochrone(loc = c(-1.545682, 53.363896), breaks = c(30),
                     returnclass="sf", res=100, osrm.profile="car")
bike <- osrmIsochrone(loc = c(-1.545682, 53.363896), breaks = c(30),
                     returnclass="sf", res=70, osrm.profile="bike")
foot <- osrmIsochrone(loc = c(-1.545682, 53.363896), breaks = c(30),
                     returnclass="sf", res=70, osrm.profile="foot")

bbox <- c(bottom=53.12, top=53.55, left=-1.95, right=-1.17)

map <- get_stamenmap(bbox, zoom=12, maptype="toner-background")

agg_png("Day22_Boundaries.png", units="in", width=8, height=8, res=500)
  ggmap(map, fill="cornsilk")+
  geom_sf(data=car, aes(geometry=geometry), fill="#2EC4B6", alpha=0.5, inherit.aes=FALSE, colour=NA)+
  geom_sf(data=bike, aes(geometry=geometry), fill="#E71D36", alpha=0.5, inherit.aes=FALSE, colour=NA)+
  geom_sf(data=foot, aes(geometry=geometry), fill="#FF9F1C", alpha=0.5, inherit.aes=FALSE, colour=NA)+
  theme_void()+
  theme(plot.subtitle=element_markdown(),
        text=element_text(family="PT Sans", colour="Grey30"),
        plot.title=element_text(face="bold", size=rel(2.5)))+
  labs(title="Bounded by an hour",
       subtitle="How far I could travel by <span style='color:#2EC4B6;'>car</span>,<span style='color:#E71D36;'> bike</span> or <span style='color:#FF9F1C;'>on foot</span> and still get home within 60 minutes",
       caption="Routing data from OpenStreetMap | Map by @VictimOfMaths\n ")
dev.off()
