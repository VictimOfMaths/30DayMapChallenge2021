rm(list=ls())

library(rnaturalearth)
library(sf)
library(ragg)
library(extrafont)
library(tidyverse)
library(lwgeom)

data <- ne_countries(scale=10, returnclass="sf") %>% 
  filter(grepl("land", name)) %>% 
  st_transform("+proj=moll")

sphere <- st_graticule(ndiscr = 100000, margin = 10e-6) %>%
  st_transform("+proj=moll") %>%
  st_convex_hull() %>%
  summarise(geometry = st_union(geometry))

agg_png("Outputs/Day17_Land.png", units="in", width=9, height=6, res=800, background="midnightblue")
  ggplot()+
  geom_sf(data=sphere, aes(geometry=geometry), fill="SkyBlue", colour="SkyBlue")+
  geom_sf(data=data, aes(geometry=geometry), fill="forestgreen", colour="forestgreen")+
  theme_void()+
  theme(plot.background=element_rect(fill="Midnightblue", colour="Midnightblue"),
        panel.background=element_rect(fill="Midnightblue", colour="Midnightblue"),
        text=element_text(family="Estonia", colour="antiquewhite"),
        plot.title=element_text(size=rel(5)),
        plot.subtitle=element_text(size=rel(2)),
        plot.caption=element_text(size=rel(1.5)))+
  labs(title="The Land Islands",
       subtitle="Every country in the worldwith 'Land' in its name",
       caption="Data from Natural Earth | Map by @VictimOfMaths")
  dev.off()
  