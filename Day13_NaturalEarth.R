rm(list=ls())

library(tidyverse)
library(rnaturalearth)
library(sf)
library(ggfx)
library(ragg)

islands <- ne_download(scale=10, type="minor_islands", category="physical", returnclass = "sf")
outlines <- ne_download(scale=10, type="countries", returnclass="sf")
lakes <- ne_download(scale=10, type="lakes", category="physical", returnclass="sf")

agg_png("Day13_NaturalEarth.png", units="in", width=6.2, height=8.2, res=1000, background="navy")
ggplot()+
  geom_sf(data=outlines, aes(geometry=geometry), fill=NA, colour="White", size=0.1)+
  geom_sf(data=islands, aes(geometry=geometry), colour="Red", fill="Red", size=1)+
  geom_sf(data=lakes, aes(geometry=geometry), colour="Royalblue", fill="Royalblue")+
  xlim(c(-9, 32))+
  ylim(c(49, 73))+
  theme_void()+
  theme(plot.background=element_rect(fill="navy",  colour="navy"),
        panel.background=element_rect(fill="navy", colour="navy"),
        text=element_text(colour="AntiqueWhite"),
        plot.title=element_text(size=rel(3.8), family="High Alpine"),
        plot.subtitle=element_text(size=rel(1), family="Belltopo Sans"),
        plot.caption=element_text(size=rel(0.9), family="Belltopo Sans"))+
  labs(title="The tiny islands of Northern Europe",
       subtitle="Minor islands with a surface area of less than 2 square kilometers",
       caption="Data from Natural Earth | Fonts from @sarahbellmaps | Map by @VictimOfMaths")
dev.off()
