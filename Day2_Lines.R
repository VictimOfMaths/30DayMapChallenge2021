rm(list=ls())

library(sf)
library(ragg)
library(extrafont)
library(ggfx)
library(tidyverse)
library(paletteer)

data1 <- st_read("Data/open-rivers_4241952/oprvrs_gb.gpkg", layer="HydroNode")
data2 <- st_read("Data/open-rivers_4241952/oprvrs_gb.gpkg", layer="WatercourseLink")


mapdata <- data2 %>% 
  filter(!form %in% c("lake", "canal")) %>% 
  mutate(type=case_when(
    grepl("River", watercourseName)==TRUE ~ "River",
    grepl("Burn", watercourseName)==TRUE ~ "Burn",
    grepl("Water", watercourseName)==TRUE ~ "Water",
    grepl("Beck", watercourseName)==TRUE ~ "Beck",
    grepl("Brook", watercourseName)==TRUE ~ "Brook",
    grepl("Afon", watercourseName)==TRUE ~ "Afon",
    grepl("Abhainn", watercourseName)==TRUE ~ "Abhainn",
    grepl("Allt", watercourseName)==TRUE ~ "Allt",
    grepl("Nant", watercourseName)==TRUE ~ "Nant",
    grepl("Drain", watercourseName)==TRUE ~ "Drain",
    
    TRUE ~ NA_character_
  ))

agg_tiff("Day2_Lines.tiff", units="in", width=7.5, height=8, res=800, background="cornsilk")
ggplot(mapdata %>% filter(!is.na(type)), aes(geometry=geom, colour=type, fill=type))+
  geom_sf(size=0.2)+
  theme_void()+
  xlim(-200000, 800000)+
  ylim(50000,1200000)+
  scale_colour_manual(name="", values=c("#3366cc", "#dc3912", "#ff9900", "#109618", "#990099",
                                        "#0099c6", "#dd4477", "#66aa00","Grey50", "#b82e2e"))+
  scale_fill_manual(name="", values=c("#3366cc", "#dc3912", "#ff9900", "#109618", "#990099",
                                        "#0099c6", "#dd4477", "#66aa00","Grey50", "#b82e2e"))+
  theme(text=element_text(family="Lobster Two", colour="royalblue4"), plot.title.position="plot",
        plot.caption.position="plot", plot.title=element_text(face="bold", size=rel(2.4)),
        plot.background=element_rect(fill="cornsilk", colour="cornsilk"))+
  labs(title="Rivers flow not past, but through us",
       subtitle="Most common synonyms for 'river' in Great Britain",
       caption="Data from OS Open Rivers | Plot by @VictimOfMaths\n\nContains Ordnance Survey data © Crown copyright and database right 2020\n")
  
dev.off()

#Plot of the missing rivers
ggplot(mapdata %>% filter(is.na(type)), aes(geometry=geom))+
  geom_sf(size=0.1)+
  theme_void()+
  xlim(-200000, 900000)+
  ylim(100000,1300000)

