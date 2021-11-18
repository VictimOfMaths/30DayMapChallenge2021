rm(list=ls())

library(sf)
library(ragg)
library(extrafont)
library(tidyverse)
library(paletteer)
library(forcats)
library(cowplot)

data1 <- st_read("Data/open-rivers_4241952/oprvrs_gb.gpkg", layer="HydroNode")
data2 <- st_read("Data/open-rivers_4241952/oprvrs_gb.gpkg", layer="WatercourseLink")

mapdata <- data2 %>% 
  mutate(colour=case_when(
    grepl("Red ", watercourseName)==TRUE ~ "Red",
    grepl(" Coch", watercourseName)==TRUE ~ "Red",
    grepl(" Goch", watercourseName)==TRUE ~ "Red",
    grepl(" Dearg", watercourseName)==TRUE ~ "Red",
    grepl(" Dhearg", watercourseName)==TRUE ~ "Red",

    grepl("Brown ", watercourseName)==TRUE ~ "Brown",
    grepl(" Donn", watercourseName)==TRUE ~ "Brown",
    grepl(" Dhonn", watercourseName)==TRUE ~ "Brown",
    
    grepl("Blue ", watercourseName)==TRUE ~ "Blue",
    grepl(" Gorm", watercourseName)==TRUE ~ "Blue",
    grepl(" Ghorm", watercourseName)==TRUE ~ "Blue",
    
    grepl("White ", watercourseName)==TRUE ~ "White",
    grepl(" Geal", watercourseName)==TRUE ~ "White",
    grepl(" Gwyn", watercourseName)==TRUE ~ "White",
    
    grepl("Grey ", watercourseName)==TRUE ~ "Grey",
    grepl(" Glas", watercourseName)==TRUE ~ "Grey",
    grepl(" Ghlas", watercourseName)==TRUE ~ "Grey",
    grepl(" Llwyd", watercourseName)==TRUE ~ "Grey",
    
    grepl("Green ", watercourseName)==TRUE ~ "Green",
    grepl(" Uaine", watercourseName)==TRUE ~ "Green",
    grepl(" Gwyrdd", watercourseName)==TRUE ~ "Green",
    
    grepl("Yellow ", watercourseName)==TRUE ~ "Yellow",
    grepl(" Buidhe", watercourseName)==TRUE ~ "Yellow",
    grepl(" Bhuidhe", watercourseName)==TRUE ~ "Yellow",
    grepl(" Melyn", watercourseName)==TRUE ~ "Yellow",
    grepl(" Felyn", watercourseName)==TRUE ~ "Yellow",
    
    grepl("Black ", watercourseName)==TRUE ~ "Black",
    grepl(" Dubh", watercourseName)==TRUE ~ "Black",
    grepl(" Dhubh", watercourseName)==TRUE ~ "Black",
    grepl(" Du", watercourseName)==TRUE ~ "Black",
    grepl(" Ddu", watercourseName)==TRUE ~ "Black",
    
    TRUE ~ NA_character_
  ))

table(mapdata$colour)

test <- data1 %>% 
  st_join(mapdata, by="id")

map <- ggplot(test %>% filter(!is.na(colour)), aes(geometry=geom, colour=colour, fill=colour))+
  geom_sf(size=1, alpha=0.7)+
  theme_void()+
  xlim(-200000, 800000)+
  ylim(50000,1200000)+
  scale_fill_identity()+
  scale_colour_identity()+
  theme(text=element_text(family="Noto Sans", colour="cornsilk"), plot.title.position="plot",
        plot.caption.position="plot", plot.title=element_text(face="bold", size=rel(2.6)),
        plot.background=element_rect(fill="Grey40", colour="Grey40"))+
  labs(title="Coloured water",
       subtitle="Water features with colours in their names, coloured accordingly",
       caption="Data from OS Open Rivers | Plot by @VictimOfMaths\n\nContains Ordnance Survey data © Crown copyright and database right 2020\n")


bars <- as.data.frame(table(mapdata$colour)) %>% 
  ggplot(aes(y=fct_reorder(Var1, Freq), x=Freq, fill=Var1))+
  geom_col(show.legend=FALSE)+
  scale_x_continuous(name="")+
  scale_y_discrete(name="")+
  scale_fill_identity()+
  theme_classic()+
  theme(text=element_text(family="Noto Sans"),
        plot.background=element_rect(fill="Grey40", colour="Grey40"), 
        panel.background=element_rect(fill="Grey40", colour="Grey40"),
        axis.text=element_text(colour="cornsilk"),
        plot.title=element_text(colour="cornsilk"))+
  labs(title="Frequency")

agg_png("Day18_Water.png", units="in", width=6.5, height=8, res=800, background="Grey40")
ggdraw(map+draw_plot(bars, -320000, 80000, 400000, 500000))
dev.off()
