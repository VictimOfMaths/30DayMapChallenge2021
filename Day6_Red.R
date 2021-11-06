rm(list=ls())

library(sf)
library(ragg)
library(extrafont)
library(ggfx)
library(tidyverse)
library(rnaturalearth)
library(cowplot)
library(magick)

#Load OS' Open Names dataset
#The data is a million nested csv files. Fun times
#Get the list of folders containing the csvs
folders <- list.dirs("Data/open-names_4238365")
#Remove a couple of the folders which have no data in them
folders <- folders[!(folders %in% c("Data/open-names_4238365", "Data/open-names_4238365/docs"))]

#Initialise empty data frame
data <- data.frame(name=character(), type=character(), x=integer(), y=integer())

#Loop over the list of folders
for(folder in folders) {
  #Get a list of the csv files in each folder
  filelist <- paste0(folder, "/", list.files(folder, pattern="*.csv"))
  #Load them all into a list
  temp <- lapply(filelist, function(i){
    read.csv(i, header=FALSE)}) %>% 
    #Collapse them into a single data frame
    bind_rows(.) %>% 
    select(c("V3", "V8", "V9", "V10")) %>% 
    set_names("name", "type", "x", "y")
  
  #Stick them together
  data <- data %>% 
    bind_rows(temp)
}

#Remove postcodes
places <- data %>% 
  filter(type!="Postcode")

red <- places %>% 
  #Filter out the places with red/coch/goch in the name
  mutate(flag=case_when(
    grepl("Red ", name) ~ "Red",
    grepl("Coch", name) | grepl("Goch", name) ~ "Goch/Coch")) %>% 
  filter(!is.na(flag)) %>% 
  #Remove roads named after points
  filter(type!="Named Road") %>% 
  #Convert to an sf object
  st_as_sf(coords=c("x", "y")) %>% 
  st_set_crs(27700)

#Download outline of Wales
map <- ne_countries(scale="large", country="United Kingdom", type="map_units", returnclass="sf") %>% 
  filter(geounit=="Wales") %>% 
  st_transform(27700)

plotmap <- st_filter(red, map)

flag <- image_read("Data/WelshFlag.png") %>%

map <- ggplot()+
  geom_sf(data=map, aes(geometry=geometry), fill="#00AD36")+
  geom_sf(data=plotmap, aes(geometry=geometry), shape=21, fill="#D30731")+
  theme_void()+
  theme(plot.background=element_rect(fill="#deebf7", colour="#deebf7"),
        text=element_text(colour="#D30731", family="Corbel"),
        plot.title=element_text(size=rel(4), family="Castellar"),
        plot.subtitle=element_text(size=rel(1.2)),
        plot.caption=element_text(size=rel(1)),
        plot.caption.position="plot", plot.title.position="plot")+
  labs(title="Y ddraig goch",
       subtitle="\nAll the places in Wales with Red/Coch/Goch in their name",
       caption="Data from OS Open Names | Map by @VictimOfMaths\n\nContains Ordnance Survey data © Crown copyright and database right 2020\n")

agg_tiff("Day6_Red.tiff", units="in", width=7, height=9, res=500, background="#deebf7")
ggdraw()+
  draw_plot(map)+
  draw_image(flag, x=0.1, y=0, width=0.2)
dev.off()
