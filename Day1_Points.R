rm(list=ls())

library(tidyverse)
library(sf)
library(ragg)
library(extrafont)
library(ggfx)

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

#Separate out postcodes
postcodes <- data %>% 
  filter(type=="Postcode")

#And everything else
places <- data %>% 
  filter(type!="Postcode")

points <- places %>% 
  #Filter out the Points
  filter(grepl("Point", name)) %>% 
  #Remove roads named after points
  filter(type!="Named Road") %>% 
  #Convert to an sf object
  st_as_sf(coords=c("x", "y"))

agg_png("Day1_Points.png", units="in", width=7, height=9, res=500, background="midnightblue")
ggplot(points, aes(geometry=geometry))+
  with_blur(geom_sf(colour="antiquewhite"), sigma=unit(1, "mm"))+
  geom_sf(size=rel(0.03), colour="antiquewhite")+
  theme_void()+
  theme(plot.background=element_rect(fill="midnightblue", colour="midnightblue"),
        text=element_text(colour="antiquewhite", family="Zen Kurenaido"),
        plot.title=element_text(size=rel(3)),
        plot.subtitle=element_text(size=rel(1.2)),
        plot.caption=element_text(size=rel(1)),
        plot.caption.position="plot", plot.title.position="plot")+
  labs(title="Where's the point?",
       subtitle="All the places in Great Britain with Point in the name",
       caption="Data from OS Open Names | Map by @VictimOfMaths\n\nContains Ordnance Survey data © Crown copyright and database right 2020\n")

dev.off()
