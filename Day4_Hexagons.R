rm(list=ls())

library(curl)
library(readxl)
library(tidyverse)
library(sf)
library(ragg)
library(extrafont)

#Download LSOA to MSOA lookup
temp <- tempfile()
source <- ("https://opendata.arcgis.com/datasets/fe6c55f0924b4734adf1cf7104a0173e_0.csv")
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

lookup <- read.csv(temp) %>% 
  select(LSOA11CD, MSOA11CD) %>% 
  unique()

#Download ONS population estimates at LSOA level (can't seem to find the MSOA ones)
url <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2flowersuperoutputareamidyearpopulationestimates%2fmid2020sape23dt2/sape23dt2mid2020lsoasyoaestimatesunformatted.xlsx"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

popdata <- read_excel(temp, sheet="Mid-2020 Persons", range="A5:CT34758") %>% 
  select(-c(2:7)) %>% 
  rename(LSOA11CD=`LSOA Code`) %>% 
  #Collapse to MSOA level dataset
  merge(lookup) %>% 
  group_by(MSOA11CD) %>% 
  summarise(across(c(2:92), sum)) %>% 
  ungroup() %>% 
  gather(age, pop, c(2:92)) %>% 
  #Calculate medians (approach feels a bit hacky, but I can't think of a better way)
  mutate(age=as.numeric(if_else(age=="90+", "90", age))) %>% 
  group_by(`MSOA11CD`) %>% 
  mutate(weightpop=cumsum(pop)/sum(pop), error=weightpop-0.5) %>% 
  arrange(abs(error)) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(MSOA11CD, age) %>% 
  rename(medianage=age)

#Download Carl Baker's lovely hex cartogram
msoa <- tempfile()
source <- ("https://github.com/houseofcommonslibrary/uk-hex-cartograms-noncontiguous/raw/main/geopackages/MSOA.gpkg")
msoa <- curl_download(url=source, destfile=msoa, quiet=FALSE, mode="wb")

BackgroundMSOA <- st_read(msoa, layer="5 Background")

MSOA <- st_read(msoa, layer="4 MSOA hex") %>% 
  left_join(popdata, by=c("msoa11cd"="MSOA11CD"))

GroupsMSOA <- st_read(msoa, layer="2 Groups")

Group_labelsMSOA <- st_read(msoa, layer="1 Group labels") %>% 
  mutate(just=if_else(LabelPosit=="Left", 0, 1))

LAsMSOA <- st_read(msoa, layer="3 Local authority outlines (2019)")

plotage <- ggplot()+
  geom_sf(data=BackgroundMSOA, aes(geometry=geom), fill="#a1d99b")+
  geom_sf(data=MSOA %>% filter(RegionNation!="Wales"), 
          aes(geometry=geom, fill=medianage), colour=NA)+
  geom_sf(data=LAsMSOA %>% filter(RegionNation!="Wales"), 
          aes(geometry=geom), fill=NA, colour="White", size=0.1)+
  geom_sf(data=GroupsMSOA %>% filter(RegionNation!="Wales"), 
          aes(geometry=geom), fill=NA, colour="Black")+
  geom_sf_text(data=Group_labelsMSOA %>% filter(RegionNation!="Wales"), 
               aes(geometry=geom, label=Group.labe,
                   hjust=just), size=rel(2.8), colour="Black", family="Buxton Sketch")+
  scale_fill_viridis_c(option="rocket",
                         name="Median age")+
  theme_void()+
  theme(text=element_text(family="Buxton Sketch"), plot.title=element_text(size=rel(2.5)),
        plot.title.position="plot", plot.caption.position="plot",
        plot.background=element_rect(fill="#9ecae1", colour="#9ecae1"))+
  labs(title="England's cities are young",
       subtitle="Median age for neighbourhoods (MSOAs) in England based on 2020 population estimates",       
       caption="Data from ONS, Cartogram from @carlbaker/House of Commons Library\nPlot by @VictimOfMaths")

agg_png("Day4_Hexagons.png", units="in", width=7, height=8.5, res=800, background="#9ecae1")
plotage
dev.off()

