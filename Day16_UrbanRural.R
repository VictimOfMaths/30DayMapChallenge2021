rm(list=ls())

library(curl)
library(tidyverse)
library(sf)
library(paletteer)
library(ggtext)
library(extrafont)
library(ragg)

#Download building age data from VOA via data.london.gov.uk
#https://data.london.gov.uk/dataset/property-build-period-lsoa
temp <- tempfile()
url <- "https://data.london.gov.uk/download/property-build-period-lsoa/d022a431-1687-422e-ae53-fca9ec221c45/dwelling-period-built-2014-lsoa.csv"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

rawdata <- read.csv(temp) %>% 
  #Filter MSOA level data
  filter(GEOG=="MSOA") %>%
  select(-`UNKNOWN`) %>% 
  gather(age, count, c(4:15)) %>% 
  mutate(count=as.numeric(count), 
         count=if_else(is.na(count), 0, count))

data <- rawdata %>% 
  merge(rawdata %>% 
          group_by(lsoa) %>% 
          summarise(total=sum(count))) %>% 
  mutate(ageprop=count/total) %>% 
  group_by(lsoa) %>% 
  mutate(maxageprop=max(ageprop)) %>% 
  ungroup() %>% 
  filter(ageprop==maxageprop) %>% 
  mutate(age=gsub("X", "", age),
         age=gsub("_", "-", age),
         age=factor(age, levels=c("Pre-1900", "1900-1918", "1919-1929", "1930-1939", "1945-1954",
                                  "1955-1964", "1965-1972", "1973-1982", "1983-1992", "1993-1999",
                                  "2000-2009", "2010-2014")))

#Download Carl Baker's lovely cartogram
msoa <- tempfile()
source <- ("https://github.com/houseofcommonslibrary/uk-hex-cartograms-noncontiguous/raw/main/geopackages/MSOA.gpkg")
msoa <- curl_download(url=source, destfile=msoa, quiet=FALSE, mode="wb")

BackgroundMSOA <- st_read(msoa, layer="5 Background")

MSOA <- st_read(msoa, layer="4 MSOA hex") %>% 
  left_join(data, by=c("msoa11cd"="lsoa"))

GroupsMSOA <- st_read(msoa, layer="2 Groups")

Group_labelsMSOA <- st_read(msoa, layer="1 Group labels") %>% 
  mutate(just=if_else(LabelPosit=="Left", 0, 1))

LAsMSOA <- st_read(msoa, layer="3 Local authority outlines (2019)")

plot <- ggplot()+
  geom_sf(data=BackgroundMSOA, aes(geometry=geom), fill="#9ecae1")+
  geom_sf(data=MSOA %>% filter(RegionNation!="Wales"), 
          aes(geometry=geom, fill=age), colour=NA)+
  geom_sf(data=LAsMSOA %>% filter(RegionNation!="Wales"), 
          aes(geometry=geom), fill=NA, colour="White", size=0.1)+
  geom_sf(data=GroupsMSOA %>% filter(RegionNation!="Wales"), 
          aes(geometry=geom), fill=NA, colour="Black")+
  geom_sf_text(data=Group_labelsMSOA %>% filter(RegionNation!="Wales"), 
               aes(geometry=geom, label=Group.labe,
                   hjust=just), size=rel(2.8), colour="Navy", family="Roboto")+
  scale_fill_paletteer_d("colorBlindness::Blue2Orange12Steps", name="Most common\nbuild date")+
  theme_void()+
  theme(text=element_text(family="Roboto", colour="Navy"), 
        plot.title=element_text(size=rel(2.5), family="Merriweather"),
        plot.title.position="plot", plot.caption.position="plot",
        plot.background=element_rect(fill="#deebf7", colour="#deebf7"))+
  labs(title="Where are the old houses?",
       subtitle="Most common construction date recorded for dwellings in each neighborhood in England,\nexcluding those with unknown build dates.",       
       caption="Data from Valuation Office Agency, Cartogram from @carlbaker/House of Commons Library\nPlot by @VictimOfMaths")

agg_png("Day16_UrbanRural.png", units="in", width=7.5, height=8.5, res=800, background="#deebf7")
plot
dev.off()

################
#Bonus LSOA analysis
lsoadataraw <- read.csv(temp) %>% 
  #Filter MSOA level data
  filter(GEOG=="LSOA") %>%
  select(-`UNKNOWN`) %>% 
  gather(age, count, c(4:15)) %>% 
  mutate(count=as.numeric(count), 
         count=if_else(is.na(count), 0, count))

lsoadata <- lsoadataraw %>% 
  merge(lsoadataraw %>% 
          group_by(lsoa) %>% 
          summarise(total=sum(count))) %>% 
  mutate(ageprop=count/total) %>% 
  group_by(lsoa) %>% 
  mutate(maxageprop=max(ageprop)) %>% 
  ungroup() %>% 
  filter(ageprop==maxageprop) %>% 
  mutate(age=gsub("X", "", age),
         age=gsub("_", "-", age),
         age=factor(age, levels=c("Pre-1900", "1900-1918", "1919-1929", "1930-1939", "1945-1954",
                                  "1955-1964", "1965-1972", "1973-1982", "1983-1992", "1993-1999",
                                  "2000-2009", "2010-2014")))


lsoaurl <- "https://opendata.arcgis.com/api/v3/datasets/1f23484eafea45f98485ef816e4fee2d_0/downloads/data?format=shp&spatialRefId=27700"
temp <- tempfile()
temp2 <- tempfile()
temp <- curl_download(url=lsoaurl, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

#The actual shapefile has a different name each time you download it, so need to fish the name out of the unzipped file
name <- list.files(temp2, pattern=".shp")
shapefile <- st_read(file.path(temp2, name))

mapdata <- left_join(shapefile, lsoadata, by=c("LSOA11CD"="lsoa"))

agg_png("Day16_UrbanRuralv2LSOA.png", units="in", width=7.5, height=8.2, res=2000, background="White")
ggplot(mapdata %>% filter(!is.na(age)), aes(geometry=geometry, fill=age))+
  geom_sf(colour=NA)+
  scale_fill_paletteer_d("colorBlindness::Blue2Orange12Steps", name="Modal\nbuild date")+
  theme_void()+
  theme(text=element_text(family="Playfair Display"),
        plot.title=element_text(face="bold", size=rel(1.5)))+
  labs(title="Where are the old houses?",
       subtitle="Most common build age for dwellings in Lower Super Output Areas in England & Wales",
       caption="Data from Valuation Office Agency | Plot by @VictimOfMaths")

dev.off()

#Download M25 boundary
#http://opendata.fusiondatascience.com/2019/02/16/motorway-boundary-files/
M25url <- "http://opendata.fusiondatascience.com/downloads/M25_Polygon.zip"
temp <- tempfile()
temp2 <- tempfile()
temp <- curl_download(url=M25url, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

name <- list.files(temp2, pattern=".shp")
M25 <- st_read(file.path(temp2, name))

Londonmap <- st_filter(mapdata, M25 )

agg_png("Day16_UrbanRuralv3London.png", units="in", width=8, height=6, res=800, background="White")
ggplot()+
  geom_sf(data=mapdata %>% filter(!is.na(age)), aes(geometry=geometry, fill=age), colour=NA)+
  geom_sf(data=M25, aes(geometry=geometry), fill=NA, colour="Black")+
  scale_fill_paletteer_d("colorBlindness::Blue2Orange12Steps", name="Modal\nbuild date")+
  xlim(c(500000, 560000))+
  ylim(c(150000, 205000))+
  theme_void()+
  theme(text=element_text(family="Playfair Display"),
        plot.title=element_text(size=rel(1.9)), 
        plot.title.position="plot", plot.caption.position="plot")+
  labs(title="Where are the old houses?",
       subtitle="Most common build age for dwellings in London. Data at Lower Super Output Area (LSOA) level",
       caption="Data from Valuation Office Agency | Plot by @VictimOfMaths")

dev.off()
