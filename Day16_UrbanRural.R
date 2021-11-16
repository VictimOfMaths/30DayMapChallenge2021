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
