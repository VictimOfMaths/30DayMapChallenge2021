rm(list=ls())

library(tidyverse)
library(curl)
library(readxl)
library(sf)
library(extrafont)
library(ragg)
library(ggtext)
library(cowplot)
library(readODS)
library(gtools)

#Read in Scottish DRD data at Council Level 2018-2020
temp <- tempfile()
source <- "https://www.nrscotland.gov.uk/files//statistics/drug-related-deaths/20/drug-related-deaths-20-tabs-figs.xlsx"
rawdata <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

DRD.s <- read_excel(rawdata, sheet="C1 - summary", range="A10:L41", col_names=FALSE) %>% 
  select(`...1`, `...10`, `...11`, `...12`) %>% 
  rename(LA=`...1`) %>% 
  mutate(DRD=(`...10`+`...11`+`...12`)/3) %>% 
  select(LA, DRD)

#Read in Scottish ASD data at Council Level 2017-2019
temp <- tempfile()
source <- "https://www.nrscotland.gov.uk/files//statistics/alcohol-deaths/2020/alcohol-specific-deaths-20-all-tabs.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

ASD.s <- as.data.frame(t(read_excel(temp, sheet="Table 7B", range=c("C46:AH48"), col_names=FALSE))) %>% 
  mutate(ASD=(V1+V2+V3)/3) %>% 
  select(ASD)

#The columes in the ASD data match the DRD data, so don't bother faffing about with names
DRDASD.s <- cbind(DRD.s, ASD.s)

#Read in English & Welsh data at LTLA level
#DRDs 2018-20
temp <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fdrugmisusedeathsbylocalauthority%2fcurrent/2020localauthorities.xls"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

DRD.ew <- read_excel(temp, sheet="Table 6", range="A7:E438", col_names=FALSE) %>% 
  mutate(LA=coalesce(`...3`, `...4`)) %>% 
  select(`...1`, `...5`, LA) %>% 
  rename(code=`...1`, DRD=`...5`) %>%
  #adjust for the fact that the number of deaths is ths cumulative 3 year total
  mutate(DRD=DRD/3) %>% 
  #fix names that don't align with ASD data
  mutate(LA=case_when(
    LA=="Kingston upon Hull, City of" ~ "Kingston upon Hull",
    LA=="Herefordshire, County of" ~ "Herefordshire",
    LA=="Bristol, City of" ~ "Bristol",
    TRUE ~ as.character(LA)),
    code=if_else(LA=="Buckinghamshire", "E10000002", as.character(code)),
    #Tidy up Welsh LA names
    LA=if_else(substr(code, 1, 1)=="W", substr(LA, 1, regexpr("/", LA)-2), as.character(LA))) %>% 
  mutate(LA=case_when(
    LA %in% c("South Northamptonshire", "North Northamptonshire") ~ "Northamptonshire",
    TRUE ~ LA),
    code=case_when(
      code %in% c("E06000061", "E06000062") ~ "E10000021",
      TRUE ~ code)) %>% 
  group_by(LA) %>% 
  summarise(DRD=sum(DRD), code=unique(code)) %>% 
  ungroup()

#ASDs for England 2017-19
temp <- tempfile()
source <- "https://fingertipsws.phe.org.uk/api/all_data/csv/by_profile_id?parent_area_code=E92000001&parent_area_type_id=6&child_area_type_id=102&profile_id=87&category_area_code="
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

ASD.e <- read.csv(temp) %>% 
  filter(Indicator.ID=="91380" & Sex=="Persons" & Area.Type=="Counties & UAs (pre Apr 2019)") %>% 
  select(Area.Code, Area.Name, Value, Time.period) %>% 
  rename(code=Area.Code, LA=Area.Name, rate=Value)

#Bring in population (based on 2017 data to cope with Dorset)
temp <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2fpopulationestimatesforukenglandandwalesscotlandandnorthernireland%2fmid2017/ukmidyearestimates2017finalversion.xls"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

LApop <- read_excel(temp, sheet="MYE2 - All", range="A6:D445", col_names=FALSE) %>% 
  select(-`...3`) %>% 
  rename(code=`...1`, LA=`...2`, pop=`...4`)

ASD.e <- merge(ASD.e, LApop, by="code", all.x=TRUE) %>% 
  select(-LA.y) %>% 
  rename(LA=LA.x) %>% 
  mutate(ASD=rate*pop/100000)

#Faff about with Dorset & Bournemouth, which are missing from the latest data
temp <- subset(ASD.e, code %in% c("E06000028", "E06000029", "E10000009") & 
                 Time.period=="2015 - 17") %>% 
  mutate(code=case_when(
    code %in% c("E06000028", "E06000029") ~ "E06000058",
    TRUE ~ "E06000059"), 
    LA=case_when(
      code=="E06000058" ~ "Bournemouth, Christchurch and Poole",
      TRUE ~ "Dorset"))

ASD.e <- ASD.e %>% 
  filter(Time.period=="2017 - 19") %>% 
  bind_rows(temp) %>% 
  select(-Time.period) %>% 
  group_by(code, LA) %>% 
  summarise(ASD=sum(ASD)) %>% 
  ungroup()

#ASDs for Wales 2017-19
#Because Wales like to be awkward, I had to manually download this data from here:
#https://www.healthmapswales.wales.nhs.uk/data-catalog-explorer/indicator/I1327?geoId=G108&view=table

ASD.w <- read.csv("Data/Wales ASD Data.csv") %>%
  select(c(1, 11)) %>% 
  set_names("LA", "ASD") %>% 
  mutate(LA=if_else(LA=="The Vale of Glamorgan", "Vale of Glamorgan", as.character(LA)))

DRDASD.ew <- merge(bind_rows(ASD.e, ASD.w), DRD.ew, by="LA", all.x=TRUE) %>% 
  mutate(code=coalesce(code.x, code.y)) %>% 
  select(-code.x, -code.y)

#Read in NI DRD by LA 2017-19
temp <- tempfile()
source <- "https://www.ninis2.nisra.gov.uk/Download/Population/Drug%20Related%20Deaths%20and%20Deaths%20due%20to%20Drug%20Misuse%20(administrative%20geographies).ods"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

DRD.ni <- read_ods(temp, sheet="LGD2014", range="A5:H15", col_names=FALSE) %>% 
  select(-C, -E, -G) %>% 
  mutate(DRD=(D+`F`+H)/3) %>% 
  rename(LA=A, code=B) %>% 
  select(LA, code, DRD)

#Read in NI ASD by LA 2017-19
temp <- tempfile()
source <- "https://www.ninis2.nisra.gov.uk/Download/Population/Alcohol%20Specific%20Deaths%20(administrative%20geographies).ods"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

ASD.ni <- read_ods(temp, sheet="LGD2014", range="A5:E15", col_names=FALSE) %>% 
  mutate(ASD=(C+D+E)/3) %>% 
  rename(LA=A, code=B) %>% 
  select(LA, code, ASD)

DRDASD.ni <- merge(DRD.ni, ASD.ni)

#Bring in populations (2019)
temp <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2fpopulationestimatesforukenglandandwalesscotlandandnorthernireland%2fmid2019april2020localauthoritydistrictcodes/ukmidyearestimates20192020ladcodes.xls"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

LApop2 <- read_excel(temp, sheet="MYE2 - Persons", range="A6:D431", col_names=FALSE) %>% 
  select(-`...3`) %>% 
  rename(code=`...1`, LA=`...2`, pop=`...4`) %>% 
  mutate(code=if_else(code=="E06000060", "E10000002", as.character(code)))

#Scotland
DRDASD.s <- DRDASD.s %>% 
  mutate(LA=str_replace(LA, "&", "and")) %>% 
  merge(LApop2, by="LA", all.x=TRUE)

#NI
DRDASD.ni <- merge(DRDASD.ni, LApop2, all.x=TRUE)

#England
DRDASD.ew <- merge(DRDASD.ew, LApop2, all.x=TRUE, by="code") %>% 
  select(-LA.y) %>% 
  rename(LA=LA.x)

#Merge
DRDASD <- bind_rows(DRDASD.s, DRDASD.ew, DRDASD.ni) %>% 
  gather(cause, deaths, c("DRD", "ASD")) %>% 
  mutate(mortrate=deaths*100000/pop) %>% 
  mutate(country=case_when(
    substr(code, 1, 1)=="E" ~ "England",
    substr(code, 1, 1)=="W" ~ "Wales",
    substr(code, 1, 1)=="S" ~ "Scotland",
    substr(code, 1, 1)=="N" ~ "Northern Ireland"))


#Download shapefile
temp <- tempfile()
temp2 <- tempfile()
source <- "https://opendata.arcgis.com/datasets/43b324dc1da74f418261378a9a73227f_0.zip?outSR=%7B%22latestWkid%22%3A27700%2C%22wkid%22%3A27700%7D"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

#The actual shapefile has a different name each time you download it, so need to fish the name out of the unzipped file
name <- list.files(temp2, pattern=".shp")
shapefile <- st_read(file.path(temp2, name))
names(shapefile)[names(shapefile) == "ctyua19cd"] <- "code"

map.data <- full_join(shapefile, DRDASD, by="code")

bidata <- DRDASD %>% 
  select(LA, code, cause, mortrate, country) %>% 
  spread(cause, mortrate) %>% 
  #generate tertiles
  mutate(alctert=quantcut(ASD, q=3, labels=FALSE),
         drgtert=quantcut(DRD, q=3, labels=FALSE),
         #generate key for colours
         key=case_when(
           alctert==1 & drgtert==1 ~ 1,
           alctert==1 & drgtert==2 ~ 2,
           alctert==1 & drgtert==3 ~ 3,
           alctert==2 & drgtert==1 ~ 4,
           alctert==2 & drgtert==2 ~ 5,
           alctert==2 & drgtert==3 ~ 6,
           alctert==3 & drgtert==1 ~ 7,
           alctert==3 & drgtert==2 ~ 8,
           alctert==3 & drgtert==3 ~ 9),
         #assign colours
         colour=case_when(
           key==1 ~ "#CABED0",
           key==2 ~ "#BC7C5F",
           key==3 ~ "#AE3A4E",
           key==4 ~ "#89A1C8",
           key==5 ~ "#806A8A",
           key==6 ~ "#77324C",
           key==7 ~ "#4885C1",
           key==8 ~ "#435786",
           key==9 ~ "#3f2949"))

#generate dataframe for key
keydata <- bidata %>%
  filter(!is.na(colour)) %>%
  group_by(alctert, drgtert) %>%
  summarise(RGB=unique(colour))

bimap <- full_join(shapefile, bidata, by="code")

labels <- data.frame(x=c(450000, 80000, 180000, 400000), y=c(970000, 290000, 470000, 41000),
                     label=c("<span style='color:#3f2949;'>Purple<span style='color:black;'> areas mean\nhigh rates of alcohol and \nhigh rates of drug deaths",
                             "<span style='color:#4885C1;'>Blue<span style='color:black;'> areas mean\nhigh rates of alcohol and \nlow rates of drug deaths",
                             "<span style='color:#AE3A4E;'>Red<span style='color:black;'> areas mean\nlow rates of alcohol and \nhigh rates of drug deaths",
                             "<span style='color:#CABED0;'>Grey<span style='color:black;'> areas mean\nlow rates of alcohol and \nlow rates of drug deaths"))


BIVAR <- ggplot(bimap)+
  geom_sf(aes(geometry=geometry, fill=colour), colour="white", size=0.01)+
  scale_fill_identity()+ labs(title="Regional patterns in deaths from alcohol and drugs across the UK",
                              subtitle="Comparative rates of alcohol-specific deaths and deaths from drug misuse by Local Authority.\n
                              Data is not available for Rutland.",
                              caption="Data from ONS, NRS, NISRA & OHID | Plot by @VictimOfMaths\nData reflects a 3-year average of the most recently-available figures for each jurisdiction")+
  geom_textbox(data=labels, aes(x=x, y=y, label=label), 
               width=grid::unit(0.2, "npc"), hjust=0, vjust=1, halign=0.5, size=rel(2.7),
               fill="White", colour="White", box.padding = grid::unit(rep(0, 4), "pt"))+
  geom_curve(aes(x=440000, y=955000, xend=220000, yend=850000), curvature=0.15)+
  geom_curve(aes(x=220000, y=280000, xend=393000, yend=290000), curvature=-0.15)+
  geom_curve(aes(x=300000, y=475000, xend=463000, yend=452000), curvature=-0.2)+
  geom_curve(aes(x=420000, y=57000, xend=370000, yend=100000), curvature=0.1)+
  theme_classic()+
  theme(axis.line=element_blank(), axis.ticks=element_blank(), axis.text=element_blank(),
        axis.title=element_blank(), plot.title=element_text(face="bold", size=rel(1.6)),
        text=element_text(family="Lato"))

key <- ggplot(keydata)+
  geom_tile(aes(x=alctert, y=drgtert, fill=RGB))+
  scale_fill_identity()+
  labs(x = expression("More alcohol-specific deaths" %->%  ""),
       y = expression("More drug poisoning deaths" %->%  "")) +
  theme_classic() +
  # make font small enough
  theme(
    axis.title = element_text(size = 9),axis.line=element_blank(), 
    axis.ticks=element_blank(), axis.text=element_blank(),
    text=element_text(family="Lato"))+
  # quadratic tiles
  coord_fixed()

#Add zoomed in areas
#London
London <- ggplot(bimap)+
  geom_sf(aes(geometry=geometry, fill=colour), colour="white", size=0.01)+  
  xlim(500000,560000)+
  ylim(156000,200000)+
  theme_classic()+
  scale_fill_identity()+
  labs(title="Greater London")+
  theme(axis.line=element_blank(), axis.ticks=element_blank(), axis.text=element_blank(),
        axis.title=element_blank(), plot.title=element_text(face="bold"),
        text=element_text(family="Lato"))

#North-West England
NWEng <- ggplot(bimap)+
  geom_sf(aes(geometry=geometry, fill=colour), colour="white", size=0.01)+  
  xlim(310000,440000)+
  ylim(370000,430000)+
  theme_classic()+
  scale_fill_identity()+
  labs(title="NW England")+
  theme(axis.line=element_blank(), axis.ticks=element_blank(), axis.text=element_blank(),
        axis.title=element_blank(), plot.title=element_text(face="bold"),
        text=element_text(family="Lato"))

#Tyne/Tees  
NEEng <- ggplot(bimap)+
  geom_sf(aes(geometry=geometry, fill=colour), colour="white", size=0.01)+  
  xlim(405000,490000)+
  ylim(505000,580000)+
  theme_classic()+
  scale_fill_identity()+
  labs(title="NE England")+
  theme(axis.line=element_blank(), axis.ticks=element_blank(), axis.text=element_blank(),
        axis.title=element_blank(), plot.title=element_text(face="bold"),
        text=element_text(family="Lato"))

#Central Belt
CScot <- ggplot(bimap)+
  geom_sf(aes(geometry=geometry, fill=colour), colour="white", size=0.01)+  
  xlim(220000,341000)+
  ylim(620000,710000)+
  theme_classic()+
  scale_fill_identity()+
  labs(title="Central Scotland")+
  theme(axis.line=element_blank(), axis.ticks=element_blank(), axis.text=element_blank(),
        axis.title=element_blank(), plot.title=element_text(face="bold"),
        text=element_text(family="Lato"))

png("Day26_Choropleth.png", units="in", width=8.5, height=13, res=800)
ggdraw()+
  draw_plot(BIVAR, 0,0,0.75,1)+
  draw_plot(key, 0.03,0.7,0.24,0.24)+
  draw_plot(London, 0.72,0.12,0.27,0.18)+
  draw_plot(NWEng, 0.62,0.34, 0.35, 0.18)+
  draw_plot(NEEng, 0.72, 0.48, 0.2, 0.2)+
  draw_plot(CScot, 0.67, 0.71, 0.3, 0.2)
dev.off()
