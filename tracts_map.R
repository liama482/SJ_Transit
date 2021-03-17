####SET UP SCRIPT####
library(sf)
library(tidyverse)
library(tmap)
library(sp)
library(tigris)
library(tidycensus)
library(dplyr)
library(tmaptools)

setwd("//samba1.engr.scu.edu/cwuethri/dcengr/Desktop/Capstone_Analysis/ENVSJustice")
census_api_key("2261de883be84ef5125887716e3c8ff1dd812446")


#####LOAD ALL DATA####
v19 <- load_variables(2019, "acs5", cache = TRUE)
RPbound<- st_read("RPark_bound/RPark_boundary.shp")
SKbound <- st_read("SPKeys_bound/SPKeys_boundary.shp")
tracts<- tracts("CA", "Santa Clara")
citylim<- st_read("City_Limits/City_Limits.shp")
CES<- st_read("CESJune2018Update_SHP/CES3June2018Update.shp")

income <- get_acs(geography = "tract",
                  year = 2019,
                  table = "B19013",
                  cache_table = TRUE,
                  state = "CA",
                  county = "Santa Clara",
                  output = "wide",
                  geometry = TRUE) 

race<- get_acs(geography = "tract",
               year = 2019,
               table = "B02001",
               cache_table = TRUE,
               state = "CA",
               county = "Santa Clara",
               output = "wide",
               geometry = TRUE)


####TIDY DATA####
SKtracts<- filter(tracts, NAME== "5016" |
                    NAME== "5031.12")

RPtracts <- filter(tracts, NAME== "5014.01" |
                     NAME== "5014.02")
CES<- select(CES, 1, 6:7, 9)

income<- select(income, 3)

race <- select(race, 1:3,7,9,11,13) %>%
  `colnames<-`(c("GEOID", "NAME", "Tot", "Blck", "Ind", "As", "Nat", "geometry"))%>%
  mutate(pct = ((Blck + Ind + As + Nat)/ Tot)*100)

####PROJECT####
SKbound <- st_transform(SKbound, crs = 2227)
RPbound <- st_transform(RPbound, crs = 2227)
tracts <- st_transform(tracts, crs= 2227)
citylim<-st_transform(citylim, crs = 2227)
income<- st_transform(income, crs= 2227)
race <-  st_transform(race, crs= 2227)
CES <-  st_transform(CES, crs= 2227)

#####CLIP DATA TO CITY BOUNDS#######
income <- st_intersection(income, citylim)
CES<- st_intersection(CES, citylim)
race<- st_intersection(race, citylim)



race<-st_as_sf(race, wkt = "geometry")
CES<-st_as_sf(CES, wkt = "geometry")
income<-st_as_sf(income, wkt = "geometry")

st_write(race, "race.shp")
st_write(CES, "CES.shp")
st_write(income, "income.shp")
st_write(SKtracts, "SKtracts.shp")
st_write(RPtracts, "RPtracts.shp")

#####MAPPING#####
tmap_mode("view")
neighborhoods_map <- 
  tm_shape(RPbound) + tm_polygons(col = "black", alpha = 0.5, id = "name") +
  tm_shape(SKbound) + tm_polygons(col = "black", alpha = 0.5, id = "name") +  
  tm_basemap(server = "CartoDB.Positron")
neighborhoods_map

income_map<- tm_shape(income) + tm_fill(legend.format=list(fun=function(x) paste0(formatC(x, digits=0, format="f"), "$")), alpha = .7, col="B19013_001E", id = "B19013_001E", title = "Median Income") +
  tm_shape(RPtracts) + tm_borders(col = "black") +
  tm_shape(SKtracts) + tm_borders(col = "black") + 
  tm_compass()+
  tm_basemap(server = "CartoDB.Positron")
income_map

pollution_map<- tm_shape(CES) + tm_fill(legend.format=list(fun=function(x) paste0(formatC(x, digits=0, format="f"), "%")),alpha = .7, col="CIscoreP", id = "CIscoreP", n=10, title = "Pollution Burden Percentile") +
  tm_shape(RPtracts) + tm_borders(col = "black") +
  tm_shape(SKtracts) + tm_borders(col = "black") + 
  tm_compass()+
  tm_basemap(server = "CartoDB.Positron")
pollution_map

race_map<- tm_shape(race) + tm_fill(legend.format=list(fun=function(x) paste0(formatC(x, digits=0, format="f"), "%")), n = 10, alpha = .7, col="pct", id = "pct", title = "Percentage of Non-White Residents") +
  tm_shape(RPtracts) + tm_borders(col = "black") +
  tm_shape(SKtracts) + tm_borders(col = "black") + 
  tm_compass()+
  tm_basemap(server = "CartoDB.Positron")
race_map

