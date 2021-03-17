#set-up
library(tidyverse)
library(sf)
library(readxl)
library(tidycensus)
library(tmap)
library(shiny)

setwd("C:/Users/lp2ab/Dropbox/SCU_4th_Year/Capstone_Analysis/CommuteDataLiam/neighborhood_boundaries")
neighborhoods <- st_read("Boundaries SJ_TDM_Parking.shp")

major_roads <- st_read("roads_major.shp")
#major_roads <- filter(major_roads, roadlabel == 101 | roadlabel == 280)
I280 <- filter(major_roads, roadlabel == 280)
US101 <- filter(major_roads, roadlabel == 101)

neighborhoods <- st_transform(neighborhoods, crs = 2227) # project to California State Plane Zone 3
st_is_valid(neighborhoods)
I280 <- st_transform(I280, crs = 2227) # project to California State Plane Zone 3
st_is_valid(I280)
US101 <- st_transform(US101, crs = 2227) # project to California State Plane Zone 3
st_is_valid(US101)

#https://www.sccgov.org/sites/gis/GISData/Pages/Available-GIS-Data.aspx
#https://data.sanjoseca.gov/dataset/089e2c87-a8cc-40fd-9c00-01df94ec503c

##Mapping
tmap_mode("view") #FOR viewing!!!
map_roads1 <- tm_shape(I280) + tm_lines(col = "black", lwd=5, ) + tm_text("roadlabel", size=3, ymod=2, xmod=-7)
map_roads2 <- tm_shape(US101) + tm_lines(col = "black", lwd=5) + tm_text("roadlabel", size=3, xmod=148, ymod=-53)
map_neighborhoods <- tm_shape(neighborhoods) + tm_polygons(col = "MAP_COLORS") + tm_basemap(server = "OpenStreetMap.HOT") + tm_text("name", size=4, xmod=-5)
map_neighborhoods + map_roads1 + map_roads2

