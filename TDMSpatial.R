  #####SETUP####
  library(sf)
  library(tidyverse)
  library(tmap)
  library(sp)
  library(tigris)
  library(dplyr)
  library(tidycensus)
  
  setwd("C:/Users/Cameron/Desktop/TDM_Spatial")
  
  ####LOAD DATA####
  census_api_key("2261de883be84ef5125887716e3c8ff1dd812446")
  
  nocar_19 <- get_acs(geography = "tract",
                              year = 2019,
                              table = "B25044",
                              cache_table = TRUE,
                              state = "CA",
                              county = "Santa Clara",
                              output = "wide",
                              geometry = TRUE) 
  nocar_19 <- dplyr::select(nocar_19, 1, 2, 3, 7, 21)
  colnames(nocar_19) <- c("GEOID_TR", "NAME", "HH_19", "NoCar_Own_19", "NoCar_Rent_19", "geometry")
  nocar_19 <- nocar_19 %>%
    dplyr::filter(HH_19 > 0) %>%
    mutate(noCar_pct_19 = ((NoCar_Own_19 + NoCar_Rent_19) / HH_19)*100) %>%
  dplyr::select(2, 6, 7)
  
  
  bart<- st_read("BART_Berryessa_Extension_and_phase_2-shp/BART_Berryessa_Extension_and_phase_2.shp")
  bus<- st_read("Bus_Stop_Inventory/Bus_Stop_Inventory.shp")
  lr<- st_read("Light_Rail_Track_2020-shp/Light_Rail_Track_2020.shp")
  pr<- st_read("VTA_ParkandRide-shp/80dc811e-6120-4ccc-a7a9-61778afdfeb5202048-1-1y1r5n0.xqssi.shp")
  RPbound<- st_read("RPark_bound/RPark_boundary.shp")
  SKbound <- st_read("SPKeys_bound/SPKeys_boundary.shp")
  citylim<- st_read("City_Limits/City_Limits.shp")
  
  
  ###PROJECT DATA####
  SKbound <- st_transform(SKbound, crs = 2227)
  RPbound <- st_transform(RPbound, crs = 2227)
  bart <- st_transform(bart, crs = 2227)
  bus <- st_transform(bus, crs = 2227)
  lr <- st_transform(lr, crs = 2227)
  pr <- st_transform(pr, crs = 2227)
  nocar_19 <- st_transform(nocar_19, crs = 2227)
  
  
  
  #####FILTER FOR NEIGHBORHOOD TRACTS #####
  tracts<- filter(nocar_19, NAME == "Census Tract 5016, Santa Clara County, California" | 
                          NAME == "Census Tract 5031.12, Santa Clara County, California" |
                          NAME == "Census Tract 5014.01, Santa Clara County, California" | 
                          NAME == "Census Tract 5014.02, Santa Clara County, California")
  
  SKtracts<- filter(tracts, NAME== "5016" |
                      NAME== "5031.12")
  
  RPtracts <- filter(tracts, NAME== "5014.01" |
                       NAME== "5014.02")
  ####CLIP DATA######
  SKbus<- st_intersection(bus, SKbound)
  RPbus<- st_intersection(bus, RPbound)
  
  SKbus <- st_zm(SKbus, drop=T, what='ZM')
  RPbus <- st_zm(RPbus, drop=T, what='ZM')
  
  st_write(SKbus, "skbus.shp")
  st_write(RPbus, "rpbus.shp")
  st_write(tracts, "tracts.shp")
  
  
  ####MAP DATA#####
  tmap_mode("view")
  tdm_map<-  
    tm_shape(tracts) + 
    tm_polygons(legend.format=list(fun=function(x) paste0(formatC(x, digits=0, format="f"), "%")),col= "noCar_pct_19", id = "noCar_pct_19", alpha = .8, palette = "Reds", title = "Percentage of Residents Without a Vehicle")+ 
    tm_shape(SKbus) + tm_dots(col = "black") +
    tm_shape(RPbus) + tm_dots(col = "black") +
    tm_shape(lr) + tm_lines(col = "red") +
    tm_shape(bart) + tm_dots(col = "purple") +
    tm_shape(pr) + tm_dots(col = "blue") +
    tm_add_legend(type = "fill", labels = c('Bus Stops','Light Rail Lines','BART Stations','Park and Ride Stations'), col = c("black", "red", "purple", "blue"),
                  title="Transportation")  + 
    tm_basemap(server = "CartoDB.Positron")
  tdm_map
  
