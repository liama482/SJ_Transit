library(sf)
library(tidyverse)
library(tmap)
library(sp)
library(tigris)
library(dplyr)

setwd("C:/Users/Cameron/Desktop/Parcel_Analysis")

##IMPORT data
parc_sk<- st_read("Parcel_data/ParcelOwnership.shp")
parc_rp<- st_read("Parcel_data/ParcelOwnership.shp")
RPbound<- st_read("RPark_bound/RPark_boundary.shp")
SKbound <- st_read("SPKeys_bound/SPKeys_boundary.shp")
#SC_parc <- st_read("Parcels/geo_export_8fbbe52b-b4fe-4080-b2f5-38b5f874b730.shp")

###Project data
parc_rp <- st_transform(parc_rp, crs = 2229)
parc_sk <- st_transform(parc_sk, crs = 2229)
SKbound <- st_transform(SKbound, crs = 2229)
RPbound <- st_transform(RPbound, crs = 2229)

###Tidy data
SKint<- st_intersection(parc_sk, SKbound) %>%
            filter(APN =="47218063"|
                    APN == "47248016"|
                    APN ==  "47248027"|
                    APN == "47248034"|
                    APN == "47248041"|
                    APN =="47248042"|
                    APN =="47248050"|
                    APN =="47248065"|
                    APN == "47248072"|
                    APN == "47248075"|
                    APN =="47723043"|
                    APN == "47723044"|
                    APN == "47215026")
RPint <- st_intersection(parc_rp, RPbound) %>%
            filter(APN == "46701121"|
                  APN == "46720018"|
                  APN == "46721046"|
                  APN == "46757084")

tmap_mode("view")
sj_map<- tm_shape(RPbound) +tm_borders()+ tm_shape(SKbound)+tm_borders() +
  tm_shape(SKint) +tm_fill(col="APN", id = "APN", legend.show = FALSE) +
  tm_shape(RPint) +  tm_fill(col="APN", id = "APN",legend.show = FALSE) +
  tm_basemap(server = "CartoDB.Positron")
sj_map


SK_APN<- filter(parc_sk, 
            APN =="47218063"|
            APN == "47248016"|
            APN ==  "47248027"|
            APN == "47248034"|
            APN == "47248041"|
            APN =="47248042"|
            APN =="47248050"|
            APN =="47248065"|
            APN == "47248072"|
            APN == "47248075"|
            APN =="47723043"|
            APN == "47723044"|
            APN == "47215026")

RP_APN <- filter(parc_rp, 
            APN == "46701121" |
            APN == "46720018" |
            APN == "46721046" |
            APN == "46757084")

###### Mapping
APN_map<- tm_shape(RPbound) +tm_borders()+ tm_shape(SKbound)+tm_borders() +
  tm_shape(SK_APN) +tm_fill(col="APN", id = "APN", legend.show = FALSE) +
  tm_shape(RP_APN) +  tm_fill(col="APN", id = "APN",legend.show = FALSE) +
  tm_basemap(server = "CartoDB.Positron")
APN_map
  