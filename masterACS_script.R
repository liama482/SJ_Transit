#set-up
library(tidyverse)
library(sf)
library(readxl)
library(tidycensus)
library(tmap)
library(stats)
library(leaflet)
library(viridis)
setwd("C:/Users/lp2ab/Dropbox/SCU_4th_Year/Capstone_Analysis/CommuteDataLiam")
census_api_key("ca1e6bccd0bcdbf2cd3462c3f055921aae8e3957")

#import data
v2019 <- load_variables(2019, "acs5", cache = TRUE)

Black_transport <- get_acs(geography="tract", variables=c("B08105B_001", "B08105B_002", "B08105B_003", "B08105B_004", "B08105B_005", "B08105B_006"),
                           output = "wide", state = "CA",
                           county="Santa Clara", geometry = TRUE)

Asian_transport <- get_acs(geography="tract", variables=c("B08105D_001", "B08105D_002", "B08105D_003", "B08105D_004", "B08105D_005", "B08105D_006"),
                           output = "wide", state = "CA",
                           county="Santa Clara", geometry = TRUE)

PacIsl_transport <- get_acs(geography="tract", variables=c("B08105E_001", "B08105E_002", "B08105E_003", "B08105E_004", "B08105E_005", "B08105E_006"),
                            output = "wide", state = "CA",
                            county="Santa Clara", geometry = FALSE)

OtherRace_transport <- get_acs(geography="tract", variables=c("B08105F_001", "B08105F_002", "B08105F_003", "B08105F_004", "B08105F_005", "B08105F_006"),
                               output = "wide", state = "CA",
                               county="Santa Clara", geometry = TRUE)

TwoRaces_transport <- get_acs(geography="tract", variables=c("B08105G_001", "B08105G_002", "B08105G_003", "B08105G_004", "B08105G_005", "B08105G_006"),
                              output = "wide", state = "CA",
                              county="Santa Clara", geometry = FALSE)

White_transport <- get_acs(geography="tract", variables=c("B08105H_001", "B08105H_002", "B08105H_003", "B08105H_004", "B08105H_005", "B08105H_006"),
                           output = "wide", state = "CA",
                           county="Santa Clara", geometry = TRUE)

Latino_transport <- get_acs(geography="tract", variables=c("B08105I_001", "B08105I_002", "B08105I_003", "B08105I_004", "B08105I_005", "B08105I_006"),
                            output = "wide", state = "CA",
                            county="Santa Clara", geometry = TRUE)



#Tidying all Data
#Filtering to include only four census tracts project concerns
Black_transport2 <- filter(Black_transport, NAME== "Census Tract 5016, Santa Clara County, California" | NAME == "Census Tract 5014.01, Santa Clara County, California"
                           | NAME == "Census Tract 5014.02, Santa Clara County, California" | NAME == "Census Tract 5031.12, Santa Clara County, California")
Asian_transport2 <- filter(Asian_transport, NAME== "Census Tract 5016, Santa Clara County, California" | NAME == "Census Tract 5014.01, Santa Clara County, California"
                           | NAME == "Census Tract 5014.02, Santa Clara County, California" | NAME == "Census Tract 5031.12, Santa Clara County, California")
Latino_transport2 <- filter(Latino_transport, NAME== "Census Tract 5016, Santa Clara County, California" | NAME == "Census Tract 5014.01, Santa Clara County, California"
                            | NAME == "Census Tract 5014.02, Santa Clara County, California" | NAME == "Census Tract 5031.12, Santa Clara County, California")
OtherRace_transport2 <- filter(OtherRace_transport, NAME== "Census Tract 5016, Santa Clara County, California" | NAME == "Census Tract 5014.01, Santa Clara County, California"
                               | NAME == "Census Tract 5014.02, Santa Clara County, California" | NAME == "Census Tract 5031.12, Santa Clara County, California")
PacIsl_transport2 <- filter(PacIsl_transport, NAME== "Census Tract 5016, Santa Clara County, California" | NAME == "Census Tract 5014.01, Santa Clara County, California"
                            | NAME == "Census Tract 5014.02, Santa Clara County, California" | NAME == "Census Tract 5031.12, Santa Clara County, California")
TwoRaces_transport2 <- filter(TwoRaces_transport, NAME== "Census Tract 5016, Santa Clara County, California" | NAME == "Census Tract 5014.01, Santa Clara County, California"
                              | NAME == "Census Tract 5014.02, Santa Clara County, California" | NAME == "Census Tract 5031.12, Santa Clara County, California")
White_transport2 <- filter(White_transport, NAME== "Census Tract 5016, Santa Clara County, California" | NAME == "Census Tract 5014.01, Santa Clara County, California"
                           | NAME == "Census Tract 5014.02, Santa Clara County, California" | NAME == "Census Tract 5031.12, Santa Clara County, California")

#Renaming variables
Black_transport2 <- Black_transport2 %>%
  dplyr::rename(ALL = B08105B_001E) %>%
  dplyr::rename(DrAl = B08105B_002E) %>%
  dplyr::rename(CarPl = B08105B_003E) %>%
  dplyr::rename(PubTrans = B08105B_004E) %>%
  dplyr::rename(Walked = B08105B_005E) %>%
  dplyr::rename(BikeEtc = B08105B_006E) %>%
  dplyr::select(GEOID, NAME, ALL, DrAl, CarPl, PubTrans, Walked, BikeEtc, geometry)

Asian_transport2 <- Asian_transport2 %>%
  dplyr::rename(ALL = B08105D_001E) %>%
  dplyr::rename(DrAl = B08105D_002E) %>%
  dplyr::rename(CarPl = B08105D_003E) %>%
  dplyr::rename(PubTrans = B08105D_004E) %>%
  dplyr::rename(Walked = B08105D_005E) %>%
  dplyr::rename(BikeEtc = B08105D_006E) %>%
  dplyr::select(GEOID, NAME, ALL, DrAl, CarPl, PubTrans, Walked, BikeEtc, geometry)

Latino_transport2 <- Latino_transport2 %>%
  dplyr::rename(ALL = B08105I_001E) %>%
  dplyr::rename(DrAl = B08105I_002E) %>%
  dplyr::rename(CarPl = B08105I_003E) %>%
  dplyr::rename(PubTrans = B08105I_004E) %>%
  dplyr::rename(Walked = B08105I_005E) %>%
  dplyr::rename(BikeEtc = B08105I_006E) %>%
  dplyr::select(GEOID, NAME, ALL, DrAl, CarPl, PubTrans, Walked, BikeEtc, geometry)

OtherRace_transport2 <- OtherRace_transport2 %>%
  dplyr::rename(ALL = B08105F_001E) %>%
  dplyr::rename(DrAl = B08105F_002E) %>%
  dplyr::rename(CarPl = B08105F_003E) %>%
  dplyr::rename(PubTrans = B08105F_004E) %>%
  dplyr::rename(Walked = B08105F_005E) %>%
  dplyr::rename(BikeEtc = B08105F_006E) %>%
  dplyr::select(GEOID, NAME, ALL, DrAl, CarPl, PubTrans, Walked, BikeEtc, geometry)

TwoRaces_transport2 <- TwoRaces_transport2 %>%
  dplyr::rename(ALL = B08105G_001E) %>%
  dplyr::rename(DrAl = B08105G_002E) %>%
  dplyr::rename(CarPl = B08105G_003E) %>%
  dplyr::rename(PubTrans = B08105G_004E) %>%
  dplyr::rename(Walked = B08105G_005E) %>%
  dplyr::rename(BikeEtc = B08105G_006E) %>%
  dplyr::select(GEOID, NAME, ALL, DrAl, CarPl, PubTrans, Walked, BikeEtc)

PacIsl_transport2 <- PacIsl_transport2 %>%
  dplyr::rename(ALL = B08105E_001E) %>%
  dplyr::rename(DrAl = B08105E_002E) %>%
  dplyr::rename(CarPl = B08105E_003E) %>%
  dplyr::rename(PubTrans = B08105E_004E) %>%
  dplyr::rename(Walked = B08105E_005E) %>%
  dplyr::rename(BikeEtc = B08105E_006E) %>%
  dplyr::select(GEOID, NAME, ALL, DrAl, CarPl, PubTrans, Walked, BikeEtc)

White_transport2 <- White_transport2 %>%
  dplyr::rename(ALL = B08105H_001E) %>%
  dplyr::rename(DrAl = B08105H_002E) %>%
  dplyr::rename(CarPl = B08105H_003E) %>%
  dplyr::rename(PubTrans = B08105H_004E) %>%
  dplyr::rename(Walked = B08105H_005E) %>%
  dplyr::rename(BikeEtc = B08105H_006E) %>%
  dplyr::select(GEOID, NAME, ALL, DrAl, CarPl, PubTrans, Walked, BikeEtc, geometry)


#Combine datasets
OtherRace_transport2 <- st_transform(OtherRace_transport2, crs = 2227) # project to California State Plane Zone 3
st_is_valid(OtherRace_transport2)
OtherRaces_transport2 <- left_join(OtherRace_transport2, PacIsl_transport2, by = "GEOID")
OtherRaces_transport2 <- left_join(OtherRaces_transport2, TwoRaces_transport2, by = "GEOID")

#Tidy combined dataset
OtherRaces_transport2 <- OtherRaces_transport2 %>%
  dplyr::mutate(ALL3races = ALL + ALL.x + ALL.y) %>%
  dplyr::mutate(DrAl3races = DrAl + DrAl.x + DrAl.y) %>%
  dplyr::mutate(CarPl3races = CarPl + CarPl.x + CarPl.y) %>%
  dplyr::mutate(PubTrans3races = PubTrans + PubTrans.x + PubTrans.y) %>%
  dplyr::mutate(Walked3races = Walked + Walked.x + Walked.y) %>%
  dplyr::mutate(BikeEtc3races = BikeEtc + BikeEtc.x + BikeEtc.y) %>%
  dplyr::select(GEOID, NAME, geometry, ALL3races, DrAl3races, CarPl3races, PubTrans3races, Walked3races, BikeEtc3races)

#Delete unnecessary datasets)
rm(White_transport)
rm(Black_transport)
rm(PacIsl_transport)
rm(Asian_transport)
rm(OtherRace_transport)
rm(TwoRaces_transport)
rm(Latino_transport)

#Project other data
White_transport2 <- st_transform(White_transport2, crs = 2227) # project to California State Plane Zone 3
st_is_valid(White_transport2)
Black_transport2 <- st_transform(Black_transport2, crs = 2227) # project to California State Plane Zone 3
st_is_valid(Black_transport2)
Asian_transport2 <- st_transform(Asian_transport2, crs = 2227) # project to California State Plane Zone 3
st_is_valid(Asian_transport2)
Latino_transport2 <- st_transform(Latino_transport2, crs = 2227) # project to California State Plane Zone 3
st_is_valid(Latino_transport2)

#Delete more datasets
rm(PacIsl_transport2)
rm(TwoRaces_transport2)
rm(OtherRace_transport2)

st_write(Asian_transport2, dsn = "Asian.shp", driver = "ESRI Shapefile")
st_write(Black_transport2, dsn = "Black.shp", driver = "ESRI Shapefile")
st_write(Latino_transport2, dsn = "Latino.shp", driver = "ESRI Shapefile")
st_write(OtherRaces_transport2, dsn = "OtherRaces.shp", driver = "ESRI Shapefile")
st_write(White_transport2, dsn = "White.shp", driver = "ESRI Shapefile")


#######
###### Rearranging Data
#First 2 entries in vector are Roosevelt, last 2 are SK
CNames <- colnames(Black_transport2)
BlackALL <- Black_transport2$ALL
WhiteALL <- White_transport2$ALL
AsianALL <- Asian_transport2$ALL
LatinoALL <- Latino_transport2$ALL
OtherALL <- OtherRaces_transport2$ALL3races
BlackDrAl <- Black_transport2$DrAl
WhiteDrAl <- White_transport2$DrAl
AsianDrAl <- Asian_transport2$DrAl
LatinoDrAl <- Latino_transport2$DrAl
OtherDrAl <- OtherRaces_transport2$DrAl3races

ALLR <- c(1,2,3,4,5)
DrAlR <- c(1,2,3,4,5)
ALLR[1] <- BlackALL[1] + BlackALL[2]
DrAlR[1] <- BlackDrAl[1] + BlackDrAl[2]
ALLR[2] <- WhiteALL[1] + WhiteALL[2]
DrAlR[2] <- WhiteDrAl[1] + WhiteDrAl[2]
ALLR[3] <- AsianALL[1] + AsianALL[2]
DrAlR[3] <- AsianDrAl[1] + AsianDrAl[2]
ALLR[4] <- LatinoALL[1] + LatinoALL[2]
DrAlR[4] <- LatinoDrAl[1] + LatinoDrAl[2]
ALLR[5] <- OtherALL[1] + OtherALL[2]
DrAlR[5] <- OtherDrAl[1] + OtherDrAl[2]
BlackCarPl <- Black_transport2$CarPl
BlackPubTrans <- Black_transport2$PubTrans
BlackWalked <- Black_transport2$Walked
BlackBikeEtc <- Black_transport2$BikeEtc
WhiteCarPl <- White_transport2$CarPl
WhitePubTrans <- White_transport2$PubTrans
WhiteWalked <- White_transport2$Walked
WhiteBikeEtc <- White_transport2$BikeEtc
AsianCarPl <- Asian_transport2$CarPl
AsianPubTrans <- Asian_transport2$PubTrans
AsianWalked <- Asian_transport2$Walked
AsianBikeEtc <- Asian_transport2$BikeEtc
LatinoCarPl <- Latino_transport2$CarPl
LatinoPubTrans <- Latino_transport2$PubTrans
LatinoWalked <- Latino_transport2$Walked
LatinoBikeEtc <- Latino_transport2$BikeEtc
OtherCarPl <- OtherRaces_transport2$CarPl3races
OtherPubTrans <- OtherRaces_transport2$PubTrans3races
OtherWalked <- OtherRaces_transport2$Walked3races
OtherBikeEtc <- OtherRaces_transport2$BikeEtc3races

#Continue making Roosevelt data frame
CarPlR <- c(1,2,3,4,5)
PubTransR <- c(1,2,3,4,5)
WalkedR <- c(1,2,3,4,5)
BikeEtcR <- c(1,2,3,4,5)
CarPlR[1] <- BlackCarPl[1] + BlackCarPl[2]
PubTransR[1] <- BlackPubTrans[1] + BlackPubTrans[2]
CarPlR[2] <- WhiteCarPl[1] + WhiteCarPl[2]
PubTransR[2] <- WhitePubTrans[1] + WhitePubTrans[2]
CarPlR[3] <- AsianCarPl[1] + AsianCarPl[2]
PubTransR[3] <- AsianPubTrans[1] + AsianPubTrans[2]
CarPlR[4] <- LatinoCarPl[1] + LatinoCarPl[2]
PubTransR[4] <- LatinoPubTrans[1] + LatinoPubTrans[2]
CarPlR[5] <- OtherCarPl[1] + OtherCarPl[2]
PubTransR[5] <- OtherPubTrans[1] + OtherPubTrans[2]
WalkedR[1] <- BlackWalked[1] + BlackWalked[2]
BikeEtcR[1] <- BlackBikeEtc[1] + BlackBikeEtc[2]
WalkedR[2] <- WhiteWalked[1] + WhiteWalked[2]
BikeEtcR[2] <- WhiteBikeEtc[1] + WhiteBikeEtc[2]
WalkedR[3] <- AsianWalked[1] + AsianWalked[2]
BikeEtcR[3] <- AsianBikeEtc[1] + AsianBikeEtc[2]
WalkedR[4] <- LatinoWalked[1] + LatinoWalked[2]
BikeEtcR[4] <- LatinoBikeEtc[1] + LatinoBikeEtc[2]
WalkedR[5] <- OtherWalked[1] + OtherWalked[2]
BikeEtcR[5] <- OtherBikeEtc[1] + OtherBikeEtc[2]


#Calculate_Percents
Percent <- rep(1, 25)
Race <- rep(2, 25)
TransitMode <- rep(3, 25)
RNames2 <- c("Black","White","Asian","Latino","Other")

Percent[1] <- DrAlR[1]/ALLR[1]*100
Percent[2] <- DrAlR[2]/ALLR[2]*100
Percent[3] <- DrAlR[3]/ALLR[3]*100
Percent[4] <- DrAlR[4]/ALLR[4]*100
Percent[5] <- DrAlR[5]/ALLR[5]*100

#Continue making Roosevelt data frame
Percent[6] <- CarPlR[1]/ALLR[1]*100
Percent[7] <- CarPlR[2]/ALLR[2]*100
Percent[8] <- CarPlR[3]/ALLR[3]*100
Percent[9] <- CarPlR[4]/ALLR[4]*100
Percent[10] <- CarPlR[5]/ALLR[5]*100

Percent[11] <- PubTransR[1]/ALLR[1]*100
Percent[12] <- PubTransR[2]/ALLR[2]*100
Percent[13] <- PubTransR[3]/ALLR[3]*100
Percent[14] <- PubTransR[4]/ALLR[4]*100
Percent[15] <- PubTransR[5]/ALLR[5]*100

Percent[16] <- WalkedR[1]/ALLR[1]*100
Percent[17] <- WalkedR[2]/ALLR[2]*100
Percent[18] <- WalkedR[3]/ALLR[3]*100
Percent[19] <- WalkedR[4]/ALLR[4]*100
Percent[20] <- WalkedR[5]/ALLR[5]*100

Percent[21] <- BikeEtcR[1]/ALLR[1]*100
Percent[22] <- BikeEtcR[2]/ALLR[2]*100
Percent[23] <- BikeEtcR[3]/ALLR[3]*100
Percent[24] <- BikeEtcR[4]/ALLR[4]*100
Percent[25] <- BikeEtcR[5]/ALLR[5]*100

CNames2 <- CNames[4:8]
CNames2 <- c("Drive Alone", "Carpool", "Public Transit", "Walking", "Bike or Other Means")

for (col2 in 1:5){
  print(col2)
  xMode <- CNames2[col2]
  for (col3 in 1:5) {
    xRace <- RNames2[col3]
    xRow <- ((col2-1)*5+col3)
    Race[xRow] <- xRace
    TransitMode[xRow] <- xMode
  }
}

Roosevelt <- data.frame(Percent, TransitMode, Race)


#Make Spartan Keyes Dataframe
ALLS <- c(1,2,3,4,5)
DrAlS <- c(1,2,3,4,5)
CarPlS <- c(1,2,3,4,5)
PubTransS <- c(1,2,3,4,5)
WalkedS <- c(1,2,3,4,5)
BikeEtcS <- c(1,2,3,4,5)
ALLS[1] <- BlackALL[3] + BlackALL[4]
DrAlS[1] <- BlackDrAl[3] + BlackDrAl[4]
ALLS[2] <- WhiteALL[3] + WhiteALL[4]
DrAlS[2] <- WhiteDrAl[3] + WhiteDrAl[4]
ALLS[3] <- AsianALL[3] + AsianALL[4]
DrAlS[3] <- AsianDrAl[3] + AsianDrAl[4]
ALLS[4] <- LatinoALL[3] + LatinoALL[4]
DrAlS[4] <- LatinoDrAl[3] + LatinoDrAl[4]
ALLS[5] <- OtherALL[3] + OtherALL[4]
DrAlS[5] <- OtherDrAl[3] + OtherDrAl[4]

CarPlS[1] <- BlackCarPl[3] + BlackCarPl[4]
PubTransS[1] <- BlackPubTrans[3] + BlackPubTrans[4]
CarPlS[2] <- WhiteCarPl[3] + WhiteCarPl[4]
PubTransS[2] <- WhitePubTrans[3] + WhitePubTrans[4]
CarPlS[3] <- AsianCarPl[3] + AsianCarPl[4]
PubTransS[3] <- AsianPubTrans[3] + AsianPubTrans[4]
CarPlS[4] <- LatinoCarPl[3] + LatinoCarPl[4]
PubTransS[4] <- LatinoPubTrans[3] + LatinoPubTrans[4]
CarPlS[5] <- OtherCarPl[3] + OtherCarPl[4]
PubTransS[5] <- OtherPubTrans[3] + OtherPubTrans[4]
WalkedS[1] <- BlackWalked[3] + BlackWalked[4]
BikeEtcS[1] <- BlackBikeEtc[3] + BlackBikeEtc[4]
WalkedS[2] <- WhiteWalked[3] + WhiteWalked[4]
BikeEtcS[2] <- WhiteBikeEtc[3] + WhiteBikeEtc[4]
WalkedS[3] <- AsianWalked[3] + AsianWalked[4]
BikeEtcS[3] <- AsianBikeEtc[3] + AsianBikeEtc[4]
WalkedS[4] <- LatinoWalked[3] + LatinoWalked[4]
BikeEtcS[4] <- LatinoBikeEtc[3] + LatinoBikeEtc[4]
WalkedS[5] <- OtherWalked[3] + OtherWalked[4]
BikeEtcS[5] <- OtherBikeEtc[3] + OtherBikeEtc[4]

#Calculate_Percents
Percent[1] <- DrAlS[1]/ALLS[1]*100
Percent[2] <- DrAlS[2]/ALLS[2]*100
Percent[3] <- DrAlS[3]/ALLS[3]*100
Percent[4] <- DrAlS[4]/ALLS[4]*100
Percent[5] <- DrAlS[5]/ALLS[5]*100

#Continue making Spartan Keyes data frame
Percent[6] <- CarPlS[1]/ALLS[1]*100
Percent[7] <- CarPlS[2]/ALLS[2]*100
Percent[8] <- CarPlS[3]/ALLS[3]*100
Percent[9] <- CarPlS[4]/ALLS[4]*100
Percent[10] <- CarPlS[5]/ALLS[5]*100

Percent[11] <- PubTransS[1]/ALLS[1]*100
Percent[12] <- PubTransS[2]/ALLS[2]*100
Percent[13] <- PubTransS[3]/ALLS[3]*100
Percent[14] <- PubTransS[4]/ALLS[4]*100
Percent[15] <- PubTransS[5]/ALLS[5]*100

Percent[16] <- WalkedS[1]/ALLS[1]*100
Percent[17] <- WalkedS[2]/ALLS[2]*100
Percent[18] <- WalkedS[3]/ALLS[3]*100
Percent[19] <- WalkedS[4]/ALLS[4]*100
Percent[20] <- WalkedS[5]/ALLS[5]*100

Percent[21] <- BikeEtcS[1]/ALLS[1]*100
Percent[22] <- BikeEtcS[2]/ALLS[2]*100
Percent[23] <- BikeEtcS[3]/ALLS[3]*100
Percent[24] <- BikeEtcS[4]/ALLS[4]*100
Percent[25] <- BikeEtcS[5]/ALLS[5]*100

SKeyes <- data.frame(Percent, TransitMode, Race)


#Clean environment a bit
rm(AsianALL)
rm(AsianBikeEtc)
rm(AsianCarPl)
rm(AsianDrAl)
rm(AsianPubTrans)
rm(AsianWalked)
rm(BlackALL)
rm(BlackBikeEtc)
rm(BlackCarPl)
rm(BlackDrAl)
rm(BlackPubTrans)
rm(BlackWalked)
rm(LatinoALL)
rm(LatinoBikeEtc)
rm(LatinoCarPl)
rm(LatinoDrAl)
rm(LatinoPubTrans)
rm(LatinoWalked)
rm(WhiteALL)
rm(WhiteBikeEtc)
rm(WhiteCarPl)
rm(WhiteDrAl)
rm(WhitePubTrans)
rm(WhiteWalked)
rm(OtherALL)
rm(OtherDrAl)
rm(OtherCarPl)
rm(OtherPubTrans)
rm(OtherWalked)
rm(OtherBikeEtc)
rm(ALLR)
rm(DrAlR)
rm(CarPlR)
rm(PubTransR)
rm(WalkedR)
rm(BikeEtcR)

write.csv(Roosevelt, "Roosevelt.csv", row.names = TRUE)
write.csv(SKeyes, "Spartan_Keyes.csv", row.names = TRUE)

###################################################################################
#graph transit percentages for each neighborhood

tmap_mode("view") #FOR viewing!!!
Roosevelt <- read.csv(file = "Roosevelt.csv")
Spartan_Keyes <- read.csv(file = "Spartan_Keyes.csv")
mytheme <- theme(panel.grid.major = element_line(colour="gray25", size = (0.7)),
                 panel.grid.minor = element_line(size = (0.2), colour="gray65"))

ylimR = 30
ylimSK = 40
transitlevels = c("Carpool", "Public Transit", "Walking", "Bike or Other Means", "Drive Alone")
Roosevelt$TransitMode <- factor(Roosevelt$TransitMode, transitlevels)
Spartan_Keyes$TransitMode <- factor(Spartan_Keyes$TransitMode, transitlevels)

alt_Roosevelt <- filter(Roosevelt, TransitMode == "Carpool" | TransitMode == "Public Transit" | TransitMode == "Walking" | TransitMode == "Bike or Other Means")
alt_SKeyes <- filter(Spartan_Keyes, TransitMode == "Carpool" | TransitMode == "Public Transit" | TransitMode == "Walking" | TransitMode == "Bike or Other Means")
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

RGrouped_chart <- ggplot(Roosevelt, aes(fill=TransitMode, y=Percent, x=Race)) + 
  geom_bar(position="dodge", stat="identity") +
  ggtitle("Transit Mode by Race in Roosevelt Park") +
  scale_fill_manual(values=cbPalette)
RGrouped_chart + mytheme

RGrouped_chart2 <- ggplot(alt_Roosevelt, aes(fill=TransitMode, y=Percent, x=Race)) + 
  geom_bar(position="dodge", stat="identity") +
  ggtitle("Alternative Modes of Transit by Race in Roosevelt Park") +
  scale_y_continuous(limits=c(0,ylimR)) +
  scale_fill_manual(values=cbPalette)
RGrouped_chart2 + mytheme

SKGrouped_chart <- ggplot(Spartan_Keyes, aes(fill=TransitMode, y=Percent, x=Race)) + 
  geom_bar(position="dodge", stat="identity") +
  ggtitle("Transit Mode by Race in Spartan Keyes") +
  scale_fill_manual(values=cbPalette)
SKGrouped_chart + mytheme

SKGrouped_chart2 <- ggplot(alt_SKeyes, aes(fill=TransitMode, y=Percent, x=Race)) + 
  geom_bar(position="dodge", stat="identity") +
  ggtitle("Alternative Modes of Transit by Race in Spartan Keyes") +
  scale_y_continuous(limits=c(0,ylimSK)) +
  scale_fill_manual(values=cbPalette)
SKGrouped_chart2 + mytheme

#######################################################################################
#making graphs with data of the 2 neighborhoods combined, used for our presentations

Asian_transport2 <- st_read("Asian.shp")
Black_transport2 <- st_read("Black.shp")
Latino_transport2 <- st_read("Latino.shp")
OtherRaces_transport2 <- st_read("OtherRaces.shp")
White_transport2 <- st_read("White.shp")

#Start rearranging data to be suitable for a graph of grouped columns
CNames <- colnames(Black_transport2)
BlackALL <- sum(Black_transport2$ALL)
WhiteALL <- sum(White_transport2$ALL)
AsianALL <- sum(Asian_transport2$ALL)
LatinoALL <- sum(Latino_transport2$ALL)
OtherALL <- sum(OtherRaces_transport2$ALL3rcs)
BlackDrAl <- sum(Black_transport2$DrAl)
WhiteDrAl <- sum(White_transport2$DrAl)
AsianDrAl <- sum(Asian_transport2$DrAl)
LatinoDrAl <- sum(Latino_transport2$DrAl)
OtherDrAl <- sum(OtherRaces_transport2$DrAl3rc)

ALLBoth <- c(1,2,3,4,5)
DrAlBoth <- c(1,2,3,4,5)
ALLBoth[1] <- BlackALL
DrAlBoth[1] <- BlackDrAl
ALLBoth[2] <- WhiteALL
DrAlBoth[2] <- WhiteDrAl
ALLBoth[3] <- AsianALL
DrAlBoth[3] <- AsianDrAl
ALLBoth[4] <- LatinoALL
DrAlBoth[4] <- LatinoDrAl
ALLBoth[5] <- OtherALL
DrAlBoth[5] <- OtherDrAl
BlackCarPl <- sum(Black_transport2$CarPl)
BlackPubTrans <- sum(Black_transport2$PubTrans)
BlackWalked <- sum(Black_transport2$Walked)
BlackBikeEtc <- sum(Black_transport2$BikeEtc)
WhiteCarPl <- sum(White_transport2$CarPl)
WhitePubTrans <- sum(White_transport2$PubTrans)
WhiteWalked <- sum(White_transport2$Walked)
WhiteBikeEtc <- sum(White_transport2$BikeEtc)
AsianCarPl <- sum(Asian_transport2$CarPl)
AsianPubTrans <- sum(Asian_transport2$PubTrans)
AsianWalked <- sum(Asian_transport2$Walked)
AsianBikeEtc <- sum(Asian_transport2$BikeEtc)
LatinoCarPl <- sum(Latino_transport2$CarPl)
LatinoPubTrans <- sum(Latino_transport2$PubTrans)
LatinoWalked <- sum(Latino_transport2$Walked)
LatinoBikeEtc <- sum(Latino_transport2$BikeEtc)
OtherCarPl <- sum(OtherRaces_transport2$CrPl3rc)
OtherPubTrans <- sum(OtherRaces_transport2$PbTrns3)
OtherWalked <- sum(OtherRaces_transport2$Wlkd3rc)
OtherBikeEtc <- sum(OtherRaces_transport2$BkEtc3r)

#Continue perparing to make data frame for the two neighborhoods combined
CarPlR <- c(1,2,3,4,5)
PubTransR <- c(1,2,3,4,5)
WalkedR <- c(1,2,3,4,5)
BikeEtcR <- c(1,2,3,4,5)
CarPlR[1] <- BlackCarPl
PubTransR[1] <- BlackPubTrans
CarPlR[2] <- WhiteCarPl
PubTransR[2] <- WhitePubTrans
CarPlR[3] <- AsianCarPl
PubTransR[3] <- AsianPubTrans
CarPlR[4] <- LatinoCarPl
PubTransR[4] <- LatinoPubTrans
CarPlR[5] <- OtherCarPl
PubTransR[5] <- OtherPubTrans
WalkedR[1] <- BlackWalked
BikeEtcR[1] <- BlackBikeEtc
WalkedR[2] <- WhiteWalked
BikeEtcR[2] <- WhiteBikeEtc
WalkedR[3] <- AsianWalked
BikeEtcR[3] <- AsianBikeEtc
WalkedR[4] <- LatinoWalked
BikeEtcR[4] <- LatinoBikeEtc
WalkedR[5] <- OtherWalked
BikeEtcR[5] <- OtherBikeEtc


#Calculate Percents, to move away from raw numbers of people using each mode of transit
Percent <- rep(1, 25)
Race <- rep(2, 25)
TransitMode <- rep(3, 25)
RNames2 <- c("Black","White","Asian","Latino","Other")

Percent[1] <- DrAlBoth[1]/ALLBoth[1]*100
Percent[2] <- DrAlBoth[2]/ALLBoth[2]*100
Percent[3] <- DrAlBoth[3]/ALLBoth[3]*100
Percent[4] <- DrAlBoth[4]/ALLBoth[4]*100
Percent[5] <- DrAlBoth[5]/ALLBoth[5]*100

#Continue making Roosevelt data frame
Percent[6] <- CarPlR[1]/ALLBoth[1]*100
Percent[7] <- CarPlR[2]/ALLBoth[2]*100
Percent[8] <- CarPlR[3]/ALLBoth[3]*100
Percent[9] <- CarPlR[4]/ALLBoth[4]*100
Percent[10] <- CarPlR[5]/ALLBoth[5]*100

Percent[11] <- PubTransR[1]/ALLBoth[1]*100
Percent[12] <- PubTransR[2]/ALLBoth[2]*100
Percent[13] <- PubTransR[3]/ALLBoth[3]*100
Percent[14] <- PubTransR[4]/ALLBoth[4]*100
Percent[15] <- PubTransR[5]/ALLBoth[5]*100

Percent[16] <- WalkedR[1]/ALLBoth[1]*100
Percent[17] <- WalkedR[2]/ALLBoth[2]*100
Percent[18] <- WalkedR[3]/ALLBoth[3]*100
Percent[19] <- WalkedR[4]/ALLBoth[4]*100
Percent[20] <- WalkedR[5]/ALLBoth[5]*100

Percent[21] <- BikeEtcR[1]/ALLBoth[1]*100
Percent[22] <- BikeEtcR[2]/ALLBoth[2]*100
Percent[23] <- BikeEtcR[3]/ALLBoth[3]*100
Percent[24] <- BikeEtcR[4]/ALLBoth[4]*100
Percent[25] <- BikeEtcR[5]/ALLBoth[5]*100

CNames2 <- c("Drive Alone", "Carpool", "Public Transit", "Walking", "Bike or Other Means")

#Add the labels of race and mode of transit to their respective vectors
#so they can be added to the dataframe that will be used to make the graph
for (col2 in 1:5){
  print(col2)
  xMode <- CNames2[col2]
  for (col3 in 1:5) {
    xRace <- RNames2[col3]
    xRow <- ((col2-1)*5+col3)
    Race[xRow] <- xRace
    TransitMode[xRow] <- xMode
  }
}

#Create the data frame containing the percent data for both SK and Roosevelt
Both_neighborhoods <- data.frame(Percent, TransitMode, Race)



#Clean environment a bit
rm(AsianALL)
rm(AsianBikeEtc)
rm(AsianCarPl)
rm(AsianDrAl)
rm(AsianPubTrans)
rm(AsianWalked)
rm(BlackALL)
rm(BlackBikeEtc)
rm(BlackCarPl)
rm(BlackDrAl)
rm(BlackPubTrans)
rm(BlackWalked)
rm(LatinoALL)
rm(LatinoBikeEtc)
rm(LatinoCarPl)
rm(LatinoDrAl)
rm(LatinoPubTrans)
rm(LatinoWalked)
rm(WhiteALL)
rm(WhiteBikeEtc)
rm(WhiteCarPl)
rm(WhiteDrAl)
rm(WhitePubTrans)
rm(WhiteWalked)
rm(OtherALL)
rm(OtherDrAl)
rm(OtherCarPl)
rm(OtherPubTrans)
rm(OtherWalked)
rm(OtherBikeEtc)
rm(ALLBoth)
rm(DrAlBoth)


#Creating the graphs
tmap_mode("view") #FOR viewing!!!
mytheme <- theme(panel.grid.major = element_line(colour="gray25", size = (0.7)),
                 panel.grid.minor = element_line(size = (0.2), colour="gray65"))
ylimB = 30

BGrouped_chart <- ggplot(Both_neighborhoods, aes(fill=TransitMode, y=Percent, x=Race)) + 
  geom_bar(position="dodge", stat="identity") +
  ggtitle("Transit Mode by Race in Roosevelt Park + Spartan Keyes")
BGrouped_chart + mytheme

BGrouped_chart2 <- ggplot(Both_neighborhoods, aes(fill=TransitMode, y=Percent, x=Race)) + 
  geom_bar(position="dodge", stat="identity") +
  ggtitle("Alternative Modes of Transit by Race in Roosevelt Park + Spartan Keyes") +
  scale_y_continuous(limits=c(0,ylimB))
BGrouped_chart2 + mytheme

########################################################
#Code to make Drive Alone map:

census_api_key("ca1e6bccd0bcdbf2cd3462c3f055921aae8e3957")

#import data
v2019 <- load_variables(2019, "acs5", cache = TRUE)

Tracts_DrAl <- get_acs(geography="tract", variables=c("B08105B_001", "B08105B_002", "B08105D_001", "B08105D_002",
                                                      "B08105I_001", "B08105I_002", "B08105F_001", "B08105F_002",
                                                      "B08105G_001", "B08105G_002", "B08105E_001", "B08105E_002",
                                                      "B08105H_001", "B08105H_002"),
                       output = "wide", state = "CA",
                       county="Santa Clara", geometry = TRUE)
Tracts_DrAl <- filter(Tracts_DrAl, NAME== "Census Tract 5016, Santa Clara County, California" | NAME == "Census Tract 5014.01, Santa Clara County, California"
                      | NAME == "Census Tract 5014.02, Santa Clara County, California" | NAME == "Census Tract 5031.12, Santa Clara County, California")

#Tidying Data
Tracts_DrAl <- Tracts_DrAl %>%
  dplyr::rename(ALL1 = B08105B_001E) %>%
  dplyr::rename(DrAl1 = B08105B_002E) %>%
  dplyr::rename(ALL2 = B08105D_001E) %>%
  dplyr::rename(DrAl2 = B08105D_002E) %>%
  dplyr::rename(ALL3 = B08105I_001E) %>%
  dplyr::rename(DrAl3 = B08105I_002E) %>%
  dplyr::rename(ALL4 = B08105F_001E) %>%
  dplyr::rename(DrAl4 = B08105F_002E) %>%
  dplyr::rename(ALL5 = B08105G_001E) %>%
  dplyr::rename(DrAl5 = B08105G_002E) %>%
  dplyr::rename(ALL6 = B08105E_001E) %>%
  dplyr::rename(DrAl6 = B08105E_002E) %>%
  dplyr::rename(ALL7 = B08105H_001E) %>%
  dplyr::rename(DrAl7 = B08105H_002E)

Tracts_DrAl <- Tracts_DrAl %>%
  dplyr::mutate(ALLALL = ALL1 + ALL2 + ALL3 + ALL4 + ALL5 + ALL6 + ALL7) %>%
  dplyr::mutate(DrAlALL = DrAl1 + DrAl2 + DrAl3 + DrAl4 + DrAl5 + DrAl6 + DrAl7)

Tracts_DrAl <- Tracts_DrAl %>%
  dplyr::mutate(Percent_Drive_Alone = DrAlALL/ALLALL*100)

Tracts_DrAl <- st_transform(Tracts_DrAl, crs = 2227) # project to California State Plane Zone 3
st_is_valid(Tracts_DrAl)

##Mapping
tmap_mode("view") #FOR viewing!!!
map_perc_DrAl <- tm_shape(Tracts_DrAl) + tm_polygons(col = "Percent_Drive_Alone", palette = "YlOrRd", style="quantile") + tm_basemap(leaflet::providers$CartoDB.Positron, group = "CartoDB.Positron")
map_perc_DrAl
