
#----------
#LIBRARIES
#----------

library(tidyverse)
library(tidyr)
library(base)
library(dplyr)
library(knitr)
library(osmdata)
library(sp)
library(sf)
library(ggmap)
library(raster)
library(lwgeom)
library(GISTools)
library(nngeo)
library(stats)
library(fields)
library(tmap)
library(ggsn)

#--------------------------------------------------------------------------------------------------------------------------

#------------
#IMPORT DATA
#------------

#The Boundaries of the studied areas and their Middle Lower Super Output Areas were collected from 
#the UK Data Service Census Support website in shapefile format.

uk_regions <- st_read('/Users/christosnicolaou/R_Projects/thesis_data/UK_Boundaries/English_Regions/england_ct_2011.shp')
cityoflondon_bound <- st_read('/Users/christosnicolaou/R_Projects/thesis_data/UK_Boundaries/cityoflondon_bound/london_bound.shp')
chelsea_bound <- st_read('/Users/christosnicolaou/R_Projects/thesis_data/UK_Boundaries/chelsea_bound/Chelsea_boundary.shp')
leicester_bound <- st_read('/Users/christosnicolaou/R_Projects/thesis_data/UK_Boundaries/leic_boundary/leic_boundary.shp')
westminster_bound <- st_read('/Users/christosnicolaou/R_Projects/thesis_data/UK_Boundaries/west_bound/west_bound.shp')

lsoa <- st_read('/Users/christosnicolaou/R_Projects/thesis_data/msoa_2011/england_msoa_2011.shp')

#-----------
#OS_Dataset
#-----------

#The Authoritative Data that are going to be used as reference for the quality assessment of the OSM data were obtained 
#from the Digimap Ordnance Survey in shapefile format.

cityoflondon_build_os <- st_read('/Users/christosnicolaou/R_Projects/thesis_data/OS_Data/CityOfLondon_OS_data/Download/open-map/TQ_Building.shp')
leicester_build_os <- st_read('/Users/christosnicolaou/R_Projects/thesis_data/OS_Data/Leicester_OS_data/open-map/SK_Building.shp')
chelsea_build_os <- st_read('/Users/christosnicolaou/R_Projects/thesis_data/OS_Data/Chelsea_OS_data/open-map/TQ_Building.shp')
westminster_build_os <- st_read('/Users/christosnicolaou/R_Projects/thesis_data/OS_Data/Westminster_OS/west_os/map/TQ_Building.shp')

#------------
#OSM_Dataset
#------------

#The OSM data were obtained from Geofabrik.

leicester_build_osm <- st_read('/Users/christosnicolaou/R_Projects/thesis_data/OSM_data/leicestershire/gis_osm_buildings_a_free_1.shp')
london_build_osm <- st_read('/Users/christosnicolaou/R_Projects/thesis_data/OSM_data/greater-london/gis_osm_buildings_a_free_1.shp')

#--------------------------------------------------------------------------------------------------------------------------

#---------------
#PRE-PROCESSING
#---------------

#After the data were imported they needed to be transformed to the same Coordinate System before any further process.
#So every dataset was transformed to the OS coordinate system.

#The following command lines were used to get information about the coordinate system of each dataset

st_crs(cityoflondon_build_os)
st_crs(leicester_build_os)
st_crs(chelsea_build_os)
st_crs(lambeth_build_os)
st_crs(westminster_build_os)

st_crs(london_build_osm)
st_crs(leicester_build_osm)

st_crs(uk_regions)
st_crs(cityoflondon_bound)
st_crs(leicester_bound)
st_crs(chelsea_bound)
st_crs(lambeth_bound)
st_crs(westminster_bound)

st_crs(lsoa)

#After making sure that the datasets do not have the same Coordinate System they were transformed accordingly.

uk_regions <- st_transform(uk_regions, st_crs(leicester_build_os))
cityoflondon_bound <- st_transform(cityoflondon_bound, st_crs(leicester_build_os))
leicester_bound <- st_transform(leicester_bound, st_crs(leicester_build_os))
chelsea_bound <- st_transform(chelsea_bound, st_crs(leicester_build_os))
lambeth_bound <- st_transform(lambeth_bound, st_crs(leicester_build_os))
westminster_bound <- st_transform(westminster_bound, st_crs(leicester_build_os))

london_build_osm <- st_transform(london_build_osm, st_crs(leicester_build_os))
leicester_build_osm <- st_transform(leicester_build_osm, st_crs(leicester_build_os))

lsoa <- st_transform(lsoa, st_crs(leicester_bound))

#Now that the data have the same coordinate system they can be manipulated to export some of the areas of interest that
#couldnt be obtained online.

#This part includes the crop of OS and OSM building footprints that are within the boundary of each area of study.

cityoflondon_build_os <- 
  cityoflondon_build_os %>%
  filter(st_contains(cityoflondon_bound, ., sparse = FALSE))

chelsea_build_os <-
  chelsea_build_os %>%
  filter(st_contains(chelsea_bound, .,sparse = FALSE))

lambeth_build_os <-
  lambeth_build_os %>%
  filter(st_contains(lambeth_bound, ., sparse = FALSE))

west_build_os <-
  westminster_build_os %>%
  filter(st_contains(westminster_bound, ., sparse = FALSE))

leicester_build_os <-
  leicester_build_os %>%
  filter(st_contains(leicester_bound, ., sparse = FALSE))

cityoflondon_build_osm <-
  london_build_osm %>%
  filter(st_contains(cityoflondon_bound, ., sparse = FALSE))

chelsea_build_osm <- 
  london_build_osm %>%
  filter(st_contains(chelsea_bound, ., sparse = FALSE))

lambeth_build_osm <-
  london_build_osm %>%
  filter(st_contains(lambeth_bound, ., sparse = FALSE))

west_build_osm <-
  london_build_osm %>%
  filter(st_contains(westminster_bound, ., sparse = FALSE))

leicester_build_osm <-
  leicester_build_osm %>%
  filter(st_contains(leicester_bound, ., sparse = FALSE))

#For an in depth study of the studied areas the Middle Lower Super Output Areas were cropped for each one of
#them respectively.

leicester_lsoa <- 
  lsoa %>%
  filter(st_contains(leicester_bound, ., sparse = FALSE))

leicester01 <- 
  leicester_lsoa %>%
  filter(name == "Leicester 001")

leicester02 <- 
  leicester_lsoa %>%
  filter(name == "Leicester 002")

leicester03 <- 
  leicester_lsoa %>%
  filter(name == "Leicester 003")

leicester04 <- 
  leicester_lsoa %>%
  filter(name == "Leicester 004")

leicester05 <- 
  leicester_lsoa %>%
  filter(name == "Leicester 005")

leicester06 <- 
  leicester_lsoa %>%
  filter(name == "Leicester 006")

leicester07 <- 
  leicester_lsoa %>%
  filter(name == "Leicester 007")

leicester08 <- 
  leicester_lsoa %>%
  filter(name == "Leicester 008")

leicester09 <- 
  leicester_lsoa %>%
  filter(name == "Leicester 009")

leicester10 <- 
  leicester_lsoa %>%
  filter(name == "Leicester 010")

leicester11 <- 
  leicester_lsoa %>%
  filter(name == "Leicester 011")

leicester12 <- 
  leicester_lsoa %>%
  filter(name == "Leicester 012")

leicester13 <- 
  leicester_lsoa %>%
  filter(name == "Leicester 013")

leicester16 <- 
  leicester_lsoa %>%
  filter(name == "Leicester 016")

leicester17 <- 
  leicester_lsoa %>%
  filter(name == "Leicester 017")

leicester18 <- 
  leicester_lsoa %>%
  filter(name == "Leicester 018")

leicester19 <- 
  leicester_lsoa %>%
  filter(name == "Leicester 019")

leicester20 <- 
  leicester_lsoa %>%
  filter(name == "Leicester 020")

leicester21 <- 
  leicester_lsoa %>%
  filter(name == "Leicester 021")

leicester22 <- 
  leicester_lsoa %>%
  filter(name == "Leicester 022")

leicester23 <- 
  leicester_lsoa %>%
  filter(name == "Leicester 023")

leicester25 <- 
  leicester_lsoa %>%
  filter(name == "Leicester 025")

leicester26 <- 
  leicester_lsoa %>%
  filter(name == "Leicester 026")

leicester27 <- 
  leicester_lsoa %>%
  filter(name == "Leicester 027")

leicester28 <- 
  leicester_lsoa %>%
  filter(name == "Leicester 028")

leicester29 <- 
  leicester_lsoa %>%
  filter(name == "Leicester 029")

leicester30 <- 
  leicester_lsoa %>%
  filter(name == "Leicester 030")

leicester31 <- 
  leicester_lsoa %>%
  filter(name == "Leicester 031")

leicester32 <- 
  leicester_lsoa %>%
  filter(name == "Leicester 032")

leicester34 <- 
  leicester_lsoa %>%
  filter(name == "Leicester 034")

leicester35 <- 
  leicester_lsoa %>%
  filter(name == "Leicester 035")

leicester36 <- 
  leicester_lsoa %>%
  filter(name == "Leicester 036")

leicester37 <- 
  leicester_lsoa %>%
  filter(name == "Leicester 037")

leicester38 <- 
  leicester_lsoa %>%
  filter(name == "Leicester 038")

leicester39 <- 
  leicester_lsoa %>%
  filter(name == "Leicester 039")

leicester40 <- 
  leicester_lsoa %>%
  filter(name == "Leicester 040")

leicester41 <- 
  leicester_lsoa %>%
  filter(name == "Leicester 041")

leicester01_build_os <-
  leicester_build_os %>%
  filter(st_contains(leicester01, ., sparse = FALSE))

leicester02_build_os <-
  leicester_build_os %>%
  filter(st_contains(leicester02, ., sparse = FALSE))

leicester03_build_os <-
  leicester_build_os %>%
  filter(st_contains(leicester03, ., sparse = FALSE))

leicester04_build_os <-
  leicester_build_os %>%
  filter(st_contains(leicester04, ., sparse = FALSE))

leicester05_build_os <-
  leicester_build_os %>%
  filter(st_contains(leicester05, ., sparse = FALSE))

leicester06_build_os <-
  leicester_build_os %>%
  filter(st_contains(leicester06, ., sparse = FALSE))

leicester07_build_os <-
  leicester_build_os %>%
  filter(st_contains(leicester07, ., sparse = FALSE))

leicester08_build_os <-
  leicester_build_os %>%
  filter(st_contains(leicester08, ., sparse = FALSE))

leicester09_build_os <-
  leicester_build_os %>%
  filter(st_contains(leicester09, ., sparse = FALSE))

leicester10_build_os <-
  leicester_build_os %>%
  filter(st_contains(leicester10, ., sparse = FALSE))

leicester11_build_os <-
  leicester_build_os %>%
  filter(st_contains(leicester11, ., sparse = FALSE))

leicester12_build_os <-
  leicester_build_os %>%
  filter(st_contains(leicester12, ., sparse = FALSE))

leicester13_build_os <-
  leicester_build_os %>%
  filter(st_contains(leicester13, ., sparse = FALSE))

leicester16_build_os <-
  leicester_build_os %>%
  filter(st_contains(leicester16, ., sparse = FALSE))

leicester17_build_os <-
  leicester_build_os %>%
  filter(st_contains(leicester17, ., sparse = FALSE))

leicester18_build_os <-
  leicester_build_os %>%
  filter(st_contains(leicester18, ., sparse = FALSE))

leicester19_build_os <-
  leicester_build_os %>%
  filter(st_contains(leicester19, ., sparse = FALSE))

leicester20_build_os <-
  leicester_build_os %>%
  filter(st_contains(leicester20, ., sparse = FALSE))

leicester21_build_os <-
  leicester_build_os %>%
  filter(st_contains(leicester21, ., sparse = FALSE))

leicester22_build_os <-
  leicester_build_os %>%
  filter(st_contains(leicester22, ., sparse = FALSE))

leicester23_build_os <-
  leicester_build_os %>%
  filter(st_contains(leicester23, ., sparse = FALSE))

leicester25_build_os <-
  leicester_build_os %>%
  filter(st_contains(leicester25, ., sparse = FALSE))

leicester26_build_os <-
  leicester_build_os %>%
  filter(st_contains(leicester26, ., sparse = FALSE))

leicester27_build_os <-
  leicester_build_os %>%
  filter(st_contains(leicester27, ., sparse = FALSE))

leicester28_build_os <-
  leicester_build_os %>%
  filter(st_contains(leicester28, ., sparse = FALSE))

leicester29_build_os <-
  leicester_build_os %>%
  filter(st_contains(leicester29, ., sparse = FALSE))

leicester30_build_os <-
  leicester_build_os %>%
  filter(st_contains(leicester30, ., sparse = FALSE))

leicester31_build_os <-
  leicester_build_os %>%
  filter(st_contains(leicester31, ., sparse = FALSE))

leicester32_build_os <-
  leicester_build_os %>%
  filter(st_contains(leicester32, ., sparse = FALSE))

leicester34_build_os <-
  leicester_build_os %>%
  filter(st_contains(leicester34, ., sparse = FALSE))

leicester35_build_os <-
  leicester_build_os %>%
  filter(st_contains(leicester35, ., sparse = FALSE))

leicester36_build_os <-
  leicester_build_os %>%
  filter(st_contains(leicester36, ., sparse = FALSE))

leicester37_build_os <-
  leicester_build_os %>%
  filter(st_contains(leicester37, ., sparse = FALSE))

leicester38_build_os <-
  leicester_build_os %>%
  filter(st_contains(leicester38, ., sparse = FALSE))

leicester39_build_os <-
  leicester_build_os %>%
  filter(st_contains(leicester39, ., sparse = FALSE))

leicester40_build_os <-
  leicester_build_os %>%
  filter(st_contains(leicester40, ., sparse = FALSE))

leicester41_build_os <-
  leicester_build_os %>%
  filter(st_contains(leicester41, ., sparse = FALSE))

leicester01_build_osm <-
  leicester_build_osm %>%
  filter(st_contains(leicester01, ., sparse = FALSE))

leicester02_build_osm <-
  leicester_build_osm %>%
  filter(st_contains(leicester02, ., sparse = FALSE))

leicester03_build_osm <-
  leicester_build_osm %>%
  filter(st_contains(leicester03, ., sparse = FALSE))

leicester04_build_osm <-
  leicester_build_osm %>%
  filter(st_contains(leicester04, ., sparse = FALSE))

leicester05_build_osm <-
  leicester_build_osm %>%
  filter(st_contains(leicester05, ., sparse = FALSE))

leicester06_build_osm <-
  leicester_build_osm %>%
  filter(st_contains(leicester06, ., sparse = FALSE))

leicester07_build_osm <-
  leicester_build_osm %>%
  filter(st_contains(leicester07, ., sparse = FALSE))

leicester08_build_osm <-
  leicester_build_osm %>%
  filter(st_contains(leicester08, ., sparse = FALSE))

leicester09_build_osm <-
  leicester_build_osm %>%
  filter(st_contains(leicester09, ., sparse = FALSE))

leicester10_build_osm <-
  leicester_build_osm %>%
  filter(st_contains(leicester10, ., sparse = FALSE))

leicester11_build_osm <-
  leicester_build_osm %>%
  filter(st_contains(leicester11, ., sparse = FALSE))

leicester12_build_osm <-
  leicester_build_osm %>%
  filter(st_contains(leicester12, ., sparse = FALSE))

leicester13_build_osm <-
  leicester_build_osm %>%
  filter(st_contains(leicester13, ., sparse = FALSE))

leicester16_build_osm <-
  leicester_build_osm %>%
  filter(st_contains(leicester16, ., sparse = FALSE))

leicester17_build_osm <-
  leicester_build_osm %>%
  filter(st_contains(leicester17, ., sparse = FALSE))

leicester18_build_osm <-
  leicester_build_osm %>%
  filter(st_contains(leicester18, ., sparse = FALSE))

leicester19_build_osm <-
  leicester_build_osm %>%
  filter(st_contains(leicester19, ., sparse = FALSE))

leicester20_build_osm <-
  leicester_build_osm %>%
  filter(st_contains(leicester20, ., sparse = FALSE))

leicester21_build_osm <-
  leicester_build_osm %>%
  filter(st_contains(leicester21, ., sparse = FALSE))

leicester22_build_osm <-
  leicester_build_osm %>%
  filter(st_contains(leicester22, ., sparse = FALSE))

leicester23_build_osm <-
  leicester_build_osm %>%
  filter(st_contains(leicester23, ., sparse = FALSE))

leicester25_build_osm <-
  leicester_build_osm %>%
  filter(st_contains(leicester25, ., sparse = FALSE))

leicester26_build_osm <-
  leicester_build_osm %>%
  filter(st_contains(leicester26, ., sparse = FALSE))

leicester27_build_osm <-
  leicester_build_osm %>%
  filter(st_contains(leicester27, ., sparse = FALSE))

leicester28_build_osm <-
  leicester_build_osm %>%
  filter(st_contains(leicester28, ., sparse = FALSE))

leicester29_build_osm <-
  leicester_build_osm %>%
  filter(st_contains(leicester29, ., sparse = FALSE))

leicester30_build_osm <-
  leicester_build_osm %>%
  filter(st_contains(leicester30, ., sparse = FALSE))

leicester31_build_osm <-
  leicester_build_osm %>%
  filter(st_contains(leicester31, ., sparse = FALSE))

leicester32_build_osm <-
  leicester_build_osm %>%
  filter(st_contains(leicester32, ., sparse = FALSE))

leicester34_build_osm <-
  leicester_build_osm %>%
  filter(st_contains(leicester34, ., sparse = FALSE))

leicester35_build_osm <-
  leicester_build_osm %>%
  filter(st_contains(leicester35, ., sparse = FALSE))

leicester36_build_osm <-
  leicester_build_osm %>%
  filter(st_contains(leicester36, ., sparse = FALSE))

leicester37_build_osm <-
  leicester_build_osm %>%
  filter(st_contains(leicester37, ., sparse = FALSE))

leicester38_build_osm <-
  leicester_build_osm %>%
  filter(st_contains(leicester38, ., sparse = FALSE))

leicester39_build_osm <-
  leicester_build_osm %>%
  filter(st_contains(leicester39, ., sparse = FALSE))

leicester40_build_osm <-
  leicester_build_osm %>%
  filter(st_contains(leicester40, ., sparse = FALSE))

leicester41_build_osm <-
  leicester_build_osm %>%
  filter(st_contains(leicester41, ., sparse = FALSE))

chelsea_lsoa <-
  lsoa %>%
  filter(st_contains(chelsea_bound, ., sparse = FALSE))

chelsea01 <-
  chelsea_lsoa %>%
  filter(name == "Kensington and Chelsea 001")

chelsea02 <-
  chelsea_lsoa %>%
  filter(name == "Kensington and Chelsea 002")

chelsea03 <-
  chelsea_lsoa %>%
  filter(name == "Kensington and Chelsea 003")

chelsea04 <-
  chelsea_lsoa %>%
  filter(name == "Kensington and Chelsea 004")

chelsea05 <-
  chelsea_lsoa %>%
  filter(name == "Kensington and Chelsea 005")

chelsea06 <-
  chelsea_lsoa %>%
  filter(name == "Kensington and Chelsea 006")

chelsea07 <-
  chelsea_lsoa %>%
  filter(name == "Kensington and Chelsea 007")

chelsea08 <-
  chelsea_lsoa %>%
  filter(name == "Kensington and Chelsea 008")

chelsea09 <-
  chelsea_lsoa %>%
  filter(name == "Kensington and Chelsea 009")

chelsea10 <-
  chelsea_lsoa %>%
  filter(name == "Kensington and Chelsea 010")

chelsea11 <-
  chelsea_lsoa %>%
  filter(name == "Kensington and Chelsea 011")

chelsea12 <-
  chelsea_lsoa %>%
  filter(name == "Kensington and Chelsea 012")

chelsea13 <-
  chelsea_lsoa %>%
  filter(name == "Kensington and Chelsea 013")

chelsea14 <-
  chelsea_lsoa %>%
  filter(name == "Kensington and Chelsea 014")

chelsea15 <-
  chelsea_lsoa %>%
  filter(name == "Kensington and Chelsea 015")

chelsea16 <-
  chelsea_lsoa %>%
  filter(name == "Kensington and Chelsea 016")

chelsea17 <-
  chelsea_lsoa %>%
  filter(name == "Kensington and Chelsea 017")

chelsea18 <-
  chelsea_lsoa %>%
  filter(name == "Kensington and Chelsea 018")

chelsea19 <-
  chelsea_lsoa %>%
  filter(name == "Kensington and Chelsea 019")

chelsea20 <-
  chelsea_lsoa %>%
  filter(name == "Kensington and Chelsea 020")

chelsea21 <-
  chelsea_lsoa %>%
  filter(name == "Kensington and Chelsea 021")

chelsea01_build_os <-
  chelsea_build_os %>%
  filter(st_contains(chelsea01, ., sparse = FALSE))

chelsea02_build_os <-
  chelsea_build_os %>%
  filter(st_contains(chelsea02, ., sparse = FALSE))

chelsea03_build_os <-
  chelsea_build_os %>%
  filter(st_contains(chelsea03, ., sparse = FALSE))

chelsea04_build_os <-
  chelsea_build_os %>%
  filter(st_contains(chelsea04, ., sparse = FALSE))

chelsea05_build_os <-
  chelsea_build_os %>%
  filter(st_contains(chelsea05, ., sparse = FALSE))

chelsea06_build_os <-
  chelsea_build_os %>%
  filter(st_contains(chelsea06, ., sparse = FALSE))

chelsea07_build_os <-
  chelsea_build_os %>%
  filter(st_contains(chelsea07, ., sparse = FALSE))

chelsea08_build_os <-
  chelsea_build_os %>%
  filter(st_contains(chelsea08, ., sparse = FALSE))

chelsea09_build_os <-
  chelsea_build_os %>%
  filter(st_contains(chelsea09, ., sparse = FALSE))

chelsea10_build_os <-
  chelsea_build_os %>%
  filter(st_contains(chelsea10, ., sparse = FALSE))

chelsea11_build_os <-
  chelsea_build_os %>%
  filter(st_contains(chelsea11, ., sparse = FALSE))

chelsea12_build_os <-
  chelsea_build_os %>%
  filter(st_contains(chelsea12, ., sparse = FALSE))

chelsea13_build_os <-
  chelsea_build_os %>%
  filter(st_contains(chelsea13, ., sparse = FALSE))

chelsea14_build_os <-
  chelsea_build_os %>%
  filter(st_contains(chelsea14, ., sparse = FALSE))

chelsea15_build_os <-
  chelsea_build_os %>%
  filter(st_contains(chelsea15, ., sparse = FALSE))

chelsea16_build_os <-
  chelsea_build_os %>%
  filter(st_contains(chelsea16, ., sparse = FALSE))

chelsea17_build_os <-
  chelsea_build_os %>%
  filter(st_contains(chelsea17, ., sparse = FALSE))

chelsea18_build_os <-
  chelsea_build_os %>%
  filter(st_contains(chelsea18, ., sparse = FALSE))

chelsea19_build_os <-
  chelsea_build_os %>%
  filter(st_contains(chelsea19, ., sparse = FALSE))

chelsea20_build_os <-
  chelsea_build_os %>%
  filter(st_contains(chelsea20, ., sparse = FALSE))

chelsea21_build_os <-
  chelsea_build_os %>%
  filter(st_contains(chelsea21, ., sparse = FALSE))

chelsea01_build_osm <-
  chelsea_build_osm %>%
  filter(st_contains(chelsea01, ., sparse = FALSE))

chelsea02_build_osm <-
  chelsea_build_osm %>%
  filter(st_contains(chelsea02, ., sparse = FALSE))

chelsea03_build_osm <-
  chelsea_build_osm %>%
  filter(st_contains(chelsea03, ., sparse = FALSE))

chelsea04_build_osm <-
  chelsea_build_osm %>%
  filter(st_contains(chelsea04, ., sparse = FALSE))

chelsea05_build_osm <-
  chelsea_build_osm %>%
  filter(st_contains(chelsea05, ., sparse = FALSE))

chelsea06_build_osm <-
  chelsea_build_osm %>%
  filter(st_contains(chelsea06, ., sparse = FALSE))

chelsea07_build_osm <-
  chelsea_build_osm %>%
  filter(st_contains(chelsea07, ., sparse = FALSE))

chelsea08_build_osm <-
  chelsea_build_osm %>%
  filter(st_contains(chelsea08, ., sparse = FALSE))

chelsea09_build_osm <-
  chelsea_build_osm %>%
  filter(st_contains(chelsea09, ., sparse = FALSE))

chelsea10_build_osm <-
  chelsea_build_osm %>%
  filter(st_contains(chelsea10, ., sparse = FALSE))

chelsea11_build_osm <-
  chelsea_build_osm %>%
  filter(st_contains(chelsea11, ., sparse = FALSE))

chelsea12_build_osm <-
  chelsea_build_osm %>%
  filter(st_contains(chelsea12, ., sparse = FALSE))

chelsea13_build_osm <-
  chelsea_build_osm %>%
  filter(st_contains(chelsea13, ., sparse = FALSE))

chelsea14_build_osm <-
  chelsea_build_osm %>%
  filter(st_contains(chelsea14, ., sparse = FALSE))

chelsea15_build_osm <-
  chelsea_build_osm %>%
  filter(st_contains(chelsea15, ., sparse = FALSE))

chelsea16_build_osm <-
  chelsea_build_osm %>%
  filter(st_contains(chelsea16, ., sparse = FALSE))

chelsea17_build_osm <-
  chelsea_build_osm %>%
  filter(st_contains(chelsea17, ., sparse = FALSE))

chelsea18_build_osm <-
  chelsea_build_osm %>%
  filter(st_contains(chelsea18, ., sparse = FALSE))

chelsea19_build_osm <-
  chelsea_build_osm %>%
  filter(st_contains(chelsea19, ., sparse = FALSE))

chelsea20_build_osm <-
  chelsea_build_osm %>%
  filter(st_contains(chelsea20, ., sparse = FALSE))

chelsea21_build_osm <-
  chelsea_build_osm %>%
  filter(st_contains(chelsea21, ., sparse = FALSE))

west_lsoa <- 
  lsoa %>%
  filter(st_contains(westminster_bound, ., sparse = FALSE))

west01 <-
  west_lsoa %>%
  filter(name == "Westminster 001")

west02 <-
  west_lsoa %>%
  filter(name == "Westminster 002")

west03 <-
  west_lsoa %>%
  filter(name == "Westminster 003")

west04 <-
  west_lsoa %>%
  filter(name == "Westminster 004")

west05 <-
  west_lsoa %>%
  filter(name == "Westminster 005")

west06 <-
  west_lsoa %>%
  filter(name == "Westminster 006")

west07 <-
  west_lsoa %>%
  filter(name == "Westminster 007")

west08 <-
  west_lsoa %>%
  filter(name == "Westminster 008")

west09 <-
  west_lsoa %>%
  filter(name == "Westminster 009")

west10 <-
  west_lsoa %>%
  filter(name == "Westminster 010")

west11 <-
  west_lsoa %>%
  filter(name == "Westminster 011")

west12 <-
  west_lsoa %>%
  filter(name == "Westminster 012")

west13 <-
  west_lsoa %>%
  filter(name == "Westminster 013")

west14 <-
  west_lsoa %>%
  filter(name == "Westminster 014")

west15 <-
  west_lsoa %>%
  filter(name == "Westminster 015")

west16 <-
  west_lsoa %>%
  filter(name == "Westminster 016")

west17 <-
  west_lsoa %>%
  filter(name == "Westminster 017")

west18 <-
  west_lsoa %>%
  filter(name == "Westminster 018")

west19 <-
  west_lsoa %>%
  filter(name == "Westminster 019")

west20 <-
  west_lsoa %>%
  filter(name == "Westminster 020")

west21 <-
  west_lsoa %>%
  filter(name == "Westminster 021")

west22 <-
  west_lsoa %>%
  filter(name == "Westminster 022")

west23 <-
  west_lsoa %>%
  filter(name == "Westminster 023")

west24 <-
  west_lsoa %>%
  filter(name == "Westminster 024")

west01_build_os <-
  west_build_os %>%
  filter(st_contains(west01, ., sparse = FALSE))

west02_build_os <-
  west_build_os %>%
  filter(st_contains(west02, ., sparse = FALSE))

west03_build_os <-
  west_build_os %>%
  filter(st_contains(west03, ., sparse = FALSE))

west04_build_os <-
  west_build_os %>%
  filter(st_contains(west04, ., sparse = FALSE))

west05_build_os <-
  west_build_os %>%
  filter(st_contains(west05, ., sparse = FALSE))

west06_build_os <-
  west_build_os %>%
  filter(st_contains(west06, ., sparse = FALSE))

west07_build_os <-
  west_build_os %>%
  filter(st_contains(west07, ., sparse = FALSE))

west08_build_os <-
  west_build_os %>%
  filter(st_contains(west08, ., sparse = FALSE))

west09_build_os <-
  west_build_os %>%
  filter(st_contains(west09, ., sparse = FALSE))

west10_build_os <-
  west_build_os %>%
  filter(st_contains(west10, ., sparse = FALSE))

west11_build_os <-
  west_build_os %>%
  filter(st_contains(west11, ., sparse = FALSE))

west12_build_os <-
  west_build_os %>%
  filter(st_contains(west12, ., sparse = FALSE))

west13_build_os <-
  west_build_os %>%
  filter(st_contains(west13, ., sparse = FALSE))

west14_build_os <-
  west_build_os %>%
  filter(st_contains(west14, ., sparse = FALSE))

west15_build_os <-
  west_build_os %>%
  filter(st_contains(west15, ., sparse = FALSE))

west16_build_os <-
  west_build_os %>%
  filter(st_contains(west16, ., sparse = FALSE))

west17_build_os <-
  west_build_os %>%
  filter(st_contains(west17, ., sparse = FALSE))

west18_build_os <-
  west_build_os %>%
  filter(st_contains(west18, ., sparse = FALSE))

west19_build_os <-
  west_build_os %>%
  filter(st_contains(west19, ., sparse = FALSE))

west20_build_os <-
  west_build_os %>%
  filter(st_contains(west20, ., sparse = FALSE))

west21_build_os <-
  west_build_os %>%
  filter(st_contains(west21, ., sparse = FALSE))

west22_build_os <-
  west_build_os %>%
  filter(st_contains(west22, ., sparse = FALSE))

west23_build_os <-
  west_build_os %>%
  filter(st_contains(west23, ., sparse = FALSE))

west24_build_os <-
  west_build_os %>%
  filter(st_contains(west24, ., sparse = FALSE))

west01_build_osm <-
  west_build_osm %>%
  filter(st_contains(west01, ., sparse = FALSE))

west02_build_osm <-
  west_build_osm %>%
  filter(st_contains(west02, ., sparse = FALSE))

west03_build_osm <-
  west_build_osm %>%
  filter(st_contains(west03, ., sparse = FALSE))

west04_build_osm <-
  west_build_osm %>%
  filter(st_contains(west04, ., sparse = FALSE))

west05_build_osm <-
  west_build_osm %>%
  filter(st_contains(west05, ., sparse = FALSE))

west06_build_osm <-
  west_build_osm %>%
  filter(st_contains(west06, ., sparse = FALSE))

west07_build_osm <-
  west_build_osm %>%
  filter(st_contains(west07, ., sparse = FALSE))

west08_build_osm <-
  west_build_osm %>%
  filter(st_contains(west08, ., sparse = FALSE))

west09_build_osm <-
  west_build_osm %>%
  filter(st_contains(west09, ., sparse = FALSE))

west10_build_osm <-
  west_build_osm %>%
  filter(st_contains(west10, ., sparse = FALSE))

west11_build_osm <-
  west_build_osm %>%
  filter(st_contains(west11, ., sparse = FALSE))

west12_build_osm <-
  west_build_osm %>%
  filter(st_contains(west12, ., sparse = FALSE))

west13_build_osm <-
  west_build_osm %>%
  filter(st_contains(west13, ., sparse = FALSE))

west14_build_osm <-
  west_build_osm %>%
  filter(st_contains(west14, ., sparse = FALSE))

west15_build_osm <-
  west_build_osm %>%
  filter(st_contains(west15, ., sparse = FALSE))

west16_build_osm <-
  west_build_osm %>%
  filter(st_contains(west16, ., sparse = FALSE))

west17_build_osm <-
  west_build_osm %>%
  filter(st_contains(west17, ., sparse = FALSE))

west18_build_osm <-
  west_build_osm %>%
  filter(st_contains(west18, ., sparse = FALSE))

west19_build_osm <-
  west_build_osm %>%
  filter(st_contains(west19, ., sparse = FALSE))

west20_build_osm <-
  west_build_osm %>%
  filter(st_contains(west20, ., sparse = FALSE))

west21_build_osm <-
  west_build_osm %>%
  filter(st_contains(west21, ., sparse = FALSE))

west22_build_osm <-
  west_build_osm %>%
  filter(st_contains(west22, ., sparse = FALSE))

west23_build_osm <-
  west_build_osm %>%
  filter(st_contains(west23, ., sparse = FALSE))

west24_build_osm <-
  west_build_osm %>%
  filter(st_contains(west24, ., sparse = FALSE))

#--------------------------------------------------------------------------------------------------------------------------

#-------------
#COMPLETENESS
#-------------

#---------------
#City of London
#---------------

#The following command lines calculate the covered area of the building surfaces in the OSM and OS datasets
#of the studying areas.

colondon_area_diff <-
  matrix(
    data = c(sum(st_area(cityoflondon_build_os)) - sum(st_area(cityoflondon_build_osm))),
    ncol = 1,
    dimnames = list(
      c("City_of_London"),
      c("Area_Diff")
    )
  )

#The completeness is measured by calcaulating the difference between the surface covered area of OS and OSM respectevily,
#and then the percentage of the area that OSM covers compared to the OS buildings surface area.

colondon_area_perc <-
  matrix(
    data = c((sum(st_area(cityoflondon_build_osm)) / sum(st_area(cityoflondon_build_os))) * 100),
    ncol = 1,
    dimnames = list(
      c("City_of_London"),
      c("Area_Diff_Perc")
    )
  )

#This command returns all the objects that are included in the specified dataset.

colondon_build_number <-
  matrix(
    data = c(length(cityoflondon_build_osm$osm_id)),
    ncol = 1,
    dimnames = list(
      c("City_of_london"),
      c("Buildings/LSOA")
    )
  )

#To count the objects that are not assigned with a name attribute in the OSM dataset.

colondon_name_included <-
  matrix(
    data = c(((cityoflondon_build_osm %>%
               filter(!is.na(name)) %>%
               nrow()) / length(cityoflondon_build_osm$osm_id)) * 100),
    ncol = 1,
    dimnames = list(
      c("City_of_London"),
      c("Name_Included")))

#To count the objects that are not assigned with a type attribute in the OSM dataset.

colondon_type_included <-
  matrix(
    data = c(((cityoflondon_build_osm %>%
                filter(!is.na(type)) %>%
                nrow()) / length(cityoflondon_build_osm$osm_id)) * 100),
    ncol = 1,
    dimnames = list(
      c("City_of_London"),
      c("Type_Included")))

#To count the objects that are not assigned with a name and type attribute in the OSM dataset.

colondon_name_type_included <-
  matrix(
    data = c(((cityoflondon_build_osm %>%
                filter(!is.na(name) & !is.na(type)) %>%
                nrow()) / length(cityoflondon_build_osm$osm_id)) *100),
    ncol = 1,
    dimnames = list(
      c("City_of_London"),
      c("Name_Type_Included")))

colondon_complete <-
  cbind(
    colondon_area_diff,
    colondon_area_perc,
    colondon_build_number,
    colondon_name_included,
    colondon_type_included,
    colondon_name_type_included
  )

#--------
#Chelsea
#--------

#The completeness is measured by calcaulating the difference between the surface covered area of OS and OSM respectevily,
#and then the percentage of the area that OSM covers compared to the OS buildings surface area.

che_lsoa_names <- 
  matrix(
    data = c("LSOA01", "LSOA02", "LSOA03", "LSOA04", "LSOA05", "LSOA06", "LSOA07", "LSOA08", "LSOA09", 
             "LSOA10", "LSOA11", "LSOA12", "LSOA13", "LSOA14", "LSOA15", "LSOA16", "LSOA17", "LSOA18", 
             "LSOA19", "LSOA20", "LSOA21"),
    ncol = 1,
    dimnames = list(
      c("LSOA01", "LSOA02", "LSOA03", "LSOA04", "LSOA05", "LSOA06", "LSOA07", "LSOA08", "LSOA09", 
        "LSOA10", "LSOA11", "LSOA12", "LSOA13", "LSOA14", "LSOA15", "LSOA16", "LSOA17", "LSOA18", 
        "LSOA19", "LSOA20", "LSOA21"),
      c("LSOA"))
  )

che_area_diff <-
  matrix(
    data = c(sum(st_area(chelsea01_build_os)) - sum(st_area(chelsea01_build_osm)),
    sum(st_area(chelsea02_build_os)) - sum(st_area(chelsea02_build_osm)),
    sum(st_area(chelsea03_build_os)) - sum(st_area(chelsea03_build_osm)),
    sum(st_area(chelsea04_build_os)) - sum(st_area(chelsea04_build_osm)),
    sum(st_area(chelsea05_build_os)) - sum(st_area(chelsea05_build_osm)),
    sum(st_area(chelsea06_build_os)) - sum(st_area(chelsea06_build_osm)),
    sum(st_area(chelsea07_build_os)) - sum(st_area(chelsea07_build_osm)),
    sum(st_area(chelsea08_build_os)) - sum(st_area(chelsea08_build_osm)),
    sum(st_area(chelsea09_build_os)) - sum(st_area(chelsea09_build_osm)),
    sum(st_area(chelsea10_build_os)) - sum(st_area(chelsea10_build_osm)),
    sum(st_area(chelsea11_build_os)) - sum(st_area(chelsea11_build_osm)),
    sum(st_area(chelsea12_build_os)) - sum(st_area(chelsea12_build_osm)),
    sum(st_area(chelsea13_build_os)) - sum(st_area(chelsea13_build_osm)),
    sum(st_area(chelsea14_build_os)) - sum(st_area(chelsea14_build_osm)),
    sum(st_area(chelsea15_build_os)) - sum(st_area(chelsea15_build_osm)),
    sum(st_area(chelsea16_build_os)) - sum(st_area(chelsea16_build_osm)),
    sum(st_area(chelsea17_build_os)) - sum(st_area(chelsea17_build_osm)),
    sum(st_area(chelsea18_build_os)) - sum(st_area(chelsea18_build_osm)),
    sum(st_area(chelsea19_build_os)) - sum(st_area(chelsea19_build_osm)),
    sum(st_area(chelsea20_build_os)) - sum(st_area(chelsea20_build_osm)),
    sum(st_area(chelsea21_build_os)) - sum(st_area(chelsea21_build_osm))),
    ncol = 1,
    dimnames = list(
      c("LSOA01", "LSOA02", "LSOA03", "LSOA04", "LSOA05", "LSOA06", "LSOA07", "LSOA08", "LSOA09", 
        "LSOA10", "LSOA11", "LSOA12", "LSOA13", "LSOA14", "LSOA15", "LSOA16", "LSOA17", "LSOA18", 
        "LSOA19", "LSOA20", "LSOA21"),
      c("Area_Diff")
    )
  )

che_area_perc <-
  matrix(
    data = c((sum(st_area(chelsea01_build_osm)) / sum(st_area(chelsea01_build_os))) * 100,
             (sum(st_area(chelsea02_build_osm)) / sum(st_area(chelsea02_build_os))) * 100,
             (sum(st_area(chelsea03_build_osm)) / sum(st_area(chelsea03_build_os))) * 100,
             (sum(st_area(chelsea04_build_osm)) / sum(st_area(chelsea04_build_os))) * 100,
             (sum(st_area(chelsea05_build_osm)) / sum(st_area(chelsea05_build_os))) * 100,
             (sum(st_area(chelsea06_build_osm)) / sum(st_area(chelsea06_build_os))) * 100,
             (sum(st_area(chelsea07_build_osm)) / sum(st_area(chelsea07_build_os))) * 100,
             (sum(st_area(chelsea08_build_osm)) / sum(st_area(chelsea08_build_os))) * 100,
             (sum(st_area(chelsea09_build_osm)) / sum(st_area(chelsea09_build_os))) * 100,
             (sum(st_area(chelsea10_build_osm)) / sum(st_area(chelsea10_build_os))) * 100,
             (sum(st_area(chelsea11_build_osm)) / sum(st_area(chelsea11_build_os))) * 100,
             (sum(st_area(chelsea12_build_osm)) / sum(st_area(chelsea12_build_os))) * 100,
             (sum(st_area(chelsea13_build_osm)) / sum(st_area(chelsea13_build_os))) * 100,
             (sum(st_area(chelsea14_build_osm)) / sum(st_area(chelsea14_build_os))) * 100,
             (sum(st_area(chelsea15_build_osm)) / sum(st_area(chelsea15_build_os))) * 100,
             (sum(st_area(chelsea16_build_osm)) / sum(st_area(chelsea16_build_os))) * 100,
             (sum(st_area(chelsea17_build_osm)) / sum(st_area(chelsea17_build_os))) * 100,
             (sum(st_area(chelsea18_build_osm)) / sum(st_area(chelsea18_build_os))) * 100,
             (sum(st_area(chelsea19_build_osm)) / sum(st_area(chelsea19_build_os))) * 100,
             (sum(st_area(chelsea20_build_osm)) / sum(st_area(chelsea20_build_os))) * 100,
             (sum(st_area(chelsea21_build_osm)) / sum(st_area(chelsea21_build_os))) * 100),
             ncol = 1,
             dimnames = list(
               c("LSOA01", "LSOA02", "LSOA03", "LSOA04", "LSOA05", "LSOA06", "LSOA07", "LSOA08", "LSOA09", 
                 "LSOA10", "LSOA11", "LSOA12", "LSOA13", "LSOA14", "LSOA15", "LSOA16", "LSOA17", "LSOA18", 
                 "LSOA19", "LSOA20", "LSOA21"),
               c("Area_Diff_Perc")
             )
  )

#This command returns all the objects that are included in the specified dataset.

che_build_number <- 
  matrix(
    data = c(length(chelsea01_build_osm$osm_id),
             length(chelsea02_build_osm$osm_id),
             length(chelsea03_build_osm$osm_id),
             length(chelsea04_build_osm$osm_id),
             length(chelsea05_build_osm$osm_id),
             length(chelsea06_build_osm$osm_id),
             length(chelsea07_build_osm$osm_id),
             length(chelsea08_build_osm$osm_id),
             length(chelsea09_build_osm$osm_id),
             length(chelsea10_build_osm$osm_id),
             length(chelsea11_build_osm$osm_id),
             length(chelsea12_build_osm$osm_id),
             length(chelsea13_build_osm$osm_id),
             length(chelsea14_build_osm$osm_id),
             length(chelsea15_build_osm$osm_id),
             length(chelsea16_build_osm$osm_id),
             length(chelsea17_build_osm$osm_id),
             length(chelsea18_build_osm$osm_id),
             length(chelsea19_build_osm$osm_id),
             length(chelsea20_build_osm$osm_id),
             length(chelsea21_build_osm$osm_id)),
    ncol = 1,
    dimnames = list(
      c("LSOA01", "LSOA02", "LSOA03", "LSOA04", "LSOA05", "LSOA06", "LSOA07", "LSOA08", "LSOA09", 
        "LSOA10", "LSOA11", "LSOA12", "LSOA13", "LSOA14", "LSOA15", "LSOA16", "LSOA17", "LSOA18", 
        "LSOA19", "LSOA20", "LSOA21"),
      c("Buildings/LSOA")
  ))


#To count the objects that are not assigned with a name attribute in the OSM dataset.

che_name_included_perc <-
  matrix(
    data = c(((chelsea01_build_osm %>%
               filter(!is.na(name)) %>%
               nrow()) / length(chelsea01_build_osm$osm_id)) * 100,
             ((chelsea02_build_osm %>%
                 filter(!is.na(name)) %>%
                 nrow()) / length(chelsea02_build_osm$osm_id)) * 100,
             ((chelsea03_build_osm %>%
                 filter(!is.na(name)) %>%
                 nrow()) / length(chelsea03_build_osm$osm_id)) * 100,
             ((chelsea04_build_osm %>%
                 filter(!is.na(name)) %>%
                 nrow()) / length(chelsea04_build_osm$osm_id)) * 100,
             ((chelsea05_build_osm %>%
                 filter(!is.na(name)) %>%
                 nrow()) / length(chelsea05_build_osm$osm_id)) * 100,
             ((chelsea06_build_osm %>%
                 filter(!is.na(name)) %>%
                 nrow()) / length(chelsea06_build_osm$osm_id)) * 100,
             ((chelsea07_build_osm %>%
                 filter(!is.na(name)) %>%
                 nrow()) / length(chelsea07_build_osm$osm_id)) * 100,
             ((chelsea08_build_osm %>%
                 filter(!is.na(name)) %>%
                 nrow()) / length(chelsea08_build_osm$osm_id)) * 100,
             ((chelsea09_build_osm %>%
                 filter(!is.na(name)) %>%
                 nrow()) / length(chelsea09_build_osm$osm_id)) * 100,
             ((chelsea10_build_osm %>%
                 filter(!is.na(name)) %>%
                 nrow()) / length(chelsea10_build_osm$osm_id)) * 100,
             ((chelsea11_build_osm %>%
                 filter(!is.na(name)) %>%
                 nrow()) / length(chelsea11_build_osm$osm_id)) * 100,
             ((chelsea12_build_osm %>%
                 filter(!is.na(name)) %>%
                 nrow()) / length(chelsea12_build_osm$osm_id)) * 100,
             ((chelsea13_build_osm %>%
                 filter(!is.na(name)) %>%
                 nrow()) / length(chelsea13_build_osm$osm_id)) * 100,
             ((chelsea14_build_osm %>%
                 filter(!is.na(name)) %>%
                 nrow()) / length(chelsea14_build_osm$osm_id)) * 100,
             ((chelsea15_build_osm %>%
                 filter(!is.na(name)) %>%
                 nrow()) / length(chelsea15_build_osm$osm_id)) * 100,
             ((chelsea16_build_osm %>%
                 filter(!is.na(name)) %>%
                 nrow()) / length(chelsea16_build_osm$osm_id)) * 100,
             ((chelsea17_build_osm %>%
                 filter(!is.na(name)) %>%
                 nrow()) / length(chelsea17_build_osm$osm_id)) * 100,
             ((chelsea18_build_osm %>%
                 filter(!is.na(name)) %>%
                 nrow()) / length(chelsea18_build_osm$osm_id)) * 100,
             ((chelsea19_build_osm %>%
                 filter(!is.na(name)) %>%
                 nrow()) / length(chelsea19_build_osm$osm_id)) * 100,
             ((chelsea20_build_osm %>%
                 filter(!is.na(name)) %>%
                 nrow()) / length(chelsea20_build_osm$osm_id)) * 100,
             ((chelsea21_build_osm %>%
                 filter(!is.na(name)) %>%
                 nrow()) / length(chelsea21_build_osm$osm_id)) * 100),
    ncol = 1,
    dimnames = list(
      c("LSOA01", "LSOA02", "LSOA03", "LSOA04", "LSOA05", "LSOA06", "LSOA07", "LSOA08", "LSOA09", 
        "LSOA10", "LSOA11", "LSOA12", "LSOA13", "LSOA14", "LSOA15", "LSOA16", "LSOA17", "LSOA18", 
        "LSOA19", "LSOA20", "LSOA21"),
      c("Name_Included_Perc")
  ))

#To count the objects that are not assigned with a type attribute in the OSM dataset.

che_type_included_perc <-
  matrix(
    data = c(((chelsea01_build_osm %>%
                 filter(!is.na(type)) %>%
                 nrow()) / length(chelsea01_build_osm$osm_id)) * 100,
             ((chelsea02_build_osm %>%
                 filter(!is.na(type)) %>%
                 nrow()) / length(chelsea02_build_osm$osm_id)) * 100,
             ((chelsea03_build_osm %>%
                 filter(!is.na(type)) %>%
                 nrow()) / length(chelsea03_build_osm$osm_id)) * 100,
             ((chelsea04_build_osm %>%
                 filter(!is.na(type)) %>%
                 nrow()) / length(chelsea04_build_osm$osm_id)) * 100,
             ((chelsea05_build_osm %>%
                 filter(!is.na(type)) %>%
                 nrow()) / length(chelsea05_build_osm$osm_id)) * 100,
             ((chelsea06_build_osm %>%
                 filter(!is.na(type)) %>%
                 nrow()) / length(chelsea06_build_osm$osm_id)) * 100,
             ((chelsea07_build_osm %>%
                 filter(!is.na(type)) %>%
                 nrow()) / length(chelsea07_build_osm$osm_id)) * 100,
             ((chelsea08_build_osm %>%
                 filter(!is.na(type)) %>%
                 nrow()) / length(chelsea08_build_osm$osm_id)) * 100,
             ((chelsea09_build_osm %>%
                 filter(!is.na(type)) %>%
                 nrow()) / length(chelsea09_build_osm$osm_id)) * 100,
             ((chelsea10_build_osm %>%
                 filter(!is.na(type)) %>%
                 nrow()) / length(chelsea10_build_osm$osm_id)) * 100,
             ((chelsea11_build_osm %>%
                 filter(!is.na(type)) %>%
                 nrow()) / length(chelsea11_build_osm$osm_id)) * 100,
             ((chelsea12_build_osm %>%
                 filter(!is.na(type)) %>%
                 nrow()) / length(chelsea12_build_osm$osm_id)) * 100,
             ((chelsea13_build_osm %>%
                 filter(!is.na(type)) %>%
                 nrow()) / length(chelsea13_build_osm$osm_id)) * 100,
             ((chelsea14_build_osm %>%
                 filter(!is.na(type)) %>%
                 nrow()) / length(chelsea14_build_osm$osm_id)) * 100,
             ((chelsea15_build_osm %>%
                 filter(!is.na(type)) %>%
                 nrow()) / length(chelsea15_build_osm$osm_id)) * 100,
             ((chelsea16_build_osm %>%
                 filter(!is.na(type)) %>%
                 nrow()) / length(chelsea16_build_osm$osm_id)) * 100,
             ((chelsea17_build_osm %>%
                 filter(!is.na(type)) %>%
                 nrow()) / length(chelsea17_build_osm$osm_id)) * 100,
             ((chelsea18_build_osm %>%
                 filter(!is.na(type)) %>%
                 nrow()) / length(chelsea18_build_osm$osm_id)) * 100,
             ((chelsea19_build_osm %>%
                 filter(!is.na(type)) %>%
                 nrow()) / length(chelsea19_build_osm$osm_id)) * 100,
             ((chelsea20_build_osm %>%
                 filter(!is.na(type)) %>%
                 nrow()) / length(chelsea20_build_osm$osm_id)) * 100,
             ((chelsea21_build_osm %>%
                 filter(!is.na(type)) %>%
                 nrow()) / length(chelsea21_build_osm$osm_id)) * 100),
    ncol = 1,
    dimnames = list(
      c("LSOA01", "LSOA02", "LSOA03", "LSOA04", "LSOA05", "LSOA06", "LSOA07", "LSOA08", "LSOA09", 
        "LSOA10", "LSOA11", "LSOA12", "LSOA13", "LSOA14", "LSOA15", "LSOA16", "LSOA17", "LSOA18", 
        "LSOA19", "LSOA20", "LSOA21"),
      c("Type_Included_Perc")
    ))

#To count the objects that are not assigned with a name and type attribute in the OSM dataset.

che_type_name_included_perc <-
  matrix(
    data = c(((chelsea01_build_osm %>%
                 filter(!is.na(name) & !is.na(type)) %>%
                 nrow()) / length(chelsea01_build_osm$osm_id)) * 100,
             ((chelsea02_build_osm %>%
                 filter(!is.na(name) & !is.na(type)) %>%
                 nrow()) / length(chelsea02_build_osm$osm_id)) * 100,
             ((chelsea03_build_osm %>%
                 filter(!is.na(name) & !is.na(type)) %>%
                 nrow()) / length(chelsea03_build_osm$osm_id)) * 100,
             ((chelsea04_build_osm %>%
                 filter(!is.na(name) & !is.na(type)) %>%
                 nrow()) / length(chelsea04_build_osm$osm_id)) * 100,
             ((chelsea05_build_osm %>%
                 filter(!is.na(name) & !is.na(type)) %>%
                 nrow()) / length(chelsea05_build_osm$osm_id)) * 100,
             ((chelsea06_build_osm %>%
                 filter(!is.na(name) & !is.na(type)) %>%
                 nrow()) / length(chelsea06_build_osm$osm_id)) * 100,
             ((chelsea07_build_osm %>%
                 filter(!is.na(name) & !is.na(type)) %>%
                 nrow()) / length(chelsea07_build_osm$osm_id)) * 100,
             ((chelsea08_build_osm %>%
                 filter(!is.na(name) & !is.na(type)) %>%
                 nrow()) / length(chelsea08_build_osm$osm_id)) * 100,
             ((chelsea09_build_osm %>%
                 filter(!is.na(name) & !is.na(type)) %>%
                 nrow()) / length(chelsea09_build_osm$osm_id)) * 100,
             ((chelsea10_build_osm %>%
                 filter(!is.na(name) & !is.na(type)) %>%
                 nrow()) / length(chelsea10_build_osm$osm_id)) * 100,
             ((chelsea11_build_osm %>%
                 filter(!is.na(name) & !is.na(type)) %>%
                 nrow()) / length(chelsea11_build_osm$osm_id)) * 100,
             ((chelsea12_build_osm %>%
                 filter(!is.na(name) & !is.na(type)) %>%
                 nrow()) / length(chelsea12_build_osm$osm_id)) * 100,
             ((chelsea13_build_osm %>%
                 filter(!is.na(name) & !is.na(type)) %>%
                 nrow()) / length(chelsea13_build_osm$osm_id)) * 100,
             ((chelsea14_build_osm %>%
                 filter(!is.na(name) & !is.na(type)) %>%
                 nrow()) / length(chelsea14_build_osm$osm_id)) * 100,
             ((chelsea15_build_osm %>%
                 filter(!is.na(name) & !is.na(type)) %>%
                 nrow()) / length(chelsea15_build_osm$osm_id)) * 100,
             ((chelsea16_build_osm %>%
                 filter(!is.na(name) & !is.na(type)) %>%
                 nrow()) / length(chelsea16_build_osm$osm_id)) * 100,
             ((chelsea17_build_osm %>%
                 filter(!is.na(name) & !is.na(type)) %>%
                 nrow()) / length(chelsea17_build_osm$osm_id)) * 100,
             ((chelsea18_build_osm %>%
                 filter(!is.na(name) & !is.na(type)) %>%
                 nrow()) / length(chelsea18_build_osm$osm_id)) * 100,
             ((chelsea19_build_osm %>%
                 filter(!is.na(name) & !is.na(type)) %>%
                 nrow()) / length(chelsea19_build_osm$osm_id)) * 100,
             ((chelsea20_build_osm %>%
                 filter(!is.na(name) & !is.na(type)) %>%
                 nrow()) / length(chelsea20_build_osm$osm_id)) * 100,
             ((chelsea21_build_osm %>%
                 filter(!is.na(name) & !is.na(type)) %>%
                 nrow()) / length(chelsea21_build_osm$osm_id)) * 100),
    ncol = 1,
    dimnames = list(
      c("LSOA01", "LSOA02", "LSOA03", "LSOA04", "LSOA05", "LSOA06", "LSOA07", "LSOA08", "LSOA09", 
        "LSOA10", "LSOA11", "LSOA12", "LSOA13", "LSOA14", "LSOA15", "LSOA16", "LSOA17", "LSOA18", 
        "LSOA19", "LSOA20", "LSOA21"),
      c("Type&Name_Included_Perc")
    ))

che_complete <- 
  cbind(che_lsoa_names,
        che_area_diff, 
        che_area_perc, 
        che_build_number, 
        che_name_included_perc, 
        che_type_included_perc,
        che_type_name_included_perc)

che_complete_df <- data.frame(che_complete)

write_csv(che_complete_df, '/Users/christosnicolaou/R_Projects/thesis_data/che_complete.csv')

#------------
#Westminster
#------------

west_lsoa_names <- 
  matrix(
    data = c("LSOA01", "LSOA02", "LSOA03", "LSOA04", "LSOA05", "LSOA06", "LSOA07", "LSOA08", "LSOA09", 
             "LSOA10", "LSOA11", "LSOA12", "LSOA13", "LSOA14", "LSOA15", "LSOA16", "LSOA17", "LSOA18", 
             "LSOA19", "LSOA20", "LSOA21", "LSOA22", "LSOA23", "LSOA24"),
    ncol = 1,
    dimnames = list(
      c("LSOA01", "LSOA02", "LSOA03", "LSOA04", "LSOA05", "LSOA06", "LSOA07", "LSOA08", "LSOA09", 
        "LSOA10", "LSOA11", "LSOA12", "LSOA13", "LSOA14", "LSOA15", "LSOA16", "LSOA17", "LSOA18", 
        "LSOA19", "LSOA20", "LSOA21", "LSOA22", "LSOA23", "LSOA24"),
      c("LSOA"))
  )

west_area_diff <-
  matrix(
    data = c(sum(st_area(west01_build_os)) - sum(st_area(west01_build_osm)),
             sum(st_area(west02_build_os)) - sum(st_area(west02_build_osm)),
             sum(st_area(west03_build_os)) - sum(st_area(west03_build_osm)),
             sum(st_area(west04_build_os)) - sum(st_area(west04_build_osm)),
             sum(st_area(west05_build_os)) - sum(st_area(west05_build_osm)),
             sum(st_area(west06_build_os)) - sum(st_area(west06_build_osm)),
             sum(st_area(west07_build_os)) - sum(st_area(west07_build_osm)),
             sum(st_area(west08_build_os)) - sum(st_area(west08_build_osm)),
             sum(st_area(west09_build_os)) - sum(st_area(west09_build_osm)),
             sum(st_area(west10_build_os)) - sum(st_area(west10_build_osm)),
             sum(st_area(west11_build_os)) - sum(st_area(west11_build_osm)),
             sum(st_area(west12_build_os)) - sum(st_area(west12_build_osm)),
             sum(st_area(west13_build_os)) - sum(st_area(west13_build_osm)),
             sum(st_area(west14_build_os)) - sum(st_area(west14_build_osm)),
             sum(st_area(west15_build_os)) - sum(st_area(west15_build_osm)),
             sum(st_area(west16_build_os)) - sum(st_area(west16_build_osm)),
             sum(st_area(west17_build_os)) - sum(st_area(west17_build_osm)),
             sum(st_area(west18_build_os)) - sum(st_area(west18_build_osm)),
             sum(st_area(west19_build_os)) - sum(st_area(west19_build_osm)),
             sum(st_area(west20_build_os)) - sum(st_area(west20_build_osm)),
             sum(st_area(west21_build_os)) - sum(st_area(west21_build_osm)),
             sum(st_area(west22_build_os)) - sum(st_area(west22_build_osm)),
             sum(st_area(west23_build_os)) - sum(st_area(west23_build_osm)),
             sum(st_area(west24_build_os)) - sum(st_area(west24_build_osm))),
    ncol = 1,
    dimnames = list(
      c("LSOA01", "LSOA02", "LSOA03", "LSOA04", "LSOA05", "LSOA06", "LSOA07", "LSOA08", "LSOA09", 
        "LSOA10", "LSOA11", "LSOA12", "LSOA13", "LSOA14", "LSOA15", "LSOA16", "LSOA17", "LSOA18", 
        "LSOA19", "LSOA20", "LSOA21", "LSOA22", "LSOA23", "LSOA24"),
      c("Area_Diff")
    )
  )

west_area_perc <-
  matrix(
    data = c((sum(st_area(west01_build_osm)) / sum(st_area(west01_build_os))) * 100,
             (sum(st_area(west02_build_osm)) / sum(st_area(west02_build_os))) * 100,
             (sum(st_area(west03_build_os)) / sum(st_area(west03_build_osm))) * 100,
             (sum(st_area(west04_build_os)) / sum(st_area(west04_build_osm))) * 100,
             (sum(st_area(west05_build_os)) / sum(st_area(west05_build_osm))) * 100,
             (sum(st_area(west06_build_os)) / sum(st_area(west06_build_osm))) * 100,
             (sum(st_area(west07_build_os)) / sum(st_area(west07_build_osm))) * 100,
             (sum(st_area(west08_build_os)) / sum(st_area(west08_build_osm))) * 100,
             (sum(st_area(west09_build_os)) / sum(st_area(west09_build_osm))) * 100,
             (sum(st_area(west10_build_os)) / sum(st_area(west10_build_osm))) * 100,
             (sum(st_area(west11_build_os)) / sum(st_area(west11_build_osm))) * 100,
             (sum(st_area(west12_build_os)) / sum(st_area(west12_build_osm))) * 100,
             (sum(st_area(west13_build_os)) / sum(st_area(west13_build_osm))) * 100,
             (sum(st_area(west14_build_os)) / sum(st_area(west14_build_osm))) * 100,
             (sum(st_area(west15_build_os)) / sum(st_area(west15_build_osm))) * 100,
             (sum(st_area(west16_build_os)) / sum(st_area(west16_build_osm))) * 100,
             (sum(st_area(west17_build_os)) / sum(st_area(west17_build_osm))) * 100,
             (sum(st_area(west18_build_os)) / sum(st_area(west18_build_osm))) * 100,
             (sum(st_area(west19_build_os)) / sum(st_area(west19_build_osm))) * 100,
             (sum(st_area(west20_build_os)) / sum(st_area(west20_build_osm))) * 100,
             (sum(st_area(west21_build_os)) / sum(st_area(west21_build_osm))) * 100,
             (sum(st_area(west22_build_os)) / sum(st_area(west22_build_osm))) * 100,
             (sum(st_area(west23_build_os)) / sum(st_area(west23_build_osm))) * 100,
             (sum(st_area(west24_build_os)) / sum(st_area(west24_build_osm))) * 100),
    ncol = 1,
    dimnames = list(
      c("LSOA01", "LSOA02", "LSOA03", "LSOA04", "LSOA05", "LSOA06", "LSOA07", "LSOA08", "LSOA09", 
        "LSOA10", "LSOA11", "LSOA12", "LSOA13", "LSOA14", "LSOA15", "LSOA16", "LSOA17", "LSOA18", 
        "LSOA19", "LSOA20", "LSOA21", "LSOA22", "LSOA23", "LSOA24"),
      c("Area_Diff_Perc")
    )
  )

#This command returns all the objects that are included in the specified dataset.

west_build_number <- 
  matrix(
    data = c(length(west01_build_osm$osm_id),
             length(west02_build_osm$osm_id),
             length(west03_build_osm$osm_id),
             length(west04_build_osm$osm_id),
             length(west05_build_osm$osm_id),
             length(west06_build_osm$osm_id),
             length(west07_build_osm$osm_id),
             length(west08_build_osm$osm_id),
             length(west09_build_osm$osm_id),
             length(west10_build_osm$osm_id),
             length(west11_build_osm$osm_id),
             length(west12_build_osm$osm_id),
             length(west13_build_osm$osm_id),
             length(west14_build_osm$osm_id),
             length(west15_build_osm$osm_id),
             length(west16_build_osm$osm_id),
             length(west17_build_osm$osm_id),
             length(west18_build_osm$osm_id),
             length(west19_build_osm$osm_id),
             length(west20_build_osm$osm_id),
             length(west21_build_osm$osm_id),
             length(west22_build_osm$osm_id),
             length(west23_build_osm$osm_id),
             length(west24_build_osm$osm_id)),
    ncol = 1,
    dimnames = list(
      c("LSOA01", "LSOA02", "LSOA03", "LSOA04", "LSOA05", "LSOA06", "LSOA07", "LSOA08", "LSOA09", 
        "LSOA10", "LSOA11", "LSOA12", "LSOA13", "LSOA14", "LSOA15", "LSOA16", "LSOA17", "LSOA18", 
        "LSOA19", "LSOA20", "LSOA21", "LSOA22", "LSOA23", "LSOA24"),
      c("Buildings/LSOA")
    ))


#To count the objects that are not assigned with a name attribute in the OSM dataset.

west_name_included_perc <-
  matrix(
    data = c(((west01_build_osm %>%
                 filter(!is.na(name)) %>%
                 nrow()) / length(west01_build_osm$osm_id)) * 100,
             ((west02_build_osm %>%
                 filter(!is.na(name)) %>%
                 nrow()) / length(west02_build_osm$osm_id)) * 100,
             ((west03_build_osm %>%
                 filter(!is.na(name)) %>%
                 nrow()) / length(west03_build_osm$osm_id)) * 100,
             ((west04_build_osm %>%
                 filter(!is.na(name)) %>%
                 nrow()) / length(west04_build_osm$osm_id)) * 100,
             ((west05_build_osm %>%
                 filter(!is.na(name)) %>%
                 nrow()) / length(west05_build_osm$osm_id)) * 100,
             ((west06_build_osm %>%
                 filter(!is.na(name)) %>%
                 nrow()) / length(west06_build_osm$osm_id)) * 100,
             ((west07_build_osm %>%
                 filter(!is.na(name)) %>%
                 nrow()) / length(west07_build_osm$osm_id)) * 100,
             ((west08_build_osm %>%
                 filter(!is.na(name)) %>%
                 nrow()) / length(west08_build_osm$osm_id)) * 100,
             ((west09_build_osm %>%
                 filter(!is.na(name)) %>%
                 nrow()) / length(west09_build_osm$osm_id)) * 100,
             ((west10_build_osm %>%
                 filter(!is.na(name)) %>%
                 nrow()) / length(west10_build_osm$osm_id)) * 100,
             ((west11_build_osm %>%
                 filter(!is.na(name)) %>%
                 nrow()) / length(west11_build_osm$osm_id)) * 100,
             ((west12_build_osm %>%
                 filter(!is.na(name)) %>%
                 nrow()) / length(west12_build_osm$osm_id)) * 100,
             ((west13_build_osm %>%
                 filter(!is.na(name)) %>%
                 nrow()) / length(west13_build_osm$osm_id)) * 100,
             ((west14_build_osm %>%
                 filter(!is.na(name)) %>%
                 nrow()) / length(west14_build_osm$osm_id)) * 100,
             ((west15_build_osm %>%
                 filter(!is.na(name)) %>%
                 nrow()) / length(west15_build_osm$osm_id)) * 100,
             ((west16_build_osm %>%
                 filter(!is.na(name)) %>%
                 nrow()) / length(west16_build_osm$osm_id)) * 100,
             ((west17_build_osm %>%
                 filter(!is.na(name)) %>%
                 nrow()) / length(west17_build_osm$osm_id)) * 100,
             ((west18_build_osm %>%
                 filter(!is.na(name)) %>%
                 nrow()) / length(west18_build_osm$osm_id)) * 100,
             ((west19_build_osm %>%
                 filter(!is.na(name)) %>%
                 nrow()) / length(west19_build_osm$osm_id)) * 100,
             ((west20_build_osm %>%
                 filter(!is.na(name)) %>%
                 nrow()) / length(west20_build_osm$osm_id)) * 100,
             ((west21_build_osm %>%
                 filter(!is.na(name)) %>%
                 nrow()) / length(west21_build_osm$osm_id)) * 100,
             ((west22_build_osm %>%
                 filter(!is.na(name)) %>%
                 nrow()) / length(west22_build_osm$osm_id)) * 100,
             ((west23_build_osm %>%
                 filter(!is.na(name)) %>%
                 nrow()) / length(west23_build_osm$osm_id)) * 100,
             ((west24_build_osm %>%
                 filter(!is.na(name)) %>%
                 nrow()) / length(west24_build_osm$osm_id)) * 100),
    ncol = 1,
    dimnames = list(
      c("LSOA01", "LSOA02", "LSOA03", "LSOA04", "LSOA05", "LSOA06", "LSOA07", "LSOA08", "LSOA09", 
        "LSOA10", "LSOA11", "LSOA12", "LSOA13", "LSOA14", "LSOA15", "LSOA16", "LSOA17", "LSOA18", 
        "LSOA19", "LSOA20", "LSOA21", "LSOA22", "LSOA23", "LSOA24"),
      c("Name_Included_Perc")
    ))

#To count the objects that are not assigned with a type attribute in the OSM dataset.

west_type_included_perc <-
  matrix(
    data = c(((west01_build_osm %>%
                 filter(!is.na(type)) %>%
                 nrow()) / length(west01_build_osm$osm_id)) * 100,
             ((west02_build_osm %>%
                 filter(!is.na(type)) %>%
                 nrow()) / length(west02_build_osm$osm_id)) * 100,
             ((west03_build_osm %>%
                 filter(!is.na(type)) %>%
                 nrow()) / length(west03_build_osm$osm_id)) * 100,
             ((west04_build_osm %>%
                 filter(!is.na(type)) %>%
                 nrow()) / length(west04_build_osm$osm_id)) * 100,
             ((west05_build_osm %>%
                 filter(!is.na(type)) %>%
                 nrow()) / length(west05_build_osm$osm_id)) * 100,
             ((west06_build_osm %>%
                 filter(!is.na(type)) %>%
                 nrow()) / length(west06_build_osm$osm_id)) * 100,
             ((west07_build_osm %>%
                 filter(!is.na(type)) %>%
                 nrow()) / length(west07_build_osm$osm_id)) * 100,
             ((west08_build_osm %>%
                 filter(!is.na(type)) %>%
                 nrow()) / length(west08_build_osm$osm_id)) * 100,
             ((west09_build_osm %>%
                 filter(!is.na(type)) %>%
                 nrow()) / length(west09_build_osm$osm_id)) * 100,
             ((west10_build_osm %>%
                 filter(!is.na(type)) %>%
                 nrow()) / length(west10_build_osm$osm_id)) * 100,
             ((west11_build_osm %>%
                 filter(!is.na(type)) %>%
                 nrow()) / length(west11_build_osm$osm_id)) * 100,
             ((west12_build_osm %>%
                 filter(!is.na(type)) %>%
                 nrow()) / length(west12_build_osm$osm_id)) * 100,
             ((west13_build_osm %>%
                 filter(!is.na(type)) %>%
                 nrow()) / length(west13_build_osm$osm_id)) * 100,
             ((west14_build_osm %>%
                 filter(!is.na(type)) %>%
                 nrow()) / length(west14_build_osm$osm_id)) * 100,
             ((west15_build_osm %>%
                 filter(!is.na(type)) %>%
                 nrow()) / length(west15_build_osm$osm_id)) * 100,
             ((west16_build_osm %>%
                 filter(!is.na(type)) %>%
                 nrow()) / length(west16_build_osm$osm_id)) * 100,
             ((west17_build_osm %>%
                 filter(!is.na(type)) %>%
                 nrow()) / length(west17_build_osm$osm_id)) * 100,
             ((west18_build_osm %>%
                 filter(!is.na(type)) %>%
                 nrow()) / length(west18_build_osm$osm_id)) * 100,
             ((west19_build_osm %>%
                 filter(!is.na(type)) %>%
                 nrow()) / length(west19_build_osm$osm_id)) * 100,
             ((west20_build_osm %>%
                 filter(!is.na(type)) %>%
                 nrow()) / length(west20_build_osm$osm_id)) * 100,
             ((west21_build_osm %>%
                 filter(!is.na(type)) %>%
                 nrow()) / length(west21_build_osm$osm_id)) * 100,
             ((west22_build_osm %>%
                 filter(!is.na(type)) %>%
                 nrow()) / length(west22_build_osm$osm_id)) * 100,
             ((west23_build_osm %>%
                 filter(!is.na(type)) %>%
                 nrow()) / length(west21_build_osm$osm_id)) * 100,
             ((west24_build_osm %>%
                 filter(!is.na(type)) %>%
                 nrow()) / length(west21_build_osm$osm_id)) * 100),
    ncol = 1,
    dimnames = list(
      c("LSOA01", "LSOA02", "LSOA03", "LSOA04", "LSOA05", "LSOA06", "LSOA07", "LSOA08", "LSOA09", 
        "LSOA10", "LSOA11", "LSOA12", "LSOA13", "LSOA14", "LSOA15", "LSOA16", "LSOA17", "LSOA18", 
        "LSOA19", "LSOA20", "LSOA21", "LSOA22", "LSOA23", "LSOA24"),
      c("Type_Included_Perc")
    ))

#To count the objects that are not assigned with a name and type attribute in the OSM dataset.

west_type_name_included_perc <-
  matrix(
    data = c(((west01_build_osm %>%
                 filter(!is.na(name) & !is.na(type)) %>%
                 nrow()) / length(west01_build_osm$osm_id)) * 100,
             ((west02_build_osm %>%
                 filter(!is.na(name) & !is.na(type)) %>%
                 nrow()) / length(west02_build_osm$osm_id)) * 100,
             ((west03_build_osm %>%
                 filter(!is.na(name) & !is.na(type)) %>%
                 nrow()) / length(west03_build_osm$osm_id)) * 100,
             ((west04_build_osm %>%
                 filter(!is.na(name) & !is.na(type)) %>%
                 nrow()) / length(west04_build_osm$osm_id)) * 100,
             ((west05_build_osm %>%
                 filter(!is.na(name) & !is.na(type)) %>%
                 nrow()) / length(west05_build_osm$osm_id)) * 100,
             ((west06_build_osm %>%
                 filter(!is.na(name) & !is.na(type)) %>%
                 nrow()) / length(west06_build_osm$osm_id)) * 100,
             ((west07_build_osm %>%
                 filter(!is.na(name) & !is.na(type)) %>%
                 nrow()) / length(west07_build_osm$osm_id)) * 100,
             ((west08_build_osm %>%
                 filter(!is.na(name) & !is.na(type)) %>%
                 nrow()) / length(west08_build_osm$osm_id)) * 100,
             ((west09_build_osm %>%
                 filter(!is.na(name) & !is.na(type)) %>%
                 nrow()) / length(west09_build_osm$osm_id)) * 100,
             ((west10_build_osm %>%
                 filter(!is.na(name) & !is.na(type)) %>%
                 nrow()) / length(west10_build_osm$osm_id)) * 100,
             ((west11_build_osm %>%
                 filter(!is.na(name) & !is.na(type)) %>%
                 nrow()) / length(west11_build_osm$osm_id)) * 100,
             ((west12_build_osm %>%
                 filter(!is.na(name) & !is.na(type)) %>%
                 nrow()) / length(west12_build_osm$osm_id)) * 100,
             ((west13_build_osm %>%
                 filter(!is.na(name) & !is.na(type)) %>%
                 nrow()) / length(west13_build_osm$osm_id)) * 100,
             ((west14_build_osm %>%
                 filter(!is.na(name) & !is.na(type)) %>%
                 nrow()) / length(west14_build_osm$osm_id)) * 100,
             ((west15_build_osm %>%
                 filter(!is.na(name) & !is.na(type)) %>%
                 nrow()) / length(west15_build_osm$osm_id)) * 100,
             ((west16_build_osm %>%
                 filter(!is.na(name) & !is.na(type)) %>%
                 nrow()) / length(west16_build_osm$osm_id)) * 100,
             ((west17_build_osm %>%
                 filter(!is.na(name) & !is.na(type)) %>%
                 nrow()) / length(west17_build_osm$osm_id)) * 100,
             ((west18_build_osm %>%
                 filter(!is.na(name) & !is.na(type)) %>%
                 nrow()) / length(west18_build_osm$osm_id)) * 100,
             ((west19_build_osm %>%
                 filter(!is.na(name) & !is.na(type)) %>%
                 nrow()) / length(west19_build_osm$osm_id)) * 100,
             ((west20_build_osm %>%
                 filter(!is.na(name) & !is.na(type)) %>%
                 nrow()) / length(west20_build_osm$osm_id)) * 100,
             ((west21_build_osm %>%
                 filter(!is.na(name) & !is.na(type)) %>%
                 nrow()) / length(west21_build_osm$osm_id)) * 100,
             ((west22_build_osm %>%
                 filter(!is.na(name) & !is.na(type)) %>%
                 nrow()) / length(west22_build_osm$osm_id)) * 100,
             ((west23_build_osm %>%
                 filter(!is.na(name) & !is.na(type)) %>%
                 nrow()) / length(west23_build_osm$osm_id)) * 100,
             ((west24_build_osm %>%
                 filter(!is.na(name) & !is.na(type)) %>%
                 nrow()) / length(west24_build_osm$osm_id)) * 100),
    ncol = 1,
    dimnames = list(
      c("LSOA01", "LSOA02", "LSOA03", "LSOA04", "LSOA05", "LSOA06", "LSOA07", "LSOA08", "LSOA09", 
        "LSOA10", "LSOA11", "LSOA12", "LSOA13", "LSOA14", "LSOA15", "LSOA16", "LSOA17", "LSOA18", 
        "LSOA19", "LSOA20", "LSOA21", "LSOA22", "LSOA23", "LSOA24"),
      c("Type&Name_Included_Perc")
    ))

west_complete <-
  cbind(west_lsoa_names,
        west_area_diff,
        west_area_perc,
        west_build_number,
        west_name_included_perc,
        west_type_included_perc,
        west_type_name_included_perc)

west_complete_df <- data.frame(west_complete)

write_csv(west_complete_df, '/Users/christosnicolaou/R_Projects/thesis_data/west_complete.csv')

#----------
#Leicester
#----------

#The completeness is measured by calcaulating the difference between the surface covered area of OS and OSM respectevily,
#and then the percentage of the area that OSM covers compared to the OS buildings surface area.

leic_lsoa_names <- 
  matrix(
    data = c("LSOA01", "LSOA02", "LSOA03", "LSOA04", "LSOA05", "LSOA06", "LSOA07", "LSOA08", "LSOA09", 
             "LSOA10", "LSOA11", "LSOA12", "LSOA13", "LSOA16", "LSOA17", "LSOA18", 
             "LSOA19", "LSOA20", "LSOA21", "LSOA22", "LSOA23", "LSOA25", "LSOA26", "LSOA27", 
             "LSOA28", "LSOA29", "LSOA30", "LSOA31", "LSOA32", "LSOA34", "LSOA35", "LSOA36", 
             "LSOA37", "LSOA38", "LSOA39", "LSOA40", "LSOA41"),
    ncol = 1,
    dimnames = list(
      c("LSOA01", "LSOA02", "LSOA03", "LSOA04", "LSOA05", "LSOA06", "LSOA07", "LSOA08", "LSOA09", 
        "LSOA10", "LSOA11", "LSOA12", "LSOA13", "LSOA16", "LSOA17", "LSOA18", 
        "LSOA19", "LSOA20", "LSOA21", "LSOA22", "LSOA23", "LSOA25", "LSOA26", "LSOA27", 
        "LSOA28", "LSOA29", "LSOA30", "LSOA31", "LSOA32", "LSOA34", "LSOA35", "LSOA36", 
        "LSOA37", "LSOA38", "LSOA39", "LSOA40", "LSOA41"),
      c("LSOA"))
  )

leic_area_diff <-
  matrix(
    data = c(sum(st_area(leicester01_build_os)) - sum(st_area(leicester01_build_osm)),
             sum(st_area(leicester02_build_os)) - sum(st_area(leicester02_build_osm)),
             sum(st_area(leicester03_build_os)) - sum(st_area(leicester03_build_osm)),
             sum(st_area(leicester04_build_os)) - sum(st_area(leicester04_build_osm)),
             sum(st_area(leicester05_build_os)) - sum(st_area(leicester05_build_osm)),
             sum(st_area(leicester06_build_os)) - sum(st_area(leicester06_build_osm)),
             sum(st_area(leicester07_build_os)) - sum(st_area(leicester07_build_osm)),
             sum(st_area(leicester08_build_os)) - sum(st_area(leicester08_build_osm)),
             sum(st_area(leicester09_build_os)) - sum(st_area(leicester09_build_osm)),
             sum(st_area(leicester10_build_os)) - sum(st_area(leicester10_build_osm)),
             sum(st_area(leicester11_build_os)) - sum(st_area(leicester11_build_osm)),
             sum(st_area(leicester12_build_os)) - sum(st_area(leicester12_build_osm)),
             sum(st_area(leicester13_build_os)) - sum(st_area(leicester13_build_osm)),
             sum(st_area(leicester16_build_os)) - sum(st_area(leicester16_build_osm)),
             sum(st_area(leicester17_build_os)) - sum(st_area(leicester17_build_osm)),
             sum(st_area(leicester18_build_os)) - sum(st_area(leicester18_build_osm)),
             sum(st_area(leicester19_build_os)) - sum(st_area(leicester19_build_osm)),
             sum(st_area(leicester20_build_os)) - sum(st_area(leicester20_build_osm)),
             sum(st_area(leicester21_build_os)) - sum(st_area(leicester21_build_osm)),
             sum(st_area(leicester22_build_os)) - sum(st_area(leicester22_build_osm)),
             sum(st_area(leicester23_build_os)) - sum(st_area(leicester23_build_osm)),
             sum(st_area(leicester25_build_os)) - sum(st_area(leicester25_build_osm)),
             sum(st_area(leicester26_build_os)) - sum(st_area(leicester26_build_osm)),
             sum(st_area(leicester27_build_os)) - sum(st_area(leicester27_build_osm)),
             sum(st_area(leicester28_build_os)) - sum(st_area(leicester28_build_osm)),
             sum(st_area(leicester29_build_os)) - sum(st_area(leicester29_build_osm)),
             sum(st_area(leicester30_build_os)) - sum(st_area(leicester30_build_osm)),
             sum(st_area(leicester31_build_os)) - sum(st_area(leicester31_build_osm)),
             sum(st_area(leicester32_build_os)) - sum(st_area(leicester32_build_osm)),
             sum(st_area(leicester34_build_os)) - sum(st_area(leicester34_build_osm)),
             sum(st_area(leicester35_build_os)) - sum(st_area(leicester35_build_osm)),
             sum(st_area(leicester36_build_os)) - sum(st_area(leicester36_build_osm)),
             sum(st_area(leicester37_build_os)) - sum(st_area(leicester37_build_osm)),
             sum(st_area(leicester38_build_os)) - sum(st_area(leicester38_build_osm)),
             sum(st_area(leicester39_build_os)) - sum(st_area(leicester39_build_osm)),
             sum(st_area(leicester40_build_os)) - sum(st_area(leicester40_build_osm)),
             sum(st_area(leicester41_build_os)) - sum(st_area(leicester41_build_osm))),
    ncol = 1,
    dimnames = list(
      c("LSOA01", "LSOA02", "LSOA03", "LSOA04", "LSOA05", "LSOA06", "LSOA07", "LSOA08", "LSOA09", 
        "LSOA10", "LSOA11", "LSOA12", "LSOA13", "LSOA16", "LSOA17", "LSOA18", 
        "LSOA19", "LSOA20", "LSOA21", "LSOA22", "LSOA23", "LSOA25", "LSOA26", "LSOA27", 
        "LSOA28", "LSOA29", "LSOA30", "LSOA31", "LSOA32", "LSOA34", "LSOA35", "LSOA36", 
        "LSOA37", "LSOA38", "LSOA39", "LSOA40", "LSOA41"),
      c("Area_Diff")
  ))

leic_area_perc <-
  matrix(
    data = c((sum(st_area(leicester01_build_osm)) / sum(st_area(leicester01_build_os))) * 100,
             (sum(st_area(leicester02_build_osm)) / sum(st_area(leicester02_build_os))) * 100,
             (sum(st_area(leicester03_build_osm)) / sum(st_area(leicester03_build_os))) * 100,
             (sum(st_area(leicester04_build_osm)) / sum(st_area(leicester04_build_os))) * 100,
             (sum(st_area(leicester05_build_osm)) / sum(st_area(leicester05_build_os))) * 100,
             (sum(st_area(leicester06_build_osm)) / sum(st_area(leicester06_build_os))) * 100,
             (sum(st_area(leicester07_build_osm)) / sum(st_area(leicester07_build_os))) * 100,
             (sum(st_area(leicester08_build_osm)) / sum(st_area(leicester08_build_os))) * 100,
             (sum(st_area(leicester09_build_osm)) / sum(st_area(leicester09_build_os))) * 100,
             (sum(st_area(leicester10_build_osm)) / sum(st_area(leicester10_build_os))) * 100,
             (sum(st_area(leicester11_build_osm)) / sum(st_area(leicester11_build_os))) * 100,
             (sum(st_area(leicester12_build_osm)) / sum(st_area(leicester12_build_os))) * 100,
             (sum(st_area(leicester13_build_osm)) / sum(st_area(leicester13_build_os))) * 100,
             (sum(st_area(leicester16_build_osm)) / sum(st_area(leicester16_build_os))) * 100,
             (sum(st_area(leicester17_build_osm)) / sum(st_area(leicester17_build_os))) * 100,
             (sum(st_area(leicester18_build_osm)) / sum(st_area(leicester18_build_os))) * 100,
             (sum(st_area(leicester19_build_osm)) / sum(st_area(leicester19_build_os))) * 100,
             (sum(st_area(leicester20_build_osm)) / sum(st_area(leicester20_build_os))) * 100,
             (sum(st_area(leicester21_build_osm)) / sum(st_area(leicester21_build_os))) * 100,
             (sum(st_area(leicester22_build_osm)) / sum(st_area(leicester22_build_os))) * 100,
             (sum(st_area(leicester23_build_osm)) / sum(st_area(leicester23_build_os))) * 100,
             (sum(st_area(leicester25_build_osm)) / sum(st_area(leicester25_build_os))) * 100,
             (sum(st_area(leicester26_build_osm)) / sum(st_area(leicester26_build_os))) * 100,
             (sum(st_area(leicester27_build_osm)) / sum(st_area(leicester27_build_os))) * 100,
             (sum(st_area(leicester28_build_osm)) / sum(st_area(leicester28_build_os))) * 100,
             (sum(st_area(leicester29_build_osm)) / sum(st_area(leicester29_build_os))) * 100,
             (sum(st_area(leicester30_build_osm)) / sum(st_area(leicester30_build_os))) * 100,
             (sum(st_area(leicester31_build_osm)) / sum(st_area(leicester31_build_os))) * 100,
             (sum(st_area(leicester32_build_osm)) / sum(st_area(leicester32_build_os))) * 100,
             (sum(st_area(leicester34_build_osm)) / sum(st_area(leicester34_build_os))) * 100,
             (sum(st_area(leicester35_build_osm)) / sum(st_area(leicester35_build_os))) * 100,
             (sum(st_area(leicester36_build_osm)) / sum(st_area(leicester36_build_os))) * 100,
             (sum(st_area(leicester37_build_osm)) / sum(st_area(leicester37_build_os))) * 100,
             (sum(st_area(leicester38_build_osm)) / sum(st_area(leicester38_build_os))) * 100,
             (sum(st_area(leicester39_build_osm)) / sum(st_area(leicester39_build_os))) * 100,
             (sum(st_area(leicester40_build_osm)) / sum(st_area(leicester40_build_os))) * 100,
             (sum(st_area(leicester41_build_osm)) / sum(st_area(leicester41_build_os))) * 100),
    ncol = 1,
    dimnames = list(
      c("LSOA01", "LSOA02", "LSOA03", "LSOA04", "LSOA05", "LSOA06", "LSOA07", "LSOA08", "LSOA09", 
        "LSOA10", "LSOA11", "LSOA12", "LSOA13", "LSOA16", "LSOA17", "LSOA18", 
        "LSOA19", "LSOA20", "LSOA21", "LSOA22", "LSOA23", "LSOA25", "LSOA26", "LSOA27", 
        "LSOA28", "LSOA29", "LSOA30", "LSOA31", "LSOA32", "LSOA34", "LSOA35", "LSOA36", 
        "LSOA37", "LSOA38", "LSOA39", "LSOA40", "LSOA41"),
      c("Area_Diff_perc"))
)

#This command returns all the objects that are included in the specified dataset.

leic_build_number <-
  matrix(
    data = c(length(leicester01_build_osm$osm_id),
             length(leicester02_build_osm$osm_id),
             length(leicester03_build_osm$osm_id),
             length(leicester04_build_osm$osm_id),
             length(leicester05_build_osm$osm_id),
             length(leicester06_build_osm$osm_id),
             length(leicester07_build_osm$osm_id),
             length(leicester08_build_osm$osm_id),
             length(leicester09_build_osm$osm_id),
             length(leicester10_build_osm$osm_id),
             length(leicester11_build_osm$osm_id),
             length(leicester12_build_osm$osm_id),
             length(leicester13_build_osm$osm_id),
             length(leicester16_build_osm$osm_id),
             length(leicester17_build_osm$osm_id),
             length(leicester18_build_osm$osm_id),
             length(leicester19_build_osm$osm_id),
             length(leicester20_build_osm$osm_id),
             length(leicester21_build_osm$osm_id),
             length(leicester22_build_osm$osm_id),
             length(leicester23_build_osm$osm_id),
             length(leicester25_build_osm$osm_id),
             length(leicester26_build_osm$osm_id),
             length(leicester27_build_osm$osm_id),
             length(leicester28_build_osm$osm_id),
             length(leicester29_build_osm$osm_id),
             length(leicester30_build_osm$osm_id),
             length(leicester31_build_osm$osm_id),
             length(leicester32_build_osm$osm_id),
             length(leicester34_build_osm$osm_id),
             length(leicester35_build_osm$osm_id),
             length(leicester36_build_osm$osm_id),
             length(leicester37_build_osm$osm_id),
             length(leicester38_build_osm$osm_id),
             length(leicester39_build_osm$osm_id),
             length(leicester40_build_osm$osm_id),
             length(leicester41_build_osm$osm_id)),
    ncol = 1,
    dimnames = list(
      c("LSOA01", "LSOA02", "LSOA03", "LSOA04", "LSOA05", "LSOA06", "LSOA07", "LSOA08", "LSOA09", 
        "LSOA10", "LSOA11", "LSOA12", "LSOA13", "LSOA16", "LSOA17", "LSOA18", 
        "LSOA19", "LSOA20", "LSOA21", "LSOA22", "LSOA23", "LSOA25", "LSOA26", "LSOA27", 
        "LSOA28", "LSOA29", "LSOA30", "LSOA31", "LSOA32", "LSOA34", "LSOA35", "LSOA36", 
        "LSOA37", "LSOA38", "LSOA39", "LSOA40", "LSOA41"),
      c("Buildings/LSOA"))
  )

#To count the objects that are not assigned with a name attribute in the OSM dataset.

leic_name_included <-
  matrix(
    data = c(((leicester01_build_osm %>%
                filter(!is.na(name)) %>%
                nrow()) / length(leicester01_build_osm$osm_id) )*100,
             ((leicester02_build_osm %>%
                 filter(!is.na(name)) %>%
                 nrow()) / length(leicester02_build_osm$osm_id)) *100,
             ((leicester03_build_osm %>%
                 filter(!is.na(name)) %>%
                 nrow()) / length(leicester03_build_osm$osm_id)) *100,
             ((leicester04_build_osm %>%
                 filter(!is.na(name)) %>%
                 nrow()) / length(leicester04_build_osm$osm_id)) *100,
             ((leicester05_build_osm %>%
                 filter(!is.na(name)) %>%
                 nrow()) / length(leicester05_build_osm$osm_id)) *100,
             ((leicester06_build_osm %>%
                 filter(!is.na(name)) %>%
                 nrow()) / length(leicester06_build_osm$osm_id)) *100,
             ((leicester07_build_osm %>%
                 filter(!is.na(name)) %>%
                 nrow()) / length(leicester07_build_osm$osm_id)) *100,
             ((leicester08_build_osm %>%
                 filter(!is.na(name)) %>%
                 nrow()) / length(leicester08_build_osm$osm_id)) *100,
             ((leicester09_build_osm %>%
                 filter(!is.na(name)) %>%
                 nrow()) / length(leicester09_build_osm$osm_id)) *100,
             ((leicester10_build_osm %>%
                 filter(!is.na(name)) %>%
                 nrow()) / length(leicester10_build_osm$osm_id)) *100,
             ((leicester11_build_osm %>%
                 filter(!is.na(name)) %>%
                 nrow()) / length(leicester11_build_osm$osm_id)) *100,
             ((leicester12_build_osm %>%
                 filter(!is.na(name)) %>%
                 nrow()) / length(leicester12_build_osm$osm_id)) *100,
             ((leicester13_build_osm %>%
                 filter(!is.na(name)) %>%
                 nrow()) / length(leicester13_build_osm$osm_id)) *100,
             ((leicester16_build_osm %>%
                 filter(!is.na(name)) %>%
                 nrow()) / length(leicester16_build_osm$osm_id)) *100,
             ((leicester17_build_osm %>%
                 filter(!is.na(name)) %>%
                 nrow()) / length(leicester17_build_osm$osm_id)) *100,
             ((leicester18_build_osm %>%
                 filter(!is.na(name)) %>%
                 nrow()) / length(leicester18_build_osm$osm_id)) *100,
             ((leicester19_build_osm %>%
                 filter(!is.na(name)) %>%
                 nrow()) / length(leicester19_build_osm$osm_id)) *100,
             ((leicester20_build_osm %>%
                 filter(!is.na(name)) %>%
                 nrow()) / length(leicester20_build_osm$osm_id)) *100,
             ((leicester21_build_osm %>%
                 filter(!is.na(name)) %>%
                 nrow()) / length(leicester21_build_osm$osm_id)) *100,
             ((leicester22_build_osm %>%
                 filter(!is.na(name)) %>%
                 nrow()) / length(leicester22_build_osm$osm_id)) *100,
             ((leicester23_build_osm %>%
                 filter(!is.na(name)) %>%
                 nrow()) / length(leicester23_build_osm$osm_id)) *100,
             ((leicester25_build_osm %>%
                 filter(!is.na(name)) %>%
                 nrow()) / length(leicester25_build_osm$osm_id)) *100,
             ((leicester26_build_osm %>%
                 filter(!is.na(name)) %>%
                 nrow()) / length(leicester26_build_osm$osm_id)) *100,
             ((leicester27_build_osm %>%
                 filter(!is.na(name)) %>%
                 nrow()) / length(leicester27_build_osm$osm_id)) *100,
             ((leicester28_build_osm %>%
                 filter(!is.na(name)) %>%
                 nrow()) / length(leicester28_build_osm$osm_id)) *100,
             ((leicester29_build_osm %>%
                 filter(!is.na(name)) %>%
                 nrow()) / length(leicester29_build_osm$osm_id)) *100,
             ((leicester30_build_osm %>%
                 filter(!is.na(name)) %>%
                 nrow()) / length(leicester30_build_osm$osm_id)) *100,
             ((leicester31_build_osm %>%
                 filter(!is.na(name)) %>%
                 nrow()) / length(leicester31_build_osm$osm_id)) *100,
             ((leicester32_build_osm %>%
                 filter(!is.na(name)) %>%
                 nrow()) / length(leicester32_build_osm$osm_id)) *100,
             ((leicester34_build_osm %>%
                 filter(!is.na(name)) %>%
                 nrow()) / length(leicester34_build_osm$osm_id)) *100,
             ((leicester35_build_osm %>%
                 filter(!is.na(name)) %>%
                 nrow()) / length(leicester35_build_osm$osm_id)) *100,
             ((leicester36_build_osm %>%
                 filter(!is.na(name)) %>%
                 nrow()) / length(leicester36_build_osm$osm_id)) *100,
             ((leicester37_build_osm %>%
                 filter(!is.na(name)) %>%
                 nrow()) / length(leicester37_build_osm$osm_id)) *100,
             ((leicester38_build_osm %>%
                 filter(!is.na(name)) %>%
                 nrow()) / length(leicester38_build_osm$osm_id)) *100,
             ((leicester39_build_osm %>%
                 filter(!is.na(name)) %>%
                 nrow()) / length(leicester39_build_osm$osm_id)) *100,
             ((leicester40_build_osm %>%
                 filter(!is.na(name)) %>%
                 nrow()) / length(leicester40_build_osm$osm_id)) *100,
             ((leicester41_build_osm %>%
                 filter(!is.na(name)) %>%
                 nrow()) / length(leicester41_build_osm$osm_id)) *100),
    ncol = 1,
    dimnames = list(
      c("LSOA01", "LSOA02", "LSOA03", "LSOA04", "LSOA05", "LSOA06", "LSOA07", "LSOA08", "LSOA09", 
        "LSOA10", "LSOA11", "LSOA12", "LSOA13", "LSOA16", "LSOA17", "LSOA18", 
        "LSOA19", "LSOA20", "LSOA21", "LSOA22", "LSOA23", "LSOA25", "LSOA26", "LSOA27", 
        "LSOA28", "LSOA29", "LSOA30", "LSOA31", "LSOA32", "LSOA34", "LSOA35", "LSOA36", 
        "LSOA37", "LSOA38", "LSOA39", "LSOA40", "LSOA41"),
      c("Name_Included_Perc"))
  )

#To count the objects that are not assigned with a type attribute in the OSM dataset.

leic_type_included <-
  matrix(
    data = c(((leicester01_build_osm %>%
                 filter(!is.na(type)) %>%
                 nrow()) / length(leicester01_build_osm$osm_id) )*100,
             ((leicester02_build_osm %>%
                 filter(!is.na(type)) %>%
                 nrow()) / length(leicester02_build_osm$osm_id)) *100,
             ((leicester03_build_osm %>%
                 filter(!is.na(type)) %>%
                 nrow()) / length(leicester03_build_osm$osm_id)) *100,
             ((leicester04_build_osm %>%
                 filter(!is.na(type)) %>%
                 nrow()) / length(leicester04_build_osm$osm_id)) *100,
             ((leicester05_build_osm %>%
                 filter(!is.na(type)) %>%
                 nrow()) / length(leicester05_build_osm$osm_id)) *100,
             ((leicester06_build_osm %>%
                 filter(!is.na(type)) %>%
                 nrow()) / length(leicester06_build_osm$osm_id)) *100,
             ((leicester07_build_osm %>%
                 filter(!is.na(type)) %>%
                 nrow()) / length(leicester07_build_osm$osm_id)) *100,
             ((leicester08_build_osm %>%
                 filter(!is.na(type)) %>%
                 nrow()) / length(leicester08_build_osm$osm_id)) *100,
             ((leicester09_build_osm %>%
                 filter(!is.na(type)) %>%
                 nrow()) / length(leicester09_build_osm$osm_id)) *100,
             ((leicester10_build_osm %>%
                 filter(!is.na(type)) %>%
                 nrow()) / length(leicester10_build_osm$osm_id)) *100,
             ((leicester11_build_osm %>%
                 filter(!is.na(type)) %>%
                 nrow()) / length(leicester11_build_osm$osm_id)) *100,
             ((leicester12_build_osm %>%
                 filter(!is.na(type)) %>%
                 nrow()) / length(leicester12_build_osm$osm_id)) *100,
             ((leicester13_build_osm %>%
                 filter(!is.na(type)) %>%
                 nrow()) / length(leicester13_build_osm$osm_id)) *100,
             ((leicester16_build_osm %>%
                 filter(!is.na(type)) %>%
                 nrow()) / length(leicester16_build_osm$osm_id)) *100,
             ((leicester17_build_osm %>%
                 filter(!is.na(type)) %>%
                 nrow()) / length(leicester17_build_osm$osm_id)) *100,
             ((leicester18_build_osm %>%
                 filter(!is.na(type)) %>%
                 nrow()) / length(leicester18_build_osm$osm_id)) *100,
             ((leicester19_build_osm %>%
                 filter(!is.na(type)) %>%
                 nrow()) / length(leicester19_build_osm$osm_id)) *100,
             ((leicester20_build_osm %>%
                 filter(!is.na(type)) %>%
                 nrow()) / length(leicester20_build_osm$osm_id)) *100,
             ((leicester21_build_osm %>%
                 filter(!is.na(type)) %>%
                 nrow()) / length(leicester21_build_osm$osm_id)) *100,
             ((leicester22_build_osm %>%
                 filter(!is.na(type)) %>%
                 nrow()) / length(leicester22_build_osm$osm_id)) *100,
             ((leicester23_build_osm %>%
                 filter(!is.na(type)) %>%
                 nrow()) / length(leicester23_build_osm$osm_id)) *100,
             ((leicester25_build_osm %>%
                 filter(!is.na(type)) %>%
                 nrow()) / length(leicester25_build_osm$osm_id)) *100,
             ((leicester26_build_osm %>%
                 filter(!is.na(type)) %>%
                 nrow()) / length(leicester26_build_osm$osm_id)) *100,
             ((leicester27_build_osm %>%
                 filter(!is.na(type)) %>%
                 nrow()) / length(leicester27_build_osm$osm_id)) *100,
             ((leicester28_build_osm %>%
                 filter(!is.na(type)) %>%
                 nrow()) / length(leicester28_build_osm$osm_id)) *100,
             ((leicester29_build_osm %>%
                 filter(!is.na(type)) %>%
                 nrow()) / length(leicester29_build_osm$osm_id)) *100,
             ((leicester30_build_osm %>%
                 filter(!is.na(type)) %>%
                 nrow()) / length(leicester30_build_osm$osm_id)) *100,
             ((leicester31_build_osm %>%
                 filter(!is.na(type)) %>%
                 nrow()) / length(leicester31_build_osm$osm_id)) *100,
             ((leicester32_build_osm %>%
                 filter(!is.na(type)) %>%
                 nrow()) / length(leicester32_build_osm$osm_id)) *100,
             ((leicester34_build_osm %>%
                 filter(!is.na(type)) %>%
                 nrow()) / length(leicester34_build_osm$osm_id)) *100,
             ((leicester35_build_osm %>%
                 filter(!is.na(type)) %>%
                 nrow()) / length(leicester35_build_osm$osm_id)) *100,
             ((leicester36_build_osm %>%
                 filter(!is.na(type)) %>%
                 nrow()) / length(leicester36_build_osm$osm_id)) *100,
             ((leicester37_build_osm %>%
                 filter(!is.na(type)) %>%
                 nrow()) / length(leicester37_build_osm$osm_id)) *100,
             ((leicester38_build_osm %>%
                 filter(!is.na(type)) %>%
                 nrow()) / length(leicester38_build_osm$osm_id)) *100,
             ((leicester39_build_osm %>%
                 filter(!is.na(type)) %>%
                 nrow()) / length(leicester39_build_osm$osm_id)) *100,
             ((leicester40_build_osm %>%
                 filter(!is.na(type)) %>%
                 nrow()) / length(leicester40_build_osm$osm_id)) *100,
             ((leicester41_build_osm %>%
                 filter(!is.na(type)) %>%
                 nrow()) / length(leicester41_build_osm$osm_id)) *100),
    ncol = 1,
    dimnames = list(
      c("LSOA01", "LSOA02", "LSOA03", "LSOA04", "LSOA05", "LSOA06", "LSOA07", "LSOA08", "LSOA09", 
        "LSOA10", "LSOA11", "LSOA12", "LSOA13", "LSOA16", "LSOA17", "LSOA18", 
        "LSOA19", "LSOA20", "LSOA21", "LSOA22", "LSOA23", "LSOA25", "LSOA26", "LSOA27", 
        "LSOA28", "LSOA29", "LSOA30", "LSOA31", "LSOA32", "LSOA34", "LSOA35", "LSOA36", 
        "LSOA37", "LSOA38", "LSOA39", "LSOA40", "LSOA41"),
      c("Type_Included_Perc"))
  )

#To count the objects that are not assigned with a name and type attribute in the OSM dataset.

leic_type_name_included <-
  matrix(
    data = c(((leicester01_build_osm %>%
                 filter(!is.na(name) & !is.na(type)) %>%
                 nrow()) / length(leicester01_build_osm$osm_id) )*100,
             ((leicester02_build_osm %>%
                 filter(!is.na(name) & !is.na(type)) %>%
                 nrow()) / length(leicester02_build_osm$osm_id)) *100,
             ((leicester03_build_osm %>%
                 filter(!is.na(name) & !is.na(type)) %>%
                 nrow()) / length(leicester03_build_osm$osm_id)) *100,
             ((leicester04_build_osm %>%
                 filter(!is.na(name) & !is.na(type)) %>%
                 nrow()) / length(leicester04_build_osm$osm_id)) *100,
             ((leicester05_build_osm %>%
                 filter(!is.na(name) & !is.na(type)) %>%
                 nrow()) / length(leicester05_build_osm$osm_id)) *100,
             ((leicester06_build_osm %>%
                 filter(!is.na(name) & !is.na(type)) %>%
                 nrow()) / length(leicester06_build_osm$osm_id)) *100,
             ((leicester07_build_osm %>%
                 filter(!is.na(name) & !is.na(type)) %>%
                 nrow()) / length(leicester07_build_osm$osm_id)) *100,
             ((leicester08_build_osm %>%
                 filter(!is.na(name) & !is.na(type)) %>%
                 nrow()) / length(leicester08_build_osm$osm_id)) *100,
             ((leicester09_build_osm %>%
                 filter(!is.na(name) & !is.na(type)) %>%
                 nrow()) / length(leicester09_build_osm$osm_id)) *100,
             ((leicester10_build_osm %>%
                 filter(!is.na(name) & !is.na(type)) %>%
                 nrow()) / length(leicester10_build_osm$osm_id)) *100,
             ((leicester11_build_osm %>%
                 filter(!is.na(name) & !is.na(type)) %>%
                 nrow()) / length(leicester11_build_osm$osm_id)) *100,
             ((leicester12_build_osm %>%
                 filter(!is.na(name) & !is.na(type)) %>%
                 nrow()) / length(leicester12_build_osm$osm_id)) *100,
             ((leicester13_build_osm %>%
                 filter(!is.na(name) & !is.na(type)) %>%
                 nrow()) / length(leicester13_build_osm$osm_id)) *100,
             ((leicester16_build_osm %>%
                 filter(!is.na(name) & !is.na(type)) %>%
                 nrow()) / length(leicester16_build_osm$osm_id)) *100,
             ((leicester17_build_osm %>%
                 filter(!is.na(name) & !is.na(type)) %>%
                 nrow()) / length(leicester17_build_osm$osm_id)) *100,
             ((leicester18_build_osm %>%
                 filter(!is.na(name) & !is.na(type)) %>%
                 nrow()) / length(leicester18_build_osm$osm_id)) *100,
             ((leicester19_build_osm %>%
                 filter(!is.na(name) & !is.na(type)) %>%
                 nrow()) / length(leicester19_build_osm$osm_id)) *100,
             ((leicester20_build_osm %>%
                 filter(!is.na(name) & !is.na(type)) %>%
                 nrow()) / length(leicester20_build_osm$osm_id)) *100,
             ((leicester21_build_osm %>%
                 filter(!is.na(name) & !is.na(type)) %>%
                 nrow()) / length(leicester21_build_osm$osm_id)) *100,
             ((leicester22_build_osm %>%
                 filter(!is.na(name) & !is.na(type)) %>%
                 nrow()) / length(leicester22_build_osm$osm_id)) *100,
             ((leicester23_build_osm %>%
                 filter(!is.na(name) & !is.na(type)) %>%
                 nrow()) / length(leicester23_build_osm$osm_id)) *100,
             ((leicester25_build_osm %>%
                 filter(!is.na(name) & !is.na(type)) %>%
                 nrow()) / length(leicester25_build_osm$osm_id)) *100,
             ((leicester26_build_osm %>%
                 filter(!is.na(name) & !is.na(type)) %>%
                 nrow()) / length(leicester26_build_osm$osm_id)) *100,
             ((leicester27_build_osm %>%
                 filter(!is.na(name) & !is.na(type)) %>%
                 nrow()) / length(leicester27_build_osm$osm_id)) *100,
             ((leicester28_build_osm %>%
                 filter(!is.na(name) & !is.na(type)) %>%
                 nrow()) / length(leicester28_build_osm$osm_id)) *100,
             ((leicester29_build_osm %>%
                 filter(!is.na(name) & !is.na(type)) %>%
                 nrow()) / length(leicester29_build_osm$osm_id)) *100,
             ((leicester30_build_osm %>%
                 filter(!is.na(name) & !is.na(type)) %>%
                 nrow()) / length(leicester30_build_osm$osm_id)) *100,
             ((leicester31_build_osm %>%
                 filter(!is.na(name) & !is.na(type)) %>%
                 nrow()) / length(leicester31_build_osm$osm_id)) *100,
             ((leicester32_build_osm %>%
                 filter(!is.na(name) & !is.na(type)) %>%
                 nrow()) / length(leicester32_build_osm$osm_id)) *100,
             ((leicester34_build_osm %>%
                 filter(!is.na(name) & !is.na(type)) %>%
                 nrow()) / length(leicester34_build_osm$osm_id)) *100,
             ((leicester35_build_osm %>%
                 filter(!is.na(name) & !is.na(type)) %>%
                 nrow()) / length(leicester35_build_osm$osm_id)) *100,
             ((leicester36_build_osm %>%
                 filter(!is.na(name) & !is.na(type)) %>%
                 nrow()) / length(leicester36_build_osm$osm_id)) *100,
             ((leicester37_build_osm %>%
                 filter(!is.na(name) & !is.na(type)) %>%
                 nrow()) / length(leicester37_build_osm$osm_id)) *100,
             ((leicester38_build_osm %>%
                 filter(!is.na(name) & !is.na(type)) %>%
                 nrow()) / length(leicester38_build_osm$osm_id)) *100,
             ((leicester39_build_osm %>%
                 filter(!is.na(name) & !is.na(type)) %>%
                 nrow()) / length(leicester39_build_osm$osm_id)) *100,
             ((leicester40_build_osm %>%
                 filter(!is.na(name) & !is.na(type)) %>%
                 nrow()) / length(leicester40_build_osm$osm_id)) *100,
             ((leicester41_build_osm %>%
                 filter(!is.na(name) & !is.na(type)) %>%
                 nrow()) / length(leicester41_build_osm$osm_id)) *100),
    ncol = 1,
    dimnames = list(
      c("LSOA01", "LSOA02", "LSOA03", "LSOA04", "LSOA05", "LSOA06", "LSOA07", "LSOA08", "LSOA09", 
        "LSOA10", "LSOA11", "LSOA12", "LSOA13", "LSOA16", "LSOA17", "LSOA18", 
        "LSOA19", "LSOA20", "LSOA21", "LSOA22", "LSOA23", "LSOA25", "LSOA26", "LSOA27", 
        "LSOA28", "LSOA29", "LSOA30", "LSOA31", "LSOA32", "LSOA34", "LSOA35", "LSOA36", 
        "LSOA37", "LSOA38", "LSOA39", "LSOA40", "LSOA41"),
      c("Type_Name_Included_Perc"))
  )

leic_complete <-
  cbind(leic_lsoa_names,
        leic_area_diff,
        leic_area_perc,
        leic_build_number,
        leic_name_included,
        leic_type_included,
        leic_type_name_included)

leic_complete_df <- data.frame(leic_complete)

write_csv(leic_complete_df, '/Users/christosnicolaou/R_Projects/thesis_data/leic_complete.csv')

#--------------------------------------------------------------------------------------------------------------------------

#------------------
#POSITION ACCURACY    
#------------------

#---------------
#City of London
#---------------

#Reading the pre-processed data that were analyzed in ArcMap.

cityoflondon_1_1_os <- st_read('/Users/christosnicolaou/R_Projects/thesis_data/OS_Data/CityOfLondon_OS_data/1_1/colondon_1to1_os.shp')
cityoflondon_1_1_osm <- st_read('/Users/christosnicolaou/R_Projects/thesis_data/OSM_Data/cityoflondon_1_1/colondon_1to1_osm.shp')

st_crs(cityoflondon_1_1_os)
st_crs(cityoflondon_1_1_osm)

cityoflondon_1_1_os <- st_transform(cityoflondon_1_1_os, st_crs(cityoflondon_build_os))
cityoflondon_1_1_osm <- st_transform(cityoflondon_1_1_osm, st_crs(cityoflondon_build_osm))

#This command returns the centroids of the buildings in each area.

cityoflondon_1_1_cent_os <-
  cityoflondon_1_1_os %>%
  st_centroid() %>%
  st_geometry()

cityoflondon_1_1_cent_osm <-
  cityoflondon_1_1_osm %>%
  st_centroid() %>%
  st_geometry()

#The command st_nn was used to detect the nearest neighbours between the centroids and calculate the  distance between them

distance_colondon <-
  st_nn(cityoflondon_1_1_cent_os, cityoflondon_1_1_cent_osm,
      sparse = TRUE,
      k = 1,
      returnDist = TRUE)

#Extracting the column that includes information about the distance between the centroids.

distance_colondon <- distance_colondon$dist

distance_colondon <- sapply(distance_colondon, FUN = mean)

#The summary command returns a statistical summary of the distance and sd() returns the standard deviation.

summary(distance_colondon)
sd(distance_colondon)

#--------
#Chelsea
#--------

chelsea_1_1_os <- st_read('/Users/christosnicolaou/R_Projects/thesis_data/OS_Data/Chelsea_OS_data/chelsea_1to1-2/chelsea_1to1_os.shp')
chelsea_1_1_osm <- st_read('/Users/christosnicolaou/R_Projects/thesis_data/OSM_Data/Chelsea_1to1/chelsea_1to1_osm.shp')

st_crs(chelsea_1_1_os)
st_crs(chelsea_1_1_osm)

chelsea_1_1_os <- st_transform(chelsea_1_1_os, st_crs(chelsea_build_os))
chelsea_1_1_osm <- st_transform(chelsea_1_1_osm, st_crs(chelsea_build_osm))

chelsea_1_1_cent_os <-
  chelsea_1_1_os %>%
  st_centroid %>%
  st_geometry()

chelsea_1_1_cent_osm <-
  chelsea_1_1_osm %>%
  st_centroid() %>%
  st_geometry()

distance_chelsea <-
  st_nn(chelsea_1_1_cent_os, chelsea_1_1_cent_osm,
        sparse = TRUE,
        k = 1,
        returnDist = TRUE)

distance_chelsea <- distance_chelsea$dist

distance_chelsea <- sapply(distance_chelsea, FUN = mean)

summary(distance_chelsea)

#------------
#Westminster
#------------

west_1_1_os <- st_read('/Users/christosnicolaou/R_Projects/thesis_data/OS_Data/west_build_os/west_os_1to1.shp')
west_1_1_osm <- st_read('/Users/christosnicolaou/R_Projects/thesis_data/OSM_data/west_build_osm/west_osm_1to1.shp')

st_crs(west_1_1_os)
st_crs(west_1_1_osm)

west_1_1_os <- st_transform(west_1_1_os, st_crs(leicester_build_os))
west_1_1_osm <- st_transform(west_1_1_osm, st_crs(leicester_build_osm))

west_1_1_cent_os <-
  west_1_1_os %>%
  st_centroid %>%
  st_geometry()

west_1_1_cent_osm <-
  west_1_1_osm %>%
  st_centroid %>%
  st_geometry()

distance_west <-
  st_nn(west_1_1_cent_os, west_1_1_cent_osm,
        sparse = TRUE,
        k = 1,
        returnDist = TRUE)

distance_west <- distance_west$dist

distance_west <- sapply(distance_west, FUN = mean)

summary(distance_west)

#----------
#Leicester
#----------

leic_1_1_os <- st_read('/Users/christosnicolaou/R_Projects/thesis_data/OS_Data/Leicester_OS_data/leic_1to1_os/leic_1to1_os.shp')
leic_1_1_osm <- st_read('/Users/christosnicolaou/R_Projects/thesis_data/OSM_Data/Leicester_1to1/leic_1to1_osm.shp')

st_crs(leic_1_1_os)
st_crs(leic_1_1_osm)

leic_1_1_os <- st_transform(leic_1_1_os, st_crs(leicester_build_os))
leic_1_1_osm <- st_transform(leic_1_1_osm, st_crs(leicester_build_osm))

leic_1_1_cent_os <-
  leic_1_1_os %>%
  st_centroid %>%
  st_geometry()

leic_1_1_cent_osm <-
  leic_1_1_osm %>%
  st_centroid() %>%
  st_geometry()

distance_leic <-
  st_nn(leic_1_1_cent_os, leic_1_1_cent_osm,
        sparse = TRUE,
        k = 1,
        returnDist = TRUE)

distance_leic <- distance_leic$dist

distance_leic <- sapply(distance_leic, FUN = mean)

summary(distance_leic)

#--------------------------------------------------------------------------------------------------------------------------

#-----
#PLOT
#-----

#---------------
#City of London
#---------------

#This section includes the commands used to plot the generated results of the work.

ggplot2::ggplot()+
  geom_sf(data = cityoflondon_bound, fill = NA)+
  geom_sf(data = cityoflondon_build_os, fill = NA, aes(color = "blue"), show.legend = "Ordnance Survey")+
  geom_sf(data = cityoflondon_build_osm, fill = NA, aes(color = "red"), show.legend = "OpenStreetMap")+
  scale_color_manual(values = c("blue", "red"),
                     labels = c("Ordnance Survey", "OpenStreetMap"))+
  labs(title = "City of London",
       colour = "Lengend")+
  theme(legend.position = c(0.1, 0.9))+
  
ggplot2::ggplot()+
  geom_sf(data = cityoflondon_bound, fill = NA)+
  geom_sf(data = cityoflondon_build_os, fill = NA, aes(color = "blue"), show.legend = "Ordnance Survey")+
  geom_sf(data = cityoflondon_build_osm, fill = NA, aes(color = "red"), show.legend = "OpenStreetMap")+
  geom_sf(data = cityoflondoncentroids_os, color = "blue", pch = 13)+
  geom_sf(data = cityoflondoncentroids_osm, color = "red", pch = 13)+
  scale_color_manual(values = c("blue", "red"),
                     labels = c("Ordnance Survey", "OpenStreetMap"))+
  labs(title = "City of London",
       colour = "Legend")+
  theme(legend.position = c(0.1, 0.9))

ggplot2::ggplot()+
  geom_sf(data = cityoflondon_bound, fill = NA)+
  geom_sf(data = cityoflondon_1_1_os, fill = NA, aes(color = "blue"), show.legend = "Ordnance Survey")+
  geom_sf(data = cityoflondon_1_1_osm, fill = NA, aes(color = "red"), show.legend = "OpenStreetMap")+
  geom_sf(data = cityoflondon_1_1_cent_os, color = "blue", pch = 13)+
  geom_sf(data = cityoflondon_1_1_cent_osm, color = "red", pch = 13)+
  scale_color_manual(values = c("blue", "red"),
                     labels = c("Ordnance Survey", "OpenStreetMap"))+
  labs(title = "City of London",
       colour = "Legend")+
  theme(legend.position = c(0.1, 0.9))

colondon_area_perc_table <- data.frame(colondon_area_perc)

barplot(height = colondon_area_perc_table$Area_Diff_Perc,
        main = "City of London",
        ylab = "OSM Coverage (%)")

hist(distance_colondon, breaks = 30, main = "City of London",
     xlab = "Distance in meters",
     ylab = "Number of Buildings")

#--------
#Chelsea
#--------

ggplot2::ggplot()+
  geom_sf(data = chelsea_bound, fill = NA)+
  geom_sf(data = chelsea_lsoa, fill = NA)+
  geom_sf(data = chelsea_build_os, fill = NA, lwd = 0.3, color = "blue")+
  geom_sf(data = chelsea_build_osm, fill = NA, lwd = 0.3, color = "red")

ggplot2::ggplot()+
  geom_sf(data = chelsea01, fill = NA)+
  geom_sf(data = chelsea01_build_os, fill = NA, aes(color = "blue"), show.legend = "Ordnance Survey")+
  geom_sf(data = chelsea01_build_osm, fill = NA, aes(color = "red"), show.legend = "OpenStreetMap")+
  scale_color_manual(values = c("blue", "red"),
                     labels = c("Ordnance Survey", "OpenStreetMap"))+
  labs(title = "Chelsea's 01 LSOA",
       colour = "Legend")+
  theme(legend.position = c(0.9, 0.9))

ggplot2::ggplot()+
  geom_sf(data = chelsea_bound, fill = NA)+
  geom_sf(data = chelsea_1_1_os, fill = NA, aes(color = "blue"), show.legend = "Ordnance Survey")+
  geom_sf(data = chelsea_1_1_osm, fill = NA, aes(color = "red"), show.legend = "OpenStreetMap")+
  geom_sf(data = chelsea_1_1_cent_os, fill = NA, color = "blue", pch = 13)+
  geom_sf(data = chelsea_1_1_cent_osm, fill = NA, color = "red", pch = 13)+
  scale_color_manual(values = c("blue", "red"),
                     labels = c("Ordnance Survey", "OpenStreetMap"))+
  labs(title = "Chelsea",
       colour = "Legend")+
  theme(legend.position = c(0.9, 0.9))

che_area_perc_table <- data.frame(che_area_perc)

barplot(height = che_area_perc_table$Area_Diff_Perc, main = "Kensington and Chelsea",
        names.arg = c("MSOA01", "MSOA02", "MSOA03", "MSOA04", "MSOA05", "MSOA06", "MSOA07", "MSOA08", "MSOA09", 
                      "MSOA10", "MSOA11", "MSOA12", "MSOA13", "MSOA14", "MSOA15", "MSOA16", "MSOA17", "MSOA18", 
                      "MSOA19", "MSOA20", "MSOA21"),
        ylab = "OSM Coverage (%)")

summary(che_area_perc)

che_type_included_perc_df <- data.frame(che_type_included_perc)

barplot(height = che_type_included_perc_df$Type_Included_Perc, main = "Kensington and Chelsea",
        names.arg = c("MSOA01", "MSOA02", "MSOA03", "MSOA04", "MSOA05", "MSOA06", "MSOA07", "MSOA08", "MSOA09", 
                      "MSOA10", "MSOA11", "MSOA12", "MSOA13", "MSOA14", "MSOA15", "MSOA16", "MSOA17", "MSOA18", 
                      "MSOA19", "MSOA20", "MSOA21"),
        ylab = "Buildings with Type (%)")

summary(che_type_included_perc)

che_area_type_table <- rep(che_area_perc, che_type_included_perc)

che_area_type_df <- data.frame(che_area_perc, che_type_included_perc)

hist(distance_chelsea, breaks = 30, main = "Kensington and Chelsea",
     xlab = "Distance in meters",
     ylab = "Number of Buildings")

summary(distance_chelsea)
sd(distance_chelsea)

#------------
#Westminster
#------------

ggplot2::ggplot()+
  geom_sf(data = westminster_bound, fill = NA)+
  geom_sf(data = west_lsoa,  fill = NA)+
  geom_sf(data = west_build_os, fill = NA, aes(color = "blue"), show.legend = "Ordnance Survey")+
  geom_sf(data = west_build_osm, fill = NA, aes(color = "red"), show.legend = "OpenStreetMap")+
  scale_color_manual(values = c("blue", "red"),
                     labels = c("Ordnance Survey", "OpenStreetMap"))+
  labs(title = "Westminster",
       colour = "Legend")+
  theme(legend.position = c(0.9, 0.9))

west_area_perc_df <- data.frame(west_area_perc)

barplot(height = west_area_perc_df$Area_Diff_Perc, main = "Westminster",
        names.arg = c("MSOA01", "MSOA02", "MSOA03", "MSOA04", "MSOA05", "MSOA06", "MSOA07", "MSOA08", "MSOA09", 
                      "MSOA10", "MSOA11", "MSOA12", "MSOA13", "MSOA14", "MSOA15", "MSOA16", "MSOA17", "MSOA18", 
                      "MSOA19", "MSOA20", "MSOA21", "MSOA22", "MSOA23", "MSOA24"),
        ylab = "OSM Coverage (%)")

summary(west_area_perc)

west_type_included_perc_df <- data.frame(west_type_included_perc)

barplot(height = west_type_included_perc_df$Type_Included_Perc, main = "Westminster",
        names.arg = c("MSOA01", "MSOA02", "MSOA03", "MSOA04", "MSOA05", "MSOA06", "MSOA07", "MSOA08", "MSOA09", 
                      "MSOA10", "MSOA11", "MSOA12", "MSOA13", "MSOA14", "MSOA15", "MSOA16", "MSOA17", "MSOA18", 
                      "MSOA19", "MSOA20", "MSOA21", "MSOA22", "MSOA23", "MSOA24"),
        ylab = "Buildings with Type (%)")

summary(west_type_included_perc)

hist(distance_west, breaks = 30, main = "Westminster",
     xlab = "Distance in meters",
     ylab = "Number of Buildings")

summary(distance_west)

sd(distance_west)

#----------
#Leicester
#----------

ggplot2::ggplot()+
  geom_sf(data = leicester_bound, fill = NA)+
  geom_sf(data = leicester_lsoa, fill = NA)+
  geom_sf(data = leicester_build_os, fill = NA, lwd = 0.3, color = "blue")+
  geom_sf(data = leicester_build_osm, fill = NA, lwd = 0.3, color = "red")

ggplot2::ggplot()+
  geom_sf(data = leicester01, fill = NA)+
  geom_sf(data = leicester01_build_os, fill = NA, aes(color = "blue"), show.legend = "Ordnance Survey")+
  geom_sf(data = leicester01_build_osm, fill = NA, aes(color = "red"), show.legend = "OpenStreetMap")+
  scale_color_manual(values = c("blue", "red"),
                     labels = c("Ordnance Survey", "OpenStreetMap"))+
  labs(title = "Leicester's 01 LSOA",
       colour = "Legend")+
  theme(legend.position = c(0.8,0.3))

leic_area_perc_table <- data.frame(leic_area_perc)

barplot(height = leic_area_perc_table$Area_Diff_perc, main = "Leicester",
        names.arg = c("MSOA01", "MSOA02", "MSOA03", "MSOA04", "MSOA05", "MSOA06", "MSOA07", "MSOA08", "MSOA09", 
                      "MSOA10", "MSOA11", "MSOA12", "MSOA13", "MSOA16", "MSOA17", "MSOA18", 
                      "MSOA19", "MSOA20", "MSOA21", "MSOA22", "MSOA23", "MSOA25", "MSOA26", "MSOA27", 
                      "MSOA28", "MSOA29", "MSOA30", "MSOA31", "MSOA32", "MSOA34", "MSOA35", "MSOA36", 
                      "MSOA37", "MSOA38", "MSOA39", "MSOA40", "MSOA41"),
        ylab = "OSM Coverage (%)")

summary(leic_area_perc)

leic_type_included_df <- data.frame(leic_type_included)

barplot(height = leic_type_included_df$Type_Included_Perc, main = "Leicester",
        names.arg = c("MSOA01", "MSOA02", "MSOA03", "MSOA04", "MSOA05", "MSOA06", "MSOA07", "MSOA08", "MSOA09", 
                      "MSOA10", "MSOA11", "MSOA12", "MSOA13", "MSOA16", "MSOA17", "MSOA18", 
                      "MSOA19", "MSOA20", "MSOA21", "MSOA22", "MSOA23", "MSOA25", "MSOA26", "MSOA27", 
                      "MSOA28", "MSOA29", "MSOA30", "MSOA31", "MSOA32", "MSOA34", "MSOA35", "MSOA36", 
                      "MSOA37", "MSOA38", "MSOA39", "MSOA40", "MSOA41"),
        ylab = "Buildings with Type (%)")

summary(leic_type_included)

hist(distance_leic, breaks = 30, main = "Leicester",
     xlab = "Distance in meters",
     ylab = "Number of Buildings")

summary(distance_leic)

sd(distance_leic)


