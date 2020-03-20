#####LOAD PACKAGES AND SET WD####
library(sf)
library(tidyverse)
library(tmap)
library(tidycensus)
library(tigris)
library(raster)
library(dplyr)
library(readr)
library(maps)
library(ggthemes)
library(leaflet)
library(gganimate)
library(ggplot2)
library(plotly)
library(ggmap)
library(rgeos)
library(rgdal)


setwd("Z:/finalProject")

############CREATE PLOT OF GMSL############
gmslYr<-read_csv("histSLR/archive/csiro_alt_gmsl_yr_2015.csv")
gmslMo<-read_csv("histSLR/archive/csiro_alt_gmsl_mo_2015.csv")
gmslPlot<- ggplot(gmslYr, aes(x=Time, y=GMSL)) + geom_line(color="Blue") +
  theme_minimal() + ylab("Sea Height Variation (mm)") 
ggplotly(gmslPlot)

########## LOAD CENSUS DATA#############
census_api_key("2261de883be84ef5125887716e3c8ff1dd812446")

nyAcs <- load_variables(2017, "acs5", cache = TRUE)

nyTracts <- get_acs(geography = "tract", table = "B19013", 
                    year = 2017,  output = "wide", state = "NY", 
                    county= c("New York", "Kings", "Bronx", "Richmond", "Queens") , 
                    geometry = TRUE)

############## TIDY CENSUS DATA #########
nyTracts <- nyTracts[-c(2,4)] %>%
  drop_na() %>%
  rename(est=B19013_001E) %>%
  st_transform(crs = 2263)
######### LOAD BOUNDARY DATA #####
#DID NOT USE IN FINAL PROJECT 
nyBound<- st_read("bbound/geo_export_f2aaa0af-6260-4aa9-9dbb-08b97ac97c2c.shp")
st_is_valid(nyBound)
###### LOAD SLR DATA #############
slr20FP <- st_read("slrHunYr/hunYr20s.shp")
slr50FP<- st_read("slrHunYr/hunYr50s.shp")
#######LOAD SANDY DATA ############
sandy<-st_read("sandyInZone/sandyInZone.shp")
########## LOAD FEMA DATA #######


bfe<-st_read("PFIRM/s_bfe.shp")
cbrs <- st_read("PFIRM/s_cbrs.shp")
fldHaz <- st_read("PFIRM/s_fld_haz_ar.shp")

#DID NOT USE THIS DATA IN FINAL PRODUCT 
cst<- st_read("PFIRM/s_CST_TSCT_LN.shp")
firm <- st_read("PFIRM/s_firm_pan.shp")
genStruct<-st_read("PFIRM/s_gen_struct.shp")
lab <-st_read("PFIRM/s_label_ld.shp")
limwa<-st_read("PFIRM/S_LiMWA.shp")
permBmk<-st_read("PFIRM/s_perm_bmk.shp")
polAr<-st_read("PFIRM/s_pol_ar.shp")
polLn<-st_read("PFIRM/s_pol_ln.shp")
quadInd<-st_read("PFIRM/s_quad_index.shp")
wtrAr<-st_read("PFIRM/s_wtr_ar.shp")
wtrLn<-st_read("PFIRM/s_wtr_ln.shp")
xs<-st_read("PFIRM/s_xs.shp")
baseInd <- st_read("PFIRM/s_base_index.shp")

######VALIDATE OBJECTS#####
st_is_valid(fldHaz)
st_is_valid(slr20FP)
st_is_valid(slr50FP)
st_is_valid(sandy)
######## MAP 1 ###########
tmap_mode("view")
nyMap<- 
  tm_shape(nyTracts) +
  tm_fill(col = "est",
          palette = "YlOrBr", alpha = 0.7,
          title = "Median Household Income Estimates") +
  tm_shape(fldHaz)+ 
  tm_polygons(col= "FLD_ZONE", palette = "Reds", alpha = 0.7)+
  tm_shape(slr20FP) +
  tm_fill(col="abfe_0_2pc", alpha = 0.5,
          palette="Blues",
          legend.show = FALSE)+
  tm_shape(slr50FP) +
  tm_fill(col="abfe_0_2pc", alpha = 0.5,
          palette="Blues",
          legend.show = FALSE) + 
  tm_basemap(server = "CartoDB.Positron")
nyMap   
###########PFRIM MAP############
tmap_mode("view")

pfrimMap <-
  tm_shape(fldHaz)+ 
  tm_polygons(col= "FLD_ZONE", palette = "Reds", alpha = 0.7) + 
  tm_shape(bfe)+ 
  tm_lines(col="ELEV", palette = "YlGnBu", n=3)+
  tm_shape(cbrs)+ 
  tm_polygons() + 
  tm_shape(sandy)+ 
  tm_polygons(col="comments", palette="Blues", legend.show=FALSE)+ 
  tm_basemap(server = "CartoDB.Positron")
pfrimMap

#####CREATING AN ANIMATED MAP WAS UNSUCCESSFUL#####
#load shapefiles
nyBoundGG<- readOGR("bbound","geo_export_f2aaa0af-6260-4aa9-9dbb-08b97ac97c2c")
slr50 <-readOGR("2050s Mean Sea Level","geo_export_e1b71ed5-69f3-4e9e-9551-262f8be3b5c1")
slr80 <- readOGR("2080s Mean Sea Level", "geo_export_4d3142bb-b12b-4523-9eb0-60af98a52029")
slr00 <- readOGR("2100s Mean Sea Level", "geo_export_178738be-06b3-4c6e-856f-00f8c24668db")
#fortify shapefiles so they can be used in ggplot
nyBoundDF <- fortify(nyBoundGG)
slr50DF <- fortify(slr50)
slr80DF <- fortify(slr80)
slr00DF <- fortify(slr00)
#add column to data frame for gif manipulation 
slr50DF$year=2050
slr80DF$year=2080
slr00DF$year=2100
#combine dataframes
projTides<- rbind(slr50DF, slr80DF, slr00DF)
#map the projected floodplains from 2050-2100
#create static plots
boundMap<- ggplot() +
  geom_path(data = nyBoundDF, aes(x = long, y = lat, group = group)) + theme_map()
print(boundMap)

aniMap <- boundMap + 
  geom_polygon(data=projTides, aes(x = long, y = lat, group=group), colour = 'blue')
print(aniMap) 
#animate static plot 
aniMap + transition_states(year, wrap=FALSE)
