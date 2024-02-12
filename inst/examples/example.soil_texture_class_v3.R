###
### 
library(soilgridr)
library(terra)
library(stringr)
library(ggtern)
library(dplyr)
library(magrittr)
library(sf)

data(USDA)
names(USDA) <- tolower(names(USDA))

library(stars)

##source("/home/ecor/local/rpackages/jrc/soilgridr/R/soil_texture_class.R")


dem <-  rast(system.file('tif/olinda_dem_utm25s.tif',package="stars"))
##dem <-  rast(system.file('ext_data/vinschgau_elevation.tif',package="soilgridr"))
filenamex <- "/home/ecor/local/rpackages/jrc/soilgridr/inst/ext_data/vischgau_05_15cm_%s.tif"
soilm <- list()
vois <- c("clay","sand","silt")
for (voi in vois) {
  filename <- filenamex %>% sprintf(voi)
  soilm[[voi]] <- soilgridmap_from_vrt(dem,voi=voi,align=TRUE,use_crop=TRUE,quantiles="mean",zl=c(5,15),filename=filename,overwrite=TRUE)
  
}


out <- soil_texture_class(soilm,usda_classes=USDA)