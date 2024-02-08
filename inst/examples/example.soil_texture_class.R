###
### 
library(soilgridr)
library(terra)
library(stringr)
source("~/local/rpackages/jrc/soilgridr/R/soil_texture_class.R", echo=TRUE)

library(stars)
dem <-  rast(system.file('tif/olinda_dem_utm25s.tif',package="stars"))
soilm <- list()
vois <- c("clay","sand","silt")
for (voi in vois) {
  
  soilm[[voi]] <- soilgridmap_from_vrt(dem,voi=voi,align=TRUE,use_crop=TRUE,quantiles="mean",filename = "")
  
}