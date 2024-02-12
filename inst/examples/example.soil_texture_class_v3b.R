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

## 
usda_csv <- "/home/ecor/local/rpackages/jrc/soilgridr/inst/ext_data/usda_classes.csv"
write.table(USDA,file=usda_csv,row.names=FALSE,quote=FALSE,sep=",")
##
USDA <- read.table(usda_csv,sep=",",header=TRUE)


library(stars)

##source("/home/ecor/local/rpackages/jrc/soilgridr/R/soil_texture_class.R")

dem <-  rast(system.file('ext_data/vinschgau_elevation.tif',package="soilgridr"))
filenamex <- "%s/vischgau_05_15cm_%s.tif" ##  /home/ecor/local/rpackages/jrc/soilgridr/inst/ext_data/vischgau_05_15cm_%s.tif"
soilm <- list()
vois <- c("clay","sand","silt")
for (voi in vois) {
  filename <- filenamex %>% sprintf(system.file('ext_data',package="soilgridr"),voi)
  if (file.exists(filename)) {
    
    soilm[[voi]] <- rast(filename)
    
  } else {
    soilm[[voi]] <- soilgridmap_from_vrt(dem,voi=voi,align=TRUE,use_crop=TRUE,quantiles="mean",zl=c(5,15))
  }
}


out <- soil_texture_class(soilm,usda_classes=USDA)