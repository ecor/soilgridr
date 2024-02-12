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

## polygos must be closed!!!
#closing_df <- function(x,matrix=TRUE) { out <- x[c(1:nrow(x),1),];if (matrix) out <- as.matrix(out);return(out)}
#usda_classes <- split(USDA,USDA$label) %>% lapply(closing_df,matrix=TRUE) %>% lapply(as.data.table) %>% rbindlist()

##

source("~/local/rpackages/jrc/soilgridr/R/soil_texture_class.R", echo=TRUE)

library(stars)
dem <-  rast(system.file('tif/olinda_dem_utm25s.tif',package="stars"))
dem <-  rast(system.file('ext_data/vinschgau_elevation.tif',package="soilgridr"))

soilm <- list()
vois <- c("clay","sand","silt")
for (voi in vois) {
  
  soilm[[voi]] <- soilgridmap_from_vrt(dem,voi=voi,align=TRUE,use_crop=TRUE,quantiles="mean",zl=c(5,15)) ##,filename = "")
  
}


out <- soil_texture_class(soilm,usda_classes=USDA)