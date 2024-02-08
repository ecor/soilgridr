

rm(list=ls())

library(soilgridr)
library(terra)
library(stringr)
library(gdalUtilities)
library(rlang)
##source("~/local/rpackages/jrc/soilgridr/R/soilgridmap.R", echo=TRUE)
options(warn = -1)
#### https://www.google.com/search?q=pedo-transfer+function+literature+review&sca_esv=601160025&rlz=1C1VDKB_itIT1087IT1087&sxsrf=ACQVn085xYYcW0ObpXcicCS-mPDT7tOITA%3A1706127745628&ei=gXGxZZv8JbCKxc8Ppe2iiAk&ved=0ahUKEwjbwOCR7faDAxUwRfEDHaW2CJEQ4dUDCBA&uact=5&oq=pedo-transfer+function+literature+review&gs_lp=Egxnd3Mtd2l6LXNlcnAiKHBlZG8tdHJhbnNmZXIgZnVuY3Rpb24gbGl0ZXJhdHVyZSByZXZpZXdI8C9QAFijLnAAeAGQAQCYAXygAdkQqgEFMTIuMTC4AQPIAQD4AQHCAgYQABgHGB7CAggQABgHGB4YD8ICCBAAGAUYBxge4gMEGAAgQQ&sclient=gws-wiz-serp
set.seed(200)
f <- system.file("ex/elev.tif", package="terra")
r <- rast(f)
soilgrid_default_clay <- soilgridmap_from_vrt(r)

set.seed(201)
library(stars)
olinda <-  rast(system.file('tif/olinda_dem_utm25s.tif',package="stars"))
voi <- 'sand'
olinda_soilgrid_default_sand <- soilgridmap_from_vrt(olinda,voi=voi,align=TRUE)


set.seed(202)
vinschgau <-  rast(system.file('ext_data/vinschgau_elevation.tif',package="soilgridr"))
vinschgau_soilgrid_default_sand <- soilgridmap_from_vrt(vinschgau,voi="sand",align=TRUE,use_crop=TRUE)