## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  echo=TRUE,
  warning = FALSE
)



## ----basin_shp, fig.show='hold'-----------------------------------------------
library(sf)
library(magrittr)
library(scales)

basin <- "/home/ecor/local/rpackages/jrc/soilgridr/inst/ext_data/hybas_1071428650.shp" %>% st_read()

basin


## ----basin_grid, fig.show='hold'----------------------------------------------
library(terra)

rr <- rast(basin,res=0.0025)
rr


## ----soil_download, fig.show='hold',fig.width=10------------------------------
library(soilgridr)
#library(scales)
#library(ggplot2)
##
library(stringr)
library(rlang)
library(gdalUtilities)
##
###source("/home/ecor/local/rpackages/jrc/soilgridr/R/soilgridmap.R")
vois <- c("clay","silt","sand")
out <- list()

outfile <- "/home/ecor/local/rpackages/jrc/soilgridr/inst/ext_data/study_area_soil2_%s.tif"
for (voi in vois) {
  filename <- outfile %>% sprintf(voi)
  if (!file.exists(filename)) {
    out[[voi]] <- soilgridmap_from_vrt(rr,voi=voi,quantiles="mean",align=TRUE,use_crop=TRUE,filename=filename,overwrite=TRUE)
  }  else {
    out[[voi]] <- rast(filename)
  }
}

out

## ----soil_4layer, fig.show='hold',fig.height=5,fig.width=10-------------------
## Plotting the 4th layer (30-60 cm)
outp <- lapply(X=out,FUN=function(x){x[[4]]}) %>% rast()  ## 4th layer (30-60 cm)
library(ggplot2)
library(tidyterra)
cols <- brewer_pal(type="div",palette="RdYlGn",direction=-1)(10)
gg <- ggplot()+geom_spatraster(data=outp)+facet_wrap(~lyr)+theme_bw()
gg <- gg+scale_fill_gradientn(colors=cols,na.value="white")
gg <- gg+geom_sf(data=basin,fill=NA,color=muted("blue"))
gg



## ----soil_4layer_plet, fig.show='hold',fig.height=5,fig.width=10--------------

##tiles <- "Esr
##plet(vect(basin),fill=FALSE) 
outp2 <- project(outp,y="epsg:3857",method="near")
plet(outp2,names(outp2),tiles="OpenTopoMap") %>% lines(vect(basin))

## ----soil_4layer_RGB, fig.show='hold',fig.height=5,fig.width=10---------------


outpr <- outp/sum(outp)



####
vm <- list()
for (it in names(outpr)) {
  
  vm[[it]] <- which.max(outpr[[it]][])
  
}
df <- data.frame(type=names(vm),i=unlist(vm))
dc <- as.data.frame(t(outpr[df$i]))
names(dc) <- c("r","g","b")
df <- xyFromCell(outp,df$i) %>% cbind(df) %>% cbind(dc)
df$color <- rgb(red=df$r,green=df$g,blue=df$b,maxColorValue=1) ##c("red","green","blue")
#####
gg <- ggplot()+geom_spatraster_rgb(data=outpr*255)+theme_bw()
gg <- gg+geom_sf(data=basin,fill=NA,color="black")
##gg <- gg+scale_color_manual(values=c("red","green","blue"),breaks=names(outpr),name="Legend")
##gg <- gg+scale_color_manual(values=c("red","green","blue"),breaks=names(outpr),name="Legend")
colors_ <- df$color
names(colors_) <- df$type 

gg <- gg+geom_point(aes(x=x,y=y,color=type),data=df,size=1,show.legend=TRUE)+scale_color_manual(values=colors_,breaks=df$type,name="Legend")


gg



## ----generateBibliography,echo=FALSE,eval=TRUE,message=FALSE,warning=FALSE,print=FALSE,results="hide"----

require(knitcitations)
cleanbib()
options(citation_format="pandoc")
read.bibtex(file = "bibliography.bib")



