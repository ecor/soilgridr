NULL
#' Soil textural classes
#' 
#' @param x input soil maps
#' @param sand,clay,silt names for sand , clay and silt
#' @param usda_classes data frame with soil USDA classes
#' @param usda_label string column name in \code{usda_classes} with soil classes names
#' @param ... further argument for \code{\link{writeRaster}}
#' 
#' @export
#' 
#' 
#' @importFrom data.table as.data.table
#' @importFrom dplyr mutate select
#' @importFrom terra writeRaster
#' @importFrom sf st_within st_touches st_polygon st_as_sfc st_as_sf st_sf
#' @importFrom rlang .data
#' @examples
#' 
#'
#'
#' usda_soil_classes_csv <- system.file('ext_data/usda_classes.csv',package="soilgridr") 
#' usda_soil_classes <- read.table(usda_soil_classes_csv,sep=",",header=TRUE)
#' 
#' dem <-  rast(system.file('ext_data/vinschgau_elevation.tif',package="soilgridr"))
#' filenamex <- "%s/vischgau_05_15cm_%s.tif" ##  /home/ecor/local/rpackages/jrc/soilgridr/inst/ext_data/vischgau_05_15cm_%s.tif"
#' soilm <- list()
#' vois <- c("clay","sand","silt")
#' for (voi in vois) {
#'   filename <- filenamex %>% sprintf(system.file('ext_data',package="soilgridr"),voi)
#'   if (file.exists(filename)) {
#'     
#'     soilm[[voi]] <- rast(filename)
#'     
#'   } else {
#'     soilm[[voi]] <- soilgridmap_from_vrt(dem,voi=voi,align=TRUE,use_crop=TRUE,quantiles="mean",zl=c(5,15))
#'   }
#' }
#' 
#' 
#' out <- soil_texture_class(soilm,usda_classes=usda_soil_classes)
#' 
#' 
#' 

soil_texture_class <- function(x=NULL,sand="sand",clay="clay",silt="silt",usda_classes,usda_label="label",...)  {
  
  
  if (is.null(x)) x <- list()
  if (is.character(sand)) x$sand <- x[[sand]]
  if (is.character(clay)) x$clay <- x[[clay]]
  if (is.character(clay)) x$silt <- x[[silt]]
  
  x <- rast(x)
  
  x <- x[[c("sand","clay","silt")]]
  
  x <- x/sum(x)
  nas <- min(is.na(x))
  x[nas] <- 1/3
  
  vals<- as.data.table(x) %>% mutate(X=clay,Y=sand) %>% st_as_sf(coords=c("X","Y"))
  
  
  #### lapply(list) %>% lapply(st_polygon) 
  names(usda_classes)[names(usda_classes)==usda_label] <- "label2"## <- usda_classes[[usda_label]]
  names(usda_classes)[names(usda_classes)==sand] <- "sand"
  names(usda_classes)[names(usda_classes)==silt] <- "silt"
  names(usda_classes)[names(usda_classes)==clay] <- "clay"
  
  ## polygos must be closed!!!
  closing_df <- function(x,matrix=TRUE) { out <- x[c(1:nrow(x),1),];if (matrix) out <- as.matrix(out);return(out)}
  geoms <- usda_classes %>% mutate(X=clay,Y=sand) %>% split(usda_classes$label2) %>% lapply(select,.data$X,.data$Y) %>% lapply(closing_df,matrix=TRUE) %>% lapply(list) %>% lapply(st_polygon) 
  geoms2 <- st_sf(label=names(geoms),geometry=st_as_sfc(geoms)) ## SISTEMARE NAs
  geoms2$ID <- 1:nrow(geoms2)
  
 
  uu <- st_within(vals,geoms2)
  uu2 <- st_touches(vals,geoms2)
  iuu <- which(sapply(uu,length)==0)
  uu[iuu] <- uu2[iuu]
  uu10 <- sapply(uu,FUN=function(x){x[1]})

  vals$ID <- geoms2$ID[unlist(uu10)]
  out <- x[[1]]
  out[] <- vals$ID
  out[nas==1] <- NA
  levels(out) <- geoms2 %>% as.data.frame %>% select(.data$ID,.data$label)
  ###writeRaster(out_soil_texture_class,filename=out_soil_texture_class_file,overwrite=TRUE)
  ll <- list(...)
  if (length(ll)>0) {
    
    out <- writeRaster(x=out,...)
  }  
  return(out)
  
}




# 
# 
# 
# library(data.table)
# library(sf)
# 
# closing <- function(x) {x[c(1:length(x),1)]}
# closing_df <- function(x,matrix=TRUE) { out <- x[c(1:nrow(x),1),];if (matrix) out <- as.matrix(out);return(out)}
# 
# ### 
# out_soil_texture_class_file <- paste0(outwpath,"/lake_tanganyika_basin_soil_textural_class.grd")
# geoms <- USDA %>% mutate(X=clay,Y=sand) %>% split(USDA$label) %>% lapply(select,X,Y) %>% lapply(closing_df,matrix=TRUE) %>%     lapply(list) %>% lapply(st_polygon) 
# geoms2 <- st_sf(label=names(geoms),geometry=st_as_sfc(geoms)) ## SISTEMARE NAs
# geoms2$ID <- 1:nrow(geoms2)
# 
# cond <- file.exists(outfile)
# 
# 
# if (!cond) {
#   
#   
#   
#   outpr2 <- outpr
#   nas <- min(is.na(outpr))
#   
#   outpr2[nas==1] <- 1/3
#   
#   vals<- as.data.table(outpr2) %>% mutate(X=clay,Y=sand) %>% st_as_sf(coords=c("X","Y"))
#   
#   uu <- st_within(vals,geoms2)
#   uu2 <- st_touches(vals,geoms2)
#   iuu <- which(sapply(uu,length)==0)
#   uu[iuu] <- uu2[iuu]
#   
#   
#   uu10 <- sapply(uu,FUN=function(x){x[1]})
#   ##
#   vals$ID <- geoms2$ID[unlist(uu10)]
#   out_soil_texture_class <- outpr2[[1]]
#   out_soil_texture_class[] <- vals$ID
#   out_soil_texture_class[nas==1] <- NA
#   levels(out_soil_texture_class) <- geoms2 %>% as.data.frame %>% select(ID,label)
#   writeRaster(out_soil_texture_class,filename=out_soil_texture_class_file,overwrite=TRUE)
#   
# } 
# 
# 
# out_soil_texture_class <- rast(out_soil_texture_class_file)
# levels(out_soil_texture_class) <- geoms2 %>% as.data.frame %>% select(ID,label)
# 
# #outs <- USDA %>% select(label,clay,sand) %>% st_as_sf(coords=c("sand","clay")) %>% as.data.frame() %>% group_by(label) %>% #summarize(geometry=do.call(closing(geometry),what="c")) %>% ungroup() %>% st_as_sf()
# 
# ##%>% group_by(label) %>% summarize(geometry=st_polygon(list((cbind(closing(clay),closing(sand)))))) %>% ungroup()
# gt <- ggplot()+geom_spatraster(data=out_soil_texture_class)+theme_bw()
# gt <- gt+geom_sf(data=basin,fill=NA,color="black")
# gt <- gt+scale_fill_manual(values=USDA.LAB$color,breaks=USDA.LAB$label,name="Textural Class",na.value="white")
# gt
# 
# 
# 
