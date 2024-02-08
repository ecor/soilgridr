NULL
#' Getting SoilGRid Dataset through WebDav: R procedure to download vrt fils and import into 'SpatRast' object. 
#'
#'
#'
#' @param x a \code{\link{SpatRaster-class}} object. 
#' @param destdir directory where to write the SoilGrids output raw files.
#' @param voi variable of interest. See options.
#' @param zl soil layer depths (numeric)
#' @param depths soil layer names as named in SoilGrid. See default.
#' @param quantiles quantile names. See default. 
#' @param igh proj string for Homolosine projection. See SoilGRid documentation (URL below). 
#' @param sg_url url prefix (directory) to vrt files.  See SoilGRid documentation (URL below). 
#' @param use_crop logical. Default is \code{FALSE}. If \code{TRUE}, the outcome is cropped with \code{x} extent. 
#' @param method,align,filename,overwrite,... further arguments for \code{\link{project}} (and \code{\link{writeRaster}}) in which its argument \code{y} is set equal to this function's argument \code{x}.
#' 
#' @seealso \url{https://git.wur.nl/isric/soilgrids/soilgrids.notebooks/-/blob/master/markdown/webdav_from_R.md},
#' \url{https://www.isric.org/explore/soilgrids/faq-soilgrids} ,
#' \url{https://essd.copernicus.org/articles/12/299/2020/essd-12-299-2020.html},
#' \url{https://www.isric.org/sites/default/files/GlobalSoilMap_specifications_december_2015_2.pdf}
#'
#' @importFrom gdalUtilities gdal_translate
#' @importFrom magrittr %>% 
#' @importFrom terra rast ext crs project crop
#' @importFrom stringr str_sub
#' @importFrom rlang as_string ensym
#' 
#' @export
#'
#' @examples
#' 
#' library(terra)
#' f <- system.file("ex/elev.tif", package="terra")
#' r <- rast(f)
#' soilgrid_default_clay <- soilgridmap_from_vrt(r)
#' 
#' 
#' library(stars)
#' olinda <-  rast(system.file('tif/olinda_dem_utm25s.tif',package="stars"))
#' olinda_soilgrid_default_sand <- soilgridmap_from_vrt(olinda,voi="sand",align=TRUE)
#' 
#' vinschgau <-  rast(system.file('ext_data/vinschgau_elevation.tif',package="soilgridr"))
#' vinschgau_soilgrid_default_sand <- soilgridmap_from_vrt(vinschgau,voi="sand",align=TRUE,use_crop=TRUE)
#' 
#'
#' 
#'
soilgridmap_from_vrt <- function(x,
                                 destdir=tempdir(),
                                 voi=c("bdod","cec","cfvo","clay","nitrogen","phh2o","sand","silt","soc","ocd","ocs")[4],
                                 zl=c(0,5,15,30,60,100,200),
                                 depths=sprintf("%d-%dcm",zl[-length(zl)],zl[-1]),
                                 quantiles=c("Q0.05","Q0.50","mean","Q0.95","Uncertainty")[c(-2,-5)],
                                 igh='+proj=igh +lat_0=0 +lon_0=0 +datum=WGS84 +units=m +no_defs',  # proj string for Homolosine projection
                                 sg_url="/vsicurl?max_retry=3&retry_delay=1&list_dir=no&url=https://files.isric.org/soilgrids/latest/data/",
                                 method="near",align=FALSE,use_crop=FALSE,filename=NULL,overwrite=NA,...
                                 )
                                 
 {
  
  if (str_sub(destdir,start=-1)=="/") destdir <- str_sub(destdir,end=-2)
  if (str_sub(sg_url,start=-1)=="/") sg_url <- str_sub(sg_url,end=-2)
  xname <- as_string(ensym(x))
  bb=terra::project(x=ext(x),from=crs(x),to=igh)
  bb=as.vector(bb)[c("xmin","ymax","xmax","ymin")]
  out <- list()
  if (length(voi)>1) voi <- voi[1]  
  for (itq in quantiles) {
   ## out[[itq]] <- list()
    for (depth in depths) {
      out[[itq]][[depth]] <- NA 
      src_dataset="%s/%s/%s_%s_%s.vrt" %>% sprintf(sg_url,voi,voi,depth,itq)
      dst_dataset="%s/%s_%s_%s_%s.tif" %>% sprintf(destdir,xname,voi,depth,itq)
      out[[paste(itq,depth,sep="_")]] <- gdal_translate(src_dataset,
                                     dst_dataset,
                                     tr=c(250,250),
                                     projwin=bb,
                                     projwin_srs =igh) %>% rast()
      }
    
    
    
    
    
  }
  
  out <- rast(out)

  if (align==TRUE) {
    
    ##nn_out <- names(out)
    
    ##overwrite <- list(...)$overwrite
    ##filename <- list(...)$filename
    filename2 <- filename
    if (length(overwrite)==0) overwrite <- NA
    if (is.null(overwrite)) overwrite <- NA 
    if (is.na(overwrite)) overwrite <- FALSE
    
    if (use_crop) filename2 <- NULL
    ###
    out2 <- project(out,y=x,align=align,method=method,filename=filename2,overwrite=overwrite,...)
    names(out2) <- names(out)
       
    out <- out2
    if (use_crop) {
     
     out <- crop(out,x,filename=filename,overwrite=overwrite)
             
      
    }  
    
  }
  return(out)
  
}
