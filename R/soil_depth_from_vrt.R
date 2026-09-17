## --------------------------------------------------------------
## 1) SOIL DEPTH (depth to bedrock) - SoilGrids250m 2017
## --------------------------------------------------------------
## This layer (BDTICM, "Absolute Depth To Bedrock", in cm) is no
## longer part of the current SoilGrids version (soilgrids.org
## v2.0, the one downloaded by soilgridr::soilgridmap_from_vrt),
## but it is still available in the ISRIC historical archive. It is
## read directly in streaming mode via GDAL /vsicurl, without
## downloading the whole (global, heavy) file.
##
## soil_depth_from_vrt() below mirrors soilgridmap_from_vrt()'s
## interface (x = SpatRaster template, gdal_translate crop via
## /vsicurl, align/use_crop/filename/overwrite arguments), but
## targets this single, older BDTICM GeoTIFF instead of the VRT
## stack of current properties.
NULL
#' Getting the SoilGrids250m (2017) "Absolute Depth To Bedrock" (BDTICM)
#' layer through WebDAV, cropped to a reference raster's extent.
#'
#' @param x a \code{\link{SpatRaster-class}} object, used only to define
#'   the crop extent (its own values/resolution are not used, unlike
#'   in \code{align = TRUE}, see below).
#' @param destdir directory where to write the cropped raw GeoTIFF.
#' @param sg_url URL (vsicurl) to the BDTICM GeoTIFF. See default.
#' @param method,align,use_crop,filename,overwrite,... same meaning as in
#'   \code{soilgridr::soilgridmap_from_vrt}: if \code{align = TRUE} the
#'   output is reprojected/aligned onto \code{x}'s grid via
#'   \code{\link{project}} (and optionally cropped to \code{x}'s exact
#'   extent if \code{use_crop = TRUE}).
#'
#' @details Unlike the current SoilGrids VRTs (which are natively in
#'   Homolosine projection), BDTICM_M_250m_ll.tif is already in
#'   geographic coordinates (EPSG:4326, hence the "_ll" suffix), so no
#'   reprojection of the bounding box is required before cropping.
#'
#' @seealso \url{https://data.isric.org/geonetwork/srv/api/records/f36117ea-9be5-4afd-bb7d-7a3e77bf392a},
#' \url{https://files.isric.org/soilgrids/former/2017-03-10/data/}
#'
#' @export
#'
#' @examples
#' library(terra)
#' f <- system.file('ex/elev_vinschgau.tif', package="terra")
#' r <- rast(f)
#' depth_default <- soil_depth_from_vrt(r)
#' 
soil_depth_from_vrt <- function(x,
                                destdir = tempdir(),
                                sg_url = "/vsicurl/https://files.isric.org/soilgrids/former/2017-03-10/data/BDTICM_M_250m_ll.tif",
                                method = "near", align = TRUE, use_crop = TRUE,
                                filename = NULL, overwrite = NA, ...) {
  
  if (str_sub(destdir, start = -1) == "/") destdir <- str_sub(destdir, end = -2)
  
  xname <- as_string(ensym(x))
  print(xname)
  ## BDTICM is already in EPSG:4326, so the crop window only needs
  ## reprojecting x's extent to EPSG:4326 (no Homolosine conversion,
  ## unlike soilgridmap_from_vrt)
  bb <- terra::project(x = ext(x), from = crs(x), to = "EPSG:4326")
  bb <- as.vector(bb)[c("xmin", "ymax", "xmax", "ymin")]
  
  dst_dataset <- "%s/%s_soil_depth.tif" %>% sprintf(destdir, xname)
  print(dst_dataset)
  print(sg_url)
  print(bb)
  out <- gdal_translate(sg_url,
                        dst_dataset,
                        ## tr = c(250, 250),
                         projwin = bb,
                         projwin_srs = "EPSG:4326") %>% rast()
  ##out <- sg_url |> rast() |> crop(y=bb)
  names(out) <- "soil_depth_cm"
  
  if (align == TRUE) {
    filename2 <- filename
    if (length(overwrite) == 0) overwrite <- NA
    if (is.null(overwrite)) overwrite <- NA
    if (is.na(overwrite)) overwrite <- FALSE
    if (use_crop) filename2 <- NULL
    
    out2 <- project(out, y = x, align = align, method = method,
                    filename = filename2, overwrite = overwrite, ...)
    names(out2) <- names(out)
    out <- out2
    if (use_crop) {
      out <- crop(out, y = x, filename = filename, overwrite = overwrite)
    }
    
  }
  
  return(out)
}
