#' Spatial gap-filling of SoilGrids NA/masked pixels
#'
#' @description
#' Fills \code{NA} pixels in a SoilGrids \code{terra::SpatRaster} (as returned
#' by \code{\link{soilgridmap_from_vrt}}) using local spatial interpolation.
#'
#' Most \code{NA} pixels in SoilGrids are not missing data in the usual sense:
#' they come from an explicit land mask that excludes water bodies, urban
#' areas, bare rock and permanent ice/glaciers (see ISRIC documentation and
#' \url{https://groups.google.com/g/global-soil-information/c/DzVNWK9ZHeQ}).
#' Filling them is a deliberate approximation ("assume the nearest soil-like
#' value would apply here"), so by default this function refuses to fill
#' large contiguous \code{NA} patches (e.g. ice caps, big lakes) and only
#' closes small/isolated gaps such as narrow rocky ridges or thin cloud/void
#' artifacts near mountain peaks. Increase \code{max_gap_cells} (or set it to
#' \code{Inf}) if you really want everything filled.
#'
#' @param x a \code{terra::SpatRaster} (single or multi-layer), typically the
#'   output of \code{\link{soilgridmap_from_vrt}}.
#' @param method character, one of \code{"focal"} (default, iterative
#'   moving-window mean/median of valid neighbours) or \code{"idw"}
#'   (inverse-distance weighting using \pkg{gstat} on the valid cells within
#'   each patch's neighbourhood). \code{"focal"} is fast and dependency-free;
#'   \code{"idw"} is slower but usually smoother for larger gaps.
#' @param fun summary function used by the \code{"focal"} method at each
#'   iteration: \code{"mean"} (default) or \code{"median"}.
#' @param window odd integer >= 3, the moving-window side length (in cells)
#'   used by the \code{"focal"} method. Larger windows fill faster but blur
#'   more.
#' @param max_iter maximum number of focal passes. Each pass only grows the
#'   filled area by (window - 1)/2 cells inward from the gap edge, so this
#'   effectively caps how far a fill can propagate from valid data; increase
#'   it for very large permitted gaps.
#' @param max_gap_cells maximum size (in number of connected \code{NA}
#'   cells, 4- or 8-connectivity per \code{gap_directions}) of a gap that is
#'   allowed to be filled. Connected \code{NA} patches larger than this are
#'   left as \code{NA} (typical for glaciers, large lakes, sea areas).
#'   Default \code{200} cells (~12.5 km^2 at 250 m resolution) is a
#'   conservative starting point for closing peaks/ridges without
#'   hallucinating soil over ice caps; set to \code{Inf} to disable the
#'   size check and fill everything.
#' @param gap_directions connectivity used to delineate connected \code{NA}
#'   patches for the \code{max_gap_cells} check: \code{4} (rook) or \code{8}
#'   (queen, default).
#' @param mask optional \code{terra::SpatRaster} or \code{SpatVector}; cells
#'   (or areas) outside \code{mask} are never filled even if they pass the
#'   \code{max_gap_cells} check. Use this to hard-exclude e.g. a glacier
#'   inventory or a water-body layer independently of patch size.
#' @param verbose logical, print progress per iteration/per layer.
#' @param filename,... further arguments, see \code{\link[terra]{writeRaster}}.
#' 
#' @return a \code{terra::SpatRaster} with the same extent, resolution and
#'   number of layers as \code{x}, with eligible \code{NA} cells filled and
#'   large/masked gaps left as \code{NA}.
#'
#' @details
#' The \code{"focal"} method works iteratively: at each pass, every \code{NA}
#' cell that has at least one valid neighbour within \code{window} is
#' replaced by \code{fun} of its valid neighbours; the raster is updated and
#' the process repeats (so the fill "grows" inward from the edges of a gap)
#' until either no \code{NA} cells change, or \code{max_iter} passes have run.
#' The \code{max_gap_cells}/\code{mask} checks are applied *before* filling,
#' by building an eligibility mask so that oversized or excluded patches are
#' restored to \code{NA} at the end regardless of what the focal pass wrote
#' into them.
#'
#' @examples
#' \dontrun{
#' library(terra)
#' f <- rast(system.file('ext_data/vinschgau_elevation.tif',package="soilgridr"))
#'   r <- rast(f)
#' r <- soilgridmap_from_vrt(vinschgau,voi="sand",align=TRUE,use_crop=TRUE)
#' r_filled <- soilgridmap_fill_na(r, max_gap_cells = 200)
#' }
#'
#' @importFrom terra patches freq focal ifel mask compareGeom nlyr rast
#' @export
soilgridmap_fill_na <- function(x,
                                 method = c("focal", "idw"),
                                 fun = c("mean", "median"),
                                 window = 3,
                                 max_iter = 50,
                                 max_gap_cells = 200,
                                 gap_directions = 8,
                                 mask = NULL,
                                 verbose = FALSE,filename="",...) {

  method <- match.arg(method)
  fun <- match.arg(fun)

  if (!inherits(x, "SpatRaster")) {
    stop("'x' must be a terra::SpatRaster (e.g. the output of soilgridmap_from_vrt).")
  }
  if (window < 3 || window %% 2 == 0) {
    stop("'window' must be an odd integer >= 3.")
  }

  ## rasterize/align 'mask' once (independent of layer), if supplied
  mask_r <- NULL
  if (!is.null(mask)) {
    mask_r <- if (inherits(mask, "SpatVector")) {
      terra::rasterize(mask, x[[1]], field = 1, background = NA)
    } else {
      mask
    }
    if (!isTRUE(terra::compareGeom(mask_r, x[[1]], stopOnError = FALSE))) {
      mask_r <- terra::resample(mask_r, x[[1]], method = "near")
    }
  }

  out <- x
  for (i in seq_len(terra::nlyr(x))) {

    lyr <- x[[i]]

    ## eligibility mask for THIS layer: TRUE where a NA cell may be filled
    ## (part of a connected NA patch no larger than max_gap_cells, and
    ## inside 'mask' if supplied). Computed per layer since NA patterns
    ## can differ layer to layer (e.g. different depths/quantiles).
    lyr_eligible <- .soilgridmap_gap_eligibility(lyr,
                                                  max_gap_cells = max_gap_cells,
                                                  gap_directions = gap_directions,
                                                  mask_r = mask_r,
                                                  verbose = verbose)

    filled <- switch(method,
      focal = .soilgridmap_fill_focal(lyr, fun = fun, window = window,
                                       max_iter = max_iter, verbose = verbose),
      idw   = .soilgridmap_fill_idw(lyr, verbose = verbose)
    )

    ## only accept the fill where eligible; elsewhere keep original (NA stays NA)
    out[[i]] <- terra::ifel(lyr_eligible, filled, lyr)

    if (verbose) {
      message(sprintf("soilgridmap_fill_na: layer %d/%d (%s) done.",
                       i, terra::nlyr(x), names(x)[i]))
    }
  }

  names(out) <- names(x)
  #####
  if (is.null(filename)) filename <- NA
  if (is.na(filename)) filename=""
  
  if (filename!="") {
   
    out <- terra::writeRaster(out,filename=filename,...)
    
    
  }
  
  return(out)
}

## -- internal helpers --------------------------------------------------

## Build a logical SpatRaster (single layer): TRUE = NA cell eligible to
## be filled (i.e. part of a connected NA patch no larger than
## max_gap_cells, and inside mask_r if supplied). 'lyr' must be a
## single-layer SpatRaster.
.soilgridmap_gap_eligibility <- function(lyr, max_gap_cells, gap_directions, mask_r, verbose) {

  na_mask <- terra::ifel(is.na(lyr), 1, NA)

  if (is.finite(max_gap_cells)) {
    patch_ids <- terra::patches(na_mask, directions = gap_directions, zeroAsNA = TRUE)
    tab <- terra::freq(patch_ids)
    big_ids <- tab$value[tab$count > max_gap_cells]
    patch_ids_small <- if (length(big_ids) > 0) {
      terra::subst(patch_ids, from = big_ids, to = NA)
    } else {
      patch_ids
    }
    eligible <- !is.na(patch_ids_small)
  } else {
    eligible <- !is.na(na_mask)
  }
  eligible <- eligible & is.na(lyr)

  if (!is.null(mask_r)) {
    eligible <- eligible & !is.na(mask_r)
  }

  if (verbose) {
    n_elig <- as.numeric(terra::global(eligible * 1, "sum", na.rm = TRUE)[1, 1])
    message(sprintf("  eligibility: %d NA cell(s) eligible to fill.", n_elig))
  }

  eligible
}

## Iterative focal fill: replaces NA with fun() of valid neighbours,
## repeated until stable or max_iter reached. na.policy = "only" leaves
## already-valid cells untouched, so no extra cover() step is needed.
.soilgridmap_fill_focal <- function(lyr, fun, window, max_iter, verbose) {

  w <- matrix(1, window, window)
  current <- lyr

  for (it in seq_len(max_iter)) {
    n_na_before <- as.numeric(terra::global(is.na(current) * 1, "sum", na.rm = TRUE)[1, 1])
    if (n_na_before == 0) break

    current <- terra::focal(current, w = w, fun = fun, na.rm = TRUE,
                             na.policy = "only")

    n_na_after <- as.numeric(terra::global(is.na(current) * 1, "sum", na.rm = TRUE)[1, 1])
    if (verbose) {
      message(sprintf("  focal iter %d: NA cells %d -> %d", it, n_na_before, n_na_after))
    }
    if (n_na_after == n_na_before) break  # no more progress possible with this window
  }

  current
}

## IDW fill using gstat. NOTE: this predicts over the full extent/valid
## cells of 'lyr', which can be slow on a full-resolution SoilGrids tile.
## For large rasters, crop 'lyr' to a buffer around the gaps of interest
## before calling soilgridmap_fill_na(method = "idw"), or prefer the
## default "focal" method.
.soilgridmap_fill_idw <- function(lyr, verbose) {

  if (!requireNamespace("gstat", quietly = TRUE)) {
    stop("method = 'idw' requires the 'gstat' package: install.packages(\"gstat\")")
  }

  pts <- terra::as.points(lyr, na.rm = TRUE)
  if (length(pts) == 0) return(lyr)

  df <- data.frame(terra::geom(pts)[, c("x", "y")], z = terra::values(pts)[, 1])

  gs <- gstat::gstat(formula = z ~ 1, locations = ~x + y, data = df, nmax = 12)
  pred <- terra::interpolate(lyr, gs, debug.level = 0)[[1]]

  terra::cover(lyr, pred)
}
