#' Get WRS-2 path/row for a given spatial object
#'
#' @export
#' @docType methods
#' @import methods
#' @rdname get_pathrow-methods
#' @param x a spatial object
#' @return list with path and row as integers
#' @examples
#' #TODO: add examples
setGeneric("get_pathrow", function(x, wrs_type='2', wrs_mode='D', as_polys=FALSE) {
    standardGeneric("get_pathrow")
})

get_wrs_polys <- function(wrs_type, wrs_mode) {
    if (wrs_type == 2) {
        wrs_polys <- wrs2_asc_desc
    } else if (wrs_type == 1) {
        wrs_polys <- wrs1_asc_desc
    } else {
        stop('wrs_type must be 1 or 2')
    }
    if (!(wrs_mode %in% c('D', 'A'))) {
        stop('wrs_type must be "D" or "A" or 2')
    }
    return(wrs_polys[wrs_polys@data$MODE == wrs_mode, ])
}

intersect_wrs_polys <- function(wrs_polys, x, as_polys) {
    intersecting <- as.logical(gIntersects(wrs_polys, x, byid=TRUE))
    if (sum(intersecting) == 0) {
        stop('no intersecting pathrows found')
    } else {
        wrs_polys <- wrs_polys[intersecting, ]
        wrs_polys <- wrs_polys[order(wrs_polys$PATH, wrs_polys$ROW), ]
        if (!as_polys) {
            wrs_polys <- data.frame(PATH=wrs_polys@data$PATH, ROW=wrs_polys@data$ROW)
        }
        return(wrs_polys)
    }
}

#' @rdname get_pathrow-methods
#' @import raster
#' @importFrom rgeos gIntersects
#' @aliases get_pathrow,Raster-method
setMethod("get_pathrow", signature(x="Raster"),
    function(x, wrs_type, wrs_mode, as_polys) {
        wrs_polys <- get_wrs_polys(wrs_type, wrs_mode)

        x_wgs84 <- projectExtent(x, crs=crs(wrs_polys))
        x_wgs84_sp <- as(extent(x_wgs84), 'SpatialPolygons')
        crs(x_wgs84_sp) <- crs(wrs_polys)

        return(intersect_wrs_polys(wrs_polys, x_wgs84_sp, as_polys))
    }
)

#' @rdname get_pathrow-methods
#' @importFrom sp Spatial spTransform proj4string CRS
#' @importFrom rgeos gIntersects
#' @aliases get_pathrow,Spatial-method
setMethod("get_pathrow", signature(x="Spatial"),
    function(x, wrs_type, wrs_mode, as_polys) {
        wrs_polys <- get_wrs_polys(wrs_type, wrs_mode)

        x_wgs84 <- spTransform(x, CRS(proj4string(wrs_polys)))

        return(intersect_wrs_polys(wrs_polys, x_wgs84, as_polys))
    }
)
