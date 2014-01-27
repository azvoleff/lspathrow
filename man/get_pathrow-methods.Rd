\docType{methods}
\name{get_pathrow}
\alias{get_pathrow}
\alias{get_pathrow,Raster-method}
\alias{get_pathrow,Spatial-method}
\title{Get WRS-2 path/row for a given spatial object}
\usage{
get_pathrow(x, wrs_type = "2", wrs_mode = "D", as_polys = FALSE)

\S4method{get_pathrow}{Raster}(x, wrs_type, wrs_mode, as_polys)

\S4method{get_pathrow}{Spatial}(x, wrs_type, wrs_mode, as_polys)
}
\arguments{
  \item{x}{a spatial object}

  \item{wrs_type}{1 (for WRS-1) or 2 (for WRS-2)}

  \item{wrs_mode}{either 'D' for descending (daytime) or
  'A' for ascending}

  \item{as_polys}{if FALSE (default) return a data.frame.
  If TRUE, return a \code{SpatialPolygonsDataFrame}.}
}
\value{
data.frame with path and row as integers, or, if
as_polys=TRUE, a \code{SpatialPolygonsDataFrame}
}
\description{
Get WRS-2 path/row for a given spatial object
}
\examples{
#TODO: add examples
}

