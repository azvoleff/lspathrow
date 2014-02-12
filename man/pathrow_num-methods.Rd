\docType{methods}
\name{pathrow_num}
\alias{pathrow_num}
\alias{pathrow_num,Raster-method}
\alias{pathrow_num,Spatial-method}
\title{Get WRS-2 path/row numbers for a given spatial object}
\usage{
pathrow_num(x, wrs_type = "2", wrs_mode = "D", as_polys = FALSE)

\S4method{pathrow_num}{Raster}(x, wrs_type, wrs_mode, as_polys)

\S4method{pathrow_num}{Spatial}(x, wrs_type, wrs_mode, as_polys)
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
Get WRS-2 path/row numbers for a given spatial object
}
\examples{
\dontrun{
library(sp)

pathrow_num(test_poly)

x <- pathrow_num(test_poly, as_polys=TRUE)
plot(x)
plot(test_poly, add=TRUE, lty=2, col="#00ff0050")
text(coordinates(x), labels=paste(x$PATH, x$ROW, sep=', '))
}
}

