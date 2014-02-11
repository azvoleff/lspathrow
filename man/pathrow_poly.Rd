\name{pathrow_poly}
\alias{pathrow_poly}
\title{Get a polgon giving area of coverage of a given WRS-1 or WRS-2 path and row}
\usage{
pathrow_poly(wrs_path, wrs_row, wrs_type = "2", wrs_mode = "D")
}
\arguments{
  \item{wrs_path}{WRS-1 or WRS-2 path as an integer}

  \item{wrs_row}{WRS-1 or WRS-2 row as an integer}

  \item{wrs_type}{1 (for WRS-1) or 2 (for WRS-2)}

  \item{wrs_mode}{either 'D' for descending (daytime) or
  'A' for ascending (nighttime)}
}
\value{
list with path and row as integers
}
\description{
Get a polgon giving area of coverage of a given WRS-1 or
WRS-2 path and row
}
\examples{
library(sp)

x <- pathrow_poly(225, 61)
plot(x)
}

