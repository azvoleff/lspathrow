#' wrspathrow
#'
#' @name wrspathrow
#' @docType package
NULL
.onLoad <- function(libname, pkgname) {
    load(system.file("data", "wrs1_asc_desc.RData", package="wrspathrowData"), 
         envir=parent.env(environment()))
    load(system.file("data", "wrs2_asc_desc.RData", package="wrspathrowData"),
         envir=parent.env(environment()))
}
