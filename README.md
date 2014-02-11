# wrspathrow

[![Build Status](https://travis-ci.org/azvoleff/wrspathrow.png)](https://travis-ci.org/azvoleff/wrspathrow)

## Overview

The `wrspathrow` supports working in R with the Worldwide Reference System 
(WRS) 1 and WRS 2 grids used by NASA for cataloging Landsat scenes. The package 
allows fetching the path and row numbers for a given spatial object, or 
conversely, allows fetching a SpatialPolygonsDataFrame of the area covered by a 
given path and row. The 
[`wrspathrowData`](http://github.com/azvoleff/wrspathrowData) package (which 
contains the WRS-1 and WRS-2 grids) is required by the `wrspathrow` package.

## Package Installation

The easiest way to install the development version of the package is to 
download it directly from GitHub (within R) using the 
[`devtools`](http://cran.r-project.org/web/packages/devtools/index.html) package 
by Hadley Wickham. After installing `devtools` from CRAN, type:

```R
install_github('wrspathrow', username='azvoleff')
```

at the R prompt to install `wrspathrow`.

## Author Contact Information

[Alex Zvoleff](mailto:azvoleff@conservation.org)  
Postdoctoral Associate  
Tropical Ecology Assessment and Monitoring (TEAM) Network  
Conservation International  
2011 Crystal Dr. Suite 500  
Arlington, VA 22202  
USA
