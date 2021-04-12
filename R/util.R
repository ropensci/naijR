## ---- Utility function(s) - for maintenance use only ----

## Inspects the data object of a shapefile.
## This function could be useful, for example, when trying to 
## determine the value for the 'namefield' parameter for
## for the function 'maps::SpatialPolygons2map'
##
#' @importFrom utils head
.__inspectShapefileData <- function(region.type)
{
  dt <- .getSpatialPolygonsDataFrame(region.type)
  warning("'namefield' change is made in 'regionSpatialParams()'", 
          call. = FALSE)
  head(dt@data, 3)
}


## Display an array of maps for all the States
.__displayStateMaps <- function()
{
  doOneMap <- function(s) {
    lgs <- lgas_ng(s)
    ret <- map_ng(lgs, title = paste("Map of", s, "State"))
  }
  invisible(lapply(states(), doOneMap))
}
