## ---- Utility function(s) - for maintenance use only ----

## Inspects the data object of a shapefile.
## This function could be useful, for example, when trying to 
## determine the value for the 'namefield' parameter for
## for the function 'maps::SpatialPolygons2map'
##
#' @importFrom utils head
.__getShapefileData <- function(region)
{
  shp <- ShapefileProps(region)
  shp$sp@data
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
