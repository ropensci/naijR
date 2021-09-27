## ---- Utility function(s) - for maintenance use only ----
##      This file is not part of the build, but is being
##      recorded by Git. Thus will be available to maintainers only
##
## Inspects the data object of a shapefile.
## This function could be useful, for example, when trying to 
## determine the value for the 'namefield' parameter for
## for the function 'maps::SpatialPolygons2map'
#' @importFrom utils head
.__getShapefileData <- function(region)
{
  shp <- ShapefileProps(region)
  shp$sp@data
}


## Display a line-up of LGA-level maps for all the States
.__displayStateMaps <- function(verbose = FALSE)
{
  doOneMap <- function(s) {
    tryCatch({
      message("Drawing map of ", s, " State ... ", appendLF = FALSE)
      ret <- map_ng(lgas(s), title = paste("Map of", s, "State"))
      message("Done")
    }, error = function(err) {
      message("Failed")
    })
  }
  invisible(lapply(states(), doOneMap))
}
