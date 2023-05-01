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
  shp <- shpfile_props(region)
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




.__why_no_pipe <- function()
{
  cli::cli_alert_info(
    "A decision was taken not to use pipes in this package, at least not for now.
    This is because of the desire not to take a dependency on magrittr - this
    presented some challenges when debugging earlier versions of the package. We
    have an option of using native R pipes, and these will be introduced a little
    later when their use matures in the ecosystem - this would necessitate 
    depending on R >= 4.1."
  )
}
