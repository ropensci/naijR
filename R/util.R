# Source file: util.R 
# 
# GPL-3 License
# 
# Copyright (c) 2020-2023 Victor Ordu

## ---- Utility function(s) - for maintenance use only ----
##      This file is not part of the build, but is being
##      recorded by Git. Thus will be useful to developers only

# Prints out the LGAs for a given State in the shapefile
.__show_shapefile_state_lgas <- function(state) {
  stopifnot(sum(is_state(state)) == 1L)
  stateindex <- which(shp.lga$spatialObject$STATE == state)
  lganames <- shp.lga$spatialObject$LGA[stateindex]
  attr(lganames, "State") <- state
  sort(lganames)
}




## Display a line-up of LGA-level maps for all the States
.__displayStateMaps <- function(verbose = FALSE)
{
  doOneMap <- function(s) {
    ret <- NULL
    
    tryCatch({
      message("Drawing map of ", s, " State ... ", appendLF = FALSE)
      ret <- map_ng(lgas(s), title = paste("Map of", s, "State"))
      message("Done")
    }, error = function(err) {
      message("Failed")
    })
    
    ret
  }
  invisible(lapply(states(), doOneMap))
}
