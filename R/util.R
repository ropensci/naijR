# Source file: util.R 
# 
# GPL-3 License
# 
# Copyright (c) 2020-2023 Victor Ordu

## ---- Utility function(s) - for maintenance use only ----
##      This file is not part of the build, but is being
##      recorded by Git. Thus will be useful to developers only

## Inspects the data object of a shapefile.
## This function could be useful, for example, when trying to 
## determine the value for the 'namefield' parameter for
## for the function 'maps::SpatialPolygons2map'
#' @importFrom methods slot
.__getShapefileData <- function(regiontype = c("state", "lga")) {
  regiontype <- match.arg(regiontype)
  
  if (identical(regiontype, "state")) {
    return(shp.state$spatialObject)
  }
  shp.lga$spatialObject
}




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



# checks whether there are name mismatches between the canonical file
# used by the package and a shapefile. This, of course, presupposes
# that the main entries are correct.'
# Returns a character vector of the correct names found to be have
# been misapplied in the shapefile and can be used for corrections.
.__lga_mismatch <- function(state) {
  spdata <- as.data.frame(.__getShapefileData("lga"))
  splga <- spdata[spdata$STATE == state, 'LGA']

  if (anyDuplicated(splga)) {
    cli::cli_abort("Fatal: {state} State has LGA duplication in shapefile")
  }
  pkglga <- as.character(lgas(state))
  lenpkg <- length(pkglga)
  lenshp <- length(splga)
  
  if (lenpkg != lenshp) {
    cli::cli_abort(
      "{state}: Number mismatch: {lenpkg} (main) vs {lenshp} (shapefile)"
    )
  }
  if (setequal(splga, pkglga))
    return(NULL)
  
  mismatched <- setdiff(pkglga, splga)
  attr(mismatched, "incorrect") <- setdiff(splga, pkglga)
  mismatched
}



# Scans for mismatches between LGAs in main data and the shapefile
.__scan_lga_mismatch <- function() {
  # Use `sapply` without simplification so as to return a named list
  sapply(states(), function(x) try(.__lga_mismatch(x)), simplify = FALSE)
}




# Adds a column for the geo-political zones to an
# existing data frame that has a character column
# of the States.
# Arguments:
# - data: The data frame
# - statehdr: The name of the column with the States.
# - zonehdr: The proposed name of the column with the zones
#
# Returns the modified data frame
.__addGPZ <- function(data, statehdr, zonehdr) {
  stopifnot(exprs = {
    is.data.frame(data)
    is.character(statehdr)
    is.character(zonehdr)
  })

  data[[zonehdr]] <- NA_character_
  statelist <- get_all_states()
  
  for (gpz in names(statelist)) {
    rgx <- paste(statelist[[gpz]], collapse = "|")
    index <- grep(rgx, data[[statehdr]])
    data[[zonehdr]][index] <- gpz
  }

  data
}
