# Source file: util.R 
# 
# GPL-3 License
# 
# Copyright (c) 2020-2023 Victor Ordu

## ---- Utility function(s) - for maintenance use only ----
##      This file is not part of the build, but is being
##      recorded by Git. Thus will be useful to developers only

# Fixes malformed state names in a shapefile
# Arguments are:
# - regiontype: This will determine the object to be modified
# - old: The old (i.e. malformed) name
# - new: The new (corrected) name
#
# Returns the modified object
.__fix_bad_shpfile_state <- 
  function(obj, regiontype = c("state", "lga"), old, new) {
  stopifnot({
    isS4(obj)
    is.character(old)
    is.character(new)
  })
  regiontype <- match.arg(regiontype)
  
  hdr <- switch(regiontype,
                state = "admin1Name",
                lga = "STATE")
  
  isbad <- obj@data[[hdr]] %in% old
  obj@data[[hdr]][isbad] <- new
  obj
}



# Fixed malformed shapefile LGA entries
#
# Arguments:
# - state: The State whose LGAs are bre being fixed
# - oldlga: The existing (i.e. malformed name)
# - newlga: The new name used as replacement
#
# Returns nothing. Used for side-effect of modifying
# the internally saved spatial data.
.__fix_bad_shpfile_lga <- 
  function(obj, state, oldlga, newlga, verbose = FALSE) {
  stopifnot({
    is.list(obj)
    is_state(state)
    is.character(oldlga)
    is_lga(newlga)
    length(oldlga) == 1L && length(newlga) == 1L
    is.logical(verbose)
  })

  # Because of LGA synonyms between some States
  # we will index into data frame cells that are
  # specific to a State to make the replacement
  isFocusState <- which(obj$spatialObject@data[["STATE"]] %in% state)
  isFocusLga <- which(obj$spatialObject@data[["LGA"]] %in% oldlga)
  rowreplaced <- intersect(isFocusState, isFocusLga)
  numreplacement <- length(rowreplaced)
  
  if (!numreplacement) {
    cli::cli_abort(
      "Replacement not found: {newlga} => {oldlga} ({state} State)"
    )
  }
  if (numreplacement > 1L) {
    pos <- paste(rowreplaced, collapse = ", ")
    cli::cli_abort("Multiple replacements at positions {pos}")
  }
  obj$spatialObject@data[rowreplaced, "LGA"] <- newlga
  
  if (verbose) {
    pos <- paste(which(isFocusLga), collapse = ", ")
    cli::cli_inform("Rows {pos} matched. Row {rowreplaced} was used")
  }
  obj
}


## Inspects the data object of a shapefile.
## This function could be useful, for example, when trying to 
## determine the value for the 'namefield' parameter for
## for the function 'maps::SpatialPolygons2map'
#' @importFrom methods slot
.__getShapefileData <- function(regiontype = c("state", "lga")) {
  regiontype <- match.arg(regiontype)
  dt <- "data"
  if (identical(regiontype, "state")) {
    return(slot(shp.state$spatialObject, dt))
  }
  slot(shp.lga$spatialObject, dt)
}




# Prints out the LGAs for a given State in the shapefile
.__show_shapefile_state_lgas <- function(state) {
  stopifnot(is_state(state))
  stateindex <- which(shp.lga$spatialObject$STATE == state)
  lganames <- shp.lga$spatialObject$LGA[stateindex]
  attr(lganames, "State") <- state
  sort(lganames)
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
      ret <- NULL
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
  spdata <- .__getShapefileData("lga")
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
  sapply(states(), \(x) try(.__lga_mismatch(x)), USE.NAMES = TRUE)
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
  stopifnot({
    is.data.frame(data)
    is.character(statehdr)
    is.character(zonehdr)
  })

  data[[zonehdr]] <- NA_character_
  statelist <- stateList()
  
  for (gpz in names(statelist)) {
    rgx <- paste(statelist[[gpz]], collapse = "|")
    index <- grep(rgx, data[[statehdr]])
    data[[zonehdr]][index] <- gpz
  }

  data
}
