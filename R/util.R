# Source file: util.R 
# 
# GPL-3 License
# 
# Copyright (c) 2020-2023 Victor Ordu


## ---- Utility function(s) - for maintenance use only ----
##      This file is not part of the build, but is being
##      recorded by Git. Thus will be useful to developers only


## Get the properties of shape files
## First collect the path to the shapfile project directory
## Then, read the shapefile
## Collect the namefield from the data, based on the region
ShapefileProps <- function(regions)
{
  shp <- .getShapefileDir(regions)
  pkgdir <- "extdata"
  
  dsn <- system.file(file.path(pkgdir, shp),
                     package = 'naijR',
                     mustWork = TRUE)
  
  if (identical(dsn, character(1)))
    cli::cli_abort("The map data could not be found in '{pkgdir}'")
  
  rgx <- "\\.shp$"
  shpfl <- list.files(dsn, rgx)
  lyr <- sub(pattern = paste0("(.+)(", rgx, ")"), "\\1", shpfl)
  sp <- rgdal::readOGR(dsn, lyr, verbose = FALSE)
  
  ## Deal with bad data entries in the shapefiles
  if (shp == "nigeria-lgas") 
    sp <- subset(sp, STATE != "Lake")
  
  sp <- fix_nasarawa(sp, regions)
  
  
  dt <- methods::slot(sp, "data")
  
  if (shp == "nigeria-lgas")
    dt$STATE[dt$STATE == "Abuja"] <- "Federal Capital Territory"
  
  nmfld <- .find_namefield(regions, dt)
  new_ShapefileProps(shp, lyr, nmfld, sp)
}




## Low-level constructor
new_ShapefileProps <- function(dir, layer, namefield, spObj)
{
  structure(list(dir, layer, namefield, spObj),
            names = c("shapefile", "layer", "namefield", "spatialObject"), 
            class = "ShapefileProps")
}





## Retrieves the namefield
.find_namefield <- function(x, dt, class = NA_character_) {
  getfield <- function(index) names(dt)[[index]]
  stopifnot(is.character(x), is.data.frame(dt))
  nmfield <- NA_character_
  
  if (!is.na(class) && !is.object(x))
    class(x) <- class
  
  for (i in seq_len(ncol(dt))) {
    icolumn <- dt[[i]]
    iregions <- x %in% icolumn
    
    if (inherits(x, "states") && all(iregions)) {
      nmfield <- getfield(i)
      break
    }
    
    if (inherits(x, "lgas")) {
      # skip datafrane column with States
      if (all(is_state(unique(icolumn))))
        next
      
      # just any LGAs will do, as some are synonymous with States
      if (any(iregions)) {
        nmfield <- getfield(i)
        break
      }
    }
  }
  
  if (is.null(nmfield) || is.na(nmfield))
    cli::cli_abort("Problem retrieving the namefield")
  
  nmfield
}




fix_nasarawa <- function(obj, regions)
{
  statecol <-
    if (inherits(regions, "states"))
      "admin1Name"
  else if (inherits(regions, "lgas"))
    "STATE"
  
  .fix_bad_shpfile_region(obj, statecol, "Nassarawa", "Nasarawa")
}




.fix_bad_shpfile_region <- function(obj, hdr, old, new) 
{
  stopifnot({
    isS4(obj)
    is.character(hdr)
    is.character(old)
    is.character(new)
  })
  
  obj@data[[hdr]] <- sub(old, new, obj@data[[hdr]])
  obj
}



## Inspects the data object of a shapefile.
## This function could be useful, for example, when trying to 
## determine the value for the 'namefield' parameter for
## for the function 'maps::SpatialPolygons2map'
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
  spdata <- slot(shp.lga$spatialObject, "data")
  splga <- spdata[spdata$STATE == state, 'LGA']
  pklga <- as.character(lgas(state))
  lenpak <- length(pklga)
  lenshp <- length(splga)
  
  if (lenpak != lenshp) {
    return(
      sprintf("Number mismatch: %i (main) vs %i (shapefile)", 
              lenpak, lenshp)
    )
  }
  
  if (setequal(splga, pklga))
    return(NULL)
  
  correct <- paste(intersect(pklga, splga), collapse = ", ")
  mismatched <- paste(setdiff(pklga, splga), collapse = ", ")
  attr(mismatched, "correct") <- correct
  mismatched
}



# Scans for mismatches between LGAs in main data and the shapefile
.__scan_lga_mismatch <- function() {
    sapply(states(), \(x) try(.__lga_mismatch(x)), USE.NAMES = TRUE)
}




# Creates a list whose elements are the States
# by their respective geo-political zones. 
# The name of each elements is an abbreviated
# form of the name of its zone - North-Central,
# North-East, North-West, South-East, South-South
# and South-West. The Federal Capital Territory, 
# which doesn't belong to any zone is denoted
# by its own abbreviation and its element is of
# length 1L.
.__stateList <- function()
{
  list(
    nc = c("Benue", "Kogi", "Kwara", "Nasarawa", "Niger", "Plateau"),
    ne = c("Adamawa", "Bauchi", "Borno", "Gombe", "Taraba", "Yobe"),
    nw = c("Jigawa", "Kaduna", "Kano", "Katsina", "Kebbi", "Sokoto", "Zamfara"),
    se = c("Abia", "Anambra", "Ebonyi", "Enugu", "Imo"),
    ss = c("Akwa Ibom", "Bayelsa", "Cross River", "Delta", "Edo", "Rivers"),
    sw = c("Ekiti", "Lagos", "Ogun", "Ondo", "Osun", "Oyo"),
    fct = "Federal Capital Territory"
  )
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
  statelist <- .__stateList()
  
  for (gpz in names(statelist)) {
    rgx <- paste(statelist[[gpz]], collapse = "|")
    index <- grep(rgx, data[[statehdr]])
    data[[zonehdr]][index] <- gpz
  }

  data
}
