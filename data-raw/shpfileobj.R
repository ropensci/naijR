# Source file: shpfileobj.R 
# 
# GPL-3 License
# 
# Copyright (c) 2020-2023 Victor Ordu


# Preparation of the `ShapefileProps` objects used in the package
#
# The spatial data used in this packages is prepared before hand by 
# defining a special class of object called `ShapefileProps`. This 
# object contains the spatial data, as well as other fields that are
# used in retrieving data requested by functions. These objects are 
# loaded into the package namespace once the package itself is
# loaded. Whenever this script is run, the objects will be recreated.

# Dependencies ----
library(cli)

datafile <- here::here("R/sysdata.rda")

if (file.exists(datafile))
  file.remove(datafile)

devtools::load_all()

# Cleans up extra whitespace in compound LGA names
.strip_ws <- function(obj, rgx, replacement) {
  obj$spatialObject$LGA <- 
    gsub(rgx, replacement, obj$spatialObject$LGA)
  obj
}

# Gets the properties of shape files
# First collect the path to the shapfile project directory
# Then, read the shapefile
# Collect the namefield from the data, based on the region type
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
  shpfile <- list.files(dsn, pattern = rgx)
  layer <- sub(pattern = paste0("(.+)(", rgx, ")"), "\\1", shpfile)
  sp <- rgdal::readOGR(dsn, layer, verbose = FALSE)
  
  if (inherits(regions, "lgas")) {
    sp <- sp |>
      subset(STATE != "Lake") |>
      .__fix_bad_shpfile_state("lga", "Nassarawa", "Nasarawa") |>
      .__fix_bad_shpfile_state("lga", "Abuja", .fct_options("full"))
  }
  nmfld <- .find_namefield(regions, sp)
  new_ShapefileProps(shp, layer, nmfld, sp)
}


## Returns the name currently used by the directory containing the
## shapefile assets. This is found in inst/extdata.
.getShapefileDir <- function(region)
{
  stopifnot(is.object(region))
  
  if (inherits(region, "states"))
    return('ng_admin')
  
  if (inherits(region, "lgas"))
    return('nigeria-lgas')
  
  cli::cli_abort("Wrong input")
}


## Low-level constructor
new_ShapefileProps <- function(dir, layer, namefield, spObj)
{
  structure(list(dir, layer, namefield, spObj),
            names = c("shapefile", "layer", "namefield", "spatialObject"), 
            class = "ShapefileProps")
}


## Retrieves the namefield
.find_namefield <- function(x, obj, class = NA_character_) {
  stopifnot(is.character(x), isS4(obj), is.character(class))
  getfield <- function(index) names(dt)[[index]]
  dt <- methods::slot(obj, "data")
  field <- NA_character_
  
  if (!is.na(class) && !is.object(x))
    class(x) <- class
  
  for (i in seq_len(ncol(dt))) {
    icolumn <- dt[[i]]
    iregions <- x %in% icolumn
    
    if (inherits(x, "states") && all(iregions)) {
      field <- getfield(i)
      break
    }
    
    if (inherits(x, "lgas")) {
      # skip data.frane column with States
      if (all(is_state(unique(icolumn))))
        next
      
      # just any LGAs will do, as some are synonymous with States
      if (any(iregions)) {
        field <- getfield(i)
        break
      }
    }
  }
  
  if (is.null(field) || is.na(field))
    cli::cli_abort("Problem retrieving the namefield")
  
  field
}

# Main code ----
regionlevels <- c("state", "lga")
namefields <- structure(c("admin1Name", "STATE"), names = regionlevels)

for (region in regionlevels) {
  args <- if (identical(region, "state")) {
    list(states, gpz = NULL, all = TRUE, warn = TRUE)
  }
  else if (identical(region, "lga")) {
    list(region = NA_character_, strict = FALSE, warn = TRUE)
  }
  else cli::cli_abort("No argument list")
  
  regions.constructor <- paste0(region, "s")
  allregions <- do.call(regions.constructor, args = args)
  obj <- do.call("ShapefileProps", args = list(region = allregions))
  assign(paste0("shp.", region), obj, envir = globalenv())
}

# Now, check the LGA object for bad entries and try to fix automatically
shp.lga <- .strip_ws(shp.lga, "\\s/", "/")

cli_inform("Attempting automatic fixes for LGAs by State:")
remnant <- NULL
scan.result <- .__scan_lga_mismatch()

for (state in names(scan.result)) {
  newies <- scan.result[[state]]
  oldies <- attr(newies, "incorrect")
  
  if (is.null(newies)) {
    cli::cli_inform("Skipping {state}")
    next
  }
  
  cli::cli_inform("Modifying {state}")
  
  for (new in newies) {
    old <- agrep(new, oldies, value = TRUE)
    
    if (!length(old)) {
      cli::cli_inform("* Could not apply '{new}'")
      remnant <- c(remnant, new)
      currentAttr <- attr(remnant, "State")
      attr(remnant, 'State') <- c(currentAttr, state)
      next
    }
    tryCatch({
      shp.lga <- .__fix_bad_shpfile_lga(shp.lga, state, old, new)
    }, error = function(e) {
      cli::cli_inform("* {conditionMessage(e)}")
    })
  }
}

mat <- cbind(
  state = c(
    "Abia",
    "Benue",
    rep("Katsina", 2),
    "Kebbi",
    "Kogi",
    "Nasarawa",
    "Rivers"
  ),  
  old = c(
    "Oboma Ngwa",
    "Bukuru",
    "Danmusa",
    "Dutsinma",
    "Danko Wasagu",
    "Koton-Karfe",
    "Nassarawa",
    "Emuoha"
  ),
  new = c(
    "Obi Ngwa",
    "Buruku",
    "Dan Musa",
    "Dutsin-Ma",
    "Wasagu/Danko",
    "Kogi",
    "Nasarawa",
    "Emohua"
  )
)

cli_inform("\nApplying additional fixes:")

for (i in seq(nrow(mat))) {
  x <- mat[i, "state"]
  y <- mat[i, "old"]
  z <- mat[i, "new"]
  cli_inform("* {x} State: {y} => {z}")
  shp.lga <- .__fix_bad_shpfile_lga(shp.lga, x, y, z)
}

scan.result2 <- .__scan_lga_mismatch()

if (!all(sapply(scan.result2, is.null)))
  cli_abort("There are still bad LGA entries in the shapefile")

# Save objects ----
# Using RDA format; to be loaded alongside exported objects
cli_inform("Saving fixed objects")
shpobjs <- grep("^shp\\.", ls(), value = TRUE)
save(list = shpobjs, file = datafile)
