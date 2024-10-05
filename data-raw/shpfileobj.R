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
library(cli)
library(here)
library(sf)

local({
  # Local functions ----
  
  ## Checks whether there are name mismatches between the canonical file
  ## used by the package and a shapefile. This, of course, presupposes
  ## that the main entries are correct.'
  ## Returns a character vector of the correct names found to be have
  ## been misapplied in the shapefile and can be used for corrections.
  lga_mismatch <- function(state) {
    stopifnot(naijR::is_state(state))
    spdata <- as.data.frame(get_shapefile_data("lga"))
    splga <- spdata[spdata$STATE == state, 'LGA']
    
    if (anyDuplicated(splga))
      cli_abort("Fatal: {state} State has LGA duplication in shapefile")
    
    pkglga <- as.character(lgas(state))
    lenpkg <- length(pkglga)
    lenshp <- length(splga)
    
    if (lenpkg != lenshp)
      cli_abort(
        "{state}: Number mismatch: {lenpkg} (main) vs {lenshp} (shapefile)"
      )
    
    if (setequal(splga, pkglga))
      return(NULL)
    
    mismatched <- setdiff(pkglga, splga)
    attr(mismatched, "incorrect") <- setdiff(splga, pkglga)
    mismatched
  }
  
  ## Scans for mismatches between LGAs in main data and the shapefile
  scan_lga_mismatch <- function() {
    sapply(states(), \(x) try(lga_mismatch(x)), simplify = FALSE)
  }
  
  ## Cleans up extra whitespace in compound LGA names
  strip_ws <- function(obj, rgx, replacement) {
    stopifnot(exprs = {
      inherits(obj, "ShapefileProps")
      is.character(rgx)
      is.character(replacement)
    })
    
    obj$spatialObject$LGA <- gsub(rgx, replacement, obj$spatialObject$LGA)
    obj
  }
  
  ## Gets the properties of shape files
  ## First collect the path to the shapefile project directory
  ## Then, read the shapefile
  ## Collect the name field from the data, based on the region type
  ShapefileProps <- function(regions)
  {
    stopifnot(is.object(regions))
    shp <- get_shapefile_dir(regions)
    pkgdir <- "data-raw/extdata"
    dsn <- file.path(pkgdir, shp)
    
    if (!dir.exists(dsn))
      stop(sprintf("The map data could not be found in ''", pkgdir))
    
    shpExtRegex <- "\\.shp$"
    shpfile <- list.files(dsn, pattern = shpExtRegex)
    layer <- sub(pattern = paste0("(.+)(", shpExtRegex, ")"), "\\1", shpfile)
    sp <- st_read(dsn, layer)
    
    if (inherits(regions, "lgas")) {
      sp <- sp |>
        subset(STATE != "Lake") |>
        fix_bad_shpfile_state("lga", "Nassarawa", "Nasarawa") |>
        fix_bad_shpfile_state("lga", "Abuja", .fct_options("full"))
    }
    
    nmfld <- find_namefield(regions, sp)
    new_ShapefileProps(shp, layer, nmfld, sp)
  }
  
  ## Returns the name currently used by the directory containing the
  ## shapefile assets. This is found in inst/extdata.
  get_shapefile_dir <- function(region)
  {
    stopifnot(is.object(region))
    
    if (inherits(region, "states"))
      return('ng_admin')
    
    if (inherits(region, "lgas"))
      return('nigeria-lgas')
    
    stop("Objects of class ", class(obj), " are not supported")
  }
  
  ## Inspects the data object of a shapefile.
  ## This function could be useful, for example, when trying to 
  ## determine the value for the 'namefield' parameter
  get_shapefile_data <- function(regiontype = c("state", "lga")) {
    regiontype <- match.arg(regiontype)
    
    if (identical(regiontype, "state")) {
      return(shp.state$spatialObject)
    }
    shp.lga$spatialObject
  }
  
  ## Low-level constructor
  new_ShapefileProps <- function(dir, layer, namefield, spObj)
  {
    structure(
      .Data = list(dir, layer, namefield, spObj),
      names = c("shapefile", "layer", "namefield", "spatialObject"),
      class = "ShapefileProps"
    )
  }
  
  ## Retrieves the namefield
  find_namefield <- function(x, dt, class = NA_character_) {
    stopifnot(exprs = {
      is.character(x)
      inherits(dt, 'sf')
      is.character(class)
    })
    
    getfield <- function(index) names(dt)[[index]]
    
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
        # skip data.frame column with States
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
      stop("Problem retrieving the namefield")
    
    field
  }
  
  ## Fixes malformed state names in a shapefile
  ## Arguments are:
  ## - regiontype: This will determine the object to be modified
  ## - old: The old (i.e. malformed) name
  ## - new: The new (corrected) name
  ## Returns the modified object
  fix_bad_shpfile_state <-
    function(obj, regiontype = c("state", "lga"), old, new) {
      stopifnot(exprs = {
        inherits(obj, "sf")
        is.character(old)
        is.character(new)
      })
      regiontype <- match.arg(regiontype)
      
      hdr <- switch(regiontype,
                    state = "admin1Name",
                    lga = "STATE")
      
      isbad <- obj[[hdr]] %in% old
      obj[[hdr]][isbad] <- new
      obj
    }
  
  ## Fix malformed shapefile LGA entries
  ## Arguments:
  ## - state: The State whose LGAs are being fixed
  ## - oldlga: The existing (i.e. malformed name)
  ## - newlga: The new name used as replacement
  ## Returns nothing. Used for side-effect of modifying
  ## the internally saved spatial data.
  fix_bad_shpfile_lga <-
    function(obj, state, oldlga, newlga, verbose = FALSE) {
      stopifnot(exprs = {
        is.list(obj)
        sum(is_state(state)) == 1L
        is.character(oldlga)
        is_lga(newlga)
        length(oldlga) == 1L && length(newlga) == 1L
        is.logical(verbose)
      })
      
      # Because of LGA synonyms between some States
      # we will index into data frame cells that are
      # specific to a State to make the replacement
      isFocusState <- which(obj$spatialObject[["STATE"]] %in% state)
      isFocusLga <- which(obj$spatialObject[["LGA"]] %in% oldlga)
      rowreplaced <- intersect(isFocusState, isFocusLga)
      numreplacement <- length(rowreplaced)
      
      if (!numreplacement)
        cli_abort("Replacement not found: {newlga} => {oldlga} ({state} State)")
      
      if (numreplacement > 1L) {
        pos <- paste(rowreplaced, collapse = ", ")
        cli_abort("Multiple replacements at positions {pos}")
      }
      
      obj$spatialObject[rowreplaced, "LGA"] <- newlga
      
      if (verbose) {
        pos <- paste(which(isFocusLga), collapse = ", ")
        cli_inform("Rows {pos} matched. Row {rowreplaced} was used")
      }
      
      obj
    }
  
  # Main code ----
  datafile <- here("R/sysdata.rda")
  
  if (file.exists(datafile))
    file.remove(datafile)
  
  devtools::load_all()  # Load only after check for saved data
  
  regionlevels <- c("state", "lga")
  namefields <- structure(c("admin1Name", "STATE"), names = regionlevels)
  
  for (region in regionlevels) {
    args <- if (identical(region, "state")) {
      list(states,
           gpz = NULL,
           all = TRUE,
           warn = TRUE)
    }
    else if (identical(region, "lga")) {
      list(region = NA_character_,
           strict = FALSE,
           warn = TRUE)
    }
    else
      stop("No argument list")
    
    regions.constructor <- paste0(region, "s")
    allregions <- do.call(regions.constructor, args = args)
    obj <- do.call("ShapefileProps", args = list(region = allregions))
    assign(paste0("shp.", region), obj, envir = globalenv())
  }
  
  cli_inform("Attempting automatic fixes for LGAs by State:")
  shp.lga <- strip_ws(shp.lga, "\\s/", "/")
  scan.result <- scan_lga_mismatch()
  remnant <- NULL
  
  for (state in names(scan.result)) {
    newies <- scan.result[[state]]
    oldies <- attr(newies, "incorrect")
    
    if (is.null(newies)) {
      cli_inform("Skipping {state}")
      next
    }
    
    cli_inform("Modifying {state}")
    
    for (new in newies) {
      old <- agrep(new, oldies, value = TRUE)
      
      if (!length(old)) {
        cli_inform("* Could not apply '{new}'")
        remnant <- c(remnant, new)
        currentAttr <- attr(remnant, "State")
        attr(remnant, 'State') <- c(currentAttr, state)
        next
      }
      
      tryCatch({
        shp.lga <- fix_bad_shpfile_lga(shp.lga, state, old, new)
      }, 
      error = function(e) cli_inform("* {conditionMessage(e)}"))
    }
  }
  
  cli_inform("\nApplying additional fixes:")
  oldnames <- "old"
  newnames <- "new"
  
  mat <- rbind(
    c("Oboma Ngwa", "Obi Ngwa"),
    c("Bukuru","Buruku"),
    c("Danmusa", "Dan Musa"),
    c("Dutsinma", "Dutsin-Ma"),
    c("Danko Wasagu", "Wasagu/Danko"),
    c("Koton-Karfe", "Kogi"),
    c("Nassarawa", "Nasarawa"),
    c("Emuoha", "Emohua")
    )
  
  dimnames(mat) <- list(c(
    "Abia",
    "Benue",
    rep("Katsina", 2),
    "Kebbi",
    "Kogi",
    "Nasarawa",
    "Rivers"
  ),
  c(oldnames, newnames))
  
  for (i in seq_len(nrow(mat))) {
    x <- row.names(mat)[i]
    y <- mat[i, oldnames]
    z <- mat[i, newnames]
    
    cli_inform("* {x} State: {y} => {z}")
    shp.lga <- fix_bad_shpfile_lga(shp.lga, x, y, z)
  }
  
  scan.result2 <- scan_lga_mismatch()
  
  if (!all(vapply(scan.result2, is.null, logical(1))))
    cli_abort("There are still bad LGA entries in the shapefile")
  
  # Save objects ----
  # Using RDA format; to be loaded alongside exported objects; note compression
  cli_inform("Saving fixed objects")
  shpobjs <- grep("^shp\\.", ls(envir = globalenv()), value = TRUE)
  save(list = shpobjs, file = datafile, compress = "xz")
})