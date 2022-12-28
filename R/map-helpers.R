# Source file: map-helpers.R
#
# GPL-3 License
#
# Copyright (C) 2019-2022 Victor Ordu.
#
# Internal helper function(s) ---------------------------------------------------

## Processes character input, presumably States, and when empty
## character vector, provide all the States as a default value.
.processRegionParam <- function(s)
{
  stopifnot(is.character(s))
  
  if (length(s) == 0L)
    return(states(all = TRUE))
  
  isregion <- all(is_state(s)) || all(is_lga(s))
  
  if (!isregion) {
    
    if (length(s) > 1L) {
      stop(sprintf(
        "One or more elements of '%s' is not a Nigerian region",
        deparse(substitute(s))
      ),
      call. = FALSE)
    }
    else if (s != .ngName()) {
      stop(
        sprintf(
          "Single inputs for '%s' only support the value '%s'",
          deparse(substitute(s)),
          .ngName()
        ),
        call. = FALSE
      )
    }
  }
  
  s
}




# Makes sure that all the elements required for making
# a choropleth map are available. These are:
# - A data frame with a value and region column identified
# - A 2-column data frame with one column of regions
# - A region and value as separate vectors
# 

# - 

#' @importFrom rlang as_name
#' @importFrom rlang enexpr
#' @importFrom rlang is_null
#' @importFrom rlang is_symbol
.validateChoroplethParams <- function(val = NULL, region = NULL, data = NULL)
{   # TODO: Add some verbosity.
  val <- enexpr(val)
  
  ## If 'data' is NULL, then both 'val' and 'region' must be present
  ## and 'region' must have valid States or LGAs
  if (is.null(data)) {
    
    if (is.null(val) || is.null(region))
      return(FALSE)
    
    if (!.allAreRegions(region) && !is.null(val))
      return(FALSE)
    # At this point, we have two valid vectors only
  }
  
  data.has.regions <- FALSE
  
  if (is.data.frame(data)) {
    index <- .regionColumnIndex(data)
    data.has.regions <- as.logical(index)
    
    # Once identified, the regions in the data frame are
    # to replace those in the original variable. Since this
    # function is designed to return a boolean value, a 
    # super-assignment is used to effect the change.
    if (data.has.regions) {
      r <- data[[index]]
      assign(deparse(substitute(region)), r, envir = parent.frame())
    }
  }
  else if (!is.null(data)) {
    warning(sprintf("'%s' is invalid for choropleths but was ignored",
                    .arg_str(data)))
  }
  
  ## If 'region' is NULL, it must be found automatically in 'data'
  if (is.null(region)) {
    
    if (isFALSE(data.has.regions))
      return(FALSE)
    
    region <- character()
  }
  
  if (!.allAreRegions(region))
    return(FALSE)
  
  ## If 'val' is null, it must exist in 'data', but can only be
  ## deduced if 'data' has only 2 columns and the other column is 
  ## confirmed to contain strings representing regions (i.e. States
  ## or LGAs).
  if (is.null(val)) {
    if (is.null(data))
      return(FALSE)
    
    if (ncol(data) > 2L)
      return(FALSE)
    
    if (isFALSE(.allAreRegions(region)) && isFALSE(data.has.regions))
      return(FALSE)
  }
  
  if (!is.null(val)) {
    
    if (is.data.frame(data)) {
      
      if (is_symbol(val) &&
          isFALSE(as_name(val) %in% names(data))) {
        stop(sprintf("The column '%s' does not exist in '%s'",
                     .arg_str(val), .arg_str(data)),
             call. = FALSE)
      }
    }
  }
  
  TRUE
}





## S3 Class and methods for internal use:
.getMapData <- function(x)
  UseMethod(".getMapData")


#' @import mapdata
.getMapData.default <- function(x) 
{
  if (is.factor(x))
    x <- as.character(x)
  
  stopifnot(is.character(x))
  
  if (identical(x, 'Nigeria'))
    return("mapdata::worldHires")
  
  if ((length(x) == 1L && (x %in% .synonymRegions()))
      || all(is_state(x)))
    return(.getMapData(states(x)))
  
  .getMapData(lgas(x))
}


.getMapData.lgas <- function(x)
{
  spo <- .fixNasState(shp.lga[['spatialObject']], "STATE")
  st.nm <- attr(x, 'State')
  
  if (length(st.nm) > 1L)
    stop("LGA-level maps for adjoining States are not yet supported")
  
  lgaObj <- if (!is.null(st.nm)) {
    
    if (st.nm %in% .fctOptions())  # peculiar to this scope
      st.nm <- "Abuja"
    spo[grep(.regexSubsetRegions(st.nm), spo@data$STATE), ]
  }
  else
    spo[grep(.regexSubsetRegions(x), spo@data$LGA), ]
  
  .createBaseMapsObject(lgaObj, shp.lga)
}





.getMapData.states <- function(x)
{
  spo <- .fixNasState(shp.state[['spatialObject']], "admin1Name")
  
  stateObj <- 
    spo[grep(.regexSubsetRegions(x), spo@data$admin1Name), ]
  
  .createBaseMapsObject(stateObj, shp.state)
}





#' @importFrom maps SpatialPolygons2map
.createBaseMapsObject <- function(obj, shapefileObj) {
  SpatialPolygons2map(obj, namefield = shapefileObj[['namefield']])
}


.fixNasState <- function(obj, namefield)
{
  .fixBadShpfileRegion(obj, namefield, "Nassarawa", "Nasarawa")
}


.fixBadShpfileRegion <- function(obj, namefield, old, new) 
{
  # TODO: Consider using reference semantics for 'obj'
  stopifnot({
    isS4(obj)
    is.character(namefield)
    is.character(old)
    is.character(new)
  })
  obj@data[[namefield]] <- sub(old, new, obj@data[[namefield]])
  obj
}


# .fixBadNames <- function(map)
# {
#   map$names[map$names == "Nasarawa"] <- "Nassarawa"
#   map
# }




# States that are also the names of LGAs
.LgaLikeStates <-
  function()
    c("Bauchi",
      "Ebonyi",
      "Ekiti",
      "Gombe",
      "Katsina",
      "Kogi",
      "Nasarawa",
      "Oyo")



## Returns the name currently used by the directory containing the
## shapefile assets. This is found in inst/extdata.
.getShapefileDir <- function(region)
  UseMethod(".getShapefileDir")

.getShapefileDir.states <- function(region)
{
  'ng_admin'
}

.getShapefileDir.lgas <- function(region)
{
  'nigeria-lgas'
}









# For possible export later
.shpLayer <- function(level) {
  
  if (level == 'state')
    "nga_admbnda_adm1_osgof_20161215"
  else if (level == 'lga')
    "new_lga_nigeria_2003"
  # "NIGERIA_LGA"
  # "Nigeria_census_2006_WGS84"
  else
    stop("An appropriate layer is not avaiable")
}






## Find the index number for the column housing the region names
## used for drawing a choropleth map
#' @importFrom rlang abort
#' @importFrom rlang warn
.regionColumnIndex <- function(dt, s = NULL)
{
  stopifnot(is.data.frame(dt))
  
  ## Checks if a column has the names of States, returning TRUE if so.
  .fx <- function(x) {
    
    if (is.factor(x))    # TODO: Earmark for removal
      x <- as.character(x)
    
    ret <- FALSE
    
    if (is.character(x)) {
      ret <- .allAreRegions(x)
      
      # TODO: apply a ?restart here when there are misspelt States
      # and try to fix them automatically and then apply the function
      # one more time. Do so verbosely.
      if (!ret && .someAreRegions(x))
        warning("Misspelt region(s) in the dataset", call. = FALSE)
    }
    ret
  }
  
  n <- vapply(dt, .fx, logical(1))
  
  if (is.null(s))
    s <- states()
  
  if (!sum(n))
    stop(sprintf("No column with elements in '%s'.", 
                 deparse(substitute(dt))),
         call. = FALSE)
  
  if (sum(n) > 1)
    warning("Multiple columns have regions, so the first was used",
            call. = FALSE)
  
  which(n)[1]
}








#' @importFrom rlang abort
.prepChoroplethOpts <-
  function(map, opts, col = NULL, ...)
  {
    # TODO: Set limits for variables and brk
    # TODO: Accept numeric input for col
    stopifnot(inherits(map, 'map'))
    
    if(!.assertListElements(opts))
      abort("One or more inputs for generating choropleth options are invalid")
    
    brks <- opts$breaks
    
    df <-
      data.frame(
        region = opts$region,
        value = opts$value,
        stringsAsFactors = FALSE
      )
    
    df$cat <- .createCategorized(df$value, brks)
    cats <- levels(df$cat)
    colrange <- .processColouring(col, length(cats))
    
    # At this point, our value of 
    # interest is definitely a factor
    df$ind <- as.integer(df$cat)
    df$color <- colrange[df$ind]
    rgx <- "(^.+)(:.+$)"
    indexMultiPolygons <- grep(rgx, map$names)
    mapregions <- sub(rgx, "\\1", map$names)
    m <- mapregions[indexMultiPolygons]
    m <-  m[!duplicated(m)]
    mapregions <- mapregions[-indexMultiPolygons]
    mapregions <- c(mapregions, m)
    
    if (nrow(df) < length(mapregions))
      mapregions <- mapregions[mapregions %in% df$region]
    
    new.ind <- order(as.character(df$region), mapregions)
    ord.df <- df[new.ind, ]    # This is why a data frame was made
    colors <- .reassignColours(map$names, ord.df$region, ord.df$color, ...)
    
    list(colors = colors,
         scheme = colrange,
         bins = cats)
  }






.assertListElements <- function(x) {
  stopifnot(c('region', 'value', 'breaks') %in% names(x))
  
  region.valid <- .allAreRegions(x$region)
  v <- x$value
  value.valid <- is.numeric(v) || is.factor(v) || is.character(v)
  c <- x$categories
  
  cat.valid <-
    if (!is.null(c))
      is.character(c) || is.factor(c)
  else
    TRUE
  
  all(region.valid, value.valid, cat.valid)
}








# Creates a  categorised variable from its inputs if not already a factor
# and is to be used in generating choropleth maps
#' @importFrom rlang abort
#' @importFrom rlang is_scalar_integer
.createCategorized <- function(val, brks = NULL, ...)
{
  if (is.character(val))
    val <- as.factor(val)
  
  if (is.factor(val)) {
    
    if (length(levels(val)) >= 10L)
      abort("Too many categories")
    
    return(val)
  }
  
  if (!is.numeric(val)) {
    msg <- paste(sQuote(typeof(val)), "is not a supported type")
    abort(msg)
  }
  
  if (is.null(brks))
    abort(paste("Breaks were not provided for the", 
                "categorization of a numeric type"))
  
  rr <- range(val)
  
  if (is_scalar_integer(brks))
    brks <- seq(rr[1], rr[2], diff(rr) / brks)
  
  if (rr[1] < min(brks) || rr[2] > max(brks))
    abort("Values are out of range of breaks")
  
  cut(val, brks, include.lowest = TRUE)
}







#' @importFrom RColorBrewer brewer.pal
#' @importFrom RColorBrewer brewer.pal.info
#' @importFrom rlang abort
#' @importFrom tools toTitleCase
.processColouring <- function(col = NULL, n, ...)
{
  .DefaultChoroplethColours <- getOption('choropleth.colours') # set in zzz.R
  
  if (is.null(col))
    col <- .DefaultChoroplethColours[1]
  
  if (is.numeric(col)) {
    default.pal <- .get_R_palette()
    all.cols <- sub("(green)(3)", "\\1", default.pal)
    all.cols <- sub("gray", "grey", all.cols)
    
    if (!col %in% seq_along(all.cols))
      abort(sprintf("'color' must range between 1L and %iL", 
                    length(all.cols)))
    
    col <- all.cols[col]
  }
  
  among.def.cols <- col %in% .DefaultChoroplethColours
  in.other.pal <- !among.def.cols && (col %in% rownames(brewer.pal.info))
  
  pal <-
    if (!among.def.cols) {
      
      if (!in.other.pal)
        abort(
          sprintf("'%s' is not a supported colour or palette", col)
        )
      
      col
    }
  else
    paste0(tools::toTitleCase(col), "s")
  
  RColorBrewer::brewer.pal(n, pal)
}







#' @importFrom grDevices palette
.get_R_palette <- function()
{
  if (getRversion() < as.numeric_version('4.0.0'))
    return(palette())
  palette('R3')
  pp <- palette()
  palette('R4')
  pp
}







# Reassigns colours to polygons that refer to similar regions i.e. duplicated
# polygon, ensuring that when the choropleth is drawn, the colours are 
# properly applied to the respective regions and not recycled.
#' @importFrom grDevices colours
.reassignColours <- 
  function(names, all.regions, in.colours, excl.region = NULL, excl.col = NULL)
  {
    stopifnot({
      is.character(names)
      all(is_state(all.regions))
      .isHexColor(in.colours)
    })
    
    out.colours <- new.names <- rep(NA, length(names))
    
    for (i in seq_along(all.regions)) {
      regx <- .regexDuplicatedPolygons(all.regions[i])
      ind <- grep(regx, names)
      out.colours[ind] <- in.colours[i]
      new.names[ind] <- sub(regx, "\\1", names[ind])[1]
    }
    
    ## Take care of situations where a region is not to be part of
    ## the choropleth and should be given an 'off-colour'
    if (!is.null(excl.region)) {
      off.color <- "grey"
      
      if (!is.null(excl.col)) {
        
        if (length(excl.col) > 1L)
          stop(paste("Only one colour can be used to denote regions excluded",
                     "from the choropleth colouring scheme"))
        
        if (!is.character(excl.col))
          stop(sprintf("Colour indicators of type '%s' are not supported",
                       typeof(excl.col)))
        
        if (!excl.col %in% colours())
          stop(paste("The colour used for excluded regions must be valid",
                     "i.e. an element of the built-in set 'colours()'"))
        
        off.color <- excl.col
      }
      
      excluded <- match(excl.region, new.names)
      out.colours[excluded] <- off.color
    }
    
    structure(out.colours, names = new.names)
  }






# Provides a regex pattern for checking polygons for jurisdictions that
# are matched more than once e.g. foo:1, foo:2, bar:1, bar:2, bar:3
.regexDuplicatedPolygons <- function(x)
{
  stopifnot(is.character(x))
  paste0("^(", paste0(x, collapse = "|"),")(\\:\\d)?$")
}




.regexSubsetRegions <- function(x) {
  stopifnot(is.character(x))
  paste0(x, collapse = "|")
}






.isHexColor <- function(x) 
{
  if (!is.character(x)) return(FALSE)
  all(grepl("^#", x), nchar(x) == 7L)
}





# Rejigs text that is used for labeling a region that has more than 1 polygon
# TODO: Quite a bit of hard-coding was used (v. 0.1.0) and should be reviewed
.adjustLabels <- function(x) 
{
  stopifnot(is.character(x))
  x <- vapply(
    x,
    FUN.VALUE = character(1L),
    FUN = function(l)
      sub("\\:\\d*$", "", l)
  )
  
  dd <- data.frame(state = c("Akwa Ibom", "Cross River"),
                   poly = c(2, 4))
  
  ss <- dd[["state"]]
  
  if (all(grepl(paste0(ss, collapse = "|"), x))) {
    .fadj <- function(a, reg, pos) {
      ind <- grep(reg, a)
      finalPos <- ind[pos]
      a[ind] <- ""
      a[finalPos] <- reg
      a
    }
    
    for (i in seq_len(2))
      x <- .fadj(x, ss[i], dd[["poly"]][i])
  }
  
  dup <- which(duplicated(x))
  x[dup] <- character(1L)
  .toggleFct(x, use = "abbrev") # when FCT is labelled
}





# Checks that x and y coordinates are within the bounds of given map
# Note: This check is probably too expensive. Consider passing just the range
# though the loss of typing may make this less reliable down the line
#' @importFrom rlang is_double
.xyWithinBounds <- function(map, x, y)
{ 
  stopifnot(inherits(map, 'map'), is_double(x), is_double(y))
  rr <- map$range
  xx <- x >= rr[1] & x <= rr[2]
  yy <- y >= rr[3] & y <= rr[4]
  all(xx, yy)
}





## Get the properties of shape files
## First collect the path to the shapfile project directory
## Then, read the shapefile
## Collect the namefield from the data, based on the region
#' @importFrom methods slot
#' @importFrom rgdal readOGR
ShapefileProps <- function(regions)
{
  shp <- .getShapefileDir(regions)
  pkgdir <- "extdata"
  
  dsn <- system.file(file.path(pkgdir, shp),
                     package = 'naijR',
                     mustWork = TRUE)
  
  if (identical(dsn, character(1)))
    stop(sprintf("The map data could not be found in '%s'", pkgdir))
  
  rgx <- "\\.shp$"
  shpfl <- list.files(dsn, rgx)
  lyr <- sub(pattern = paste0("(.+)(", rgx, ")"), "\\1", shpfl)
  sp <- readOGR(dsn, lyr, verbose = FALSE)
  
  ## The following shapefile, has some unwanted data
  if (shp == "nigeria-lgas") 
    sp <- subset(sp, STATE != "Lake")
  
  dt <- slot(sp, "data")
  
  if (shp == "nigeria-lgas")
    dt$STATE[dt$STATE == "Abuja"] <- "Federal Capital Territory"
  
  nmfld <- .fetchNamefield(regions, dt)
  stopifnot(!is.na(nmfld))
  new_ShapefileProps(shp, lyr, nmfld, sp)
}




## Low-level constructor
new_ShapefileProps <- function(dir, layer, namefield, spObj)
{
  structure(list(dir, layer, namefield, spObj),
            names = c("shapefile", "layer", "namefield", "spatialObject"), 
            class = "ShapefileProps")
}






## Fetch the namefield
## This is a generic function. The methods we have are for distinguishing
## how namefields are retrieved, since this varies depending on the kind 
## of region passed. The interesting case in point is when getting this 
## field for LGA regions, since the spatial data also contains data on 
## States. To avoid confusion, when iterating through the data frame, when
## a column with States is encountered it is skipped as can be seen in the
## `.fetchNamefield.lgas` method.
.fetchNamefield <- function(x, ...)
  UseMethod(".fetchNamefield")


.fetchNamefield.lgas <- function(x, dt) {
  nmfld <- NA
  
  for (i in seq_len(ncol(dt))) {
    
    if (all(unique(dt[[i]]) %in% states()))  # skip column with States
      next
    
    if (any(x %in% dt[[i]])) {   # just any LGAs will do, since some of them
      nmfld <- colnames(dt)[i]   # also share names with their State
      break
    }
  }
  
  nmfld
}




.fetchNamefield.states <- function(x, dt) {
  # dt[[1]][dt[[1]] == "Nasarawa"] <- "Nassarawa" # Fix for 'ng_admin'
  nmfld <- NA
  
  for (i in seq_len(ncol(dt))) {
    
    if (all(x %in% dt[[i]])) {   # all MUST be states
      nmfld <- colnames(dt)[i]
      break
    }
    
  }
  
  nmfld
}




# Returns either a logical(1) or character(n)
.setLegendText <- function(val)
{
  arg <- "legend.text"
  
  # The default setting of 'legend.text' is to return TRUE
  if (is.null(val))
    return(TRUE)
  
  if (is.logical(val)) {
    if (length(val) > 1L)
      warning(.first_elem_warn(arg))
    
    return(val[1])
  }
  
  if (is.character(val))
    return(val)
  
  stop(sprintf("'%s' must be of type character or logical", arg))
}





.setLegendParams <- function(leg.arg)
{
  result <- .setLegendText(leg.arg)
  obj <- list(x = 13L, y = 7L, text = NULL, show = TRUE, xpd = NA)
  
  if (is.character(result))
    obj$text <- result
  
  if (is.logical(result))
    obj$show <- result
  
  obj
}





.setTextSize <- function(cex = NULL)
{
  if (is.null(cex)) {
    cex <- 0.75  # default for maps::map.text
    return(cex)
  }
  stopifnot(is.numeric(cex))
  cex
}





## Messages -----------------------------------------------------------------
.ngName <- function()
{
  "Nigeria"
}

.arg_str <- function(arg)
{
  deparse(substitute(arg))
}


.first_elem_warn <- function(arg)
{
  stopifnot(is.character(arg) && length(arg) == 1L)
  sprintf("Only the first element of '%s' was used", arg)
}


.nextMinorVer <- function()
{
  ">= 0.6.0"
}


.deprecMsg <- function(arg) {
  sprintf("map_ng(%s = )", deparse(substitute(arg)))
}
