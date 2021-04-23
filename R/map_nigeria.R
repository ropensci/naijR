# Source file: map_nigeria.R
#
# GPL-3 License
#
# Copyright (C) 2019-2021 Victor Ordu.
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>

globalVariables(c(".", "STATE"))

#' Map of Nigeria
#'
#' Maps of the Federal Republic of Nigeria that are based on the basic
#' plotting idiom utilised by \link[maps:map]{maps:map} and its variants.
#' 
#' @importFrom rlang !!
#' @importFrom rlang as_name
#' @importFrom rlang enexpr
#' @importFrom rlang enquo
#' @importFrom rlang eval_tidy
#' @importFrom rlang expr
#' @importFrom rlang is_false
#' @importFrom rlang is_null
#' @importFrom rlang is_symbol
#' @importFrom graphics legend
#' @importFrom graphics par
#' @importFrom graphics points
#' @importFrom magrittr %>%
#' @importFrom maps map
#' @importFrom maps map.text
#' 
#' @param region A character vector of regions to be displayed. This could be 
#' States or Local Government Areas.
#' @param data An object containing data, principally the variables required to
#' plot in a map.
#' @param x,y Numeric object or factor (or coercible to one). See \emph{Details}.
#' @param breaks Numeric. A vector of length >= 1. If a single value i.e.
#' scalar, it denotes the expected number of breaks. Internally, the function
#' will attempt to compute appropriate category sizes or fail if out-of bounds. 
#' Where length is >= 3L, it is expected to be an arithmetic sequence that 
#' represents category bounds as for \code{\link[base]{cut}} (applicable 
#' only to choropleth maps).
#' @param categories The legend for the choropleth-plotted categories. If not 
#' defined, internally created labels are used.
#' @param title,caption An optional string for annotating the map.
#' @param leg.x,leg.y Numeric. Position of the legend.
#' @param leg.title String. The legend Title
#' @param leg.orient The orientation of the legend i.e. whether horizontal or
#' vertical.
#' @param show.neighbours Logical; \code{TRUE} to display the immediate vicinity
#' neighbouring regions/countries.
#' @param show.text Logical. Apply labels to the regions of the map.
#' @param ... Further arguments passed to \code{\link[maps]{map}}
#' 
#' @details The default value for \code{region} is to print all State boundaries.
#' \code{data} enables the extraction of data for plotting from an object
#' of class \code{data.frame}. Columns containing States are identified. The
#' argument also provides context for quasiquotation when providing the 
#' \code{x} and \code{y} arguments.
#' 
#' For \code{x} and \code{y}, when both arguments are supplied, they are taken
#' to be point coordinates. If only \code{x} is supplied, it is presumed that the
#' intention is to make a choropleth map, and thus, numeric vector arguments are
#' converted into factors i.e. number classes. Otherwise factors or any object 
#' that can be coerced to a factor should be used.
#' 
#' For plain plots, the \code{col} argument works the same as with
#' \code{\link[maps]{map}}. For choropleth maps, the colour provided represents 
#' a (sequential) colour palette based on \code{RColorBrewer::brewer.pal}. The 
#' available colour options can be checked with 
#' \code{getOption("choropleth.colours")} and this can also be modified by the user.
#' 
#' @note When adjusting the default colour choices for choropleth maps, it is
#' advisable to use one of the sequential palettes. For a list of of available
#' palettes, especially for more advanced use, review 
#' \code{RColorBrewer::display.brewer.all}
#'
#' @examples
#' \dontrun{
#' map_ng() # Draw a map with default settings
#' map_ng(states("sw"))
#' map_ng("Kano")
#' }
#'
#' @return An object of class \code{maps} containing the data used to draw the
#' map and which can be used for additional calls to \code{\link[maps]{map}} or
#' other similar functions (e.g. \code{graphics::plot.default}).
#'
#' @export
map_ng <- function(region = character(),
                   data = NULL,
                   x = NULL,
                   y = NULL,
                   breaks = NULL,
                   categories = NULL,
                   title = NULL,
                   caption = NULL,
                   show.neighbours = FALSE,
                   show.text = FALSE,
                   leg.x = 13L,
                   leg.y = 7L,
                   leg.title,
                   leg.orient = c('vertical', 'horizontal'),
                   ...)
{
  ## TODO: Allow this function to accept a matrix e.g. for plotting points
  if (!is.character(region))
    stop("Expected a character vector as 'region'")
  region <- .processRegionParam(region)
  if (!is.logical(show.neighbours))
    stop("'show.neighbours' should be a boolean")
  if (length(show.neighbours) > 1L) {
    warning("Only the first element of 'show.neighbours' was used")
    show.neighbours <- show.neighbours[1]
  }
  if (show.neighbours)
    message("Display of neighbouring countries is temporarily disabled")
  value.x <- if (is_null(data) && !is_null(x))
    enquo(x) 
  else 
    enexpr(x)
  
  chrplth <- if (is_null(value.x) || is_symbol(value.x)) {
    .validateChoroplethParams(!!value.x, region, data)  # TODO: Refactor
  } else {
    value.x <- eval_tidy(value.x)
    .validateChoroplethParams(value.x, region, data)
  }
  if (!is_null(y))
    chrplth <- FALSE
  
  database <- .getMapData(region)
  mapq <- expr(map(database, regions = region, ...))
  
  ## Capture 'dots'
  dots <- list(...)
  
  ## Prepare to draw choropleth 
  if (chrplth) {
    mapq <- expr(map(database, region))  ## TODO: Consider rlang::call2
    if (!is.null(dots$plot))
      if (!dots$plot)
        mapq$plot <- FALSE
    mapq$fill <- TRUE
    if (!is.null(data)) {
      vl.col <- as_name(value.x)
      st.col <- .regionColumnIndex(data, region)
      cValue <-  data[[vl.col]]
      cStates <- data[[st.col]]
    }
    else {
      cStates <- region
      cValue <- value.x
    }
    cParams <-
      list(
        region = cStates,
        value = cValue,   
        breaks = breaks,
        categories = categories
      )
    cOpts <- .prepareChoroplethOptions(database, cParams, dots$col)
    mapq$col <- cOpts$colors
    lego <- match.arg(leg.orient)
    horiz <- if (identical(lego, 'vertical')) 
      FALSE 
    else 
      TRUE
    leg.tit <- if (!missing(leg.title)) 
      as.character(leg.title)
  }
  
  outlineMap <- identical(region, 'Nigeria')
  
  ## Draw a `maps::map()` (with or without labels) or capture the object.
  ##
  ## NOTE: In the call to map.text, the name 'database' is actually
  ## required. This is because, internally, there is a call to `eval()`
  ## which uses its default argument for `envir` i.e. `parent.frame()`.
  ## An object of any other name is not seen by the quoted call to
  ## `maps::map()` used by the evaluator function. For more details,
  ## inspect the source code for `maps::map.text()`. This is actually a 
  ## bug in the 'maps' package.
  database <- eval_tidy(mapq)    # possibly, a new `database` is created
  if (!is.null(dots$plot) && !dots$plot)
    return(database)
  if (show.text) {
    txt <- "Nigeria"
    if (!outlineMap) {
      txt <- database$name %>%
        { 
          # Account for multi-polygon regions
          # TODO: Scope this to parent environment
          rgxRegions <-
            function(x, nms) {
              rgx <- paste0("^", x, "(\\:\\d*)?$")
              grep(rgx, nms, value = TRUE)
            }
          is <- lapply(region, rgxRegions, nms = .)
          unlist(is)
        }
      map.text(
        database,
        regions = txt,
        exact = TRUE,                # disallow partial matching
        labels = .adjustLabels(txt),
        add = TRUE)
    }
  }
  
  ## Annotate
  title(main = title, sub = caption)
  if(!is_null(y)) {
    if (!.xyWithinBounds(database, x, y))
      stop("Coordinates are out of bounds of the map")
  }
  if (chrplth) {
    if (is.null(categories))
      categories <- cOpts$bins
    legend(
      x = leg.x,
      y = leg.y,
      legend = categories,
      fill = cOpts$scheme,
      xpd = NA,
      horiz = horiz,
      title = leg.tit
    )
  }
  else if(!is_null(y))
    points(x, y, pch = "+") 
  
  invisible(database)
}







## MARK FOR DEPRECATION
## Processes character input, presumably States, and when empty
## character vector, provide all the States as a default value.
## This function's use will be overtaken by the introduction 
## of LGA level mapping.
#' @importFrom rlang abort
.processRegionParam <- function(s)
{
  stopifnot(is.character(s))
  if (identical(s, "Nigeria"))
    return(s)
  all.st <- states(all = TRUE)
  if (length(s) == 0L)
    s <- all.st
  if (!all(s %in% all.st) && !all(s %in% lgas()))
    abort("One or more elements of 'region' is not a Nigerian region")
  s
}





#' @importFrom rlang as_name
#' @importFrom rlang enexpr
#' @importFrom rlang is_null
#' @importFrom rlang is_symbol
.validateChoroplethParams <- function(val = NULL, region = NULL, data = NULL)
{
  # TODO: Add some verbosity.
  no.region <- is.null(region) || identical(region, "Nigeria")
  if (no.region && is.null(data))
    return(FALSE)
  arg <- enexpr(val)
  if (!no.region &&
      is.character(region) && all(is_state(region)) && !is_null(arg))
    return(TRUE)
  if (!is.data.frame(data))
    return(FALSE)
  ind <- try(.regionColumnIndex(data), silent = TRUE)
  if (inherits(ind, 'try-error'))
    return(FALSE)
  if (is_symbol(arg))
    return(as_name(arg) %in% names(data))
  if (is.null(arg))
    if (ncol(data) < 2L)
      return(FALSE)
  TRUE
}







## S3 Class and methods for internal use:
.getMapData <- function(x)
  UseMethod(".getMapData")


#' @import mapdata
.getMapData.default <- function(x) 
{
  if (identical(x, 'Nigeria'))
    return("mapdata::worldHires")
  .getMapData(states())
}


.getMapData.lgas <- function(x)
{
    obj <- shp.lga
    .getMapFromSpObject(obj)
 }



.getMapData.states <- function(x)
{
  obj <- shp.state
  .getMapFromSpObject(obj)
}



#' @importFrom maps SpatialPolygons2map
.getMapFromSpObject <- function(shp.data)
{
  stopifnot(inherits(shp.data, 'ShapefileProps'))
  SpatialPolygons2map(shp.data[['spatialObject']],
                      namefield = shp.data[['namefield']])
}






.fixBadNames <- function(map)
{
  map$names[map$names == "Nasarawa"] <- "Nassarawa"
  map
}




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
  if (is.null(s))
    s <- states()
  
  ## Checks if a column has the names of States, returning TRUE is so.
  .fx <- function(x) {
    if (is.factor(x))    # TODO: Earmark for removal
      x <- as.character(x)
    ret <- logical(1)
    if (is.character(x)) {
      areStates <- is_state(x)
      ret <- all(areStates)
      if (!ret && any(areStates)) {
        misspelt <- which(!areStates)
        msg <- sprintf("The following regions are misspelt: %s",
                       paste(x[!areStates], collapse = ","))
        warning(msg, call. = FALSE)
      }
    }
    ret
  }
  n <- vapply(dt, .fx, logical(1))
  if (!sum(n))
    abort(sprintf("No column with elements in %s.", deparse(substitute(s))))
  if (sum(n) > 1)
    warning("Multiple columns have States, so the first is used")
  which(n)[1]
}








#' @importFrom rlang abort
.prepareChoroplethOptions <-
  function(map, opts, col = NULL)
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
    mapregions <- .getUniqueStateNames(map)
    new.ind <- order(df$region, mapregions)
    ord.df <- df[new.ind, ]    # This is why a data frame was made
    colors <- .reassignColours(map$names, ord.df$region, ord.df$color)
    list(colors = colors,
         scheme = colrange,
         bins = cats)
  }






#' @import magrittr
.assertListElements <- function(x) {
  stopifnot(c('region', 'value', 'breaks') %in% names(x))
  region.valid <- all(is_state(x$region))
  value.valid <- 
    x$value %>% 
    {
      is.numeric(.) || is.factor(.) || is.character(.)
    }
  cat.valid <-
    x$categories %>%
    {
      if (!is.null(.))
        is.character(.) || is.factor(.)
      else
        TRUE
    }
  
  all(region.valid, value.valid, cat.valid)
}






.getUniqueStateNames <- function(map)
{
  stopifnot(inherits(map, 'map'))
  unique(sub("(^.+)(:.+$)", "\\1", map$names))
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
  if (is.numeric(val)) {
    if (is.null(brks))
      abort("Breaks were not provided for the categorization of a numeric type")
    rr <- range(val)
    if (is_scalar_integer(brks))
      brks <- seq(rr[1], rr[2], diff(rr) / brks)
    if (rr[1] < min(brks) || rr[2] > max(brks))
      abort("Values are out of range of breaks")
    return(cut(val, brks, include.lowest = TRUE))
  }
  msg <- paste(sQuote(typeof(val)), "is not a supported type")
  abort(msg)
}







#' @importFrom RColorBrewer brewer.pal
#' @importFrom RColorBrewer brewer.pal.info
#' @importFrom magrittr %>%
#' @importFrom rlang abort
#' @importFrom tools toTitleCase
.processColouring <- function(col = NULL, n)
{
  .DefaultChoroplethColours <- getOption('choropleth.colours') # set in zzz.R
  if (is.null(col))
    col <- .DefaultChoroplethColours[1]
  if (is.numeric(col)) {
    default.pal <- .get_R_palette()
    all.cols <- default.pal %>% 
      sub("(green)(3)", "\\1", .) %>% 
      sub("gray", "grey", .)
    if (!col %in% seq_along(all.cols))
      abort(sprintf("'color' must range between 1L and %iL", length(all.cols)))
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
.reassignColours <- function(names, regions, in.colours)
{
  stopifnot(is.character(names), all(is_state(regions)), .isHexColor(in.colours))
  out.colours <- new.names <- rep(NA, length(names))
  for (i in seq_along(regions)) {
    regx <- .regexDuplicatedPolygons(regions[i])
    ind <- grep(regx, names)
    out.colours[ind] <- in.colours[i]
    new.names[ind] <- sub(regx, "\\1", names[ind])[1]
  }
  structure(out.colours, names = new.names)
}






# Provides a regex pattern for checking polygons for jurisdictions that
# are matched more than once e.g. foo:1, foo:2, bar:1, bar:2, bar:3
# TODO: DEPRECATE.
.regexDuplicatedPolygons <- function(x)
{
  stopifnot(is.character(x))
  paste0("^(", x,")(.?|\\:\\d*)$")
}









.isHexColor <- function(x) 
{
  if (!is.character(x)) return(FALSE)
  all(grepl("^#", x), nchar(x) == 7L)
}





# Rejigs text that is used for labeling a region that has more than 1 polygon
# TODO: Quite a bit of hard-coding was used (v. 0.1.0) and should be reviewed
#' @importFrom magrittr %>%
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
  x
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
  dsn <- system.file(file.path("extdata", shp),
                     package = 'naijR',
                     mustWork = TRUE)
  if (identical(dsn, character(1)))
    stop("The map data could not be found in 'extdata'")
  pat <- "\\.shp$"
  shpfl <- list.files(dsn, pat)
  lyr <- sub(pattern = paste0("(.+)(", pat, ")"), "\\1", shpfl)
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
  dt[[1]][dt[[1]] == "Nasarawa"] <- "Nassarawa" # Fix for 'ng_admin'
  nmfld <- NA
  for (i in seq_len(ncol(dt))) {
    if (all(x %in% dt[[i]])) {   # all MUST be states
      nmfld <- colnames(dt)[i]
      break
    }
  }
  nmfld
}
