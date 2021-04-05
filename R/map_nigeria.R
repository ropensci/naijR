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

globalVariables(".")

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
#' @param region A character vector of a list of Nigerian States to be displayed.
#' @param data An object containing data, principally the variables required to
#' plotted in a map.
#' @param x Numeric object or factor (or coercible to one). See \emph{Details}.
#' @param y Numeric. See \emph{Details}
#' @param breaks Numeric. A vector of length >= 1. If a single value i.e.
#' scalar, it denote the expected number of breaks. Internally, the function
#' will attempt to compute approprate category sizes or fail if out-of bounds. 
#' Where length is >= 3L, it is expected to be an arithmetic sequence that 
#' represents category bounds as for \code{\link[base]{cut}} (applicable 
#' only to choropleth maps).
#' @param categories The legend for the choropleth-plotted categories. If not 
#' defined, internally created labels are used.
#' @param col Colour to be used for the plot. 
#' @param fill Logical. Whether to colour the plotted map region(s). When 
#' drawing a choropleth map \code{fill == TRUE} is implied.
#' @param title Character vector of length 1.
#' @param caption Character vector of length 1.
#' @param leg.x Numeric. Position of the legend.
#' @param leg.y Numeric. Position of the legend.
#' @param leg.title Character. The legend Title
#' @param leg.orient The orientation of the legend i.e. whether horizontal or
#' vertical.
#' @param show.neighbours Logical; \code{TRUE} to display borders of
#' neighbouring countries.
#' @param show.text Logical. Apply labels to the regions of the map.
#' @param ... Further arguments for function \code{\link[maps]{map}}
#' 
#' @details The default value for \code{region} is to print all States. 
#' \code{NULL} will print an outline map, i.e. without internal boundaries.
#' \code{data} enables the extraction of data for plotting from an object
#' of class \code{data.frame}. Columns containing States are identified. The
#' argument also provided context for quasiquotation when providing the 
#' \code{x} and {y} arguments.
#' For \code{x} and \code{y}, when both arguments are supplied, they are taken
#' to be point coordinates and are plotted as such. If only \code{x} is supplied,
#' it is presumed that the intention is to make a choropleth map, and thus, 
#' numeric vector arguments are converted into factors i.e. number classes. 
#' Otherwise factors or any object that can be coerced to a factor should be used.
#' For plain plots, the \code{col} argument works the same as with
#' \code{\link[maps]{map}} and variants. For choropleth maps, the colour
#' provided represents a (sequential) colour palette based on 
#' \code{RColorBrewer::brewer.pal}. The possible colour options can be
#' checked with \code{getOption("choropleth.colours")} and this can also be 
#' modified by the user.
#' 
#' @note When adjusting the default colour choiced for choropleth maps, it is
#' advisable to use one of the sequential palettes. For a list of of available
#' palettes, especially for more advanced use, review 
#' \code{RColorBrewer::display.brewer.all}
#' 
#' @return An object of class \code{map}, invisibly; as a side-effect,
#' results in the drawing of a map of Nigeria.
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
                   col = NULL,
                   fill = FALSE,
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
    .validateChoroplethParams(region, data, !!value.x)  # TODO: Refactor
  } else {
    value.x <- eval_tidy(value.x)
    .validateChoroplethParams(region, data, value.x)
  }
  if (!is_null(y))
    chrplth <- FALSE
  if (is.null(col) && is_false(chrplth))
    col <- 1L
  database <- .getMapData(region)
  mapq <- expr(map(database, regions = region, col = col, fill = fill, ...))
  if (chrplth) {
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
    cOpts <- .prepareChoroplethOptions(database, cParams, col)
    col <- cOpts$colors
    fill <- TRUE
    lego <- match.arg(leg.orient)
    horiz <- if (identical(lego, 'vertical')) FALSE else TRUE
    leg.tit <- if (!missing(leg.title)) as.character(leg.title)
  }
  mp <- if (show.text) {
    if (!identical(region, 'Nigeria')) {
      txt <- database$name %>%
        {
          is <- lapply(region, grep, x = .)
          ind <- unlist(is)
          .[ind]
        } %>%
        .adjustLabels
    }
    
    ## NOTE: In the call to map.text, the name 'database' is actually
    ## required. This is because internally, there is a call to `eval()`
    ## which uses its default argument for `envir` i.e. `parent.frame()`.
    ## An object of any other name is not seen by the quoted call to
    ## maps::map used by the evaluator function. For more details,
    ## inspect the source code for `maps::map.text`. This is a bug in the
    ## `maps` package.
    map.text(database, region, labels = txt, col = col, fill = fill, ...)
  }
  else
    eval_tidy(mapq)
  
  if(!is_null(y))
    if (!.xyWithinBounds(mp, x, y))
      stop("Coordinates are out of bounds of the map")
  
  ## Capture 'dots' and return visible 'map' object if `plot == FALSE`
  dots <- list(...)
  params <- names(dots)
  if ('plot' %in% params)
    if (is_false(dots$plot))
      return(mp)
  
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
  title(title, caption)
  invisible(mp)
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
  if (!all(s %in% all.st) && !all(s %in% lgas_ng()))
    abort("One or more elements of 'region' is not a Nigerian region")
  s
}





#' @importFrom rlang as_name
#' @importFrom rlang enexpr
#' @importFrom rlang is_null
#' @importFrom rlang is_symbol
.validateChoroplethParams <- function(region = NULL, data = NULL, val = NULL)
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




.getMapData <- function(region)
  UseMethod(".getMapData")



#' @import mapdata
.getMapData.default <- function(region)
{
  stopifnot(is.character(region))
  if (identical(region, 'Nigeria'))
    return("mapdata::worldHires")
  hasStates <- is_state(region)
  hasLgas <- is_lga(region)
  isStateMap <- any(hasStates)
  isLgaMap <- any(hasLgas)
  if (isStateMap && isLgaMap) {
    if (sum(.LgaStates() %in% region) > 1L)
      isLgaMap <- FALSE
    else
      stop("Map must be based on either States or LGAs, not both.")
  }
  if (!isStateMap && !isLgaMap)
    stop("Neither States nor LGAs could be properly mapped.")
  
  if (isStateMap) {
    invalid <- region[!hasStates]
    params <- stateSpatialParams()
  }
  else if (isLgaMap) {
    invalid <- region[!hasLgas]
    params <- lgaSpatialParams()
  }
  
  if (length(invalid) > 0L) {
    invalid <- paste(invalid, collapse = ', ')
    stop("Invalid region(s) for the map: ", invalid)
  }
  
  getMapFromSpatialDataFiles(params)
}


.getMapData.lgas_ng <- function(region)
{
  getMapFromSpatialDataFiles(lgaSpatialParams())
}


# Takes a list with 2 elements - regtyp ('state' or 'lga') and
# namefield (the field in the shapefile the contains the regions' names)
# Returns an object of class 'map' via SpatialPolygons2map()
#' @importFrom maps SpatialPolygons2map
getMapFromSpatialDataFiles <- function(param)
{
  sp <- .getSpatialPolygonsDataFrame(param$regtyp)
  SpatialPolygons2map(sp, namefield = param$namefield)
}





# Parameters used for determining the region-specific elements 
# for the sp S4 objects, namely the region type (State/LGA) and
# the field containing the names (This is determined by inspecting
# the S4 object returned from the shapefiles.)
regionSpatialParams <- function(...) {
  list(...)
}


stateSpatialParams <- function()
{
  regionSpatialParams(regtyp = 'state', namefield = 'admin1Name')
}


lgaSpatialParams <- function()
{
  regionSpatialParams(regtyp = 'lga', namefield = 'LGA')
}



# States that are also the names of LGAs
.LgaStates <-
  function()
    c("Bauchi",
      "Ebonyi",
      "Ekiti",
      "Gombe",
      "Katsina",
      "Kogi",
      "Nasarawa",
      "Oyo")






## Read the data from an internal shapefile
#' @importFrom rgdal readOGR
.getSpatialPolygonsDataFrame <- function(region.type) {
  src <-
    if (region.type == 'state')
      'ng_admin'
  else if (region.type == 'lga')
    'lg_ng'
  dsn <-
    system.file(file.path("extdata", src),
                package = 'naijR',
                mustWork = TRUE)
  if (identical(dsn, character(1)))
    stop("The map data could not be found in 'extdata'")
  readOGR(dsn, .shpLayer(region.type), verbose = FALSE)
}







# For possible export later
.shpLayer <- function(level) {
  if (level == 'state')
    "nga_admbnda_adm1_osgof_20161215"
  else if (level == 'lga')
    "Nigeria_census_2006_WGS84"
  else
    stop("An appropriate layer is not avaiable")
}
    






#' @importFrom rlang abort
#' @importFrom rlang warn
.regionColumnIndex <- function(dt, s = NULL)
{
  stopifnot(is.data.frame(dt))
  if (is.null(s))
    s <- states()
  n <- vapply(dt, function(x) {
    if (is.factor(x))
      x <- as.character(x)
    if (is.character(x))
      all(is_state(x))
    else
      FALSE
  }, logical(1))
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
.processColouring <- function(color = NULL, n)
{
  .DefaultChoroplethColours <- getOption('choropleth.colours')
  if (is.null(color))
    color <- .DefaultChoroplethColours[1]
  if (is.numeric(color)) {
    default.pal <- .get_R_palette()
    all.cols <- default.pal %>% 
      sub("(green)(3)", "\\1", .) %>% 
      sub("gray", "grey", .)
    if (!color %in% seq_along(all.cols))
      abort(sprintf("'color' must range between 1L and %iL", length(all.cols)))
    color <- all.cols[color]
  }
  among.def.cols <- color %in% .DefaultChoroplethColours
  in.other.pal <- !among.def.cols && color %in% rownames(brewer.pal.info)
  pal <-
    if (!among.def.cols) {
      if (!in.other.pal)
        abort(
          sprintf("'%s' is not a supported colour or palette", color)
        )
      color
    }
    else
      paste0(tools::toTitleCase(color), "s")
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
# NOTE: Quite a bit of hard-coding was used (v. 0.1.0) and should be reviewed
#' @importFrom magrittr %>%
.adjustLabels <- function(x) 
{
  stopifnot(is.character(x))
  .fadj <- function(x, region, pos) {
    ind <- grep(region, x)
    finalPos <- ind[pos]
    x[ind] <- ""
    x[finalPos] <- region
    x
  }
  
  x <- vapply(
    x,
    FUN.VALUE = character(1L),
    FUN = function(l)
      sub("\\:\\d*$", "", l)
  )
  
  ss <- c("Akwa Ibom", "Cross River")
  cent <- c(2, 4)
  for (i in seq_len(2))
    x <- .fadj(x, ss[i], cent[i])
  
  dups <- which(duplicated(x))
  x[dups] <- character(1L)
  x
}





# Checks that x and y coordinates are withing the bounds of given map
# Note: This check is probably too expensive. Conider passing just the range
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
