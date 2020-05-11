# Source file: map_nigeria.R
#
# GPL-3 License
#
# Copyright (C) 2019-2020 Victor Ordu.
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
#' A map of the Federal Republic of Nigeria.
#' 
#' @details This function is essentially a wrapper to \code{maps::map}.
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
#' @importFrom magrittr %>%
#' @importFrom maps map
#' @importFrom maps map.text
#' 
#' @param state A character vector of a list of Nigerian States to be displayed.
#' The default value is to print all States. \code{NULL} will print an outline
#' map, where internal boundaries are not drawn.
#' @param data An object containing data, principally the variables required to
#' plotted in a map.
#' @param value Factor (or an object coercible to one) or numeric. If numerical, 
#' and depending on the size of \code{range(value)}, it will segregated into 
#' appropriately sized categories, or as defined by \code{breaks}.
#' @param breaks Numeric. A vector of length >= 1. If a single value i.e.
#' scalar, it denote the expected number of breaks. Internally, the function
#' will attempt to compute approprate category sizes or fail if out-of bounds. 
#' Where length is >= 3L, it is expected to be an arithmetic sequence that 
#' represents category bounds as for \code{\link[base]{cut}} (applicable 
#' only to choropleth maps).
#' @param categories The legend for the choropleth-plotted categories. If not 
#' defined, internally created labels are used.
#' @param col Colour to be used for the plot. For plain plots, this works just
#' as in \code{\link[maps]{map}} and variants. For choropleth maps, the colour
#' provided represents a (sequential) colour palette based on 
#' \code{\link[RColorBrewer]{brewer.pal}}. The possible colour options can be
#' checked with \code{getOption("choropleth.colours")} and can indeed be 
#' modified by the user.
#' @param fill Logical. Whether to colour the plotted map region(s). When 
#' drawing a choropleth map \code{fill == TRUE} is implied. 
#' @param show.neighbours logical; \code{TRUE} to display borders of
#' neighbouring countries.
#' @param show.text Logical. Apply labels to the regions of the map.
#' @param ... Further arguments for function \code{\link[maps]{map}}
#' 
#' @details ...
#' 
#' @note When adjusting the default colour choiced for choropleth maps, it is
#' advisable to use one of the sequential palettes. For a list of of available
#' palettes, especially for more advanced use, review 
#' \code{\link[RColorBrewer]{display.brewer.all}}
#' 
#' @return An object of class \code{map}, invisibly; as a side-effect,
#' results in the drawing of a map of Nigeria.
#'
#' @examples
#' map_ng() # Draw a map with default settings
#' map_ng(states("sw"))
#' map_ng("Kano")
#'
#' @return An object of class \code{maps} containing the data used to draw the
#' map and which can be used for additional calls to \code{\link[maps]{map}} or
#' other similar functions (e.g. \code{\link[graphics]{plot}}).
#'
#' @export
map_ng <- function(state = character(),
                   data = NULL,
                   value = NULL,
                   breaks = NULL,
                   categories = NULL,
                   col = NULL,
                   fill = FALSE,
                   show.neighbours = FALSE,
                   show.text = FALSE,
                   ...)
{
  ## NOTE: In the call to map.text, the name 'database' is actually
  ## required. This is because internally, there is a call to `eval()`
  ## which uses its default argument for `envir` i.e. `parent.frame()`.
  ## An object of any other name is not seen by the quoted call to
  ## maps::map used by the evaluator function. For more details,
  ## inspect the source code for `maps::map.text`. This is a bug in the
  ## `maps` package.
  state <- .processStateParam(state)
  stopifnot(is.logical(show.neighbours))
  if (show.neighbours)
    message("Display of neighbouring countries is disabled")
  value <- if (is.null(data) && !is_null(value))
    enquo(value) 
  else 
    enexpr(value)
  chrplth <- if (is_null(value) || is_symbol(value)) {
    .validateChoroplethParams(state, data, !!value)  # TODO: Refactor
  } else {
    value <- eval_tidy(value)
    .validateChoroplethParams(state, data, value)
  }
  if (is.null(col) && is_false(chrplth))
    col <- 1L
  database <- .getMapData(state)
  mapq <- expr(map(database, regions = state, col = col, fill = fill, ...))
  if (chrplth) {
    if (!is.null(data)) {
      vl.col <- as_name(value)
      st.col <- .stateColumnIndex(data, state)
      cValue <-  data[[vl.col]]
      cStates <- data[[st.col]]
    }
    else {
      cStates <- state
      cValue <- value
    }
    cParams <-
      list(
        state = cStates,
        value = cValue,   
        breaks = breaks,
        categories = categories
      )
    cOpts <- .prepareChoroplethOptions(database, cParams, col)
    col <- cOpts$colors
    fill <- TRUE
  }
  mp <- if (show.text) {
    if (!identical(state, 'Nigeria')) {
      txt <- database$name %>%
        {
          is <- lapply(state, grep, x = .)
          ind <- unlist(is)
          .[ind]
        } %>%
        .adjustLabels()
    }
    map.text(database, regions = state, labels = txt, col = col, fill = fill, ...)
    # mapq[[1]] <- sym('map.text')  TODO: put off for now
  }
  else
    eval_tidy(mapq)
  
  # Capture 'dots' and return visible 
  # 'map' object if `plot == FALSE`
  dots <- list(...)
  params <- names(dots)
  if ('plot' %in% params)
    if (is_false(dots$plot))
      return(mp)
  
  if (chrplth) {
    if (is.null(categories))
      categories <- cOpts$bins
    legend(x = 12, y = 5, legend = categories, fill = cOpts$scheme, xpd = NA)
  }
  invisible(mp)
}








#' @importFrom rlang abort
.processStateParam <- function(s)
{
  if (!identical(s, character()))
    if (!is.character(s) && !is.null(s))
      abort("Type of argument supplied to 'state' is invalid.")
  all.st <- states(all = TRUE)
  if (is.character(s)) {
    if (length(s) == 0L)
      s <- all.st
    if (!all(s %in% all.st))
      abort("One or more elements of 'state' is not a Nigerian state")
  }
  if (is.null(s))
    s <- "Nigeria"
  s
}





#' @importFrom rlang as_name
#' @importFrom rlang enexpr
#' @importFrom rlang is_null
#' @importFrom rlang is_symbol
.validateChoroplethParams <- function(state = NULL, data = NULL, val = NULL)
{
  # TODO: Add some verbosity.
  no.state <- is.null(state) || identical(state, "Nigeria")
  if (no.state && is.null(data))
    return(FALSE)
  arg <- enexpr(val)
  if (!no.state &&
      is.character(state) && is_state(state) && !is_null(arg))
    return(TRUE)
  if (!is.data.frame(data))
    return(FALSE)
  ind <- try(.stateColumnIndex(data), silent = TRUE)
  if (inherits(ind, 'try-error'))
    return(FALSE)
  if (is_symbol(arg))
    return(as_name(arg) %in% names(data))
  if (is.null(arg))
    if (ncol(data) < 2L)
      return(FALSE)
  TRUE
}






#' @importFrom maps SpatialPolygons2map
#' @importFrom rgdal readOGR
.getMapData <- function(state)
{
  if (identical(state, 'Nigeria'))
    return("mapdata::worldHires")
  else if (is_state(state)) {
    dsn <- system.file("extdata/ng_admin", package = 'naijR', mustWork = TRUE)
    if (identical(dsn, character(1)))
      stop("The map data could not be found in 'extdata'")
    sp <- readOGR(dsn, .shpLayer, verbose = FALSE)
    return(SpatialPolygons2map(sp, namefield = 'admin1Name'))
  }
  ss <- paste(state, collapse = ', ')
  stop("Invalid region(s) for the map: ", ss)
}








# For possible export later
.shpLayer <- "nga_admbnda_adm1_osgof_20161215"






#' @importFrom rlang abort
#' @importFrom rlang warn
.stateColumnIndex <- function(dt, s = NULL)
{
  stopifnot(is.data.frame(dt))
  if (is.null(s))
    s <- states()
  n <- vapply(dt, function(x) {
    if (is.factor(x))
      x <- as.character(x)
    if (is.character(x))
      is_state(x)
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
        state = opts$state,
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
    mapstates <- .getUniqueStateNames(map)
    new.ind <- order(df$state, mapstates)
    ord.df <- df[new.ind, ]    # This is why a data frame was made
    colors <- .reassignColours(map$names, ord.df$state, ord.df$color)
    list(colors = colors,
         scheme = colrange,
         bins = cats)
  }






#' @import magrittr
.assertListElements <- function(x) {
  stopifnot(c('state', 'value', 'breaks') %in% names(x))
  state.valid <- is_state(x$state)
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
  
  all(state.valid, value.valid, cat.valid)
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
#' @importFrom grDevices palette
#' @importFrom magrittr %>%
#' @importFrom rlang abort
#' @importFrom tools toTitleCase
.processColouring <- function(color = NULL, n)
{
  .DefaultChoroplethColours <- getOption('choropleth.colours')
  if (is.null(color))
    color <- .DefaultChoroplethColours[1]
  if (is.numeric(color)) {
    all.cols <- grDevices::palette() %>% 
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





# .generateCatIndices <- function(val, brks = NULL)
# {
#   i <- if (is.factor(val))
#     as.integer(val)
#   else if (is.numeric(val))
#     findInterval(val, brks, all.inside = TRUE)
#   else
#     stop(sprintf("'%s' is not supported.", typeof(val)))
#   invisible(i)
# }





# Reassigns colours to polygons that refer to similar regions i.e. duplicated
# polygon, ensuring that when the choropleth is drawn, the colours are 
# properly applied to the respective regions and not recycled.
.reassignColours <- function(names, states, in.colours)
{
  stopifnot(is.character(names), is_state(states), .isHexColor(in.colours))
  out.colours <- new.names <- rep(NA, length(names))
  for (i in seq_along(states)) {
    regx <- .regexDuplicatedPolygons(states[i])
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
  .fadj <- function(x, state, pos) {
    ind <- grep(state, x)
    finalPos <- ind[pos]
    x[ind] <- ""
    x[finalPos] <- state
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




# # TODO: Review this approach
# .toggleEmpties <- function(vec, start, pos) {
#   stopifnot(is.character(vec), is.numeric(pos))
#   pos <- pos - start
#   vec[pos] <- vec[start]
#   vec[start] <- character(1L)
#   vec
# }
