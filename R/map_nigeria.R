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
#' @import rlang
#' @importFrom graphics legend
#' @importFrom graphics par
#' @importFrom maps map
#' @importFrom maps map.text
#' 
#' @param state A character vector of a list of Nigerian States to be displayed.
#' The default value is to print all States.
#' @param flavour The type of map to be drawn. Current options are \code{plain}
#' and \code{choropleth}.
#' @param data An object containing data, principally the variables required to
#' plotted in a map (applicable only to \code{choropleth})
#' @param value The value to be categorised for choropleth mapping.
#' @param breaks Categories to be plotted on a map (applicable only 
#' to \code{choropleth}).
#' @param categories The name of the choropleth-plotted categories. If not 
#' provided, a set of default labels created internally by 
#' \code{\link[base]{cut}} is used.
#' @param col Colour to be used for the plot. For plain plots, this works just
#' as in \code{\link[maps]{map}} and variants. For choropleth maps, the colour
#' provided represents a sequential colour palette based on 
#' \code{\link[RColorBrewer]{brewer.pal}}. The possible colour options can be
#' checked with \code{getOption("choropleth.colours")} and can indeed be 
#' modified by the user.
#' @param fill Logical. Whether to colour the plotted map region(s). When 
#' drawing a choropleth map it is understood that colouring is intented, and
#' thus, this parameter is overriden. 
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
                   flavour = c("plain", 'choropleth'),
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
  all.st <- states(all = TRUE)
  if (length(state) == 0L && !is.null(state))
    state <- all.st
  if (!any(state %in% all.st))
    stop("One or more elements of 'state' is not a Nigerian state")
  if (is.null(flavour))
    stop("Invalid input for 'flavour'.")  ## TODO: Huh?
  flavour <- match.arg(flavour)
  if (is.null(col) && flavour == 'plain')
    col <- 1L
  stopifnot(is.logical(show.neighbours))
  if (show.neighbours)
    message("Display of neighbouring countries is disabled")
  dots <- list(...)
  params <- names(dots)
  database <- .getMapData()
  
  ## NOTE: In the call to map.text, the name 'database' is actually
  ## required. This is because internally, there is a call to `eval()`
  ## which uses its default argument for `envir` i.e. `parent.frame()`.
  ## An object of any other name is not seen by the quoted call to
  ## maps::map used by the evaluator function. For more details,
  ## inspect the source code for `maps::map.text`. This is a bug in the
  ## `maps` package.
  mapq <- 
    call2('map', database, regions = state, col = col, fill = fill, ...)
  if (isChoropleth <- flavour == 'choropleth') {
    .validateChoroplethParams(data, value, breaks)
    st.ind <- .stateColumnIndex(data, state)
    st.column <- data[[st.ind]]
    intMp <- map(database, regions = state, plot = FALSE)
    inputList <-
      list(
        state = st.column,
        value = data[[value]],
        breaks = breaks,
        categories = categories
      )
    cOpts <- .prepareChoroplethOptions(intMp, inputList, col)
    col <- cOpts$colors
    fill <- TRUE
  }
  dontPlot <- FALSE
  if ('plot' %in% params) {
    if (dontPlot <- isFALSE(dots$plot))
      show.text <- FALSE
  }
  if (show.text)
    mapq[[1]] <- sym('map.text')
  mp <- eval_tidy(mapq)
  if (isChoropleth) {
    if (dontPlot)
      return(invisible(mp))
    if (is.null(categories))
      categories <- cOpts$bins
    legend(x = 12, y = 5, legend = categories, fill = cOpts$scheme, xpd = NA)
  }
  invisible(mp)
}









#' @importFrom maps SpatialPolygons2map
#' @importFrom rgdal readOGR
.getMapData <- function()
{
  dsn <-
    system.file("extdata/ng_admin",
                package = 'naijR',
                mustWork = TRUE)
  if (identical(dsn, character(1)))
    stop("The map data could not be found in 'extdata'")
  sp <- readOGR(dsn, .shpLayer, verbose = FALSE)
  SpatialPolygons2map(sp, namefield = 'admin1Name')
}








.validateChoroplethParams <- function(x, y, z)
{
  if (is.null(x) || is.null(y) || is.null(z)) {
    # TODO: Rewrite
    stop(
      sprintf(
        "'%s', '%s' and '%s' are required for choropleths.",
        deparse(quote(data)),
        deparse(quote(value)),
        deparse(quote(breaks))
      )
    )
  }
}






.stateColumnIndex <- function(dt, s)
{
  stopifnot(is.data.frame(dt), is_state(s))
  n <- vapply(dt, function(x) {
    if (is.factor(x))
      x <- as.character(x)
    if (is.character(x))
      all(x %in% s)
    else
      FALSE
  }, logical(1))
  if (!sum(n))
    stop(sprintf("No column with elements in %s.", deparse(substitute(s))))
  if (sum(n) > 1)
    warning("Multiple columns have States, so the first is used")
  which(n)[1]
}









.prepareChoroplethOptions <-
  function(map, opts, col = NULL)
  {
    # TODO: Set limits for variables and brk
    # TODO: Accept numeric input for col
    stopifnot(inherits(map, 'map'))
    if(!.assertListElements(opts))
      stop("One or more inputs for generating choropleth options are invalid")
    mapstates <- .getUniqueStateNames(map)
    brks <- opts$breaks
    colrange <- .processColouring(col, brks)
    dframe <-
      data.frame(
        state = opts$state,
        value = opts$value,
        stringsAsFactors = FALSE
      )
    dframe$cats <- cut(dframe$value, brks, include.lowest = TRUE)
    ind <- findInterval(dframe$value, brks, all.inside = TRUE)
    dframe$color <- colrange[ind]
    newind <- order(dframe$state, mapstates)
    dframe <- dframe[newind, ]
    colors <- .reassignColours(map$names, dframe$state, dframe$color)
    list(colors = colors,
         scheme = colrange,
         bins = levels(dframe$cats))
  }







.assertListElements <- function(x) {
  stopifnot(c('state', 'value', 'breaks') %in% names(x))
  category.is.char <- 
    if (!is.null(x$categories)) 
      is.character(x$categories) 
    else TRUE
  all(is_state(x$state), is.numeric(x$value), category.is.char)
}






.getUniqueStateNames <- function(map)
{
  stopifnot(inherits(map, 'map'))
  unique(sub("(^.+)(:.+$)", "\\1", map$names))
}





#' @importFrom RColorBrewer brewer.pal
#' @importFrom RColorBrewer brewer.pal.info
#' @importFrom grDevices palette
#' @importFrom magrittr %>%
#' @importFrom tools toTitleCase
.processColouring <- function(color = NULL, breaks)
{
  .DefaultChoroplethColours <- getOption('choropleth.colours')
  if (is.null(color))
    color <- .DefaultChoroplethColours[1]
  if (is.numeric(color)) {
    all.cols <- grDevices::palette() %>% 
      sub("(green)(3)", "\\1", .) %>% 
      sub("gray", "grey", .)
    if (!color %in% seq_along(all.cols))
      stop(sprintf("'color' must range between 1L and %iL", length(all.cols)))
    color <- all.cols[color]
  }
  among.def.cols <- color %in% .DefaultChoroplethColours
  in.other.pal <- !among.def.cols && color %in% rownames(brewer.pal.info)
  pal <-
    if (!among.def.cols) {
      if (!in.other.pal)
        stop(
          sprintf("'%s' is not one of the supported colours or palettes", color)
        )
      color
    }
    else
      paste0(tools::toTitleCase(color), "s")
  RColorBrewer::brewer.pal(length(breaks) - 1, pal)
}






.reassignColours <- function(names, states, in.colours)
{
  stopifnot(is.character(names), is_state(states), .assertHexColor(in.colours))
  out.colours <- new.names <- rep(NA, length(names))
  for (i in seq_along(states)) {
    regx <- paste0("^(", states[i],")(.?|\\:\\d*)$")
    ind <- grep(regx, names)
    out.colours[ind] <- in.colours[i]
    new.names[ind] <- sub(regx, "\\1", names[ind])[1]
  }
  structure(out.colours, names = new.names)
}










.assertHexColor <- function(x) 
{
  stopifnot(is.character(x))
  all(grepl("^#", x), nchar(x) == 7L)
}









# For possible export later
.shpLayer <- "nga_admbnda_adm1_osgof_20161215"