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

#' Map of Nigeria
#'
#' A map of the Federal Republic of Nigeria.
#' 
#' @details This function is essentially a wrapper to \code{maps::map}.
#'
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
#' to \code{choropleth}) 
#' @param show.neighbours logical; \code{TRUE} to display borders of
#' neighbouring countries.
#' @param col A character vector of R \code{\link[grDevices]{colors}} to be
#' used in the map. If \code{fill == TRUE}, colours are applied to the mapped
#' area; otherwise it is applied to the borders.
#' @param fill Logical. Whether to fill the mapped area or not.
#' @param label Logical. Apply labels to the map.
#' @param ... Further arguments for function \code{\link[maps]{map}}
#' 
#' @details ...
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
                   show.neighbours = FALSE,
                   col = if (flavour == 'plain') 1L else NULL,
                   fill = FALSE,
                   label = FALSE,
                   ...)
{
  # TODO: Intelligently search for states in a data frame
  all.st <- states(all = TRUE)
  if (length(state) == 0L && !is.null(state))
    state <- all.st
  if (!any(state %in% all.st))
    stop("One or more elements of 'state' is not a Nigerian state")
  if (is.null(flavour))
    stop("Invalid input for 'flavour'.")
  flavour <- match.arg(flavour)
  stopifnot(is.logical(show.neighbours))
  if (show.neighbours)
    message("Display of neighbouring countries is disabled")
  mapdata <- .getMapData()
  dots <- list(...)
  plot <- TRUE
  if ('plot' %in% names(dots))
    plot <- dots[['plot']]
  if (flavour == 'choropleth') {
    if (is.null(data) || is.null(value) || is.null(breaks)) {
      # We want this to fail automatically once parameters are changed
      stop(
        sprintf(
          "'%s', '%s' and '%s' are required for choropleths.",
          deparse(quote(data)),
          deparse(quote(value)),
          deparse(quote(breaks))
        )
      )
    }
    ind <- .stateColumnIndex(data, state)
    state.col <- data[[ind]]
    fill <- TRUE
    intMp <- map(mapdata, regions = state, plot = FALSE)
    cOpts <- 
      .prepareChoroplethOptions(intMp, data, state.col, value, breaks, col)
    col <- cOpts$colors
  }
  mp <- map(
    mapdata,
    regions = state,
    plot = plot,
    fill = fill,
    col = col
  )
  # if (label)
  #   map.text(dt, regions = state, plot = plot, fill = fill, col = col)
  if (plot && flavour == 'choropleth')
    legend(
      x = 12,
      y = 5,
      legend = cOpts$bins,
      fill = cOpts$scheme,
      xpd = NA
    )
  invisible(mp)
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








#' @import magrittr
#' @importFrom RColorBrewer brewer.pal
#' @importFrom tools toTitleCase
.prepareChoroplethOptions <-
  function(map, dframe, state, val.name, bins, col = NULL)
  {
    # TODO: Set limits for variables and brk
    # TODO: Accept numeric input for col
    # stopifnot(inherits(map, 'map'))
    cols <- c("grey", "red", "green", "blue")
    if (is.null(col))
      col <- cols[1]
    else if (!col %in% cols)
      stop(sprintf(
        "%s is not in supported colour range of %s",
        col,
        paste(cols, collapse = '-'),
        cols[1]
      ))
    pal <- col %>%
      toTitleCase %>%
      paste0("s")
    mapstates <- .getUniqueStateNames(map)
    dframe <-
      data.frame(state = state,
                 value = dframe[[val.name]],
                 stringsAsFactors = FALSE)
    dframe$cats <- cut(dframe$value, bins, include.lowest = TRUE)
    dframe$ind <- findInterval(dframe$value, bins, all.inside = TRUE)
    colrange <- RColorBrewer::brewer.pal(length(bins) - 1, pal)
    dframe$color <- colrange[dframe$ind]
    newind <- order(dframe$state, mapstates)
    dframe <- dframe[newind,]
    colors <- .reassignColours(map$names, dframe$state, dframe$color)
    list(colors = colors,
         scheme = colrange,
         bins = levels(dframe$cats))
  }






.reassignColours <- function(names, states, in.colours)
{
  out.colours <- rep(NA, length(names))
  for (i in seq_along(states)) {
    c <- states[i]
    ind <- grep(c, names)
    out.colours[ind] <- in.colours[i]
  }
  out.colours
}





.getUniqueStateNames <- function(map)
{
  stopifnot(inherits(map, 'map'))
  unique(sub("(^.+)(:.+$)", "\\1", map$names))
}





#' @export
.shpLayer <- "nga_admbnda_adm1_osgof_20161215"