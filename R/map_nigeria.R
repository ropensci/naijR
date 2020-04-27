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
#' @param region The subdivision for choropleth mapping.
#' @param value The value to be categorised for choropleth mapping.
#' @param breaks Categories to be plotted on a map (applicable only 
#' to \code{choropleth}) 
#' @param show.neighbours logical; \code{TRUE} to display borders of
#' neighbouring countries.
#' @param col A character vector of R \code{\link[grDevices]{colours}} to be
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
                   region = NULL,
                   value = NULL,
                   breaks = NULL,
                   show.neighbours = FALSE,
                   col = if (flavour == 'plain') 1L else NULL,
                   fill = FALSE,
                   label = FALSE,
                   ...)
{
  # TODO: Add an argument for labelling the States
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
  dt <- .getMapData()
  dots <- list(...)
  plot <- TRUE
  if ('plot' %in% names(dots))
    plot <- dots[['plot']]
  if (flavour == 'choropleth') {
    if (is.null(data) || is.null(value) || is.null(breaks)) {
      # We want this to fails when parameters are changed
      stop(
        sprintf(
          "'%s', '%s' and '%s' are required for choropleths.",
          deparse(quote(data)),
          deparse(quote(value)),
          deparse(quote(breaks))
        )
      )
    }
    fill <- TRUE
    intMp <- map(dt, regions = state, plot = FALSE)
    cOpts <- .prepareChoroplethOptions(intMp, data, region, value, breaks, col)
    col <- cOpts$colors
  }
  mp <- map(
    dt,
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








#' @import hellno
#' @import magrittr
#' @importFrom RColorBrewer brewer.pal
#' @importFrom tools toTitleCase
.prepareChoroplethOptions <-
  function(map, dt, state.col, val.col, brk, col = NULL)
  {
    # TODO: Set limits for variables and brk
    # TODO: Accept numeric input for col
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
    bins <- length(brk) - 1
    mapstates <- .getUniqueStateNames(map)
    dt <- dt[c(state.col, val.col)]
    dt$cats <- cut(dt[[val.col]], brk, include.lowest = TRUE)
    dt$ind <- findInterval(dt[[val.col]], brk, all.inside = TRUE)
    cr <- RColorBrewer::brewer.pal(bins, pal)
    dt$color <- cr[dt$ind]
    newind <- order(dt[[state.col]], mapstates)
    dt <- dt[newind,]
    list(colors = dt$color,
         scheme = cr,
         bins = levels(dt$cats))
  }




.getUniqueStateNames <- function(map)
{
  stopifnot(inherits(map, 'map'))
  unique(sub("(^.+)(:.+$)", "\\1", map$names))
}





#' @export
.shpLayer <- "nga_admbnda_adm1_osgof_20161215"