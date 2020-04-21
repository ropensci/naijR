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
#' @importFrom graphics par
#' @importFrom maps map
#' 
#' @param state A character vector of a list of Nigerian States to be displayed.
#' The default value is to print all States.
#' @param style The type of map to be drawn. Current options are \code{plain}
#' and \code{choropleth}.
#' @param var A variable to be plotted in a map (applicable only to 
#' \code{choropleth})
#' @param breaks Categories to be plotted on a map (applicable only 
#' to \code{choropleth}) 
#' @param show.neighbours logical; \code{TRUE} to display borders of
#' neighbouring countries.
#' @param ... Further arguments for function \code{\link[maps]{map}}
#' 
#' @return An object of class \code{map}, invisibly; as a side-effect,
#' results in the drawing of a map of Nigeria.
#'
#' @examples
#' map_ng() # Draw a map with default settings
#' 
#' ## Choropleth
#' set.seed(4)
#' vals <- sample(0:6, 37, TRUE)
#' brk <- seq(0, 6, 2)
#' map_ng('choropleth', vals, brk)
#'
#' @return An object of class \code{maps} containing the data used to draw the
#' map and which can be used for additional calls to \code{\link[maps]{map}} or
#' other similar functions (e.g. \code{\link[graphics]{plot}}).
#'
#' @export
map_ng <- function(state = character(),
                   style = c("plain", 'choropleth'),
                   var = NULL,
                   breaks = NULL,
                   show.neighbours = FALSE,
                   ...)
{
  # TODO: Add an argument for labelling the States
  all.st <- states(with.fct = TRUE)
  if (length(state) == 0L && !is.null(state))
    state <- all.st
  if (!any(state %in% all.st))
    stop("One or more elements of 'state' is not a Nigerian state")
  if (is.null(style))
    stop("Invalid input for 'style'.")
  style <- match.arg(style)
  stopifnot(is.logical(show.neighbours))
  if (show.neighbours)
    warning("Display of neighbouring countries is disabled")
  dt <- .getMapData()
  
  dots <- list(...)
  plot <- TRUE
  if ('plot' %in% names(dots))
    plot <- dots[['plot']]
  fill <- FALSE
  col <- 1L
  if (style == 'choropleth') {
    if (is.null(var) || is.null(breaks))
      stop("'var' and 'breaks' must be supplied to plot chropleth maps")
    fill <- TRUE
    colobj <- .prepareChoroplethColors(var, breaks, ...)
    col <- colobj$colors
  }
  else if (style == 'plain' && 'fill' %in% names(dots)) {
    fill <- dots[['fill']]
  }
  m <- map(
    dt,
    region = state,
    plot = plot,
    fill = fill,
    col = col
  )
  if (plot && style == 'choropleth')
    legend(
      x = 12,
      y = 5,
      legend = colobj$bins,
      fill = colobj$scheme,
      xpd = NA
    )
  invisible(m)
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
  rgdal::readOGR(dsn = dsn,
                 layer = .shpLayer,
                 verbose = FALSE) %>%
    SpatialPolygons2map(namefield = 'admin1Name')
}








#' @import magrittr
#' @importFrom RColorBrewer brewer.pal
#' @importFrom tools toTitleCase
.prepareChoroplethColors <- 
  function(x, brk, col = c("grey", "red", "green", "blue"))
  {
    stopifnot(is.atomic(x), is.atomic(brk))
    if (is.array(x))
      stop("Expected dim(x) to evaluate to NULL")
    # TODO: Set limits for variables and brk
    col <- match.arg(col)  # TODO: Accept numeric input
    pal <- col %>% toTitleCase %>% paste0("s")
    bins <- length(brk) - 1
    cr <- RColorBrewer::brewer.pal(bins, pal)
    ind <- findInterval(x, brk, all.inside = TRUE)
    list(colors = cr[ind], scheme = cr, bins = levels(cut(x, brk)))
  }






#' @export
.shpLayer <- "nga_admbnda_adm1_osgof_20161215"