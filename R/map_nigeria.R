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
#' A basic map of the Federal Republic of Nigeria.
#' 
#' @details This function is essentially a wrapper to \code{maps::map}.
#'
#' @return An object of class \code{map}, invisibly; as a side-effect,
#' results in the drawing of a map of Nigeria.
#' 
#' @param style The type of map to be drawn. Current options are \code{basic}
#' and \code{choropleth}.
#' @param var A variable to be plotted in a map (applicable only to 
#' \code{choropleth})
#' @param breaks Categories to be plotted on a map (applicable only 
#' to \code{choropleth}) 
#' @param show.neighbours logical; \code{TRUE} to display borders of
#' neighbouring countries.
#' @param ... Further arguments for function \code{\link[maps]{map}}
#' 
#' @importFrom graphics par
#' @importFrom maps map
#'
#' @examples
#' map_ng() # Draw a map with default settings
#'
#' @return An object of class \code{maps} containing the data used to draw the
#' map and which can be used for additional calls to \code{\link[maps]{map}}.
#'
#' @export
map_ng <- function(style = c("basic", 'choropleth'),
                   var = NULL,
                   breaks = NULL, 
                   show.neighbours = FALSE, ...)
{
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
    fill <- TRUE
    colobj <- .prepareChoroplethColors(var, breaks, ...)
    col <- colobj$colors
  }
  else if (style == 'basic' && 'fill' %in% names(dots)) {
    fill <- dots[['fill']]
  }
  m <- map(dt, plot = plot, fill = fill, col = col)
  # if (plot && style == 'choropleth')
  #   legend("bottomleft", legend = colobj$bins, fill = colobj$scheme)
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
    # TODO: Validate `x` and `brk`
    stopifnot(is.atomic(x), is.atomic(brk))
    # TODO: Set limits for variables and brk
    col <- match.arg(col)  # TODO: Accept numeric input
    pal <- col %>% toTitleCase %>% paste0("s")
    bins <- length(brk - 1)
    cr <- RColorBrewer::brewer.pal(bins, pal)
    ind <- findInterval(x, brk, all.inside = TRUE)
    list(colors = cr[ind], scheme = cr, bins = levels(cut(x, brk)))
  }






#' @export
.shpLayer <- "nga_admbnda_adm1_osgof_20161215"