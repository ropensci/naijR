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
#' @param show.neighbours logical; \code{TRUE} to display borders of
#' neighbouring countries.
#' @param mar Plot margins as in \code{\link[graphics]{par}}
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
map_ng <-
  function(show.neighbours = FALSE,
           mar = c(2.1, 2.1, par('mar')[3], 0.1),
           ...)
  {
    stopifnot(is.logical(show.neighbours))
    if (show.neighbours)
      warning("Display of neighbouring countries is disabled")
    dt <- .getMapData()
    map(dt, mar = mar, ...)
  }



#' @importFrom maps SpatialPolygons2map
#' @importFrom rgdal readOGR
.getMapData <- function()
{
  dsn <- system.file("extdata/ng_admin", package = 'naijR', mustWork = TRUE)
  if (identical(dsn, character(1)))
    stop("The map data could not be found in 'extdata'")
  rgdal::readOGR(dsn = dsn, layer = .shpLayer, verbose = FALSE) %>%
    SpatialPolygons2map(namefield = 'admin1Name')
}


#' Choropleth Maps of Nigeria

#' @import graphics
#' @import magrittr
#' @importFrom RColorBrewer brewer.pal
#' @importFrom tools toTitleCase

#' @param x A variable to be mapped
#' @param breaks Numeric vector > 3 and < 10 representing the limits of the
#' categories for variable \code{x}.
#' @param base.col The base colour upon which the colour scale is based
#' @param ... Arguments to be passed to \code{\link[maps]{map}}
## TODO: Change function name
choropleth_ng <- function(x, 
                          breaks, 
                          base.col = c("red", "grey", "green", "blue"),
                          ...) 
{
  # TODO: Validate `x` and `breaks`
  stopifnot(is.character(base.col))  # TODO: Accept numeric input
  base.col <- match.arg(base.col)
  bc <- base.col %>% 
    extract(1) %>% 
    toTitleCase %>% 
    paste0("s")
  clrs <- breaks %>%
    length %>%
    subtract(1) %>%
    brewer.pal(bc) %>%
    `[`(findInterval(x, breaks))
  myMar <- c(mar = c(0.1, 1.1, 2.1, 4.1))
  dots <- list(...)
  plotting <- TRUE
  if ('plot' %in% names(dots))
    plotting <- dots[['plot']]
  m <- map_ng(mar = myMar, plot = FALSE) %>% 
    map(mar = myMar, col = clrs, fill = TRUE, ...)
  if (plotting)
    legend("bottomright", legend = levels(cut(x, breaks)), fill = clrs)
  par(mar = myMar)
  invisible(m)
}



#' @export
.shpLayer <- "nga_admbnda_adm1_osgof_20161215"