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
#' @param ... Further arguments for function \code{\link[maps]{map}}
#'
#' @import mapdata
#' @importFrom maps map
#'
#' @examples
#' map_ng() # Draw a map with default settings
#' map_ng(show = TRUE) # Display portions of neighbouring countries' borders
#'
#' @return An object of class \code{maps} containing the data used to draw the
#' map and which can be used for additional calls to \code{\link[maps]{map}}.
#'
#' @export
map_ng <- function(show.neighbours = FALSE, ...)
{
  stopifnot(is.logical(show.neighbours), !is.na(show.neighbours))
  db <- 'mapdata::worldHires'
  m <- map(db, "Nigeria", ...)
  if (show.neighbours)
    m <-
    map(db, c("Cameroon", "Chad", "Niger", "Benin"), add = TRUE, ...)
  invisible(m)
}


#' Choropleth Maps of Nigeria
#'
#' @import graphics
#' @import sp
#' @importFrom RColorBrewer brewer.pal
#' @importFrom rgdal readOGR
#' @importFrom tools toTitleCase
#'
#' @param x A variable to be mapped
#' @param breaks Numeric vector > 3 and < 10 representing the limits of the
#' categories for variable \code{x}.
#' @param base.col The base colour upon which the colour scale is based
#' @param ... Arguments to be passed to \code{\link[graphics]{plot}}
#'
#' @export
## TODO: Change function name
cmap <- function(x, breaks, base.col = character(), ...) {
  # TODO: Validate x and breaks
  stopifnot(is.character(base.col))
  if (missing(base.col))
    base.col <- 'red'
  scl <- c("red", "purple", "orange", "grey", "green", "blue")
  bc <- base.col[[1]]
  if (!tolower(bc) %in% scl)
    stop("The selected colour scale is not supported")
  op <- par()
  par(mar = c(1, 0.1, 2, 0.1))
  on.exit(suppressWarnings(par(op)))  # TODO: Review approach to warnings
  .dir <- system.file("extdata/ng_admin", package = 'naijR', mustWork = TRUE)
  if (identical(.dir, character(1)))
    stop("The map data could not be found in 'inst/extdata'")
  m <-
    rgdal::readOGR(dsn = .dir,
                   layer = "nga_admbnda_adm1_osgof_20161215",
                   verbose = FALSE)
  clr <- RColorBrewer::brewer.pal(length(breaks) - 1,
                                  paste0(tools::toTitleCase(bc), "s"))
  clr <- clr[findInterval(x, breaks)]
  plot(m, col = clr, ...)
  legend("bottomright", legend = levels(cut(x, breaks)), fill = clr)
}
