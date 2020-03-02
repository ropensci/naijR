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
#' @importFrom maps map.text
#'
#' @examples
#' map_ng() # Draw a map with default settings
#' map_ng(show = TRUE) # Display portions of neighbouring countries' borders
#' 
#' @export
map_ng <- function(show.neighbours = FALSE, ...)
{
  stopifnot(is.logical(show.neighbours), !is.na(show.neighbours))
  db <- 'mapdata::worldHires'
  mp <- map(db, "Nigeria", fill = TRUE, ...)
  if (show.neighbours) 
    mp <- .addToCurrentMap(db, fill = TRUE, ...)
  map.text(mp, ...)
  invisible(mp)
}




#' @importFrom maps map
.addToCurrentMap <- function(database, neighbours, ...)
{
  maps::map(database, c("Cameroon", "Chad", "Niger", "Benin"), add = TRUE, ...)
}