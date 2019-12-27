# Copyright (C) 2019 Victor Ordu.
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
#' @return \code{NULL} invisibly; results in the drawing of a map of Nigeria.
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
#' map_ng(TRUE) # Display portions of neighbouring countries' borders
#' 
#' @export
map_ng <- function(show.neighbours = FALSE, ...)
{
    db <- 'worldHires'
    maps::map(db, "Nigeria", ...)
    if (show.neighbours) jn(db)
}




#' @importFrom maps map
jn <- function(data, ...) 
{
    maps::map(data, c("Cameroon", "Chad", "Niger", "Benin"), add = TRUE, ...)
}