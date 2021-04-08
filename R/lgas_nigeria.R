# Copyright (C) 2019-2021 Victor Ordu.
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

globalVariables(c("lgas_nigeria", "state", "lga"))

#' Local Government Areas of Nigeria
#'
#' A dataset containing the 774 Local Government Areas of Nigeria
#'
#' @format A dataframe with 774 rows and 2 columns
#' \describe{
#'    \item{lga}{Local Government Area}
#'    \item{state}{State of the Federation}
#'    }
"lgas_nigeria"



#' List Local Government Areas
#'
#' @param ng.state Character; State(s) in the Federation of Nigeria. Default is
#' \code{NA_character_}.
#' 
#' @return If length of \code{ng.state} == 1L, a character vector containing 
#' the names of Local Government Areas; otherwise a named list whose elements 
#' are character vectors of the LGAs in each state.
#' 
#' @examples
#' how_many_lgas <- function(state) {
#'   require(naijR)
#'   stopifnot(state %in% states())
#'   cat(sprintf("No. of LGAs in %s State:", state),
#'     length(lgas_ng(state)),
#'     fill = TRUE)
#' }
#' how_many_lgas("Sokoto")
#' how_many_lgas("Ekiti")
#'
#' @export
lgas_ng <- function(region = NA_character_) {
  if (!is.character(region))
    stop("Expected an object of type 'character'")
  if (length(region) == 1L && is.na(region))
    return(new_lgas_ng(lgas_nigeria$lga))
  lst <- if (all(is_state(region))) {
    sl <- lapply(region, function(s)
      subset(
        lgas_nigeria,
        state %in% s,
        select = lga,
        ## TODO: Refactor
        drop = TRUE
      ))
    names(sl) <- region
    if (length(region) == 1L)
      sl <- unname(unlist(sl))
    sl
  }
  else if (all(is_lga(region)))
    region
  else
    stop("One or more elements is not a valid region in Nigeria")
    
  new_lgas_ng(lst)
}


#' Test for Local Government Areas
#' 
#' Checks a given object for Local Government Areas, represented as
#' strings.
#' 
#' @param x An object of type \code{character}. This includes higher
#' dimension objects like matrices and arrays. 
#' 
#' @return A logical vector the same length as the input object. Each
#' element that is not a valid Local Government Area will evaluate to
#' \code{FALSE}.
#' 
#' @export
is_lga <- function(x)
{
  if (!is.character(x))
    stop("x should be of type 'character'")
  x %in% lgas_ng()
}





# Low-level S3 constructor for lgas_ng object
new_lgas_ng <- function(x)
{
  structure(x, class = c("lgas_ng", class(x)))
}