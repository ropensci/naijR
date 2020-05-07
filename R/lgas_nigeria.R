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
#' @param ng.state Character vector of length 1, whose value is the name of a
#' State in the Federation of Nigeria. Default is \code{NA_character_}, in 
#' which case all the countrys '774 Local Government Areas will be returned.
#' 
#' @return A character vector containing the names of Local Government Areas.
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

#' @export
lgas_ng <- function(ng.state = NA_character_) {
  stopifnot(is.character(ng.state))
  if (!all(is.na(ng.state))) {
    if (!is_state(ng.state))
      stop("One or more elements of 'ng.state' is not a State in Nigeria")
    lst <- lapply(
      ng.state,
      FUN = function(s) {
        subset(lgas_nigeria, state %in% s, lga, TRUE)
      }
    )
    names(lst) <- ng.state
    if (length(ng.state) == 1L)
      lst <- unname(unlist(lst))
    return(lst)
  }
  lgas_nigeria$lga
}
