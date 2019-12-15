# Copyright (C) 2019 DevSolutions Ltd.
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

globalVariables("lgas_nigeria")

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
#' @param num Logical; return the number of LGAs. Is only applicable in the
#' case where \code{length(ng.state) == 1L}.
#' 
#'
#' @return A character vector containing the names of Local Government Areas.
#'
#' @export
lgas_ng <- function(ng.state = NA_character_, num = FALSE) {
  stopifnot(is.character(ng.state))
  if (!is.logical(num))
    stop("argument 'num' can only be a logical 'T/F' value")
  if (!all(is.na(ng.state))) {
    if (isFALSE(all(ng.state %in% states())))
      stop("One or more elements of 'ng.state' is not a State in Nigeria")
    lst <- sapply(
      ng.state,
      USE.NAMES = T,
      simplify = F,
      FUN = function(s)
        with(lgas_nigeria, lga[state %in% s])
    )
    if (length(ng.state) == 1L) {
      lst <- unname(unlist(lst))
      if (num)
        return(length(lst))
    }
    return(lst)
  }
  lgas_nigeria$lga
}
