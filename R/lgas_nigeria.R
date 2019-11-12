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
#' State in the Federation of Nigeria. Default is \code{NULL}, in which case
#' all the countries Local Government Areas will be returned.
#'
#' @return A character vector containing the names of Local Government Areas.
#'
#' @export
lgas_ng <- function(ng.state = NULL) {
  retlga <- function(s)
    with(lgas_nigeria, lga[state %in% s])
  
  if (!is.null(ng.state)) {
    stopifnot(all(ng.state %in% naijR::states()))
    lst <- lapply(ng.state, retlga)
    if (length(ng.state) == 1L)
      lst <- unlist(lst)
    return(lst)
  }
  lgas_nigeria$lga
}
