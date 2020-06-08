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



#' Display States of the Federal Republic of Nigeria
#' 
#' @param gpz Geopolitical zone. Default is \code{NULL}; optionally \code{"nc",
#'  "ne", "nw", "se", "ss"} and \code{"sw"} (see \code{Details}).
#' @param all logical; whether to include FCT in the result
#' 
#' @return The States of Nigeria as a whole or by zones, as a character vector
#' 
#' @details gpz A geo-political zone, in the Nigerian 
#' context, is a national subdivision that groups contiguous states. 
#' Historically, they arise from subnational administrative divisions 
#' known as 'regions' that existed at the time of the country's independence.
#' There are 6 zones - North-Central, North-East, North-West, South-East,
#' South-South and South-West.
#' 
#' @examples
#' states()  # lists names of all States
#' states("se")  # lists States in South-East zone
#' @export
states <- function(gpz = NULL, all = TRUE)
{
  stopifnot(is.logical(all))
  stl <- .getStateList()
  if (!all)
    stl$fct <- NULL
  if (!is.null(gpz)) {
    if (!is.character(gpz))
      stop("argument supplied 'gpz' is not of type 'character'")
    gpz <- tolower(gsub("\\s+", "", gpz))
    x <- match.arg(gpz, names(stl), several.ok = TRUE)
    stl <- stl[x]
  }
  ss <- as.vector(unlist(stl), mode = 'character')
  if (is.null(gpz))
    ss <- sort(ss)
  ss
}



#' @importFrom stats setNames
.getStateList <- function()
{
  nm <- make.names(c('nc', 'ne', 'nw', 'se', 'ss', 'sw', 'fct'))
  setNames(..LL, nm)
}


..LL <- list(
  c("Benue", "Kogi", "Kwara", "Nasarawa", "Niger", "Plateau"),
  c("Adamawa", "Bauchi", "Borno", "Gombe", "Taraba", "Yobe"),
  c("Jigawa", "Kaduna", "Kano", "Katsina", "Kebbi", "Sokoto", "Zamfara"),
  c("Abia", "Anambra", "Ebonyi", "Enugu", "Imo"),
  c("Akwa Ibom", "Bayelsa", "Cross River", "Delta", "Edo", "Rivers"),
  c("Ekiti", "Lagos", "Ogun", "Ondo", "Osun", "Oyo"),
  "Federal Capital Territory"
)




#' Test an Object for States
#' 
#' @param x A character vector to be tested.
#' @param test The type of test to be carried out - on the vector as a whole 
#' i.e. \code{all} (the default argument) or on the individual elements i.e.
#' \code{selected}.
#' @param allow.na logical. If \code{TRUE}, all \code{NA}s are ignored in
#' the result.
#' 
#' @import stats
#'
#' @return A logical vector.
#' @export
is_state <- function(x, test = c("all", "selected"), allow.na = TRUE)
{
  if (!is.character(x))
    return(FALSE)
  test <- match.arg(test)
  na.pos <- 0L
  if (anyNA(x) && allow.na) {
    exc <- stats::na.exclude(x)
    if (test == 'selected')
      na.pos <- stats::na.action(exc)
    else
      x <- exc
  }
  x <- sub("^FCT$", "Federal Capital Territory", x)
  val <- x %in% unlist(..LL)
  if (test == 'all')
    return(all(val))
  val[na.pos] <- NA
  val
}




# is_lga <- function(x) {}
# is_ward <- function(x) {}

# abbr_state <- function(x) {} # abbreviate State names

# population <- function(x) {}
# language <- function(x) {}