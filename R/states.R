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



#' @import stats
.getStateList <- function()
{
  nm <- make.names(c('nc', 'ne', 'nw', 'se', 'ss', 'sw', 'fct'))
  stats::setNames(..LL, nm)
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
#' @param x A vector to be tested.
#' 
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @import stats
#' 
#' @details An element-wise check of a supplied vector is carried out. To
#' test and entire vector and return a single boolean value, functions 
#' such as \code{base::all} or \code{base::any} should be used.
#' 
#' @note The function throws a warning, when a missing value is among the 
#' elements. It works only for atomic vectors, throwing an error when this 
#' is not the case or when \code{NULL} is passed to it.
#'
#' @return A logical vector.of same length as the input.
#' 
#' @export
is_state <- function(x)
{
  if (!is.atomic(x) || is.null(x)) # is.atomic(NULL) == TRUE
    stop("'x' is not a non-null atomic object")
  if (!is.character(x))
    return(FALSE)
  na.pos <- 0L
  if (anyNA(x)) {
    warning("'x' contains missing values, NA")
    exc <- stats::na.exclude(x)
    na.pos <- stats::na.action(exc)
  }
  
  x %<>%
    sub("^FCT$", "Federal Capital Territory", .) %>%
    `%in%`(unlist(..LL)) %>%
    {
      .[na.pos] <- NA
      .
    }
}




is_lga <- function(x) {}
# is_ward <- function(x) {}

# abbr_state <- function(x) {} # abbreviate State names

# population <- function(x) {}
# language <- function(x) {}