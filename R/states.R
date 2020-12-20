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



#' States of the Federal Republic of Nigeria
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
  stl <- .getAllStates()
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




.getAllStates <- function(named = TRUE)
{
  names <- if (named)
    c('nc', 'ne', 'nw', 'se', 'ss', 'sw', 'fct')
  ss <- structure(list(
    c("Benue", "Kogi", "Kwara", "Nasarawa", "Niger", "Plateau"),
    c("Adamawa", "Bauchi", "Borno", "Gombe", "Taraba", "Yobe"),
    c("Jigawa", "Kaduna", "Kano", "Katsina", "Kebbi", "Sokoto", "Zamfara"),
    c("Abia", "Anambra", "Ebonyi", "Enugu", "Imo"),
    c("Akwa Ibom", "Bayelsa", "Cross River", "Delta", "Edo", "Rivers"),
    c("Ekiti", "Lagos", "Ogun", "Ondo", "Osun", "Oyo"),
    c("Federal Capital Territory")
  ),
  names = names)
  if (!named)
    return(unlist(ss))
  ss
}






#' Test an Object for States
#' 
#' @param x A vector to be tested.
#' 
#' @importFrom magrittr %>%
#' @import stats
#' 
#' @details An element-wise check of a supplied vector is carried out. To
#' test an entire vector and return a single boolean value, functions 
#' such as \code{base::all} or \code{base::any} should be used.
#' 
#' @note The function throws a warning, when a missing value is among the 
#' elements. It works only for atomic vectors, throwing an error when this 
#' is not the case or when \code{NULL} is passed to it.
#'
#' @return A logical vector.of same length as the input. If the input object is
#' not even of type \code{character}, return the object unaltered, with a
#' warning.
#' 
#' @examples 
#' all(is_state(naijR::states()))
#' is_state(c("Maryland", "Baden-Baden", "Plateau", "Sussex"))
#' 
#' @export
is_state <- function(x)
{
  if (!is.atomic(x) || is.null(x)) # as is.atomic(NULL) == TRUE
    stop("Expected a non-null atomic vector as input", call. = FALSE)
  
  ## Return the object rather than stop execution for this condition.
  ## This is to enable unhindered traversal when this function
  ## is applied across an object.
  if (!is.character(x)) {
    warning(sQuote(x), " is not a character vector. Nothing done")
    return(x)
  }
  
  na.pos <- 0L
  if (anyNA(x)) {
    warning("Invalid entries were replaced with NAs", call. = FALSE)
    excl <- stats::na.exclude(x)
    na.pos <- stats::na.action(excl)
  }
  
  sub("^FCT$", "Federal Capital Territory", x) %>%
    `%in%`(.getAllStates(named = FALSE)) %>%
    {
      .[na.pos] <- NA
      .
    }
}






#' Fix State Names
#' 
#' Correct any misspelt names of States.
#' 
#' @details The function will look through a character vector and try to 
#' determine if State names have been wrongly entered. This presupposes that
#' the atomic vector is of type \code{character}. It does not test any missing
#' values in the vector, leaving them untouched.
#' 
#' @param x A character vector.
#' @param ... This argument is placed for possible use in the near future.
#' 
#' @note An updated version would include the ability to adjust the 
#' \href{https://en.wikipedia.org/wiki/Levenshtein_distance}{Levenshtein 
#' distance}, which will empower users to tune the function's sensitivity.
#' 
#' @importFrom rlang abort
#' @importFrom rlang warn
#' 
#' @export
fix_state <- function(x, ...)
{
  if(!inherits(x, 'character'))
    abort("'x' is not an object of class 'character'")
  if (all(is.na(x))) {
    warn("'x' has only missing values")
    return(x)
  }
  
  ## Approximately matches regex on list of states
  .getProperVal <- function(str, states) {
    if (!is.na(match(str, states)))
      return(str)
    if (agrepl(str, abbr, max.distance = 2) && 
        identical(toupper(str), abbr))
      return(abbr)
    
    # First check for matching pattern
    matched <-
      grep(paste0('^', str, '$'),
           states,
           value = TRUE,
           ignore.case = TRUE)
    if (length(matched) == 0L) {
      # Then check for approximate matches
      matched <- agrep(str, states, value = TRUE, max.distance = 1)
      if (length(matched) != 1L)
        return(NA_character_)
    }
    matched
  }
  
  ## Process possible FCT values
  abbr <- "FCT"
  full <- "Federal Capital Territory"
  hasFct <- c(abbr, full) %in% x
  if (sum(hasFct) == 2)
    x <- sub(abbr, full, x)
  if (sum(hasFct) == 1) {
    i_abbr <- grep(abbr, x)   # Huh?
    i_full <- grep(full, x)
  }
  i <- grep(paste0("^", abbr, "$"), x, ignore.case = TRUE)
  ss <- states()
  if (length(i) != 0) 
    ss <- sub(full, abbr, states())
  
  x <- vapply(x, .getProperVal, character(1), states = ss, USE.NAMES = FALSE)
  x[i] <- full
  x
}





# Toggle between full and abbreviated FCT name
#' @importFrom rlang abort
.toggleFct <- function(x)
{
  if (length(x) != 1L)
    abort("Expected a vector of length == 1L")
  if (!is.character(x))
    abort("'x' is not a character vector",)
  
  opts <- c("FCT", "Federal Capital Territory")
  if (!x %in% opts)
    abort("Invalid input")
  ind <- ifelse(match(x, opts) == 1L, 2L, 1L)
  opts[ind]
}
