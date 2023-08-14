# Source file: asreg.R
#
# GPL-3 License
#
# Copyright (C) 2019-2023 Victor Ordu.

# Functions for coercion ----

#' Explicit coercion between State and LGA names
#' 
#' Takes the names of either States or LGAs and converts them explicitly into
#' objects of the other class.
#' 
#' @details There are a few LGAs in the country that bear the same name
#' as their State, and this could create some confusion when trying to use
#' some of the the functionalities of this package. The States/LGAs in question 
#' are \emph{Bauchi, Ebonyi, Ekity, Gombe, Katsina and Kogi}.
#' 
#' There as subtle differences in the way these functions handle data
#' for States as against those for LGAs. In the case of States, an object of
#' mode \code{character} is the preferred argument; alternatively, an object 
#' of class \code{states} will serve as long as it has only one element. For 
#' LGAs, the string is the preferred argument, since an object constructed
#' with \code{lgas()} that is supplied a State's name as argument will
#' list all the LGAs in that State. If a pre-formed \code{lgas} object is to be
#' coerced to a \code{states} object, it should first be \code{unclass}ed or 
#' explicitly coerced with \code{as.character}.
#' 
#' @rdname coerce
#' 
#' @param x A string representing either States or Local Government Areas 
#' (LGAs) that dually name one of these administrative regions.
#' 
#' @return In the case of \code{as_state}, an object of class \code{states}; 
#' with \code{as_lga}, an object of class \code{lgas}.
#' 
#' @examples 
#' kt.st <- states("Katsina")  # Ensure this is a State, not an LGA.
#' kt.lg <- suppressWarnings(as_lga(kt.st))
#' is_state(kt.st)             # TRUE
#' is_lga(kt.lg)               # TRUE
#' 
#' ## Where there's no ambiguity, it doesn't make sense to coerce
#' ## This kind of operation ends with an error
#' \dontrun{
#' as_state("Kano")
#' as_lga("Michika")
#' }
#' 
#' 
#' @export
as_state <- function(x)
{
  states(.assert_if_coercible(x))
}




#' @rdname coerce
#' 
#' @export
as_lga <- function(x) {
  new_lgas(.assert_if_coercible(x))
}
