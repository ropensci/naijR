# Source file: asreg.R
#
# Copyright (C) 2019-2023 Victor Ordu.

# Functions for coercion ----
#

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



#' @importFrom cli cli_abort
.assert_if_coercible <- function(obj)
{
  if (is.factor(obj))
    obj <- as.character(obj)
  
  if (!is.character(obj))
    cli_abort("Expected a character vector")
  
  if (length(obj) > 1L)
    cli_abort("To coerce a region with synonyms, use a vector of length 1L")
  
  if (!obj %in% lgas_like_states())
    cli_abort("The object does not possess State/LGA synonyms")
  
  if (inherits(obj, "regions")) {
    obj <- unclass(obj)
    cli::cli_warn("Object was stripped down to mode 'character'")
  }
  
  obj
}




## TODO: Export in next MINOR release.
# Disambiguate Synonymous States and LGAs
# 
# Some LGAs in Nigeria bear the name of the States to which they belong to.
# This function will apply an attribute to such an LGA to distinguish it from
# its State.
# 
# @param x A character vector or \code{region} object of length 1L.
# @param parent The name of the State to which the LGA is to belong to.
# @param ... Arguments to be passed to \link{\code{[utils]{menu}}.
# 
# @details For \code{parent}, if it is not provided by the user, an interactive
# prompt will be presented to the user to select the appropriate state - but 
# only in interactive sessions; if run as a batch command, this function
# will signal an error.
#
# @importFrom utils menu
#' @importFrom cli cli_abort
# @export
disambiguate_lga <- function(x, parent = NULL, ...)
{
  if (!is.character(x))
    cli::cli_abort("{sQuote(deparse(quote(x)))} must be a character vector")
  
  if (length(x) > 1L)
    cli_abort("Disambiguation is done only for single-length objects")
  
  if (!is_lga(x))
    cli_abort("Expected an object of class 'lgas'")
  
  ss <- attr(x, "State")
  
  if (is.null(parent)) {
    
    if (!interactive())
      cli_abort("Disambiguation can only be done in interactive mode")
    
    title <- sprintf("Which State does the LGA '%s' belong to?", x)
    parent <-
      ss[menu(ss, graphics = .Platform$OS.type == 'windows', title = title)]
  }
  
  attr(x, "State") <- parent
  x
}
