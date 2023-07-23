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
#' Disambiguate Synonymous States and LGAs
#' 
#' Some LGAs in Nigeria bear the name of the States to which they belong to.
#' This function will apply an attribute to such an LGA to distinguish it from
#' its State.
#' 
#' @details For \code{state}, if it is not provided by the user, an interactive
#' prompt will be presented to the user to select the appropriate state - but 
#' only in interactive sessions; if run as a batch command, this function
#' will signal an error.
#'
#' @param lga An object of class \code{lgas} of length \code{1L}.
#' @param state The name of the State to which the LGA is to belong to.
#' @param ... Arguments to be passed to \code{\link[utils]{menu}}.
#' 
#' @return The object of class \code{lgas} with the (possibly) modified 
#' \code{State} attribute.
#' 
#' @importFrom cli cli_abort
#' 
#' @export
#' 
#' @examples
#' obi.lga <- lgas("Obi")    # Warning
#' try(map_ng(obi.lga))      # Error
#' 
#' obi.benue <- disambiguate_lga(obi.lga, "Benue")
#' map_ng(obi.benue)
#' 
disambiguate_lga <- function(lga, state = NULL, ...)
{
  if (!inherits(lga, "lgas"))
    cli_abort("Expected an object of class `lgas`")

  if (length(lga) > 1L)
    cli_abort("Disambiguation is only done for objects with one element")

  lgattr <- attr(lga, "State")

  if (is.null(state)) {
    if (!interactive())
      cli_abort("This operation can only be done in interactive mode")

    if (length(lgattr) > 1L) {
      title <- sprintf("Which State does '%s LGA' belong to?", lga)
      opt <- utils::menu(lgattr, title = title, ...)
      state <- lgattr[opt]
    }
  }
  attr(lga, "State") <- state
  
  if (isFALSE(.lga_state_is_valid(lga)))
    cli_abort("{as.character(lga)} LGA is not in {state} State")
  
  lga
}




## Checks whether the 'State' attribute of and lgas object is actually
## the correct one for that LGA. This function is provided for cases
## where a State attribute is wrongfully enforced on a LGA name. This
## could happen inadvertently when implementing some of these functions.
.lga_state_is_valid <- function(x)
{
  stopifnot(exprs = {
    is_lga(x)
    length(x) == 1L
  })
  lgastr <- as.character(x)
  
  if (length(lgastr) > 1L)
    cli::cli_abort("More than one LGA was provided")
  
  s <- attr(x, "State")
  lga.states <- lgas_nigeria$state[lgas_nigeria$lga == lgastr]
  s %in% lga.states
}