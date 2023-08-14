# Source file: asregint.R
#
# GPL-3 License
#
# Copyright (C) 2019-2023 Victor Ordu.

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