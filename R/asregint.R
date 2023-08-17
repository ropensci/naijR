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