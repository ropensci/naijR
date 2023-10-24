# Source file: helpers.R
#
# GPL-3 License
#
# Copyright (C) 2019-2023 Victor Ordu.
#
#
globalVariables("states_nigeria")

# Creates a list whose elements are the States
# by their respective geo-political zones. 
# The name of each elements is an abbreviated
# form of the name of its zone - North-Central,
# North-East, North-West, South-East, South-South
# and South-West. The Federal Capital Territory, 
# which doesn't belong to any zone is denoted
# by its own abbreviation and its element is of
# length 1L.




## Returns those LGAs that share names with their State or, in other words,
## States that are also the names of LGAs e.g. Bauchi, Ekiti
lgas_like_states <- function()
{
  ll <- unclass(lgas())
  statelike <- which(is_state(ll))
  unique(ll[statelike])
}





## Messages -----------------------------------------------------------------
country_name <- function()
{
  "Nigeria"
}




arg_str <- function(arg)
{
  deparse(substitute(arg))
}




first_elem_warn <- function(arg)
{
  stopifnot(exprs = {is.character(arg) && length(arg) == 1L})
  sprintf("Only the first element of '%s' was used", arg)
}




.next_minor_version <- function()
{
  values <- unlist(utils::packageVersion("naijR"))
  nextver <- paste(values[1], values[2] + 1, "0", sep = ".")
  numeric_version(nextver)
}




.deprec_msg <- function(arg) {
  sprintf("map_ng(%s = )", deparse(substitute(arg)))
}




# Checks whether a logical argument is correctly passed
assert.lgl.arg <- function(arg) {
  argname <- deparse(substitute(arg))
  
  if (!is.logical(arg) || is.na(arg))
    cli_abort("'{argname}' should be TRUE/FALSE")
}
