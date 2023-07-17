# Source file: map-helpers.R
#
# GPL-3 License
#
# Copyright (C) 2019-2022 Victor Ordu.
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
stateList <- function()
{
  data("states_nigeria", package = "naijR", envir = environment())
  with(states_nigeria, split(state, gpz))
}




# States that are also the names of LGAs
.lgas_like_states <- function()
{
  c("Bauchi",
    "Ebonyi",
    "Ekiti",
    "Gombe",
    "Katsina",
    "Kogi",
    "Nasarawa")
}




# Extracts an element of the ShapefileProps internal object by name
# @param regiontype A character vector of length 1 stating the type of region
# @param element A character vector of length 1 naming the element extracted
.get_shpfileprop_element <- function(region, element)
{
  stopifnot(inherits(region, "regions"), length(element) == 1L)
  suff <- sub("(.)(s$)", "\\1", class(region)[1])
  shpfileprop <- paste("shp", suff, sep = ".")
  getElement(object = get(shpfileprop), name = element)
}





## Messages -----------------------------------------------------------------
country_name <- function()
{
  "Nigeria"
}




.arg_str <- function(arg)
{
  deparse(substitute(arg))
}




.first_elem_warn <- function(arg)
{
  stopifnot(exprs = {is.character(arg) && length(arg) == 1L})
  sprintf("Only the first element of '%s' was used", arg)
}




.next_minor_version <- function()
{
  values <- unlist(packageVersion("naijR"))
  nextver <- paste(values[1], values[2] + 1, "0", sep = ".")
  numeric_version(nextver)
}




.deprec_msg <- function(arg) {
  sprintf("map_ng(%s = )", deparse(substitute(arg)))
}
