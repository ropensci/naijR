# Copyright (C) 2019-2021 Victor Ordu.
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

globalVariables(c("lgas_nigeria", "state", "lga"))

#' States of the Federal Republic of Nigeria
#' 
#' @param states One or more States of the Federation
#' @param gpz Geopolitical zone. Default is \code{NULL}; optionally \code{"nc",
#'  "ne", "nw", "se", "ss"} and \code{"sw"} (see "Details").
#' @param all logical; whether to include the Federal Capital Territory in 
#' the result
#' 
#' @return The States of Nigeria as a whole or by zones, as an S3 object 
#' of class \code{states}.
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
#' states(gpz = "se")  # lists States in South-East zone
#' @export
states <- function(states, gpz = NULL, all = TRUE)
{
  stopifnot(is.logical(all))
  if (!missing(states) && is.character(states)) {
    if (!all(is_state(states)))
      warning("One or more elements of 'states' is not an actual State",
              call. = FALSE)
    return(new_states(states))
  }
  stl <- .getAllStates()
  if (!all)
    stl$fct <- NULL
  if (!is.null(gpz) && missing(states)) {
    if (!is.character(gpz))
      stop("argument supplied 'gpz' is not of type 'character'")
    gpz <- tolower(gsub("\\s+", "", gpz))
    x <- match.arg(gpz, names(stl), several.ok = TRUE)
    stl <- stl[x]
  }
  ss <- as.vector(unlist(stl), mode = 'character')
  if (is.null(gpz))
    ss <- sort(ss)
  new_states(ss)
}







## Low-level constructor
new_states <- function(ss) 
{
  structure(ss, class = c("states", "regions", class(ss)))
}












#' List Local Government Areas
#'
#' @param region Character; State(s) in the Federation of Nigeria. Default is
#' \code{NA_character_}.
#' 
#' @return If length of \code{ng.state} == 1L, a character vector containing 
#' the names of Local Government Areas; otherwise a named list whose elements 
#' are character vectors of the LGAs in each state.
#' 
#' @examples
#' how_many_lgas <- function(state) {
#'   require(naijR)
#'   stopifnot(all(is_state(state)))
#'   cat(sprintf("No. of LGAs in %s State:", state),
#'     length(lgas(state)),
#'     fill = TRUE)
#' }
#' how_many_lgas("Sokoto")
#' how_many_lgas("Ekiti")
#'
#' @export
lgas <- function(region = NA_character_) {
  if (!is.character(region))
    stop("Expected an object of type 'character'")
  if (length(region) == 1L && is.na(region))
    return(new_lgas(lgas_nigeria$lga))
  lst <- if (all(is_state(region))) {
    sl <- lapply(region, function(s)
      subset(
        lgas_nigeria,
        state %in% s,
        select = lga,  # TODO: Refactor
        drop = TRUE
      ))
    names(sl) <- region
    if (length(region) == 1L)
      sl <- unname(unlist(sl))
    sl
  }
  else if (any(areLgas <- is_lga(region))) {
    if (!all(areLgas))
      warning("One or more elements is not an LGA")
    region
  }
  else  # TODO: remove this condition?
    stop("One or more elements is not a valid region in Nigeria")
  
  new_lgas(lst)
}








# Low-level S3 constructor for lgas object
new_lgas <- function(x)
{
  structure(x, class = c("lgas", "regions", class(x)))
}


#' @rdname lgas
#' @param state Character; State(s) in the Federation of Nigeria. Default is
#' \code{NA_character_}.
#' 
#' @note \code{lga_ng} stands deprecated and will be removed in the next minor
#' version. New code should use \code{lgas} instead.
#' 
#' @export
lgas_ng <- function(state = NA_character_) {
  .Deprecated("lgas")
  as.character(lgas(region = state))
}