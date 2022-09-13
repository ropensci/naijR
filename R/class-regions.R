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

globalVariables(c("lgas_nigeria", "state", "lga", "gpz"))

#' Create an Object for the States of Nigeria
#' 
#' @param states A character vector with strings representing one or more 
#' States of Nigeria. If missing, the function will return a \code{states} 
#' all the States, with or without the Federal Capital Territory (FCT).
#' @param gpz \code{NULL} (the default) or, case insensitively, one or more of
#' the following strings: \code{"nc", "ne", "nw", "se", "ss"} and \code{"sw"} 
#' (see "Details").
#' @param all logical; whether to include the FCT in the result.
#' @param warn logical; issue a warning when one or more elements are not
#' actually States (i.e. they were misspelt).
#' 
#' @return The States of Nigeria as a whole or by zones, as an S3 object 
#' of class \code{states}.
#' 
#' @details \code{gpz} represents a geopolitical zone which, in the Nigerian 
#' context, is a national subdivision that groups contiguous states that bear
#' certain socio-cultural and political similarities. Historically, they arise
#' from sub-national administrative divisions known as 'Regions' that existed 
#' at the time of the country's independence. There are at present 6 such 
#' zones - North-Central, North-East, North-West, South-East,South-South and 
#' South-West.
#' 
#' @examples
#' states()  # lists names of all States
#' states(gpz = "se")  # lists States in South-East zone
#' 
#' @export
states <- function(states, gpz = NULL, all = TRUE, warn = TRUE)
{
  if (!is.logical(all))
    stop("'all' is not logical")
  
  if (!is.logical(warn))
    stop("'warn' is not logical")
  
  if (!missing(states) && is.character(states)) {
    if (warn && !all(is_state(states)))
      warning(.warnSpelling('state'), call. = FALSE)
    
    return(new_states(states))
  }
  
  stl <- getAllStates()
  
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




## Provides some uniformity in the messaging b/w States & LGAs
.warnSpelling <- function(region.type) {
  region.type <- match.arg(region.type, c("state", "lga"))
  
  txt <- switch(
    region.type, 
    state = "a State", 
    lga = "an LGA"
  )
  
  sprintf("One or more items is not %s. Spelling error?", txt)
}



#' @importFrom magrittr %>%
#' @importFrom magrittr %$%
getAllStates <- function(named = TRUE)
{
  stopifnot(
    length(named) == 1L,
    is.logical(named),
    !is.na(named)
  )
  data("lgas_nigeria", package = "naijR", envir = environment())
  
  zones <- lgas_nigeria$gpz %>% 
    unique %>% 
    sort
  
  ss <- sapply(zones, function(zn) {
    lgas_nigeria %$%
      {
        state[gpz == zn]
      } %>% 
      unique
  })
  
  stopifnot(!is.null(ss))
  
  if (!named)
    return({
      ss %>% 
        unlist %>% 
        unname %>% 
        sort
    })
  
  names(ss) <- sub("\\.state", "", names(ss))
  ss
}






## Low-level constructor
new_states <- function(ss) 
{
  structure(ss, class = c("states", "regions", class(ss)))
}












#' Create on Object for Local Government Areas
#'
#' @param region Context-dependent. Either State(s) of the Federation 
#' or Local Government Area(s) - internal checks are performed to determine
#' what applies. In cases where States are synonymous to LGAs, the default 
#' behaviour is to use the State as a basis for selecting the LGAs. This
#' can be modified with \code{strict}. The default value is 
#' \code{NA_character_} and will return all 774 LGAs.
#' @param strict logical; in the event of a name clash between State/LGA, 
#' return only the specified LGA when this argument is set to \code{TRUE}.
#' @param warn logical; issue a warning when one or more elements are not
#' actually Local Government Areas (or were misspelt).
#' 
#' @note There are six (6) LGAs that share names with their State - Bauchi, 
#' Ebonyi, Gombe, Katsina, Kogi and Ekiti.
#' 
#' @return If length of \code{ng.state} == 1L, a character vector containing 
#' the names of Local Government Areas; otherwise a named list, whose elements 
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
#' @importFrom utils data
#'
#' @export
lgas <- function(region = NA_character_, strict = FALSE, warn = TRUE) {
  data("lgas_nigeria", package = "naijR", envir = environment())
  
  if (is.factor(region))
    region <- as.character(region)
  
  if (!is.character(region))
    stop("Expected an object of type 'character'")
  
  if (strict && !any(region %in% .synonymRegions()))
    stop("strict can only be set to TRUE where State/LGA syonnyms exist")
  
  if (length(region) == 1L && is.na(region))
    return(new_lgas(lgas_nigeria$lga))
  
  lst <- if (all(is_state(region)) && !strict) {
    sl <- lapply(region, function(s)
      subset(
        lgas_nigeria,
        state %in% s,
        select = lga,
        drop = TRUE
      ))
    names(sl) <- region
    
    if (length(region) == 1L)
      sl <- unname(unlist(sl))
    
    sl
  }
  else if (all(is_lga(region))) {
    lg <- region
    region <- unique(lgas_nigeria$state[lgas_nigeria$lga %in% lg])
    
    if ((numSt <- length(region)) > 1L)
      warning(sprintf("The LGA '%s' is found in %i States", lg, numSt))
    
    lg
  }
  else if (.hasMisspeltLgas(region)) { 
    if (warn)
      warning(.warnSpelling('lga'), call. = FALSE)
    
    ret <- region
    region <- as.null(region)  # set to NULL b/c of attribute in final output
    ret
  }
  else if (.hasNonLgaAll(region))
    stop("None of the items is a valid LGA")
  
  structure(new_lgas(lst), State = region)
}


.hasNonLgaMixed <- function(x) {
  stopifnot(is.character(x))
  matches <- .boolPartialLgaMatches(x)
  
  if (.hasNonLgaAll(x))
    return(FALSE)
  
  sum(matches) < length(x)
}

.hasNonLgaAll <- function(x) {
  stopifnot(is.character(x))
  sum(.boolPartialLgaMatches(x)) == 0L
}

.hasMisspeltLgas <- function(x) {
  stopifnot(is.character(x))
  matches <- .boolExactLgaMatches(x)
  
  if (.hasNonLgaAll(x))
    return(FALSE)
  
  sum(matches) < length(x)
}

.boolExactLgaMatches <- function(x) {
  stopifnot(is.character(x))
  grepl(.lgaRegex(x), lgas())
}


.boolPartialLgaMatches <- function(x) {
  stopifnot(is.character(x))
  
  agrepl(.lgaRegex(x),
         lgas(),
         fixed = FALSE,
         max.distance = .pkgLevDistance())
}

.lgaRegex <- function(x) {
  stopifnot(is.character(x))
  paste0("^", paste(x, collapse = "|"), "$")
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







# ----
# Functions for coercion
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
#' @rdname coercion
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
  states(.validateCoercible(x))
}






#' @rdname coercion
#' 
#' @export
as_lga <- function(x) {
  new_lgas(.validateCoercible(x))
}




.validateCoercible <- function(obj)
{
  if (is.factor(obj))
    obj <- as.character(obj)
  
  if (!is.character(obj))
    stop("Expected a character vector")
  
  if (length(obj) > 1L)
    stop("To coerce a region with synonyms, use a vector of length 1L")
  
  if (!obj %in% .synonymRegions())
    stop("The object does not possess State/LGA synonyms")
  
  if (inherits(obj, "regions")) {
    obj <- unclass(obj)
    warning("Object was stripped down to mode 'character'", call. = FALSE)
  }
  obj
}



## Returns those LGAs that share names with their State
## e.g. Bauchi, Ekiti
#' @importFrom magrittr %>%
#' @importFrom magrittr extract
.synonymRegions <- function()
{
  ll <- lgas()
  
  ll %>% 
    is_state %>% 
    which %>% 
    extract(ll, .) %>% 
    unclass %>% 
    unique       # because Nasarawa exists in 2 different States
}







.stateSharedLgas <- function()
{
  list(
    Nasarawa = c("Nasarawa", "Kano"),
    Obi = c("Benue", "Nasarawa"),
    Ifelodun = c("Kwara", "Osun"),
    Irepodun = c("Kwara", "Osun"),
    Surulere = c("Lagos", "Oyo"),
    Bassa = c("Kogi", "Plateau")
  )
}






## Get a vector with both the abbreviated and full versions of the 
## national capital's name, just return one of the two.
.fctOptions <- function(opt = c("all", "full", "abbrev")) {
  opt <- match.arg(opt)
  vec <- c(full = "Federal Capital Territory", abbrev = "FCT")
  
  if (opt != "all")
    return(vec[opt])
  
  vec
}






# TODO: Export this function in next release
# Disambiguate Synonymous States and LGAs
# 
# Some LGAs in Nigeria bear the name of the States to which they belong to.
# This function will apply an attribute to such an LGA to distinguish it from
# its State.
# 
# @param x A character vector or \code{region} object of length 1L.
# @param parent The name of the State to which the LGA is to belong to.
# 
# @details For \code{parent}, if it is not provided by the user, an interactive
# prompt will be presented to the user to select the appropriate state - but 
# only in interactive sessions; if run as a batch command, this functionality
# will signal an error.
#
# @importFrom utils menu
# @export
disambiguate_lga <- function(x, parent = NULL)
{
  if (!is.character(x))
    stop(sQuote(deparse(quote(x))), "must be a character vector")
  
  if (length(x) > 1L)
    stop("Disambiguation is done only for single-length objects")
  
  if (!is_lga(x))
    stop("Expected an object of class 'lgas'")
  
  ss <- attr(x, "State")
  
  if (is.null(parent)) {
    
    if (!interactive())
      stop("Disambiguation can only be done in interactive mode")
    
    title <- sprintf("Which State does the LGA '%s' belong to?", x)
    parent <- ss[menu(ss, graphics = TRUE, title = title)]
  }
  
  attr(x, "State") <- parent
  x
}






# Methods for internal generics ####

## Because 'regions' is an abstract class i.e. it does not have
## a constructor, we have to provide a means of creating the
## states/lgas objects post-method-dispatch. The behaviour we are
## trying to establish is for both States and LGA data, and thus
## it would be redundant to create distinct methods for them.
## Perhaps there might be a cleaner approach, but this is as far
## current skills can go.
.chooseRegionsMethod <- function(m, obj)
{
  if (all(is_state(obj)))
    new_states(m)
  else
    new_lgas(m)
}




#' Print regions
#' 
#' @rdname states
#' 
#' @param x An object of class \code{regions}
#' @param ... Additional arguments, though not set. Left for future use
#' 
#' @export
print.regions <- function(x, ...) {
  if (!interactive())
    return(x)
  
  st <- "States"
  lg <- "LGAs"
  
  hdr <- if (length(x) > 1L) {
    if (all(is_state(x)) || inherits(x, "states"))
      st
    else
      lg
  }
  else {
    if (inherits(x, "lgas"))
      lg
    else
      st
  }
  
  underline <- strrep("-", nchar(hdr))
  newline <- "\n"
  cat(paste(hdr, underline, sep = newline), newline)
  cat(paste("*", x, collapse = newline), "\n")
}








#' Return the First or Last Parts of a Region Object
#' 
#' @rdname states
#' 
#' @importFrom utils head
#' @param x The object of class \code{region}.
#' @param ... Arguments to \code{head.default}
#' 
#' @export
head.regions <- function(x, ...)
{
  .chooseRegionsMethod(NextMethod(), x)
}




#' @rdname states
#' 
#' @importFrom utils tail
#' @param x The object of class \code{region}
#' @param ... Arguments to \code{tail.default}
#' 
#' @export
tail.regions <- function(x, ...)
{
  .chooseRegionsMethod(NextMethod(), x)
}


#' @export
c.regions <- function(...)
{
  ls <- unlist(list(...), use.names = FALSE)
  #
  # if (!Reduce(identical, lapply(all, class)))
  #   stop("All objects must be of the same class")
  # ls <- lapply(list(...), unclass)
  # new_states(unlist(ls, use.names = FALSE))
  .chooseRegionsMethod(NextMethod(), ls)
}

## Extraction functions for 'regions' objects
## Note: The replacement versions already work adequately
## with their default methods
#' @export
`[[.regions` <- function(x, i, exact = TRUE)
{
  .chooseRegionsMethod(NextMethod(), x)
}

#' @export
`[.regions` <- function(x, i)
{
  .chooseRegionsMethod(NextMethod(), x)
}


#' @importFrom stats na.exclude
#' @export
na.exclude.regions <- function(object, ...)
{
  if (!anyNA(object))
    return(object)
  
  object <- na.exclude(unclass(object), ...)
  na.attr <- attributes(object)
  
  object <- if (all(is_state(object)))
    new_states(object)
  else
    new_lgas(object)
  
  class(object) <- c(class(na.attr$na.action), class(object))
  attr(object, "na.action") <- na.attr$na.action
  object
}
