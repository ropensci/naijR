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
  structure(ss, class = c("states", class(ss)))
}





.getAllStates <- function(named = TRUE)
{
  names <- if (named)
    c('nc', 'ne', 'nw', 'se', 'ss', 'sw', 'fct')
  ss <- structure(list(
    c("Benue", "Kogi", "Kwara", "Nassarawa", "Niger", "Plateau"),
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
  
  if (length(x) == 0L)
    return(FALSE)
  fctOpts <- .fctOptions()
  x %>%
    sub(fctOpts["abbrev"], fctOpts["full"], .) %>%
    `%in%`(.getAllStates(named = FALSE)) %>%
    {
      .[na.pos] <- NA
      .
    }
}






#' Fix Region Names
#' 
#' Correct any misspelt names of administrative regions i.e. States and LGAs
#' 
#' @details The function will look through a character vector and try to 
#' determine if State or LGA names have been wrongly entered. This presupposes that
#' the atomic vector is of type \code{character}. It does not test any missing
#' values in the vector, leaving them untouched.
#' 
#' @param x An S3 object of class \code{states} or \code{lgas}. For 
#' \code{fix_region.default}, a character vector can be passed but only
#' that for States will be interpretable.
#' @param ... Arguments passed to methods.
#' 
#' @return The transformed object. If all names are correct, the object is
#' returned unchanged.
fix_region <- function(x, ...)
  UseMethod("fix_region")




#' @rdname fix_region
#' @export
fix_region.states <- function(x, ...)
{
  ## Process possible FCT values
  abbrFCT <- .fctOptions("abbrev")
  fullFCT <- .fctOptions("full")
  sumFct <- sum(.fctOptions() %in% x)
  if (sumFct == 2)
    x <- sub(abbrFCT, fullFCT, x)
  if (sumFct == 1) {
    i_abbr <- grep(abbrFCT, x)   # TODO: Warisdis?
    i_full <- grep(fullFCT, x)
  }
  i <- grep(sprintf("^%s$", abbrFCT), x, ignore.case = TRUE)
  ss <- states()
  if (length(i)) 
    ss <- sub(fullFCT, abbrFCT, ss)
  
  x <- .fixRegionInternal(x, ss)
  x[i] <- fullFCT
  if (!inherits(x, "states"))
    x <- suppressWarnings(states(x))
  invisible(x)
}





#' @rdname fix_region
#' 
#' @param interactive Logical. When \code{TRUE}, the function prompts the user
#' to interactively select the correct LGA names from a list of available
#' options.
#' 
#' @export
fix_region.lgas <- function(x, interactive = FALSE, ...)
{
  vals <- .fixRegionInternal(x, lgas(), ...)
  if (interactive) {
    ans <- readline("Do you want to repair interactively? (Y/N): ")
    if (tolower(ans) == "y")
      vals <- .fixLgasInteractively(vals)
  }
  attributes(vals) <- NULL
  invisible(vals)
}






#' @rdname fix_region
#' @importFrom magrittr %>%
#' 
#' @export
fix_region.default <- function(x, ...)
{ ## TODO: Provide verbosity by reporting on fixed items?
  if (!is.character(x))
    stop("'x' is not a character vector")
  empty <- grepl("^$", x)
  if (length(empty) > 0L && all(empty))  ## diff character(0) and character(1)
    stop("'x' only has empty strings")
  if (any(empty))
    warning("Tried to fix empty strings - may produce errors")
  if (all(is.na(x)) || !length(x)) {
    warning("'x' has length 0L or only missing values", call. = FALSE)
    return(x)
  }
  zz <- suppressWarnings(states(x)) %>% fix_region
  as.character(zz)
}





.fixRegionInternal <- function(x, region, ...)
{
  ## Approximately matches a string on a list of regions i.e States or LGAs.
  ## @param str A string to be matched
  ## @param regions The values to be matched against, specifically from regions
  ##
  ## @return A vector of the same length as str with the approximetly
  ## matched value, which in the case of misspelling should be the correct one.
  ##
  ## This function is mapped to a vector of states/LGAs
  .getProperVal <- function(str, regions) {
    abbrFCT <- .fctOptions("abbrev")
    if (!is.na(match(str, regions)))
      return(str)
    if (inherits(regions, "states")) {
      if (agrepl(str, abbrFCT, max.distance = 2)
          && identical(toupper(str), abbrFCT))
        return(abbrFCT)
    }
    
    ## First remove spaces around slashes
    str <- str %>% 
      gsub("\\s\\/", "/", .) %>% 
      gsub("\\/\\s", "/", .) %>% 
      sub("-\\s", "-", .) %>% 
      sub("^Egbado/", "", .)
      
    
    ## Now check for exact matching
    matched <-
      grep(paste0('^', str, '$'),
           regions,
           value = TRUE,
           ignore.case = TRUE)
    
    
    if (length(matched) == 1L) 
      return(matched)
    
    ## Otherwise check for approximate matches
    matched <- agrep(str, regions, value = TRUE, max.distance = 1)
    
    if (length(matched) == 1L)
      return(matched)
    
    if (length(matched) > 1L) {
      .warnOnMultipleMatches(str, matched)
    }
    else if (!length(matched))
      notFound <<- c(notFound, str)
    str
  }
  
  notFound <- character()
  v <-
    vapply(x, .getProperVal, character(1), regions = region, USE.NAMES = FALSE)
  if (length(notFound)) {
    message("Approximate match(es) not found for the following:")
    sapply(notFound, function(x) message(paste("*", x)))
    attr(v, "misspelt") <- notFound
  }
  v
}





.warnOnMultipleMatches <- function(x, matches)
{
  multimatch <- paste(matches, collapse = ", ")
  warning(sprintf(
    "'%s' approximately matched more than one region - %s",
    x,
    multimatch
  ), call. = FALSE)
}






## Interactively fixes regions that are bad - this function is primarily
## used for repairing LGA names, since they are so many.
## @param lga.list The vector of LGA names that is being repaired. This vector
## is generated by `.fixRegionInternal` and has an attribute called `misspelt`,
## which is the collection of names needing repair.
#' @importFrom utils menu
.fixLgasInteractively <- function(lga.list)
{
  stopifnot(interactive())
  allLgas <- lgas()
  opt <- NA
  retry <- "Retry"
  quit <- "Quit"
  skip <- "Skip"
  skipped <- character()
  bad.values <- attr(lga.list, "misspelt")
  for (i in bad.values) {
    message("Fixing ", sQuote(i))
    repeat {
      choices <- readline("Search pattern: ") %>% 
        grep(allLgas, value = TRUE, ignore.case = TRUE) %>% 
        c(retry, skip, quit)
      opt <- menu(choices, FALSE, "Select the LGA")
      chosen <- choices[opt]
      if (chosen != retry)
        break
    }
    if (chosen == quit)
      break
    if (chosen == skip) {
      skipped <- c(skipped, i)
      next
    }
    lga.list <- sub(i, chosen, lga.list)
  }
  if (length(skipped))
    warning("The following items were skipped and should be fixed manually: ",
            paste(skipped, collapse = ", "),
            call. = FALSE)
  lga.list
}













.fixRegionsManually <- function(wrong, correct, src)
{
  patterns <- paste0("^", wrong, "$")
  wrongIndexed <- vapply(patterns, grep, integer(1), x = src)
  src[wrongIndexed] <- correct
  src
}









## Toggle between full and abbreviated FCT name
#' @importFrom rlang abort
.toggleFct <- function(x)
{
  if (length(x) != 1L)
    abort("Expected a vector of length == 1L")
  if (!is.character(x))
    abort("'x' is not a character vector",)
  
  opts <- .fctOptions()
  if (!x %in% opts)
    abort("Invalid input")
  ind <- ifelse(match(x, opts) == 1L, 2L, 1L)
  opts[ind]
}









## Get a vector with both the abbreviated and full versions of the 
## national capital's name, just return one of the two.
.fctOptions <- function(opt = c("all", "abbrev", "full")) {
  opt <- match.arg(opt)
  vec <- c(abbrev = "FCT", full = "Federal Capital Territory")
  if (opt != "all")
    return(vec[opt])
  vec
}










#' Local Government Areas of Nigeria
#'
#' A dataset containing the 774 Local Government Areas of Nigeria
#'
#' @format A dataframe with 774 rows and 2 columns
#' \describe{
#'    \item{lga}{Local Government Area}
#'    \item{state}{State of the Federation}
#'    }
"lgas_nigeria"










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
        select = lga,
        ## TODO: Refactor
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
  else
    stop("One or more elements is not a valid region in Nigeria")
  
  new_lgas(lst)
}








#' Test for Local Government Areas
#' 
#' Checks a given object for Local Government Areas, represented as
#' strings.
#' 
#' @param x An object of type \code{character}. This includes higher
#' dimension object classes like \code{matrix} and \code{array}. 
#' 
#' @return A logical vector the same length as the input object. Each
#' element that is not a valid Local Government Area will evaluate to
#' \code{FALSE}.
#' 
#' @export
is_lga <- function(x)
{
  if (!is.character(x))
    stop("x should be of type 'character'")
  x %in% lgas()
}








# Low-level S3 constructor for lgas object
new_lgas <- function(x)
{
  structure(x, class = c("lgas", class(x)))
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