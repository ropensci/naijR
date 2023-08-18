# Source file: regions.R
#
# GPL-3 License
#
# Copyright (C) 2019-2023 Victor Ordu.

globalVariables(c("lgas_nigeria", "state", "lga"))

# States ----

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
#' @importFrom cli cli_abort
#' 
#' @export
states <- function(states, gpz = NULL, all = TRUE, warn = TRUE)
{
  assert.lgl.arg(all)
  assert.lgl.arg(warn)
  
  if (!missing(states) && is.character(states)) {
    num.missed <- sum(!is_state(states))
    
    if (num.missed) {
      if (warn && isFALSE(.is_nested_fix_dont_warn())) {
        abujas <- which(states %in% "Abuja")
        num.abuja <- length(abujas)
        
        if (num.abuja)
          cli::cli_warn(
            "'Abuja' in position(s) {paste(abujas, collapse = ', ')}
             is not a State. Use 'Federal Capital Territory' or 'FCT'"
          )
        
        if (!num.abuja || num.missed > num.abuja)
          .warn_on_misspelling('state')
      }
    }
    
    return(new_states(states))
  }
  
  state.list <- get_all_states()
  
  if (!all)
    state.list$fct <- NULL
  
  if (!is.null(gpz) && missing(states)) {
    if (!is.character(gpz))
      cli_abort("argument 'gpz' is not of type 'character'")
    
    gpz <- tolower(gsub("\\s+", "", gpz))
    x <- match.arg(gpz, names(state.list), several.ok = TRUE)
    state.list <- state.list[x]
  }
  
  ss <- as.vector(unlist(state.list), mode = 'character')
  
  if (is.null(gpz)) 
    ss <- sort(ss)
  
  new_states(ss)
}




## Low-level S3 constructor
new_states <- function(ss) 
{
  structure(ss, class = c("states", "regions", class(ss)))
}


# LGAs ----

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
#' @importFrom cli cli_abort
#' @importFrom cli cli_warn
#' @importFrom utils data
#' 
#' @export
lgas <- function(region = NA_character_, strict = FALSE, warn = TRUE) {
  data("lgas_nigeria", package = "naijR", envir = environment())
  
  if (is.factor(region))  # TODO: Perhaps implement methods.
    region <- as.character(region)
  
  if (!is.character(region))
    cli_abort("Expected an object of type 'character'")
  
  if (strict) {
    not.synonymous <- !(region %in% lgas_like_states())
    
    if (any(not.synonymous)) {
      nouns <- paste(region[not.synonymous], collapse = ", ")
      verb <- 
        sprintf(ngettext(sum(not.synonymous), "is %s", "are %ss"), "no LGA")
      cli_abort("There {verb} {nouns} sharing State names")
    }
  }
  
  if (length(region) == 1L && is.na(region))
    return(new_lgas(lgas_nigeria$lga))
  
  lst <- region
  
  if (all(is_state(region)) && !strict) {
    lst <- .list_lgas_by_state(region)
    
    if (length(region) == 1L)
      lst <- unname(unlist(lst))
  }
  else if (all(is_lga(region))) {
    lst <- .list_states_by_lga(region)
    lst.names <- names(lst)
    stt.num <- vapply(lst, length, integer(1))
    
    if (any(stt.num > 1L)) {
      multi <- which(stt.num > 1L)
      
      for (elem in multi) {
        stts <- lst[[elem]]
        nm <- lst.names[elem]
        stts.msg <- paste(stts, collapse = ", ")
        cli_warn("'{nm}' LGA is found in {length(stts)} States: {stts.msg}")
      }
    }
    lst <- unique(lst.names)
    region <- NULL
  }
  else if (.has_misspelt_lgas(region)) {
    if (warn && isFALSE(.is_nested_fix_dont_warn()))
      .warn_on_misspelling('lga')
    
    region <- NULL
  }
  else if (.all_are_not_lgas(region))
    cli_abort("None of the items is a valid LGA")
  # TODO: An object that belongs to more than one State should 
  # have a State attribute that lists the States and this should
  # apply to lgas objects that have just one element so that 
  # there is no confusion.
  structure(new_lgas(lst), State = region)
}




# Low-level S3 constructor for lgas object
new_lgas <- function(x)
{
  structure(x, class = c("lgas", "regions", class(x)))
}




# Methods for internal generics ----

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
print.regions <- function(x, ...) { # nocov start
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
  
  dash <- "-"
  underline <- strrep(dash, nchar(hdr))
  newline <- "\n"
  cat(paste(hdr, underline, sep = newline), newline)
  cat(paste(dash, x, collapse = newline), newline)
} # nocov end




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
  .chooseRegionsMethod(NextMethod(), x) # nocov
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
  .chooseRegionsMethod(NextMethod(), x) # nocov
}




#' @export
c.regions <- function(...)
{
  ls <- unlist(list(...), use.names = FALSE)
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
  obj.attrs <- attributes(object)
  
  object <- if (all(is_state(object)))
    new_states(object)
  else
    new_lgas(object)
  
  class(object) <- c(class(obj.attrs$na.action), class(object))
  attr(object, "na.action") <- obj.attrs$na.action
  object
}
