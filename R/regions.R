# Source file: regions.R
#
# GPL-3 License
#
# Copyright (C) 2019-2023 Victor Ordu.

globalVariables(c("lgas_nigeria", "state", "lga"))

# States ----

#' Objects for Representing the Federal States of Nigeria
#'
#' @param states A character vector with strings representing one or more
#' States of Nigeria. If missing, the function will return a \code{states}
#' object containing all the States.
#' @param gpz \code{NULL} (the default) or, case insensitively, one or more of
#' the following strings: \code{"nc", "ne", "nw", "se", "ss"} and \code{"sw"}
#' (see "Details").
#' @param all logical; whether to include the Federal Capital Territory (FCT)
#' in the result.
#' @param warn logical; issue a warning when one or more elements are not
#' actually States (i.e. they were misspelt).
#' @param x For \code{is_state} a vector to be tested. For \code{as_state}, a
#' string representing a State that shares its name with one of its Local
#' Government Areas.
#' @param ... Arguments used for methods. See documentation of generic
#' for details.
#'
#' @return The States of Nigeria as a whole or by zones, as an S3 object
#' of class \code{states}. \code{is_state} returns a logical vector.of same
#' length as the input. If the input object is not even of type
#' \code{character}, return the object unaltered, with a warning. In the case
#' of \code{as_state}, an object of class \code{states}.
#'
#' @details \code{gpz} represents a geopolitical zone which, in the Nigerian
#' context, is a national subdivision that groups contiguous states that bear
#' certain socio-cultural and political similarities. Historically, they arise
#' from sub-national administrative divisions known as 'Regions' that existed
#' at the time of the country's independence. There are at present 6 such
#' zones - North-Central, North-East, North-West, South-East,South-South and
#' South-West.
#'
#' For \code{is_state}, An element-wise check of a supplied vector is carried
#' out. To test an entire vector and return a single boolean value, functions
#' such as \code{base::all} or \code{base::any} should be used (see examples).
#'
#' @note \code{is_state} throws a warning, when a missing value is among the
#' elements. It works only for atomic vectors, throwing an error when this
#' is not the case or when \code{NULL} is passed to it. This design decision
#' was made to allow rapid iteration through data frames without interruption,
#' since spelling mistakes tend to be common.
#'
#' @examples
#' states()  # lists names of all States
#' states(gpz = "se")  # lists States in South-East zone
#' all(is_state(naijR::states()))
#' is_state(c("Maryland", "Baden-Baden", "Plateau", "Sussex"))
#'
#' # With coercion
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
#' @importFrom cli cli_abort
#' @importFrom cli cli_warn
#'
#' @export
states <- function(states, gpz = NULL, all = TRUE, warn = TRUE)
{
  assert.lgl.arg(all)
  assert.lgl.arg(warn)

  state.list <- get_all_states()
  localGpz <- names(state.list)

  if (!missing(states) && is.character(states)) {
    if (length(states) == 1L && (states %in% localGpz))
      cli_abort("'{states}' represents a geopolitical zone, not a State.
                Use the `gpz` argument to get the relevant States")

    num.missed <- sum(!is_state(states))

    if (num.missed) {
      if (warn && isFALSE(.is_nested_fix_dont_warn())) {
        abujas <- which(states %in% "Abuja")
        num.abuja <- length(abujas)

        if (num.abuja)
          cli_warn(
            "'Abuja' in position(s) {paste(abujas, collapse = ', ')}
             is not a State. Use 'Federal Capital Territory' or 'FCT'"
          )

        if (!num.abuja || num.missed > num.abuja)
          .warn_on_misspelling('state')
      }
    }

    return(new_states(states))
  }

  if (!all)
    state.list$fct <- NULL

  if (!is.null(gpz) && missing(states)) {
    if (!is.character(gpz))
      cli_abort("argument 'gpz' is not of type 'character'")

    gpz <- tolower(gsub("\\s+", "", gpz))
    x <- match.arg(gpz, localGpz, several.ok = TRUE)
    state.list <- state.list[x]
  }

  ss <- as.vector(unlist(state.list), mode = 'character')

  if (is.null(gpz))
    ss <- sort(ss)

  new_states(ss)
}




## Low-level S3 constructor
new_states <- function(x)
{
  structure(x, class = c("states", "regions", class(x)))
}


#' @importFrom cli cli_warn
#' @rdname states
#' @export
is_state <- function(x)
{
  if (!is.atomic(x) || is.null(x)) # as is.atomic(NULL) == TRUE
    cli::cli_abort("Expected a non-null atomic vector as input")

  ## Return the object rather than stop execution for this condition.
  ## This is to enable unhindered traversal when this function
  ## is applied across an object.
  if (!is.character(x)) {
    cli_warn("{sQuote(x)} is not a character vector. Nothing done")
    return(x)
  }

  na.pos <- 0L
  if (anyNA(x)) {
    cli_warn("Invalid entries were replaced with NAs")
    excl <- stats::na.exclude(x)
    na.pos <- stats::na.action(excl)
  }

  if (length(x) == 0L)
    return(FALSE)

  x <- .toggleFct(x, "full")
  res <- x %in% get_all_states(named = FALSE)
  res[na.pos] <- NA
  res
}




#' @rdname states
#' @export
as_state <- function(x)
{
  states(.assert_if_coercible(x))
}




# LGAs ----

#' Objects for Representing the Local Government Areas (LGAs) of Nigeria
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
#' @param x An object of type \code{character}. This includes higher
#' dimension object classes like \code{matrix} and \code{array}. For
#' \code{as_lga}, a string representing a Local Government Area that shares its
#' name with one of its States.
#' @param ... Arguments used for methods. See documentation of generic
#' for details.
#'
#' @note There are six (6) LGAs that share names with their State - Bauchi,
#' Ebonyi, Gombe, Katsina, Kogi and Ekiti.
#'
#' @return If length of \code{ng.state} == 1L, a character vector containing
#' the names of Local Government Areas; otherwise a named list, whose elements
#' are character vectors of the LGAs in each state.
#' \code{is_lga} returms a vector the same length as the input object
#' (each element that is not a valid Local Government Area will evaluate to
#' \code{FALSE}); with \code{as_lga}, an object of class \code{lgas}.
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
#' is_lga(c("Pankshen", "Pankshin"))
#'
#' # With coercion
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
#' @importFrom cli cli_abort
#' @importFrom cli cli_warn
#' @importFrom utils data
#'
#' @export
lgas <- function(region = NA_character_, strict = FALSE, warn = TRUE) {
  data(
    "lgas_nigeria",
    package = "naijR",
    envir = environment(),
    verbose = FALSE
  )

  if (is.factor(region))  # TODO: Perhaps implement methods.
    region <- as.character(region)

  if (!is.character(region))
    cli_abort("Expected an object of type 'character'")
  
  # if ((!is.na(region) && all(region == "")) || 
  #     (all(is.na(region)) && length(region) > 1))
  #   cli_abort("Illegal use of empty string/missing character values")

  if (strict) {
    not.synonymous <- !(region %in% lgas_like_states())

    if (any(not.synonymous)) {
      nouns <- paste(region[not.synonymous], collapse = ", ")
      verb <-
        sprintf(ngettext(sum(not.synonymous), "is %s", "are %ss"), "no LGA")
      cli_abort("There {verb} {nouns} sharing State names")
    }
  }

  if (length(region) == 1L && is.na(region)) {
    lgvec <- sort(lgas_nigeria$lga)
    return(new_lgas(lgvec))
  }

  lst <- region

  if (all(is_state(region)) && !strict) {
    .extract_state_lgas <- function(states, dt = lgas_nigeria) {
      idx <- dt$state %in% states
      dt$lga[idx]
    }
    
    lst <- region %>% 
      lapply(.extract_state_lgas) %>%
      stats::setNames(region)

    if (length(region) == 1L)
      lst <- lst %>% unlist %>% unname
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
  if (inherits(lst, "list")) {
   lst <-  lapply(lst, sort)
   lst <- lst[sort(names(lst))]
  }
  else if (inherits(lst, "character"))
    lst <- sort(lst)
  
  obj <- new_lgas(lst)
  attr(obj, which = "State") <- region
  obj
}




# Low-level S3 constructor for lgas object
new_lgas <- function(x)
{
  structure(x, class = c("lgas", "regions", class(x)))
}




#' @rdname lgas
#' @export
is_lga <- function(x)
{
  if (!is.character(x))
    cli::cli_abort("x should be of type 'character'")

  x %in% lgas()
}




#' @rdname lgas
#' @export
as_lga <- function(x) {
  new_lgas(.assert_if_coercible(x))
}




# Checks whether an object has all its elements as States or LGAs
.all_are_regions <- function(x) {
  stopifnot(isFALSE(is.null(x)))
  all(is_state(x)) || all(is_lga(x))
}




.some_are_regions <- function(x) {
  stopifnot(isFALSE(is.null(x)))
  isFALSE(.all_are_regions(x)) && (any(is_state(x)) || any(is_lga(x)))
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




# Methods for internal generics ----

#' @rdname states
#' @export
print.states <- function(x, ...) { # nocov start
  .printoutRegion(x, "States")
} # nocov end


#' @rdname lgas
#' @export
print.lgas <- function(x, ...) { # nocov start
  length(x)
  if (is.atomic(x))
    .printoutRegion(x, "Local Government Areas")
  else
    for (state in names(x)) {
      state_in_full <- paste(state, "State")
      mainborder <- paste0(strrep("=", nchar(state_in_full)), "\n")
      cat(mainborder)
      cat(state_in_full, "\n")
      cat(mainborder)
      .printoutRegion(x[[state]])
    }
} # nocov end



# Creates the formatted printout for 'regions' objects
.printoutRegion <- function(items, hdr = NULL) {
  if (!is.null(hdr)) {
    underline <- strrep("-", nchar(hdr))
    cat(paste(hdr, underline, sep = "\n"), "\n")
  }
  
  cat(paste("*", items, collapse = "\n"), "\n")
}




#' @rdname states
#' @export
c.states <- function(...)
{
  ls <- .unlistDots(...)
  new_states(NextMethod())
}




#' @rdname lgas
#' @export
c.lgas <- function(...)
{
  ls <- .unlistDots(...)
  new_lgas(NextMethod())
}



.unlistDots <- function(...) {
  unlist(list(...), use.names = FALSE)
}




#' @rdname states
#' @param i,exact See help file for \code{?Extract}
#' @export
`[[.states` <- function(x, i, exact = TRUE)
{
  new_states(NextMethod())
}




#' @rdname lgas
#' @param i,exact See help file for \code{?Extract}
#' @export
`[[.lgas` <- function(x, i, exact = TRUE)
{
  new_lgas(NextMethod())
}




#' @rdname states
#' @export
`[.states` <- function(x, i)
{
  new_states(NextMethod())
}


#' @rdname lgas
#' @export
`[.lgas` <- function(x, i)
{
  new_lgas(NextMethod())
}




#' @rdname states
#' @param object An object of class \code{regions}
#' @export
na.exclude.states <- function(object, ...)
{
  if (!anyNA(object))
    return(object)

  .excluderForNas(object, "states", ...)
}




#' @rdname lgas
#' @param object An object of class \code{regions}
#' @export
na.exclude.lgas <- function(object, ...)
{
  if (!anyNA(object))
    return(object)

  .excluderForNas(object, "lgas", ...)
}



#' @importFrom stats na.exclude
.excluderForNas <- function(obj, regiontype = c("states", "lgas"), ...) {
  regiontype <- match.arg(regiontype)
  obj <- stats::na.exclude(unclass(obj), ...)
  obj.attrs <- attributes(obj)
  constructor <- paste("new", regiontype, sep = "_")
  obj <- do.call(constructor, args = list(x = obj))
  class(obj) <- c(class(obj.attrs$na.action), class(obj))
  attr(obj, "na.action") <- obj.attrs$na.action
  obj
  }
