#' Test an Object for States
#' 
#' @param x A vector to be tested.
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
#' @import stats
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
    excl <- na.exclude(x)
    na.pos <- na.action(excl)
  }
  
  if (length(x) == 0L)
    return(FALSE)
  
  x <- .toggleFct(x, "full")
  res <- x %in% getAllStates(named = FALSE)
  res[na.pos] <- NA
  res
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





assertRegion <- function(x) {
  if (!is_state(x) && !is_lga(x))
    stop(sQuote(x, q = FALSE), " is not a valid region")
  x
}




# Checks whether an object has all its elements as States or LGAs
.allAreRegions <- function(x) {
  stopifnot(isFALSE(is.null(x)))
  all(is_state(x)) || all(is_lga(x))
}



.someAreRegions <- function(x) {
  stopifnot(isFALSE(is.null(x)))
  isFALSE(.allAreRegions(x)) && (any(is_state(x)) || any(is_lga(x)))
}
