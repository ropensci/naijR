globalVariables("ngdist")

#' Distance between two Nigerian state capitals
#'
#' Retrieves the precomputed road (driving) distance between Nigerian
#' state capitals. Both \code{a} and \code{b} can be vectors of the same
#' length, in which case distances are computed element-wise.
#'
#' @param a,b Character vector. Name(s) of city/cities (case-insensitive).
#'   Must be the same length; vector recycling is not permitted.
#' @param unit Character. Unit of distance to return. One of "km" (default) or
#' "miles".
#'
#' @return Numeric vector of road distances in the requested unit (rounded to
#'   1 decimal place), with the same length as \code{a} and \code{b}.
#'
#' @details
#' Distances are approximate driving distances sourced from travel allowance
#' guidance provided by UNDP Nigeria. They represent typical road travel and
#' may vary due to route choice, road conditions, or detours.
#'
#' @examples
#' ng_distance("Lagos", "Kano")
#' ng_distance("Abuja", "Port Harcourt", unit = "miles")
#' ng_distance("Ibadan", "enugu")  # case-insensitive
#' ng_distance(c("Lagos", "Abuja"), c("Kano", "Enugu"))  # vectorized
#'
#' @importFrom stringi stri_trans_totitle
#'
#' @export
ng_distance <- function(a, b, unit = c("km", "miles")) {
  if (missing(a) || missing(b))
    cli_abort("Inputs 'a' and 'b' must both be supplied")
  if (length(a) != length(b))
    cli_abort("Inputs 'a' and 'b' must be the same length")
  
  # Ikeja is the capital of Lagos State, but is most commonly called 
  # 'Lagos'. Thus, we will permit its use as a query term.
  accept_lagos <- function(x) {
    haslagos <- grepl("lagos", x, ignore.case = TRUE)
    if (any(haslagos))
      x[haslagos] <- "Ikeja"
    x
  }  # TODO: add a warning
  a <- accept_lagos(a)
  b <- accept_lagos(b)

  data("ngdist", package = "naijR", envir = environment(), verbose = FALSE)
  unit <- match.arg(unit)
  a_idx <- match(stringi::stri_trans_totitle(a), labels(ngdist))
  b_idx <- match(stringi::stri_trans_totitle(b), labels(ngdist))

  bad_a <- which(is.na(a_idx))
  bad_b <- which(is.na(b_idx))
  bad_idx <- union(bad_a, bad_b)

  if (length(bad_idx)) {
    bad_names <- unique(c(a[bad_a], b[bad_b]))
    cli_abort(c(
      "City name{?s} not found: {.val {bad_names}}",
      "i" = "At position{?s} {bad_idx} of the input"
    ))
  }

  mat <- as.matrix(ngdist)
  d <- mat[cbind(a_idx, b_idx)]

  if (identical(unit, "miles"))
    d <- d * 0.6214

  round(d, 1)
}
