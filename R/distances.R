globalVariables("ngdist")

#' Distance between two Nigerian state capitals
#'
#' Retrieves the precomputed road (driving) distance between two Nigerian
#' state capitals.
#'
#' @param a,b Character. Name of a city (case-insensitive).
#' @param unit Character. Unit of distance to return. One of "km" (default) or 
#' "miles".
#'
#' @return Numeric scalar: the road distance between the two cities in the
#'   requested unit (rounded to 1 decimal place).
#'
#' @details
#' Distances are approximate driving distances sourced from [your source, e.g.,
#' Google Maps / OSRM snapshot in 2025]. They represent typical road travel
#' and may vary due to traffic, road conditions, or security factors.
#' City names are matched case-insensitively.
#'
#' @examples
#' ng_distance("Lagos", "Kano")
#' ng_distance("Abuja", "Port Harcourt", unit = "miles")
#' ng_distance("Ibadan", "enugu")  # case-insensitive
#' 
#' @importFrom stringi stri_trans_totitle
#'
#' @export
ng_distance <- function(a, b, unit = c("km", "miles")) {
  if (missing(a) || missing(b))
    cli_abort("Inputs 'a' and 'b' must both be supplied")
  
  data("ngdist", package = "naijR", envir = environment(), verbose = FALSE)
  unit <- match.arg(unit)
  a_idx <- match(stringi::stri_trans_totitle(a), labels(ngdist))
  b_idx <- match(stringi::stri_trans_totitle(b), labels(ngdist))
  
  if (is.na(a_idx) || is.na(b_idx))
    cli_abort("One or both city names not found. Check with 'labels(ngdist)'")
  
  d <- as.matrix(ngdist)[a_idx, b_idx]
  
  if (identical(unit, "miles"))
    d <- d * 0.6214
  
  round(d, 1)
}