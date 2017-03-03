#' Display States of the Federal Republic of Nigeria
#' 
#' 
#' @param gpz Geopolitical zone. Default is \code{NULL}; optionally \code{"nc",
#'  "ne", "nw", "se", "ss"} and \code{"sw"} (see \code{Details}).
#' @param all.sorted Whether the results to be all sorted in increasing 
#' alphabetical order (default value is \code{FALSE}).
#' @param full.names the complete appellation for the States
#' 
#' @return The States of Nigeria as a whole or by zones, as a character vector
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
#' states("se")  # lists States in South-East zone
#' @export
states <- function(gpz = NULL, all.sorted = FALSE, full.names = FALSE)
{
  # TODO: FCT or not?
  sts <- list(nc = c("Benue", "Kogi", "Kwara", "Nasarawa", "Niger", "Plateau"),
              ne = c("Adamawa", "Bauchi", "Borno", "Gombe", "Taraba", "Yobe"),
              nw = c("Jigawa", "Kaduna", "Kano", "Katsina", "Kebbi", "Sokoto",
                     "Zamfara"), 
              se = c("Abia", "Anambra", "Ebonyi", "Enugu", "Imo"),
              ss = c("Akwa Ibom", "Bayelsa", "Cross River", "Delta", "Edo",
                     "Rivers"),
              sw = c("Ekiti", "Lagos", "Ogun", "Ondo", "Osun", "Oyo"))
  if (is.null(gpz))
    sts <- as.vector(unlist(sts))
  else {
    if (!is.character(gpz))
      stop("argument supplied is not of type 'character'")
    gpz <- gsub("\\s+", "", gpz)
    gpz <- tolower(gpz)
    rgn <- match.arg(gpz, c("nc", "ne", "nw", "se", "ss", "sw"),
                     several.ok = TRUE)
    sts <- as.vector(unlist(sts[rgn]))
  }
  if (!is.logical(all.sorted))
    stop("'all.sorted' expected as a logical argument of length 1")
  else {
    if (all.sorted)
      sts <- sort(sts)
  }
  if (!is.logical(full.names))
    stop("'full.names' expected as a logical argument of length 1")
  else {
    if (full.names)
      sts <- paste(sts, "State")
  }
  print(sts)
}


#' Names of Local Government Areas in Nigeria
#' 
#' @param state State of the Federation
#' @return Local Government Areas
#' @details When a State is chosen by passing in `state`, the function obtains
#' list of Local Government Areas in that particular State.
#' @return A character vector with the list of Local Government Areas. 
#' @examples 
lgalist <- function(state = NULL)
{
  d <- data("lga-by-state.csv")
  if (is.null(state))
    lg <- d[, 2]
  else
  {
    sbd <- d[d$state == as.character(state), ]
    lg <- sbd[, 2] 
  }
  print(lg)
}
