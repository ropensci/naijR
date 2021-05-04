
## Get a vector with both the abbreviated and full versions of the 
## national capital's name, just return one of the two.
.fctOptions <- function(opt = c("all", "abbrev", "full")) {
  opt <- match.arg(opt)
  vec <- c(abbrev = "FCT", full = "Federal Capital Territory")
  if (opt != "all")
    return(vec[opt])
  vec
}

