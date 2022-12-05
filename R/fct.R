## Get a vector with both the abbreviated and full versions of the 
## national capital's name, just return one of the two.
.fctOptions <- function(opt = c("all", "full", "abbrev")) 
{
  opt <- match.arg(opt)
  versions <- c(full = "Federal Capital Territory", abbrev = "FCT")
  
  if (opt != "all")
    return(versions[opt])
  
  versions
}


## Alternatively uses the abbreviated or full forms for the FCT
.toggleFct <- function(x, use)
{
  opts <- c("full", "abbrev")
  use <- match.arg(use, opts)
  i <- match(use, opts)
  versions <- .fctOptions()
  sub(versions[-i], versions[i], x, fixed = TRUE)
}



## Repairs the use of Abuja in a vector that is intended to be States
.fixAbujaFct <- function(x, type = c("full", "abbrev"))
{
  stopifnot({any(is_state(x)) && length(unique(x)) > 1L})
  abj <- "Abuja"
  
  if (!abj %in% x)
    return(x)
  
  sub(x, abj, .fctOptions(type), fixed = TRUE)
}
