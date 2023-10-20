# Source file: fct.R
#
# GPL-3 License
#
# Copyright (C) 2019-2023 Victor Ordu.

## Get a vector with both the abbreviated and full versions of the 
## national capital's name, just return one of the two.
.fct_options <- function(opt = c("all", "full", "abbrev")) 
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
  versions <- .fct_options()
  sub(versions[-i], versions[i], x, fixed = TRUE)
}



## Repairs the use of Abuja in a vector that is intended to be States
.fixAbujaFct <- function(x, type = c("full", "abbrev"))
{
  stopifnot(exprs = {any(is_state(x)) && length(unique(x)) > 1L})
  abj <- "Abuja"
  
  if (!abj %in% x)
    return(x)
  
  sub(x, abj, .fct_options(type), fixed = TRUE)
}
