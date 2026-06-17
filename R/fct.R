# Source file: fct.R
#
# GPL-3 License
#
# Copyright (C) 2019-2026 Victor Ordu.

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
