# Source file: zzz.R
#
# GPL-3 License
#
# Copyright (C) 2019-2026 Victor Ordu.
.onLoad <- function(libname, pkgname)
{
  options(
    choropleth.colours = c("grey", "red", "green", "blue", "purple", "orange")
  )
}
