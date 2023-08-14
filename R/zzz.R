# Source file: zzz.R
#
# GPL-3 License
#
# Copyright (C) 2019-2023 Victor Ordu.
.onLoad <- function(libname, pkgname)
{
  options(
    choropleth.colours = c("grey", "red", "green", "blue", "purple", "orange"),
    rgdal_show_exportToProj4_warnings = "none"
  )
}