.onLoad <- function(libname, pkgname)
{
  options(
    choropleth.colours = c("grey", "red", "green", "blue", "purple", "orange"),
    rgdal_show_exportToProj4_warnings = "none"
  )
}