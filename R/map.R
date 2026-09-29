# Source file: map.R
#
# GPL-3 License
#
# Copyright (C) 2019-2026 Victor Ordu.

globalVariables(c("STATE", "shp.state", "shp.lga"))

#' Map of Nigeria
#'
#' Maps of the Federal Republic of Nigeria that are based on the basic
#' plotting idiom utilised by \link[maps:map]{maps:map} and its variants.
#'  
#' @param region A character vector of regions to be displayed. This could be 
#' States or Local Government Areas.
#' @param data An object containing data, principally the variables required to
#' plot in a map.
#' @param x,y Numeric object or factor (or coercible to one). See 
#' \emph{Details}.
#' @param breaks Numeric. A vector of length >= 1. If a single value i.e.
#' scalar, it denotes the expected number of breaks. Internally, the function
#' will attempt to compute appropriate category sizes or fail if out-of bounds. 
#' Where length is >= 3L, it is expected to be an arithmetic sequence that 
#' represents category bounds as for \code{\link[base]{cut}} (applicable 
#' only to choropleth maps).
#' @param categories The legend for the choropleth-plotted categories. If not 
#' defined, internally created labels are used.
#' @param excluded Regions to be excluded from a choropleth map.
#' @param exclude.fill Colour-shading to be used to indicate \code{excluded}
#' regions. Must be a vector of the same length as \code{excluded}.
#' @param title,caption An optional string for annotating the map.
#' @param show.neighbours Logical; \code{TRUE} to display the immediate vicinity
#' neighbouring regions/countries.
#' @param show.text Logical. Whether to display the labels of regions.
#' @param legend.text Logical (whether to show the legend) or character vector
#' (actual strings for the legend). The latter will override whatever is 
#' provided by \code{categories}, giving the user additional control.
#' @param leg.title String. The legend title. If missing, a default value is
#' acquired from the data. To turn off the legend title, pass \code{NULL}.
#' @param plot Logical. Turn actual plotting of the map off or on.
#' @param ... Further arguments passed to \code{\link[sf]{plot}}
#' 
#' @details The default value for \code{region} is to print all State 
#' boundaries.
#' \code{data} enables the extraction of data for plotting from an object
#' of class \code{data.frame}. Columns containing regions (i.e. States as well
#' as supported sub-national jurisdictions) are identified. The argument also
#' provides context for quasiquotation when providing the \code{x} and
#' \code{y} arguments.
#' 
#' For \code{x} and \code{y}, when both arguments are supplied, they are taken
#' to be point coordinates, where \code{x} represent longitude and \code{y}
#' latitude. If only \code{x} is supplied, it is assumed that the intention of
#' the user is to make a choropleth map, and thus, numeric vector arguments are
#' converted into factors i.e. number classes. Otherwise factors or any object 
#' that can be coerced to a factor should be used.
#' 
#' For plain plots, the \code{col} argument works the same as with
#' \code{\link[maps]{map}}. For choropleth maps, the colour provided represents 
#' a (sequential) colour palette based on \code{RColorBrewer::brewer.pal}. The 
#' available colour options can be checked with 
#' \code{getOption("choropleth.colours")} and this can also be modified by the 
#' user.
#' 
#' If the default legend is unsatisfactory, it is recommended that the user
#' sets the \code{legend.text} argument to \code{FALSE}; the next function
#' call should be \code{\link[graphics]{legend}} which will enable finer
#' control over the legend.
#' 
#' @note When adjusting the default colour choices for choropleth maps, it is
#' advisable to use one of the sequential palettes. For a list of of available
#' palettes, especially for more advanced use, review 
#' \code{RColorBrewer::display.brewer.all}.
#' 
#' @seealso \code{vignette("nigeria-maps")} for additional ways to use this 
#' function.
#'
#' @examples
#' \dontrun{
#' map_ng() # Draw a map with default settings
#' map_ng(states("sw"))
#' map_ng("Kano")}
#'
#' @return An object of class \code{sf}, which is a standard format containing 
#' the data used to draw the map and thus can be used by this and other 
#' popular R packages to visualize the spatial data.
#'
#' @importFrom cli cli_abort
#' @importFrom cli cli_warn
#' @importFrom rlang as_name
#' @importFrom rlang caller_env
#' @importFrom rlang enexpr
#' @importFrom rlang enquo
#' @importFrom rlang expr
#' @importFrom rlang is_null
#' @importFrom sf st_as_sf
#' @importFrom sf st_crs
#' @importFrom sf st_union
#' 
#' @export
## TODO: Allow this function to accept a matrix e.g. for plotting points
map_ng <- 
  function(region = character(), data = NULL, x = NULL, y = NULL, breaks = NULL,
           categories = NULL, excluded = NULL, exclude.fill = NULL,title = NULL,
           caption = NULL, show.neighbours = FALSE, show.text = FALSE,
           legend.text = NULL, leg.title, plot = TRUE, ...) { 
  .checkParams(region, data, show.neighbours, arg_str)
  region <- .process_region_params(region, call = caller_env())
  xname <- if (is_null(data) && !is_null(x)) {
    enquo(x) 
  }
  else {
    enexpr(x)  # we are making sure to account for when x is NULL (default)
  }
  mapdata <- .get_map_data(region)
  mapq <- expr(.mymap(mapdata, plot = plot, ...))
  dots <- list(...)
  use.choropleth <- .check_choropleth_use(region, data, xname, y)
  if (use.choropleth) {
    mapq <- expr(.mymap(mapdata, plot = plot))
    cpleth.opts <- 
      .get_choropleth_opts(mapdata, data, region, xname, breaks,
                           categories, dots$col, excluded, exclude.fill)
    mapq$col <- cpleth.opts$colors
    if (is_null(categories)) {
      categories <- cpleth.opts$bins
    }
    lp <- .set_legend_params(legend.text, categories)
  }
  tryCatch(sfdata <- eval(mapq), error = function(e) stop(e))
  if (!is_null(y) && !.pts_within_bounds(sfdata, x, y)) {
    cli_abort("Coordinates are beyond the bounds of the plotted area")
  }
  if (plot) { 
    graphics::title(main = title, sub = caption) # nocov start
    if (use.choropleth && lp$show) {
      if (missing(leg.title)) {
        leg.title <- xname
        if (is_null(data)) {
          leg.title <- deparse(substitute(x))
        }
      }
     legend(x = lp$x, y = lp$y, legend = lp$categories, fill = cpleth.opts$scheme,
            xpd = lp$xpd, title = leg.title)
    }
    if (!is_null(y)) {
      sfdata <- .plot_points(sfdata, x, y, ...)
    }
    if (show.text) {
      .show_map_text(sfdata, region, dots$cex)
    }
  }
  invisible(sfdata)
  }
