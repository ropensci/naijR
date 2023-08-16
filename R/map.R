# Source file: map.R
#
# GPL-3 License
#
# Copyright (C) 2019-2023 Victor Ordu.

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
#' @param leg.x,leg.y Numeric. Position of the legend (deprecated).
#' @param leg.title String. The legend title. If missing, a default value is
#' acquired from the data. To turn off the legend title, pass \code{NULL}.
#' @param plot Logical. Turn actual plotting of the map off or on.
#' @param leg.orient The orientation of the legend i.e. whether horizontal or
#' vertical (deprecated). 
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
#' @examples
#' \dontrun{
#' map_ng() # Draw a map with default settings
#' map_ng(states("sw"))
#' map_ng("Kano")
#' }
#'
#' @return An object of class \code{sf}, which is a standard format containing 
#' the data used to draw the map and thus can be used by this and other 
#' popular R packages to visualize the spatial data.
#'
#' @import sf
#' @importFrom cli cli_abort
#' @importFrom cli cli_warn
#' @importFrom lifecycle deprecate_warn
#' @importFrom lifecycle deprecated
#' @importFrom lifecycle is_present
#' @importFrom rlang !!
#' @importFrom rlang as_name
#' @importFrom rlang caller_env
#' @importFrom rlang enexpr
#' @importFrom rlang enquo
#' @importFrom rlang expr
#' @importFrom rlang eval_tidy
#' @importFrom rlang is_null
#' @importFrom rlang is_symbol
#' 
#' @export
map_ng <- function(region = character(),
                   data = NULL,
                   x = NULL,
                   y = NULL,
                   breaks = NULL,
                   categories = NULL,
                   excluded = NULL,
                   exclude.fill = NULL,
                   title = NULL,
                   caption = NULL,
                   show.neighbours = FALSE,
                   show.text = FALSE,
                   legend.text = NULL,
                   leg.x = deprecated(),
                   leg.y = deprecated(),
                   leg.title,
                   plot = TRUE,
                   leg.orient = deprecated(),
                   ...)
{    ## TODO: Allow this function to accept a matrix e.g. for plotting points
  if (!is.character(region)) {
    msg <- sprintf("Expected a character vector as '%s'.", arg_str(region))
    
    addmsg <- if (is.data.frame(region))
      "A data frame was passed; did you mean to use 'data' instead?"
    
    cli_abort("{msg} {addmsg}")
  }
  
  if (!is.null(data) && !is.data.frame(data))
    cli_abort(sprintf("A non-NULL input for '%s' must be a data frame",
                 arg_str(data)))
  
  if (is.data.frame(data) && ncol(data) < 2L)
    cli_abort(
      "Insufficient variables in '{deparse(quote(data))}' to generate a plot"
    )
  
  if (!is.logical(show.neighbours))
    cli_abort("'{arg_str(show.neighbours)}' should be a logical value")
  
  if (length(show.neighbours) > 1L) {
    show.neighbours <- show.neighbours[1]
    cli_warn("{first_elem_warn(arg_str(show.neighbours))}")
  }
  
  if (show.neighbours)
    cli::cli_alert("Display of neighbouring regions is temporarily disabled")
  
  region <- .process_region_params(region, call = caller_env())
  
  value.x <- if (is_null(data) && !is_null(x))
    enquo(x) 
  else 
    enexpr(x)
  
  use.choropleth <- if (is_null(value.x) || is_symbol(value.x)) {
    .validate_choropleth_params(!!value.x, region, data)  # TODO: Refactor
  }
  else if (!is_null(y)) {
    FALSE
  }
  else {
    value.x <- eval_tidy(value.x)
    .validate_choropleth_params(value.x, region, data)
  }
  
  mapdata <- .get_map_data(region)
  mapq <- expr(.mymap(mapdata, regions = region, plot = plot, ...))
  dots <- list(...)
  
  if (use.choropleth) {
    mapq <- expr(.mymap(mapdata, region, plot = plot))
    cParams <- list(
      region = region,
      value = value.x,
      breaks = breaks,
      categories = categories
    )
    
    if (!is_null(data)) {
      region.col <- .region_column_index(data, region)
      
      ## Bet on a two-column data frame that has a
      ## a column with valid regions
      value.x <- if (is_null(value.x) && ncol(data) == 2L)
        names(data)[-region.col]
      else
        as_name(value.x)
      
      cParams$value <-  data[[value.x]]
      cParams$region <- data[[region.col]]
    }
    
    cOpts <-
      .prep_choropleth_opts(mapdata, cParams, dots$col, excluded, exclude.fill)
    mapq$col <- cOpts$colors
    
    if (lifecycle::is_present(leg.orient))
      lifecycle::deprecate_warn(.next_minor_version(), .deprec_msg(leg.orient))
  }
  
  tryCatch({
    sfdata <- eval(mapq)
  }, 
  error = function(e) stop(e))
  
  if (!is_null(y) && !.xy_within_bounds(sfdata, x, y))
    cli_abort("Coordinates are beyond the bounds of the plotted area")
  
  if (!plot)
    return(sfdata)
  
  if (show.text) {
    txt <- country_name()
    df.only <- as.data.frame(sfdata)
    
    if (inherits(region, "regions")) {
      rtype <- sub("(.+)(s$)", "\\1", class(region)[1])
      namefield <- get(paste0("shp.", rtype))$namefield
      txt <- df.only[[namefield]]
      
      if (all(is_state(region))) 
        txt <- sub(.fct_options("full"), .fct_options("abbrev"), txt)
      
      cex <- .set_text_size(dots$cex)
      xycoord <- .get_point_coords(sfdata)
      graphics::text(xycoord[, 'x'], xycoord[, 'y'], labels = txt, cex = cex)
    }
  }
  
  ## Annotate
  graphics::title(main = title, sub = caption)
  
  if (use.choropleth) {
    
    if (is_null(categories))
      categories <- cOpts$bins
    
    lp <- .set_legend_params(legend.text)
    
    if (is.character(lp$text)) {
      if (!identical(length(categories), length(lp$text)))
        cli_abort("Lengths of categories and provided legend do not match")
      
      categories <- lp$text
    }
    
    if (lifecycle::is_present(leg.x))
      lifecycle::deprecate_warn(.next_minor_version(), .deprec_msg(leg.x))
    
    if (lifecycle::is_present(leg.y))
      lifecycle::deprecate_warn(.next_minor_version(), .deprec_msg(leg.y))
    
    if (missing(leg.title)) {  # TODO: Change this construct.
      leg.title <- if (is_null(data))
        deparse(substitute(x))
      else
        value.x
    }
    
    if (lp$show) {
      graphics::legend(
        x = lp$x,
        y = lp$y,
        legend = categories,
        fill = cOpts$scheme,
        xpd = lp$xpd,
        title = leg.title
      )
    }
  }
  else if (!is_null(y)) {
    graphics::points(x, y, ...)
  }
  
  invisible(sfdata)
}
