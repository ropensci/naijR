# Source file: map_nigeria.R
#
# GPL-3 License
#
# Copyright (C) 2019-2022 Victor Ordu.

globalVariables(c(".", "STATE", "shp.state", "shp.lga"))

# Exported function(s) ---------------------------------------------------------

#' Map of Nigeria
#'
#' Maps of the Federal Republic of Nigeria that are based on the basic
#' plotting idiom utilised by \link[maps:map]{maps:map} and its variants.
#'  
#' @param region A character vector of regions to be displayed. This could be 
#' States or Local Government Areas.
#' @param data An object containing data, principally the variables required to
#' plot in a map.
#' @param x,y Numeric object or factor (or coercible to one). See \emph{Details}.
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
#' @param leg.orient The orientation of the legend i.e. whether horizontal or
#' vertical (deprecated). 
#' @param ... Further arguments passed to \code{\link[maps]{map}}
#' 
#' @details The default value for \code{region} is to print all State boundaries.
#' \code{data} enables the extraction of data for plotting from an object
#' of class \code{data.frame}. Columns containing regions (i.e. States as well as
#' supported sub-national jurisdictions) are identified. The argument also
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
#' @return An object of class \code{maps} containing the data used to draw the
#' map and which can be used for additional calls to \code{\link[maps]{map}} or
#' other similar functions (e.g. \code{graphics::plot.default}).
#' 
#' @import rlang
#' @importFrom cli cli_abort
#' @importFrom cli cli_warn
#' @importFrom graphics legend
#' @importFrom graphics par
#' @importFrom graphics points
#' @importFrom lifecycle deprecate_warn
#' @importFrom lifecycle deprecated
#' @importFrom lifecycle is_present
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
                   leg.orient = deprecated(),
                   ...)
{    ## TODO: Allow this function to accept a matrix e.g. for plotting points
  if (!is.character(region)) {
    msg <- sprintf("Expected a character vector as '%s'.", .arg_str(region))
    
    addmsg <- if (is.data.frame(region))
      "A data frame was passed; did you mean to use 'data' instead?"
    
    cli_abort("{msg} {addmsg}")
  }
  
  if (!is.null(data) && !is.data.frame(data))
    cli_abort(sprintf("A non-NULL input for '%s' must be a data frame",
                 .arg_str(data)))
  
  if (is.data.frame(data) && ncol(data) < 2L)
    cli_abort(
      "Insufficient variables in '{deparse(quote(data))}' to generate a plot"
    )
  
  if (!is.logical(show.neighbours))
    cli_abort("'{.arg_str(show.neighbours))}' should be a logical value")
  
  if (length(show.neighbours) > 1L) {
    cli_warn("{.first_elem_warn(.arg_str(show.neighbours))}")
    show.neighbours <- show.neighbours[1]
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
  mapq <- expr(.mymap(mapdata, regions = region, ...))
  dots <- list(...)
  
  if (use.choropleth) {
    mapq <- expr(.mymap(mapdata, region))
    
    if (!is.null(dots$plot) && !dots$plot)  ## TODO: Reconsider
      mapq$plot <- FALSE
    
    mapq$fill <- TRUE
    
    cParams <- list(
      region = region,
      value = value.x,
      breaks = breaks,
      categories = categories
    )
    
    if (!is.null(data)) {
      region.col <- .region_column_index(data, region)
      
      ## Bet on a two-column data frame that has a
      ## a column with valid regions
      value.x <- 
        if (is.null(value.x) && ncol(data) == 2L)
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
    database <- eval(mapq)
  }, 
  error = function(e) stop(e))
  
  if(!is_null(y) && !.xy_within_bounds(database, x, y))
    cli_abort("Coordinates are out of bounds of the map")
  
  if ("plot" %in% names(dots) && isFALSE(dots$plot))
    return(database)
  
  if (show.text) {
    txt <- country_name()
    
    if (!identical(region, country_name())) {
      # Account for multi-polygonic regions
      # TODO: Scope this to parent environment
      rgxRegions <-
        function(x, nms) {
          rgx <- paste0("^", x, "(\\:\\d*)?$")
          grep(rgx, nms, value = TRUE)
        }
      
      is <- lapply(region, rgxRegions, nms = database$name)
      txt <- unlist(is)
      lbl <- .adjust_labels(txt)
      cex <- .set_text_size(dots$cex)
    }
  }
  
  ## Annotate
  title(main = title, sub = caption)
  
  if (use.choropleth) {
    
    if (is.null(categories))
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
      
      leg.title <- if (is.null(data))
        deparse(substitute(x))
      else
        value.x
    }
    
    if (lp$show) {
      legend(
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
    points(x, y) 
  }
  
  invisible(database)
}







# Internal helper function(s) ---------------------------------------------------
# Creates the map to be plotted
# @param sfdata An objecct of class 'sf'
# @param region An object of class 'regions'
# @param plot If FALSE, the 'sf' object is returned without plotting
# @param ... Arguments passed on to internal methods
#' @import sf
.mymap <- function(sfdata, regions, plot = TRUE, ...)
{
  stopifnot(exprs = {
    inherits(sfdata, "sf")
    is.logical(plot)
  })
  
  if (!plot)
    return(sfdata)
  
  if (!inherits(regions, "regions")) {
    plot(st_geometry(sfdata), ...)
    return()
  }
  
  namefield <- .get_shpfileprop_element(regions, "namefield")
  plot(st_geometry(sfdata, namefield), ...)
}






## Processes character input, presumably States, and when empty
## character vector, provide all the States as a default value.
.process_region_params <- function(x, ...)
{
  stopifnot(is.character(x))
  len <- length(x)
  
  if (len == 0L)
    return(states(all = TRUE))
  
  if (!(all(is_state(x)) || all(is_lga(x)))) {
    
    if (len > 1L) {
      cli::cli_abort(
        "One or more elements of '{deparse(substitute(x))}' is not a Nigerian region",
        ...)
    }
    else if (isFALSE(identical(x, country_name()))) {
      cli::cli_abort(
        "Single inputs for '{deparse(substitute(x))}' only support the value '{country_name()}'",
        ...
      )
    }
  }
  
  x
}




# Makes sure that the elements required for making a choropleth map are available. 
# These are:
# - A data frame with a value and region column identified
# - A 2-column data frame with one column of regions
# - A region and value as separate vectors
#
#' @importFrom rlang as_name
#' @importFrom rlang enexpr
#' @importFrom rlang is_null
#' @importFrom rlang is_symbol
.validate_choropleth_params <- function(val = NULL, region = NULL, data = NULL)
{   # TODO: Add some verbosity.
  val <- enexpr(val)
  
  ## If 'data' is NULL, then both 'val' and 'region' must be present
  ## and 'region' must have valid States or LGAs
  if (is.null(data)) {
    
    if (is.null(val) || is.null(region))
      return(FALSE)
    
    if (!.all_are_regions(region) && !is.null(val))
      return(FALSE)
    # At this point, we have two valid vectors only
  }
  
  data.has.regions <- FALSE
  
  if (is.data.frame(data)) {
    index <- .region_column_index(data)
    data.has.regions <- as.logical(index)
    
    # Once identified, the regions in the data frame are
    # to replace those in the original variable. Since this
    # function is designed to return a boolean value, a 
    # super-assignment is used to effect the change.
    if (data.has.regions) {
      r <- data[[index]]
      assign(deparse(substitute(region)), r, envir = parent.frame())
    }
  }
  else if (!is.null(data))
    cli::cli_warn("'{.arg_str(data)}' is invalid for choropleths but was ignored")
  
  ## If 'region' is NULL, it must be found automatically in 'data'
  if (is.null(region)) {
    
    if (isFALSE(data.has.regions))
      return(FALSE)
    
    region <- character()
  }
  
  if (!.all_are_regions(region))
    return(FALSE)
  
  ## If 'val' is null, it must exist in 'data', but can only be
  ## deduced if 'data' has only 2 columns and the other column is 
  ## confirmed to contain strings representing regions (i.e. States
  ## or LGAs).
  if (is.null(val)) {
    if (is.null(data))
      return(FALSE)
    
    if (ncol(data) > 2L)
      return(FALSE)
    
    if (isFALSE(.all_are_regions(region)) && isFALSE(data.has.regions))
      return(FALSE)
  }
  
  if (!is.null(val)) {
    
    if (is.data.frame(data)) {
      
      if (is_symbol(val) && isFALSE(as_name(val) %in% names(data))) {
        cli::cli_abort("The column '{(.arg_str(val))}'
                       does not exist in '{(.arg_str(data))}'")
      }
    }
  }
  
  TRUE
}





## S3 Class and methods for internal use:
.get_map_data <- function(x)
  UseMethod(".get_map_data")


#' @import mapdata
.get_map_data.default <- function(x) 
{
  if (is.factor(x))
    x <- as.character(x)
  
  stopifnot(is.character(x))
  ngstr <- "Nigeria"
  
  if (identical(x, ngstr)) {
    mapdata <- maps::map("mapdata::worldHires", ngstr, plot = FALSE)
    return(sf::st_as_sf(mapdata))
  }
  
  if ((length(x) == 1L && (x %in% .lgas_like_states())) || 
      all(is_state(x)))
    return(.get_map_data(states(x)))
  
  .get_map_data(lgas(x))
}




.get_map_data.lgas <- function(x)
{
  statename <- attr(x, 'State')

  if (length(statename) > 1L)
    cli::cli_abort("LGA-level maps for adjoining States are not yet supported")
  
  full.spo <- .get_shpfileprop_element(x, "spatialObject")
  
  if (isFALSE(is.null(statename))) {
    statelgas <- lgas(statename)
    return(.subset_spatial_by_region(full.spo, statelgas))
  }
  
  if (length(x) < length(lgas()))
    return(.subset_spatial_by_region(full.spo, x))
  
  full.spo
}



.get_map_data.states <- function(x)
{
  spo <- .get_shpfileprop_element(x, "spatialObject")
  .subset_spatial_by_region(spo, x)
}



# Subsets the spatial object when only a select number of
# regions are about to be plotted in the map
# @param spatialobject The spatialObject, which originally is an element
# of the ShapefileProps objects loaded by the package
# @param regions A regions object e.g. states, lgas
.subset_spatial_by_region <- function(spatialobject, regions) 
{
  stopifnot(exprs = {
    inherits(spatialobject, "sf")
    inherits(regions, "regions")
  })
  # Because of duplicated LGA names, when dealing with an `lgas` object
  # first subset the spatial object by its State
  if (inherits(regions, "lgas")) {
    state <- attr(regions, "State")
    spatialobject <- spatialobject[spatialobject$STATE == state, ]
  }
  reg.rgx <- paste0(regions, collapse = "|")
  reg.col <- .get_shpfileprop_element(regions, "namefield")
  reg.index <- grep(reg.rgx, spatialobject[[reg.col]])
  spatialobject[reg.index, ]
}




# Extracts an element of the ShapefileProps internal object by name
# @param regiontype A character vector of length 1 stating the type of region
# @param element A character vector of length 1 naming the element extracted
.get_shpfileprop_element <- function(region, element)
{
  stopifnot(inherits(region, "regions"), length(element) == 1L)
  suff <- sub("(.)(s$)", "\\1", class(region)[1])
  shpfileprop <- paste("shp", suff, sep = ".")
  getElement(object = get(shpfileprop), name = element)
}




# States that are also the names of LGAs
.lgas_like_states <- function()
{
  c("Bauchi",
    "Ebonyi",
    "Ekiti",
    "Gombe",
    "Katsina",
    "Kogi",
    "Nasarawa")
}


# # For possible export later
# .shpLayer <- function(level) {
#   
#   if (level == 'state')
#     "nga_admbnda_adm1_osgof_20161215"
#   else if (level == 'lga')
#     "new_lga_nigeria_2003"
#   # "NIGERIA_LGA"
#   # "Nigeria_census_2006_WGS84"
#   else
#     stop("An appropriate layer is not avaiable")
# }






## Find the index number for the column housing the region names
## used for drawing a choropleth map
#' @importFrom rlang abort
#' @importFrom rlang warn
.region_column_index <- function(dt, s = NULL)
{
  stopifnot(is.data.frame(dt))
  
  ## Checks if a column has the names of States, returning TRUE if so.
  .fx <- function(x) {
    
    if (is.factor(x))    # TODO: Earmark for removal
      x <- as.character(x)
    
    ret <- FALSE
    
    if (is.character(x)) {
      ret <- .all_are_regions(x)
      
      # TODO: apply a ?restart here when there are misspelt States
      # and try to fix them automatically and then apply the function
      # one more time. Do so verbosely.
      if (!ret && .some_are_regions(x))
        cli::cli_warn("Misspelt region(s) in the dataset")
    }
    ret
  }
  
  n <- vapply(dt, .fx, logical(1))
  
  if (is.null(s))
    s <- states()
  
  if (!sum(n))
    cli::cli_abort("No column with elements in '{deparse(substitute(dt))}'.")
  
  if (sum(n) > 1)
    cli::cli_warn("Multiple columns have regions, so the first was used")
  
  which(n)[1]
}




.prep_choropleth_opts <-
  function(map, opts, col = NULL, ...)
  {
    # TODO: Set limits for variables and brk
    # TODO: Accept numeric input for col
    stopifnot(inherits(map, 'sf'))
    
    if(!.assert_list_elements(opts))
      cli::cli_abort(
        "One or more inputs for generating choropleth options are invalid"
      )
    
    brks <- opts$breaks
    
    df <-
      data.frame(
        region = opts$region,
        value = opts$value,
        stringsAsFactors = FALSE
      )
    
    df$cat <- .create_categorized(df$value, brks)
    cats <- levels(df$cat)
    colrange <- .process_colouring(col, length(cats))
    
    # At this point, our value of 
    # interest is definitely a factor
    df$ind <- as.integer(df$cat)
    df$color <- colrange[df$ind]
    # rgx <- "(^.+)(:.+$)"
    # indexMultiPolygons <- grep(rgx, map$names)
    # mapregions <- sub(rgx, "\\1", map$names)
    # m <- mapregions[indexMultiPolygons]
    # m <-  m[!duplicated(m)]
    # mapregions <- mapregions[-indexMultiPolygons]
    # mapregions <- c(mapregions, m)
    # 
    # if (nrow(df) < length(mapregions))
    #   mapregions <- mapregions[mapregions %in% df$region]
    
    # new.ind <- order(as.character(df$region), mapregions)
    # ord.df <- df[new.ind, ]    # This is why a data frame was made
    # colors <- .reassign_colours(map$names, ord.df$region, ord.df$color, ...)
    
    list(# colors = colors,
         scheme = colrange,
         bins = cats)
  }






.assert_list_elements <- function(x) {
  stopifnot(c('region', 'value', 'breaks') %in% names(x))
  
  region.valid <- .all_are_regions(x$region)
  v <- x$value
  value.valid <- is.numeric(v) || is.factor(v) || is.character(v)
  c <- x$categories
  
  cat.valid <-
    if (!is.null(c))
      is.character(c) || is.factor(c)
  else
    TRUE
  
  all(region.valid, value.valid, cat.valid)
}








# Creates a  categorised variable from its inputs if not already a factor
# and is to be used in generating choropleth maps
#' @importFrom cli cli_abort
.create_categorized <- function(val, brks = NULL, ...)
{
  if (is.character(val))
    val <- as.factor(val)
  
  if (is.factor(val)) {
    
    if (length(levels(val)) >= 10L)
      cli_abort("Too many categories")
    
    return(val)
  }
  
  if (!is.numeric(val)) {
    msg <- paste(sQuote(typeof(val)), "is not a supported type")
    cli_abort(msg)
  }
  
  if (is.null(brks))
    cli_abort(paste("Breaks were not provided for the", 
                    "categorization of a numeric type"))
  
  rr <- range(val)
  
  if (rlang::is_scalar_integer(brks))
    brks <- seq(rr[1], rr[2], diff(rr) / brks)
  
  if (rr[1] < min(brks) || rr[2] > max(brks))
    cli_abort("Values are out of range of breaks")
  
  cut(val, brks, include.lowest = TRUE)
}





#' @importFrom cli cli_abort
.process_colouring <- function(col = NULL, n, ...)
{
  .DefaultChoroplethColours <- getOption('choropleth.colours') # set in zzz.R
  
  if (is.null(col))
    col <- .DefaultChoroplethColours[1]
  
  if (is.numeric(col)) {
    default.pal <- .get_R_palette()
    all.cols <- sub("(green)(3)", "\\1", default.pal)
    all.cols <- sub("gray", "grey", all.cols)
    
    if (!col %in% seq_along(all.cols))
      cli_abort("'color' must range between 1L and {length(all.cols)}L")
    
    col <- all.cols[col]
  }
  
  among.def.cols <- col %in% .DefaultChoroplethColours
  in.other.pal <-
    !among.def.cols &&
    (col %in% rownames(RColorBrewer::brewer.pal.info))
  
  pal <- if (!among.def.cols) {
    if (!in.other.pal)
      cli_abort("'{col}' is not a supported colour or palette")
    
    col
  }
  else
    paste0(tools::toTitleCase(col), "s")
  
  RColorBrewer::brewer.pal(n, pal)
}




.get_R_palette <- function()
{
  if (getRversion() < as.numeric_version('4.0.0'))
    return(grDevices::palette())
  
  grDevices::palette('R3')
  pal <- grDevices::palette()
  grDevices::palette('R4')
  pal
}




# Reassigns colours to polygons that refer to similar regions i.e. duplicated
# polygon, ensuring that when the choropleth is drawn, the colours are 
# properly applied to the respective regions and not recycled.
#' @importFrom cli cli_abort
.reassign_colours <- 
  function(names, all.regions, in.colours, excl.region = NULL, excl.col = NULL)
  {
    stopifnot(exprs = {
      is.character(names)
      all(is_state(all.regions))
      .isHexColor(in.colours)
    })
    
    out.colours <- new.names <- rep(NA, length(names))
    
    for (i in seq_along(all.regions)) {
      regx <- .regex_duplicated_poly(all.regions[i])
      ind <- grep(regx, names)
      out.colours[ind] <- in.colours[i]
      new.names[ind] <- sub(regx, "\\1", names[ind])[1]
    }
    
    ## Take care of situations where a region is not to be part of
    ## the choropleth and should be given an 'off-colour'
    if (!is.null(excl.region)) {
      off.color <- "grey"
      
      if (!is.null(excl.col)) {
        
        if (length(excl.col) > 1L)
          cli_abort("Only one colour can be used to denote regions excluded
                     from the choropleth colouring scheme")
        
        if (!is.character(excl.col))
          cli_abort("Colour indicators of type '{typeof(excl.col)}'
                    are not supported")
        
        if (!excl.col %in% grDevices::colours())
          cli_abort("The colour used for excluded regions must be valid
                     i.e. an element of the built-in set 'colours()'")
        
        off.color <- excl.col
      }
      
      excluded <- match(excl.region, new.names)
      out.colours[excluded] <- off.color
    }
    
    structure(out.colours, names = new.names)
  }






# Provides a regex pattern for checking polygons for jurisdictions that
# are matched more than once e.g. foo:1, foo:2, bar:1, bar:2, bar:3
# TODO: Deprecate
.regex_duplicated_poly <- function(x)
{
  stopifnot(is.character(x))
  paste0("^(", paste0(x, collapse = "|"),")(\\:\\d)?$")
}




.isHexColor <- function(x) 
{
  if (!is.character(x)) return(FALSE)
  all(grepl("^#", x), nchar(x) == 7L)
}





# Rejigs text that is used for labeling a region that has more than 1 polygon
# TODO: Quite a bit of hard-coding was used (v. 0.1.0) and should be reviewed
.adjust_labels <- function(x) 
{
  stopifnot(is.character(x))
  x <- vapply(
    x,
    FUN.VALUE = character(1L),
    FUN = function(l)
      sub("\\:\\d*$", "", l)
  )
  
  dd <- data.frame(state = c("Akwa Ibom", "Cross River"),
                   poly = c(2, 4))
  
  ss <- dd[["state"]]
  
  if (all(grepl(paste0(ss, collapse = "|"), x))) {
    .fadj <- function(a, reg, pos) {
      ind <- grep(reg, a)
      finalPos <- ind[pos]
      a[ind] <- ""
      a[finalPos] <- reg
      a
    }
    
    for (i in seq_len(2))
      x <- .fadj(x, ss[i], dd[["poly"]][i])
  }
  
  dup <- which(duplicated(x))
  x[dup] <- character(1L)
  .toggleFct(x, use = "abbrev") # when FCT is labelled
}





# Checks that x and y coordinates are within the bounds of given map
# Note: This check is probably too expensive. Consider passing just the range
# though the loss of typing may make this less reliable down the line
#' @importFrom rlang is_double
.xy_within_bounds <- function(map, x, y)
{ 
  stopifnot(inherits(map, 'map'), is_double(x), is_double(y))
  
  rr <- map$range
  xx <- x >= rr[1] & x <= rr[2]
  yy <- y >= rr[3] & y <= rr[4]
  all(xx, yy)
}




# Returns either a logical(1) or character(n)
.set_legend_text <- function(val)
{
  arg <- "legend.text"
  
  # The default setting of 'legend.text' is to return TRUE
  if (is.null(val))
    return(TRUE)
  
  if (is.logical(val)) {
    if (length(val) > 1L)
      cli::cli_warn(.first_elem_warn(arg))
    
    return(val[1])
  }
  
  if (is.character(val))
    return(val)
  
  cli::cli_abort("'{arg}' must be of type character or logical")
}





.set_legend_params <- function(leg.arg)
{
  result <- .set_legend_text(leg.arg)
  obj <- list(x = 13L, y = 7L, text = NULL, show = TRUE, xpd = NA)
  
  if (is.character(result))
    obj$text <- result
  
  if (is.logical(result))
    obj$show <- result
  
  obj
}





.set_text_size <- function(cex = NULL)
{
  if (is.null(cex)) {
    cex <- 0.75  # default for maps::map.text
    return(cex)
  }
  
  stopifnot(is.numeric(cex))
  cex
}
