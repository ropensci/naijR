# Source file: mapint.R
#
# GPL-3 License
#
# Copyright (C) 2019-2023 Victor Ordu.

# Internal helper function(s) for plotting Nigeria maps

# Creates the map to be plotted
# @param sfdata An objecct of class 'sf'
# @param region An object of class 'regions'
# @param plot If FALSE, the 'sf' object is returned without plotting
# @param ... Arguments passed on to internal methods
.mymap <- function(sf, regions, plot = TRUE, col = NA, ...)
{
  stopifnot(exprs = {
    inherits(sf, "sf")
    is.logical(plot)
  })
  
  if (plot) {
    geom <- if (inherits(regions, "regions")) {
      namefield <- .get_shpfileprop_element(regions, "namefield")
      sf::st_geometry(sf, namefield)
    } 
    else
      sf::st_geometry(sf)
    
    plot(geom, col = col, ...)
  }
  sf
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
    str <-  deparse(substitute(x))
    
    if (len > 1L)
      cli::cli_abort(
        "One or more elements of '{str}' is not a Nigerian region", 
        ...
      )
    else if (isFALSE(identical(x, country_name())))
      cli::cli_abort(
        "Single inputs for '{str}' only support the value '{country_name()}'",
        ...
      )
  }
  
  x
}




# Makes sure that elements required for making a choropleth map are available. 
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
    cli::cli_warn(
      "'{arg_str(data)}' is invalid for choropleths but was ignored"
    )
  
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
        cli::cli_abort("The column '{(arg_str(val))}'
                       does not exist in '{(arg_str(data))}'")
      }
    }
  }
  
  TRUE
}




## S3 Class and methods for internal use:
.get_map_data <- function(x)
  UseMethod(".get_map_data")



.get_map_data.default <- function(x) 
{
  if (is.factor(x))
    x <- as.character(x)
  
  stopifnot(is.character(x))
  ngstr <- "Nigeria"
  
  if (identical(x, ngstr)) {
    # nolint start:
    # Setting `fill` to TRUE solved problematic rendering of the polygons.
    # See https://gis.stackexchange.com/questions/230608/creating-an-sf-object-from-the-maps-package
    # nolint end
    map.data <- 
      maps::map("mapdata::worldHires", ngstr, plot = FALSE, fill = TRUE)
    map.data <- sf::st_as_sf(map.data)
    sfc <- "sf_column"
    old.geom.name <- attr(map.data, sfc)
    pos <- match(old.geom.name, names(map.data))
    new.geom.name <- "geometry"
    names(map.data)[pos] <- new.geom.name
    attr(map.data, sfc) <- new.geom.name
    return(map.data)
  }
  region.data <- lgas(x)
  single.like.state <- length(x) == 1L && (x %in% lgas_like_states())
  
  if (single.like.state || all(is_state(x)))
     region.data <- states(x)
    
  .get_map_data(region.data)
}




.get_map_data.lgas <- function(x)
{
  statename <- attr(x, 'State')
  
  if (length(statename) > 1L)
    cli::cli_abort(
      "LGA-level maps for adjoining States are not yet supported"
    )
  
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
    
    if (!.assert_list_elements(opts))
      cli::cli_abort(
        "One or more inputs for generating choropleth options are invalid"
      )
    
    if (anyDuplicated(opts$region)) {
      if (all(is_state(opts$region)))
        cli::cli_abort(
          "Data cannot be matched with map. Aggregate them by States"
        )
      
      if (all(is_lga(opts$region)))
        cli::cli_warn(
          "Duplicated LGAs found, but may or may not need a review"
        )
    }
    
    brks <- opts$breaks
    df <- data.frame(region = opts$region, value = opts$value)
    
    df$cat <- .create_categorized(df$value, brks)
    cats <- levels(df$cat)
    colrange <- .process_colouring(col, length(cats))
    
    # At this point, our value of 
    # interest is definitely a factor
    df$ind <- as.integer(df$cat)
    df$color <- colrange[df$ind]
    colors <- .reassign_colours(df$region, df$color, ...)
    list(colors = colors, scheme = colrange, bins = cats)
  }




# Reassigns colours to polygons that refer to similar regions i.e. duplicated
# polygon, ensuring that when the choropleth is drawn, the colours are 
# properly applied to the respective regions and not recycled.
#' @importFrom cli cli_abort
.reassign_colours <- 
  function(all.regions, polygon.colors, excl.region = NULL, excl.col = NULL)
  {
    stopifnot(is.character(all.regions), .isHexColor(polygon.colors))
    
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
      excluded <- which(all.regions %in% excl.region)
      polygon.colors[excluded] <- off.color
    }
    structure(polygon.colors, names = all.regions)
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




# Checks that x and y coordinates are within the bounds of given map
# Note: This check is probably too expensive. Consider passing just the range
# though the loss of typing may make this less reliable down the line
#' @importFrom rlang is_double
.pts_within_bounds <- function(map, x, y)
{ 
  stopifnot(inherits(map, 'sf'), is_double(x), is_double(y))
  
  rr <- sf::st_bbox(map)
  xx <- x >= rr[1] & x <= rr[3]
  yy <- y >= rr[2] & y <= rr[4]
  all(xx, yy)
}




# Returns either a logical(1) or character(n)
.set_legend_text <- function(val)
{
  # The default setting of 'legend.text' is to return TRUE
  if (is.null(val))
    return(TRUE)
  
  arg <- "legend.text"
  
  if (!is.character(val) && !is.logical(val))
    cli::cli_abort("'{arg}' must be of type character or logical")
  
  if (is.logical(val)) {
    if (length(val) > 1L)
      cli::cli_warn(first_elem_warn(arg))
    return(val[1])
  }
  val
}




.set_legend_params <- function(leg.arg)
{
  stopifnot(is.character(leg.arg) || is.logical(leg.arg) || is.null(leg.arg))
  result <- .set_legend_text(leg.arg)
  obj <- list(x = 13L, y = 7L, text = NULL, show = TRUE, xpd = NA)
  
  if (is.character(result))
    obj$text <- result
  
  if (is.logical(result))
    obj$show <- result
  
  obj
}




.set_text_size <- function(cex)
{
  if (is.null(cex))
    cex <- 0.7
  
  if (!is.numeric(cex))
    cli::cli_abort("'cex' is not of class 'numeric'")
  
  cex
}




.get_point_coords <- function(sfobj) {
  stopifnot(inherits(sfobj, "sf"))
  geom <- sf::st_centroid(sfobj$geometry)
  pointstr <- sf::st_as_text(geom)
  numstr <- sub("(POINT \\()(.+)(\\))", "\\2", pointstr)
  .extract_coords_from_str(numstr)
}




.extract_coords_from_str <- function(str) {
  f <- function(rgx, pos) {
    pos <- paste0("\\", pos)
    as.double(sub(rgx, pos, str))
  }
  x <- f("(^.+)( .+$)", 1L)
  y <- f("(^.+ )(.+$)", 2L)
  cbind(x, y)
}
