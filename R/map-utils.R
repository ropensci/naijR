# Source file: map-utils.R
#
# GPL-3 License
#
# Copyright (C) 2019-2026 Victor Ordu.

# Internal helper function(s) for plotting Nigeria maps
## Checks the parameters supplied to the mapping function.
.checkParams <- function(region, data, show.neighbours, fun) {
  if (!is.character(region)) {
    msg <- sprintf("Expected a character vector as '%s'.", fun(region))
    addmsg <- if (is.data.frame(region)) {
      "A data frame was passed. Did you mean to use the 'data' argument?"
    }
    cli_abort("{msg} {addmsg}")
  }
  if (!is_null(data) && !is.data.frame(data)) {
    cli_abort(sprintf("A non-NULL input for '%s' must be a data frame",
                fun(data)))
  }
  if (is.data.frame(data) && ncol(data) < 2L) {
    cli_abort(
      "Insufficient variables in '{deparse(quote(data))}' to generate a plot"
    )
  }
  if (!is.logical(show.neighbours)) {
    cli_abort("'{fun(show.neighbours)}' should be a logical value")
  }
  if (length(show.neighbours) > 1L) {
    show.neighbours <- show.neighbours[1]
    cli_warn("{first_elem_warn(fun(show.neighbours))}")
  }
  if (show.neighbours) {
    cli::cli_abort("Display of neighbouring regions is temporarily disabled")
  }
  show.neighbours
}

# Creates the map to be plotted
# @param sfdata An objecct of class 'sf'
# @param plot If FALSE, the 'sf' object is returned without plotting
# @param col Passed to the `col` argument of `plot`
# @param ... Arguments passed on to internal methods
#' @importFrom sf st_geometry
.mymap <- function(sfdata, plot, ...)
{
  stopifnot(exprs = {
    inherits(sfdata, "sf")
    is.logical(plot)
    !is.na(plot)
  })
  if (plot) {
    plot(sf::st_geometry(sfdata), ...)
  }
  sfdata  # always return this object
}




## Processes character input, presumably regions, and when a zero-length
## character vector, provide all the States as a default value.
#' @importFrom rlang as_name
#' @importFrom rlang enexpr
.process_region_params <- function(x, ...)
{
  xarg <- enexpr(x) # parse symbol prior to evaluation
  stopifnot(is.character(x))
  len <- length(x)
  if (len == 0L) {  # when the default arg is
    return(states(all = TRUE))
  }
  if (!.all_are_regions(x)) {
    # str <- deparse(substitute(x))
    str <-  as_name(xarg)
    if (len > 1L) {
      cli::cli_abort(
        "One or more elements of '{str}' is not a Nigerian region", 
        ...
      )
    }
    if (isFALSE(identical(x, country_name()))) {
      cli::cli_abort(
        "Single inputs for '{str}' only support the value '{country_name()}'",
        ...
      )
    }
  }
  x
}




# Enables a decision on whether to draw a choropleth map
# It does this by checking the kind of arguments that were
# passed into the mapping function and then carrying out 
# some validation checks.
# Returns either TRUE or FALSE
#' @importFrom rlang eval_tidy
#' @importFrom rlang is_null
#' @importFrom rlang is_symbol
.check_choropleth_use <- function(region, data, x, y) {
  if (is_null(x) || is_symbol(x)) {
    x <- enexpr(x)

    return(.validate_choropleth_params(region, data, !!x))
  }
  if (!is_null(y)) {
    return(FALSE)
  }
  x <- eval_tidy(x)
  .validate_choropleth_params(region, data, x)
}



# Makes sure that elements required for making a choropleth map are available. 
# These are the possible scenarios where this condition is fulfilled:
# - A data frame with a value and region column identified via arguments
# - A 2-column data frame with one column of regions and values deduced
# - A region and value data structure as separate atomic vectors
#
#' @importFrom rlang as_name
#' @importFrom rlang enexpr
#' @importFrom rlang is_null
#' @importFrom rlang is_symbol
.validate_choropleth_params <- function(region, data, x)
{
  val <- enexpr(x)
  region <- enexpr(region)
  if (is_null(data) && is_null(val)) {
    return(FALSE)
  }
  data.has.regions <- FALSE
  if (!is_null(data)) {
    index <- .region_column_index(data)
    data.has.regions <- as.logical(index)
    if (data.has.regions) {
      assign(as_name(region), data[[index]], envir = parent.frame())
    }
  }
  if (is_null(val)) {
    no.valid.df <- is_null(data) || ncol(data) > 2L
    if (no.valid.df || !data.has.regions) {
      return(FALSE)
    }
  }
  else {
    if (is.data.frame(data)) {
      if (is_symbol(val) && isFALSE(as_name(val) %in% names(data))) {
        cli::cli_abort("The column '{(arg_str(val))}'
                       does not exist in '{(arg_str(data))}'")
      }
    }
  }
  TRUE  # NB: Also when no data frame input but x is a vector
}




## S3 Class and methods for internal use:
.get_map_data <- function(x)
  UseMethod(".get_map_data")


#' @import mapdata 
#' @importFrom sf st_as_sf
.get_map_data.default <- function(x) 
{
  if (is.factor(x))
    x <- as.character(x)
  stopifnot(is.character(x))
  if (identical(x, "Nigeria")) {
    # nolint start:
    # Setting `fill` to TRUE solved problematic rendering of the polygons.
    # See https://gis.stackexchange.com/questions/230608/creating-an-sf-object-from-the-maps-package
    # nolint end
    map.data <- 
      maps::map("mapdata::worldHires", "Nigeria", plot = FALSE, fill = TRUE)
    map.data <- sf::st_as_sf(map.data)
    old.geom.name <- attr(map.data, "sf_column")
    pos <- match(old.geom.name, names(map.data))
    names(map.data)[pos] <- "geometry"
    attr(map.data, "sf_column") <- "geometry"
    return(map.data)
  }
  region.data <- lgas(x)
  single.like.state <- length(x) == 1L && (x %in% lgas_like_states())
  if (single.like.state || all(is_state(x))) {
     region.data <- states(x)
  }
  .get_map_data(region.data)
}




.get_map_data.lgas <- function(x)
{
  statename <- attr(x, 'State')
  if (length(statename) > 1L) {
    cli::cli_abort(
      "LGA-level maps for adjoining States are not yet supported"
    )
  }
  full.spo <- .get_shpfileprop_element(x, "spatialObject")
  if (isFALSE(is.null(statename))) {
    statelgas <- lgas(statename)
    return(.subset_spatial_by_region(full.spo, statelgas))
  }
  if (length(x) < length(lgas())) {
    return(.subset_spatial_by_region(full.spo, x))
  }
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
.region_column_index <- function(dt, state = NULL)
{
  stopifnot(is.data.frame(dt))
  ## Checks if a column has the names of States, returning TRUE if so.
  .fx <- function(x) {
    if (is.factor(x)) {    # TODO: Earmark for removal
      x <- as.character(x)
    }
    ret <- FALSE
    if (is.character(x)) {
      ret <- .all_are_regions(x)
      # TODO: apply a ?restart here when there are misspelt States
      # and try to fix them automatically and then apply the function
      # one more time. Do so verbosely.
      if (!ret && .some_are_regions(x)) {
        cli::cli_warn("Misspelt region(s) in the dataset")
      }
    }
    ret
  }
  n <- vapply(dt, .fx, logical(1))
  if (is.null(state)) {
    state <- states()
  }
  if (!sum(n)) {
    cli::cli_abort("No column with elements in '{deparse(substitute(dt))}'.")
  } 
  index <- which(n)
  if (length(index) > 1) {
    index <- index[1]
    cli::cli_warn("Multiple columns have regions, so the first was used")
  }
  index
}



#' @importFrom rlang enexpr
.get_choropleth_opts <- 
  function(mapobj, data, region, x, breaks, categories, 
    col, excluded, exclude.fill) {
  # x <- enexpr(x)
  cpleth.inputs <- list(
    region = region,
    breaks = breaks,
    categories = categories
  )
  if (!is_null(data)) {
    region.col <- .region_column_index(data, region)
    datacolname <- if (is_null(x) && ncol(data) == 2L) {
      names(data)[-region.col]
    }
    else {
      as_name(x)
    }
    cpleth.inputs$value <-  data[[datacolname]]
    cpleth.inputs$region <- data[[region.col]]
  }
  else {
    cpleth.inputs$value <- eval_tidy(x)
  }
  .prep_choropleth_opts(
    mapobj,
    cpleth.inputs,
    col,
    excluded,
    exclude.fill
  )
}


.prep_choropleth_opts <- function(map, opts, col = NULL, ...) {
  # TODO: Set limits for variables and brk
  # TODO: Accept numeric input for col
  stopifnot(inherits(map, 'sf'))
  if (!.assert_list_elements(opts)) {
    cli::cli_abort("One or more inputs for generating choropleth options are invalid")
  }
  if (anyDuplicated(opts$region)) {
    if (all(is_state(opts$region))) {
      cli::cli_abort("Data cannot be matched with map. Aggregate them by States")
    }
    if (all(is_lga(opts$region))) {
      cli::cli_warn("Duplicated LGAs found, but may or may not need a review")
    }
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
  list(colors = colors,
       scheme = colrange,
       bins = cats)
}

# Reassigns colours to polygons that refer to similar regions i.e. duplicated
# polygon, ensuring that when the choropleth is drawn, the colours are 
# properly applied to the respective regions and not recycled.
#' @importFrom cli cli_abort
.reassign_colours <- function(all.regions,
                              polygon.colors,
                              excl.region = NULL,
                              excl.col = NULL) {
  stopifnot(is.character(all.regions), .isHexColor(polygon.colors))
  if (!is.null(excl.region)) {
    off.color <- "grey"
    if (!is.null(excl.col)) {
      if (length(excl.col) > 1L) {
        cli_abort(
          "Only one colour can be used to denote regions excluded
                     from the choropleth colouring scheme"
        )
      }
      if (!is.character(excl.col)) {
        cli_abort("Colour indicators of type '{typeof(excl.col)}'
                    are not supported")
      }
      if (!excl.col %in% grDevices::colours()) {
        cli_abort(
          "The colour used for excluded regions must be valid
                     i.e. an element of the built-in set 'colours()'"
        )
      }
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
  cat.valid <- if (!is.null(c)) {
      is.character(c) || is.factor(c)
  }
  else {
    TRUE
  }
  all(region.valid, value.valid, cat.valid)
}




# Creates a  categorised variable from its inputs if not already a factor
# and is to be used in generating choropleth maps
#' @importFrom cli cli_abort
.create_categorized <- function(val, brks = NULL, ...)
{
  if (is.character(val)) {
    val <- as.factor(val)
  }
  if (is.factor(val)) {
    if (nlevels(val) >= 10L) {
      cli_abort("Too many categories")
    }
    return(val)
  }
  if (!is.numeric(val)) {
    msg <- paste(sQuote(typeof(val)), "is not a supported type")
    cli_abort(msg)
  }
  if (is.null(brks)) {
    cli_abort(paste("Breaks were not provided for the", 
                    "categorization of a numeric type"))
  }
  rr <- range(val)
  if (rlang::is_scalar_integer(brks)) {
    brks <- seq(rr[1], rr[2], diff(rr) / brks)
  }
  if (rr[1] < min(brks) || rr[2] > max(brks)) {
    cli_abort("Values are out of range of breaks")
  }
  cut(val, brks, include.lowest = TRUE)
}




#' @importFrom cli cli_abort
.process_colouring <- function(col = NULL, n, ...)
{
  .DefaultChoroplethColours <- getOption('choropleth.colours') # set in zzz.R
  if (is.null(col)) {
    col <- .DefaultChoroplethColours[1]
  }
  if (is.numeric(col)) {
    default.pal <- .get_R_palette()
    all.cols <- sub("(green)(3)", "\\1", default.pal)
    all.cols <- sub("gray", "grey", all.cols)
    if (!col %in% seq_along(all.cols)) {
      cli_abort("'color' must range between 1L and {length(all.cols)}L")
    }
    col <- all.cols[col]
  }
  among.def.cols <- col %in% .DefaultChoroplethColours
  in.other.pal <-
    !among.def.cols &&
    (col %in% rownames(RColorBrewer::brewer.pal.info))
  pal <- if (!among.def.cols) {
    if (!in.other.pal) {
      cli_abort("'{col}' is not a supported colour or palette")
    }
    col
  }
  else {
    paste0(tools::toTitleCase(col), "s")
  }
  RColorBrewer::brewer.pal(n, pal)
}




.get_R_palette <- function()
{
  if (getRversion() < as.numeric_version('4.0.0')) {
    return(grDevices::palette())
  }
  grDevices::palette('R3')
  pal <- grDevices::palette()
  grDevices::palette('R4')
  pal
}




.isHexColor <- function(x) 
{
  if (!is.character(x)) {
    return(FALSE)
  }
  all(grepl("^#", x), nchar(x) == 7L)
}




# Checks that x and y coordinates are within the bounds of given map
# Note: This check is probably too expensive. Consider passing just the range
# though the loss of typing may make this less reliable down the line
#' @importFrom rlang is_double
#' @importFrom sf st_bbox
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
  if (is.null(val)) {
    return(TRUE)
  }
  arg <- "legend.text"
  if (!is.character(val) && !is.logical(val)) {
    cli::cli_abort("'{arg}' must be of type character or logical")
  }
  if (is.logical(val)) {
    if (length(val) > 1L) {
      cli::cli_warn(first_elem_warn(arg))
    }
    return(val[1])
  }
  val
}




.set_legend_params <- function(text, categories, xcord = 13L, ycord = 7L)
{
  stopifnot(is.character(text) || is.logical(text) || is.null(text))
  stopifnot(exprs = {
    is.integer(xcord)
    is.integer(ycord)
  })
  result <- .set_legend_text(text)
  obj <- list(
    x = xcord, 
    y = ycord,
    text = NULL,
    show = TRUE, 
    xpd = NA,
    categories = categories
  )
  if (is.character(result)) {
    obj$text <- result
  }
  if (is.logical(result)) {
    obj$show <- result
  }
  if (is.character(obj$text)) {
    if (length(obj$categories) != length(obj$text)) {
      cli_abort("Lengths of 'categories' and provided legend do not match")
    }
    obj$categories <- obj$text
  }
  obj
}




.plot_points <- function(sfdata, x, y, ...) {
  st.pts <- sf::st_as_sf(data.frame(x = x, y = y), coords = c("x", "y"))
  sf::st_crs(st.pts) <- sf::st_crs(sfdata)
  arglist <- list(...)
  if_null_1 <- function(arg) if (is_null(arg)) 1 else arg
  suppressWarnings({
    plot(
      st.pts,
      add = TRUE,
      pch = if_null_1(arglist$pch),
      lwd = if_null_1(arglist$lwd),
      lty = if_null_1(arglist$lty)
    )
    sf::st_union(sfdata, st.pts)
  })
}




.show_map_text <- function(mapdata, region, cex) {
  txt <- country_name()
  df.only <- as.data.frame(mapdata) 
  if (inherits(region, "regions")) {
    region.type <- sub("(.+)(s$)", "\\1", class(region)[1])
    shpfileprop <- paste0("shp.", region.type)
    namefield <- get(shpfileprop)$namefield
    txt <- df.only[[namefield]]
    # nocov end
    if (all(is_state(region))) {
      txt <- sub(
        .toggle_fct_format("full"), 
        .toggle_fct_format("abbrev"), 
        txt
      )
    }
  }
  cex <- .set_text_size(cex)
  xycoord <- .get_point_coords(mapdata)
  graphics::text(xycoord[, 'x'], xycoord[, 'y'], labels = txt, cex = cex)
}



.set_text_size <- function(cex)
{
  if (is.null(cex)) {
    cex <- 0.7
  }
  if (!is.numeric(cex)) {
    cli::cli_abort("'cex' is not of class 'numeric'")
  }  
  cex
}



#' @importFrom sf st_centroid
#' @importFrom sf st_as_text
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




# Functions for generating messages ----
country_name <- function()
{
  "Nigeria"
}




#' @importFrom rlang as_name
#' @importFrom rlang enexpr
# NB: The internal function 'arg_str' uses non-standard evaluation
# internally. Thus, care should be taken during any refactoring, so as to 
# ensure that the target objects are parsed correctly
arg_str <- function(arg)
{
  as_name(enexpr(arg))
}




first_elem_warn <- function(arg)
{
  stopifnot(exprs = {is.character(arg) && length(arg) == 1L})
  sprintf("Only the first element of '%s' was used", arg)
}
