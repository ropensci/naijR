# Source file: map-helpers.R
#
# GPL-3 License
#
# Copyright (C) 2019-2022 Victor Ordu.
#
# Internal helper function(s) ---------------------------------------------------

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
  
  if (identical(x, 'Nigeria'))
    return("mapdata::worldHires")
  
  if ((length(x) == 1L && (x %in% .lgas_like_states())) || 
      all(is_state(x)))
    return(.get_map_data(states(x)))
  
  .get_map_data(lgas(x))
}




.get_map_data.lgas <- function(x)
{
  spo <- shp.lga$spatialObject
  statename <- attr(x, 'State')
  
  if (length(statename) > 1L)
    cli::cli_abort("LGA-level maps for adjoining States are not yet supported")
  
  if (!is.null(statename)) {
    statergx <- .regex_subset_regions(statename)
    stateindex <- grep(statergx, spo@data$STATE)
    lgaObj <- spo[stateindex, ]
  }
  else {
    lgaObj <- spo
    
    if (length(x) < length(lgas())) {
      lgargx <- .regex_subset_regions(x)
      lgaindex <- grep(lgargx, spo@data$LGA)
      lgaObj <- spo[lgaindex, ]
    }
  }
  
  maps::SpatialPolygons2map(lgaObj, shp.lga$namefield)
}



.get_map_data.states <- function(x)
{
  spo <- shp.state$spatialObject
  statergx <- .regex_subset_regions(x)
  stateindex <- grep(statergx, spo$admin1Name)
  stateObj <- spo[stateindex, ]
  maps::SpatialPolygons2map(stateObj, shp.state$namefield)
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
    stopifnot(inherits(map, 'map'))
    
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
    rgx <- "(^.+)(:.+$)"
    indexMultiPolygons <- grep(rgx, map$names)
    mapregions <- sub(rgx, "\\1", map$names)
    m <- mapregions[indexMultiPolygons]
    m <-  m[!duplicated(m)]
    mapregions <- mapregions[-indexMultiPolygons]
    mapregions <- c(mapregions, m)
    
    if (nrow(df) < length(mapregions))
      mapregions <- mapregions[mapregions %in% df$region]
    
    new.ind <- order(as.character(df$region), mapregions)
    ord.df <- df[new.ind, ]    # This is why a data frame was made
    colors <- .reassign_colours(map$names, ord.df$region, ord.df$color, ...)
    
    list(colors = colors,
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
    stopifnot({
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
.regex_duplicated_poly <- function(x)
{
  stopifnot(is.character(x))
  paste0("^(", paste0(x, collapse = "|"),")(\\:\\d)?$")
}




.regex_subset_regions <- function(x) {
  stopifnot(is.character(x))
  paste0(x, collapse = "|")
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





## Messages -----------------------------------------------------------------
country_name <- function()
{
  "Nigeria"
}




.arg_str <- function(arg)
{
  deparse(substitute(arg))
}




.first_elem_warn <- function(arg)
{
  stopifnot(is.character(arg) && length(arg) == 1L)
  sprintf("Only the first element of '%s' was used", arg)
}




.next_minor_version <- function()
{
  ">= 0.6.0"
}




.deprec_msg <- function(arg) {
  sprintf("map_ng(%s = )", deparse(substitute(arg)))
}
