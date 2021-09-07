
#' Fix Region Names
#' 
#' Correct any misspelt names of administrative regions i.e. States and LGAs
#' 
#' @details The function will look through a character vector and try to 
#' determine if State or LGA names have been wrongly entered. This presupposes that
#' the atomic vector is of type \code{character}. It does not test any missing
#' values in the vector, leaving them untouched.
#' 
#' @param x An S3 object of class \code{states} or \code{lgas}. For 
#' \code{fix_region.default}, a character vector can be passed but only
#' that for States will be interpretable.
#' @param ... Arguments passed to methods.
#' 
#' @return The transformed object. If all names are correct, the object is
#' returned unchanged.
#' @export
fix_region <- function(x, ...)
  UseMethod("fix_region")




#' @rdname fix_region
#' 
#' @export
fix_region.states <- function(x, ...)
{
  ## Process possible FCT values
  abbrFCT <- .fctOptions("abbrev")
  fullFCT <- .fctOptions("full")
  sumFct <- sum(.fctOptions() %in% x)
  if (sumFct == 2)
    x <- sub(abbrFCT, fullFCT, x)
  if (sumFct == 1) {
    i_abbr <- grep(abbrFCT, x)   # TODO: Warisdis?
    i_full <- grep(fullFCT, x)
  }
  i <- grep(sprintf("^%s$", abbrFCT), x, ignore.case = TRUE)
  ss <- states()
  if (length(i)) 
    ss <- sub(fullFCT, abbrFCT, ss)
  
  x <- .fixRegionInternal(x, ss)
  x[i] <- fullFCT
  if (!inherits(x, "states"))
    x <- suppressWarnings(states(x))
  invisible(x)
}





#' @rdname fix_region
#' 
#' @param interactive Logical. When \code{TRUE}, the function prompts the user
#' to interactively select the correct LGA names from a list of available
#' options.
#' 
#' @export
fix_region.lgas <- function(x, interactive = FALSE, ...)
{
  vals <- .fixRegionInternal(x, lgas(), ...)
  if (interactive) {
    ans <- readline("Do you want to repair interactively? (Y/N): ")
    if (tolower(ans) == "y")
      vals <- .fixLgasInteractively(vals)
  }
  attributes(vals) <- NULL
  invisible(vals)
}






#' @rdname fix_region
#' @importFrom magrittr %>%
#' 
#' @export
  fix_region.default <- function(x, ...)
{ ## TODO: Provide verbosity by reporting on fixed items?
  if (!is.character(x))
    stop("'x' is not a character vector")
  empty <- grepl("^$", x)
  if (length(empty) > 0L && all(empty))  ## diff character(0) and character(1)
    stop("'x' only has empty strings")
  if (any(empty))
    warning("Tried to fix empty strings - may produce errors")
  if (all(is.na(x)) || !length(x)) {
    warning("'x' has length 0L or only missing values", call. = FALSE)
    return(x)
  }
  region <- if (any(is_lga(x)) && (!any(x %in% .synonymRegions())))
    suppressWarnings(lgas(x))
  else
    suppressWarnings(states(x))
  zz <- region %>% 
    fix_region %>% 
    as.character
  message(
    "Use fix_region(states(x)) or fix_region(lgas(x) instead for reliable fix"
  )
  invisible(zz)
}





.fixRegionInternal <- function(x, region, ...)
{
  ## Approximately matches a string on a list of regions i.e States or LGAs.
  ## @param str A string to be matched
  ## @param regions The values to be matched against, specifically from regions
  ##
  ## @return A vector of the same length as str with the approximetly
  ## matched value, which in the case of misspelling should be the correct one.
  ##
  ## This function is mapped to a vector of states/LGAs
  .getProperVal <- function(str, regions) {
    abbrFCT <- .fctOptions("abbrev")
    if (!is.na(match(str, regions)))
      return(str)
    if (inherits(regions, "states")) {
      if (agrepl(str, abbrFCT, max.distance = 2)
          && identical(toupper(str), abbrFCT))
        return(abbrFCT)
    }
    
    ## First remove spaces around slashes
    ## TODO: Note that there is an element of hard-coding here
    str <- str %>% 
      gsub("\\s\\/", "/", .) %>% 
      gsub("\\/\\s", "/", .) %>% 
      sub("-\\s", "-", .) %>% 
      sub("^Egbado/", "", .)
    
    
    ## Now check for exact matching
    matched <-
      grep(paste0('^', str, '$'),
           regions,
           value = TRUE,
           ignore.case = TRUE)
    
    
    if (length(matched) == 1L) 
      return(matched)
    
    ## Otherwise check for approximate matches
    matched <- agrep(str, regions, value = TRUE, max.distance = 1)
    
    if (length(matched) == 1L)
      return(matched)
    
    if (length(matched) > 1L) {
      .warnOnMultipleMatches(str, matched)
    }
    else if (!length(matched))
      notFound <<- c(notFound, str)
    str
  }
  
  notFound <- character()
  v <-
    vapply(x, .getProperVal, character(1), regions = region, USE.NAMES = FALSE)
  if (length(notFound)) {
    message("Approximate match(es) not found for the following:")
    sapply(notFound, function(x) message(paste("*", x)))
    attr(v, "misspelt") <- notFound
  }
  v
}





.warnOnMultipleMatches <- function(x, matches)
{
  multimatch <- paste(matches, collapse = ", ")
  warning(sprintf(
    "'%s' approximately matched more than one region - %s",
    x,
    multimatch
  ), call. = FALSE)
}






## Interactively fixes regions that are bad - this function is primarily
## used for repairing LGA names, since they are so many.
## @param lga.list The vector of LGA names that is being repaired. This vector
## is generated by `.fixRegionInternal` and has an attribute called `misspelt`,
## which is the collection of names needing repair.
#' @importFrom utils menu
.fixLgasInteractively <- function(lga.list)
{
  stopifnot(interactive())
  allLgas <- lgas()
  opt <- NA
  retry <- "Retry"
  quit <- "Quit"
  skip <- "Skip"
  skipped <- character()
  bad.values <- attr(lga.list, "misspelt")
  for (i in bad.values) {
    message("Fixing ", sQuote(i))
    repeat {
      choices <- readline("Search pattern: ") %>% 
        grep(allLgas, value = TRUE, ignore.case = TRUE) %>% 
        c(retry, skip, quit)
      opt <- menu(choices, FALSE, "Select the LGA")
      chosen <- choices[opt]
      if (chosen != retry)
        break
    }
    if (chosen == quit)
      break
    if (chosen == skip) {
      skipped <- c(skipped, i)
      next
    }
    lga.list <- sub(i, chosen, lga.list)
  }
  if (length(skipped))
    warning("The following items were skipped and should be fixed manually: ",
            paste(skipped, collapse = ", "),
            call. = FALSE)
  lga.list
}













.fixRegionsManually <- function(wrong, correct, src)
{
  patterns <- paste0("^", wrong, "$")
  wrongIndexed <- vapply(patterns, grep, integer(1), x = src)
  src[wrongIndexed] <- correct
  src
}
