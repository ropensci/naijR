
#' Fix Region Names
#' 
#' Correct any misspelt names of administrative regions i.e. States and LGAs
#' 
#' @details The function will look through a character vector and try to 
#' determine if State or LGA names have been wrongly entered. This presupposes that
#' the atomic vector is of type \code{character}. It does not test any missing
#' values in the vector, leaving them untouched.
#' 
#' @note When passed a character vector of length \code{1L}, in the case of a
#' misspelt LGA, the function signals an error; the presumption is that a fix
#' can readily be applied interactively. When all the items provided are 
#' misspelt, nothing happens, but the user is advised to use the appropriate
#' constructor function so as to improve the accuracy of the repairs. When
#' there is a mix of misspelt and properly spelt LGAs, other functionalities
#' for fixing the mistakes are available via mode \code{interactive}.
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
  iFct <- grep(sprintf("^%s$", abbrFCT), x, ignore.case = TRUE)
  ss <- states()
  if (length(iFct)) 
    ss <- sub(fullFCT, abbrFCT, ss)
  
  x <- .fixRegionInternal(x, ss)
  x[iFct] <- fullFCT
  attributes(x) <- NULL
  x <- states(x, warn = FALSE)
  invisible(x)
}





#' @rdname fix_region
#' 
#' @param interactive Logical. When \code{TRUE}, the function prompts the user
#' to interactively select the correct LGA names from a list of available
#' options.
#' @param quietly Logical; default argument is \code{FALSE}.
#' 
#' @examples 
#' try(fix_region("Owerri north")) # ERROR
#' fix_region(c("Owerri north", "Owerri West"))
#' 
#' @export
fix_region.lgas <- function(x, interactive = FALSE, quietly = FALSE, ...)
{
  vals <- .fixRegionInternal(x, lgas(), ...)
  if (interactive) {
    ans <- readline("Do you want to repair interactively? (Y/N): ")
    if (tolower(ans) == "y")
      vals <- .fixLgasInteractively(vals)
  }
  if (!quietly)
    .reportOnFixes(vals)
  invisible(vals)
}





## Sometimes the States/LGAs will be supplied as ordinary character vectors
## For this case, this method tries to do some kind of sorting, determining
## which kind of region the input represents i.e. States or LGAs. Once this
## is determined, the appropriate constructor (`states()` or `lgas()`, 
## respectively) will be applied and from there the appropriate method is 
## called internally to attempt to fix the input.
#' @rdname fix_region
#' @importFrom magrittr %>%
#' 
#' @export
  fix_region.default <- function(x, ...)
{
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
  
  ## For the LGAs case, the expectation is that in a vector with more than
  ## one element, if any of the elements passess the test of being an LGA
  ## then one can safely assume that the other element(s) that fail the test
  ## did so because they were misspelt. An automatic fix will then be attempted.
  region <- if (any(is_lga(x)))
    lgas(x, warn = FALSE)
  else if (any(is_state(x)))
    states(x, warn = FALSE)
  else
    stop(
      paste(
        "Incorrect region name(s);",
        "consider reconstructing 'x' with",
        "`states()` or `lgas()` for a more reliable fix"
      ),
      call. = FALSE
    )
  zz <- region %>% fix_region %>% as.character
  
  invisible(zz)
}




  
## Approximately matches a string on a list of regions i.e States or LGAs.
## @param str A string to be matched
## @param regions The values to be matched against, specifically from regions
##
## @return A vector of the same length as str with the approximately
## matched value, which in the case of misspelling should be the correct one.
##
## This function is mapped to a vector of states/LGAs
.fixRegionInternal <- function(x, region, ...)
{
  .getProperVal <- function(str, regions) {
    abbrFCT <- .fctOptions("abbrev")
    if (!is.na(match(str, regions)))
      return(str)
    if (inherits(regions, "states")) {
      if (agrepl(str, abbrFCT, max.distance = .pkgLevDistance())
          && identical(toupper(str), abbrFCT))
        return(abbrFCT)
    }
    
    ## First remove spaces around slashes
    ## TODO: Note that there is an element of hard-coding here.
    ## The related issue should be addressed at source.
    str <- str %>% 
      gsub("\\s\\/", "/", .) %>% 
      gsub("\\/\\s", "/", .) %>% 
      sub("-\\s", "-", .) %>% 
      sub("^Egbado/", "", .)
    
    ## Now, check for exact matching.
    good <-
      grep(paste0('^', str, '$'),
           regions,
           value = TRUE,
           ignore.case = FALSE)
    
    if (length(good) == 1L) 
      return(good)
    
    ## Otherwise check for approximate matches.
    fixed <-
      agrep(paste0('^', str, '$'),
            regions,
            value = TRUE,
            fixed = FALSE,
            max.distance = .pkgLevDistance())
    
    if (length(fixed) == 1L) {
      fix.status <<-
        structure(c(fix.status, fixed), names = c(names(fix.status), str))
      return(fixed)
    }
    
    if (length(fixed) > 1L)
      .warnOnMultipleMatches(str, fixed)
    
    cant.fix <<- c(cant.fix, str)
    str  # return misspelt string unchanged
  }
  
  fix.status <- cant.fix <- character()
  v <-
    vapply(x, .getProperVal, character(1), regions = region, USE.NAMES = FALSE)
  attr(v, "misspelt") <- unique(cant.fix)
  attr(v, "regions.fixed") <- fix.status[which(!duplicated(names(fix.status)))]
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



.reportOnFixes <- function(obj)
{
  ATTR_ <- attributes(obj)
  badspell <- ATTR_$misspelt
  hasBadspell <- !identical(badspell, character(0))
  if (hasBadspell) {
    hdr1 <- .messageHeader("Fix(es) not applied")
    x <- sapply(badspell, function(x) paste0("* ", x, "\n"))
    message(hdr1, paste0(x), appendLF = FALSE)
  }
  fixes <- ATTR_$regions.fixed
  if (!identical(fixes, character(0))) {
    if (hasBadspell)
      message("")    # just add newline
    hdr2 <- .messageHeader("Successful fix(es)")
    x <- 
      mapply(function(a, b) sprintf("* %s => %s\n", a, b), names(fixes), fixes)
    message(hdr2, paste0(x), appendLF = FALSE)
  }
}


#' @importFrom magrittr %>%
.messageHeader <- function(hdr)
{
  hdr %>% 
    paste0(":") %>% 
    paste(strrep("-", nchar(.)), sep = '\n') %>% 
    paste0("\n")
}



## Interactively fixes regions that are bad - this function is primarily
## used for repairing LGA names, since they are so many.
## @param lga.list The vector of LGA names that is being repaired. This vector
## is generated by `.fixRegionInternal` and has an attribute called `misspelt`,
## which is the collection of names needing repair.
#' @importFrom utils menu
#' @importFrom magrittr %<>%
.fixLgasInteractively <- function(lga.list)
{
  stopifnot(interactive())
  allLgas <- lgas()
  opt <- NA
  retry <- "RETRY"
  quit <- "QUIT"
  skip <- "SKIP"
  skipped <- character()
  bad.values <- attr(lga.list, "misspelt")
  for (bad in bad.values) {
    message("Fixing ", sQuote(bad))
    repeat {
      choices <- readline("Search pattern: ") %>% 
        {
          rs <- grep(., allLgas, value = TRUE, ignore.case = TRUE)
          c(retry, skip, quit, rs)
        }
       
      opt <- menu(choices, FALSE, "Select the LGA")
      chosen <- choices[opt]
      if (chosen != retry)
        break
    }
    if (chosen == quit)
      break
    if (chosen == skip) {
      skipped <- c(skipped, bad)
      next
    }
    lga.list <- sub(bad, chosen, lga.list)
    attr(lga.list, "misspelt") <- bad.values[bad.values != bad]
    attr(lga.list, "regions.fixed") %<>% 
    {
      structure(c(., chosen), names = c(names(.), bad))
    }
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
