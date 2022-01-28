
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
  ## First, ignore synonymous elements i.e. those that are both States/LGAs.
  nonSynonyms <- x[!x %in% .synonymRegions()]
  region <- if (any(is_lga(nonSynonyms)))    # We use 'any()' because we want
    lgas(x, warn = FALSE)                    # to allow creation of temporary,
  else if (any(is_state(nonSynonyms)))       # even with misspelt elements
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
#' @importFrom magrittr %>%
.fixRegionInternal <- function(x, region, ...)
{
  stopifnot(is.character(x), is.character(region))
  cant.fix <- character()
  fix.status <- character()
  
  ## Internal function to enable identification of entries that need to
  ## be fixed and preparing attributes that will enable further processing
  ## downstream.
  .getProperVal <- function(str, regions) {
    abbrFCT <- .fctOptions("abbrev")
    if (!is.na(match(str, regions)))
      return(str)
    if (inherits(regions, "states")) {
      if (agrepl(str, abbrFCT, max.distance = .pkgLevDistance())
          && identical(toupper(str), abbrFCT))
        return(abbrFCT)
    }
    
    ## First remove spaces around slashes and hyphens
    str <- str %>% 
      gsub("\\s\\/", "/", .) %>% 
      gsub("\\/\\s", "/", .) %>% 
      sub("-\\s", "-", .) %>% 
      sub("^Egbado/", "", .) ## TODO: Note hard-coding here. Address later.
    
    ## Now, check for exact matching.
    good <-
      grep(paste0('^', str, '$'), regions, value = TRUE, ignore.case = TRUE) %>% 
      unique()
    if (length(good) == 1L) 
      return(good)
    
    ## Otherwise check for approximate matches.
    fixed <- agrep(str, regions, value = TRUE, max.distance = 1)
    numFixed <- length(fixed)
    if (numFixed == 1L) {
      fix.status <<- fix.status %>% 
        {
          structure(c(., fixed), names = c(names(.), str))
        }
      return(fixed)
    }
    if (numFixed > 1L)
      .warnOnMultipleMatches(str, fixed)
    
    # if we get to this point, return misspelt string unchanged
    cant.fix <<- c(cant.fix, str)
    str
  }
  
  v <-
    vapply(x, .getProperVal, character(1), regions = region, USE.NAMES = FALSE)
  attr(v, "misspelt") <- cant.fix
  
  ## Reduce data for reporting on fixes to only the 
  ## unique instances to avoid repetitive printouts
  if (length(fix.status) > 1L) {
    allfix <- names(fix.status)
    if (anyDuplicated(allfix)) {
      dups <- which(duplicated(allfix))
      fix.status <- fix.status[-dups]
    }
  }
  
  attr(v, "regions.fixed") <- fix.status
  return(v)
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












#' Fix Spelling of Regions Manually
#' 
#' Enable users to interactively and directly change to spelling of States
#' and/or Local Government Areas (LGAs)
#' 
#' @param x The object to be modified
#' @param wrong The misspelt element(s) of \code{x}.
#' @param correct The correction that is to be applied to the misspelt element(s)
#' 
#' @export
fix_region_manual <- function(x, wrong, correct)
{
  arg <- substitute(x)
  if (!(inherits(x, "states") && !inherits(x, "lgas"))) {
    if (!is.character(x))
      stop("The operation cannot be done on objects of type ", sQuote(typeof(x)))
  }
  if ((length(wrong) != length(correct)) && length(correct) > 1L)
    stop("Substitutions must be single or the same number as targetted fixes")
  if (length(correct) == 1L) {
    correct <- assertRegion(correct)
    x[x %in% wrong] <- correct
    return(x)
  }
  
  ## In the loop, we will allow exception handling so that execution is
  ## not made clunky when multiple corrections are attempted at once.
  for (i in seq_along(wrong)) {
    iCorrect <- correct[i]
    iWrong <- wrong[i]
    if (!match(iWrong, x, nomatch = 0))
      stop(sQuote(iWrong, q = FALSE),
           " is not an element of ",
           sQuote(arg, q = FALSE))
    tryCatch({
      iCorrect <- assertRegion(iCorrect)
      x[x %in% iWrong] <- iCorrect
    }, error = function(e) warning(conditionMessage(e), call. = FALSE))
  }
  # TODO: Apply a correctness check and warn if mistakes remain?
  x
}
