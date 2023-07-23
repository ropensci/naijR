# Source file: fixreg.R
#
# Copyright (C) 2019-2023 Victor Ordu.

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
#' \code{fix_region.default}, a character vector (or an object coercible to 
#' one) can be passed but only that for 'States' will be interpretable.
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
{ # TODO: Consider reporting on fixes made
  ## Process possible FCT values
  abbrFCT <- .fct_options("abbrev")
  fullFCT <- .fct_options("full")
  
  ## Replace any 'Abuja' with FCT in full
  x[x %in% "Abuja"] <- fullFCT
  
  ## Find and replace abbreviated with full version
  sumFct <- sum(.fct_options() %in% x)
  
  ## Both full and abbreviated versions coexist
  if (sumFct == 2)
    x <- sub(abbrFCT, fullFCT, x)
  
  ## Allow use of abbreviated version before carrying
  ## out the check
  isFct <- x %in% abbrFCT
  ss <- states()
  
  if (sum(isFct)) 
    ss <- sub(fullFCT, abbrFCT, ss)
  
  x <- .fix_region_internal(x, ss)
  nofix <- attr(x, "misspelt")
  
  if (length(nofix)) {
    commasep <- paste(nofix, collapse = ", ")
    cli::cli_abort("The following are not States: {commasep}")
  }
  ## After checking, reconstitute the 'states' object
  # x[isFct] <- fullFCT   TODO: Think again.
  attributes(x) <- NULL
  states(x)
}





#' @rdname fix_region
#' 
#' @param interactive Logical. When \code{TRUE}, the function prompts the user
#' to interactively select the correct LGA names from a list of available
#' options.
#' @param quietly Logical; default argument is \code{FALSE}.
#' @param graphic Whether to make use of native GUI elements (on Windows only).
#' 
#' @examples 
#' try(fix_region("Owerri north")) # ERROR
#' fix_region(c("Owerri north", "Owerri West"))
#' 
#' @export
fix_region.lgas <-
  function(x,
           interactive = FALSE,
           quietly = FALSE,
           graphic = FALSE,
           ...)
  {
    # TODO: add a 'state' argument to fine-tune the matching
    if (!is.logical(interactive) ||
        !is.logical(quietly) ||
        !is.logical(graphic)) {
      cli::cli_abort("Invalid input where logical argument expected")
    }
    if (graphic) 
      graphic <- interactive
    
    usedialog <- .Platform$OS.type == "windows" && graphic
    vals <- .fix_region_internal(x, lgas(), interactive)
    
    if (interactive)
      vals <- .fix_lgas_interactive(vals, usedialog)
    
    if (is.null(vals)) {
      msg <- "The operation was cancelled"
      
      if (usedialog)
        winDialog("ok", msg)
      else
        cli::cli_alert_info(msg)
      
      return(invisible(x))
    }
    if (!quietly)
      .report_on_fixes(vals, usedialog)
    
    vals
  }





## Sometimes the States/LGAs will be supplied as ordinary character vectors
## For this case, this method tries to do some kind of sorting, determining
## which kind of region the input represents i.e. States or LGAs. Once this
## is determined, the appropriate constructor (`states()` or `lgas()`, 
## respectively) will be applied and from there the appropriate method is 
## called internally to attempt to fix the input.
#' @rdname fix_region
#' 
#' @importFrom cli cli_abort
#' @importFrom cli cli_warn
#' 
#' @export
  fix_region.default <- function(x, ...)
{
  if (is.factor(x))
    x <- as.character(x)
  
  if (!is.character(x))
    cli_abort("'x' is not a character vector")
  
  empty <- grepl("^$", x)
  
  if (length(empty) > 0L && all(empty))  ## diff character(0) and character(1)
    cli_abort("'x' only has empty strings")
  
  if (any(empty))
    cli_warn("Tried to fix empty strings - may produce errors")
  
  if (all(is.na(x)) || !length(x)) {
    cli_warn("'x' has length 0L or only missing values")
    return(x)
  }
  
  ## For the LGAs case, the expectation is that in a vector with more than
  ## one element, if any of the elements passess the test of being an LGA
  ## then one can safely assume that the other element(s) that fail the test
  ## did so because they were misspelt. An automatic fix will then be attempted.
  ## First, ignore synonymous elements i.e. those that are both States/LGAs.
  nonSynonyms <- x[!x %in% lgas_like_states()]
  
  region <- if (any(is_lga(nonSynonyms)))    # We use 'any()' because we want
    lgas(x, warn = FALSE)                    # to allow creation of temporary,
  else if (any(is_state(nonSynonyms)))       # even with misspelt elements
    states(x, warn = FALSE)
  else
    cli_abort(
        "Incorrect region name(s);
        consider reconstructing 'x' with
        `states()` or `lgas()` for a more reliable fix"
    )
  
  invisible(as.character(fix_region(region)))
}




  
## Approximately matches a string on a list of regions i.e States or LGAs.
## @param str A string to be matched
## @param regions The values to be matched against, specifically from regions
##
## @return A vector of the same length as str with the approximately
## matched value, which in the case of misspelling should be the correct one.
##
## This function is mapped to a vector of states/LGAs
.fix_region_internal <- function(x, region, interactive = FALSE)
{
  stopifnot(is.character(x), is.character(region))
  cant.fix <- character()
  fix.status <- character()
  
  ## Internal function to enable identification of entries that need to
  ## be fixed and preparing attributes that will enable further processing
  ## downstream.
  get_proper_value <- function(str, regions) {
    abbrFCT <- .fct_options("abbrev")
    
    if (!is.na(match(str, regions)))
      return(str)
    
    if (inherits(regions, "states")) {
      
      if (agrepl(str, abbrFCT, max.distance = .pkgLevDistance())
          && identical(toupper(str), abbrFCT))
        
        return(abbrFCT)
    }
    
    ## First remove spaces around slashes and hyphens
    ## Note: run `.__why_no_pipe()` for rationale behind this approach
    str <- gsub("\\s\\/", "/", str)
    str <- gsub("\\/\\s", "/", str)
    str <- sub("-\\s", "-", str)
    str <- sub("^Egbado/", "", str) ## TODO: Address hard-coding
    
    ## Now, check for exact matching.
    rgx <- paste0('^', str, '$')
    good <- unique(grep(rgx, regions, value = TRUE, ignore.case = TRUE))
    
    if (length(good) == 1L) 
      return(good)
    
    ## Otherwise check for approximate matches.
    fixed <- agrep(str, regions, value = TRUE, max.distance = 1)
    numFixed <- length(fixed)
    
    if (numFixed == 1L) {
      fs <- c(fix.status, fixed)
      names(fs) <- c(names(fix.status), str)
      fix.status <<- fs
      return(fixed)
    }
    
    if (numFixed > 1L && !interactive) {
      multimatch <- paste(fixed, collapse = ", ")
      
      cli::cli_inform(
        "'{str}' approximately matched more than one region - {multimatch}"
      )
    }
    
    # if we get to this point, return the misspelt string unchanged
    cant.fix <<- c(cant.fix, str)
    str
  }
  
  spellchecked <-
    vapply(x, get_proper_value, character(1), regions = region, USE.NAMES = FALSE)
  
  attr(spellchecked, "misspelt") <- sort(unique(cant.fix))
  
  ## Reduce data for reporting on fixes to only the 
  ## unique instances i.e. avoid redundant output
  if (length(fix.status) > 1L) {
    allfix <- names(fix.status)
    
    if (anyDuplicated(allfix)) {
      dups <- which(duplicated(allfix))
      fix.status <- fix.status[-dups]
    }
  }
  
  attr(spellchecked, "regions.fixed") <- fix.status
  spellchecked
}



# Tells the user about what repairs have been made to the spellings
# @param obj - the checked object, which has attributes with relevant details
# @param usedialog Whether to display a dialog (on Windows only).
#' @import utils
.report_on_fixes <- function(obj, usedialog = FALSE)
{
  spell.details <- attributes(obj)
  badspell <- spell.details$misspelt
  hasBadspell <- !identical(badspell, character(0))
  msg.bad <- msg.good <- ""
  
  if (hasBadspell) {
    hdr.bad <- .messageHeader("Fix(es) not applied")
    nofix.bullets <- sapply(badspell, function(x) paste0("* ", x))
    msg.bad <- paste0(hdr.bad, paste(nofix.bullets, collapse = "\n"))
  }
  
  # Put the message together
  fixes <- spell.details$regions.fixed
  
  if (!identical(fixes, character(0))) {
    hdr.good <- .messageHeader("Successful fix(es)")
    
    fixed.bullets <-
      mapply(function(a, z) {
        sprintf("* %s => %s", a, z)
      }, 
      names(fixes), fixes)
    
    msg.good <- paste0(hdr.good, paste(fixed.bullets, collapse = "\n"))
    
    if (hasBadspell)
      msg.good <- paste0(msg.good, "\n")    # just add newline
  }
  
  if (!nchar(msg.good) && !nchar(msg.bad))
    return()
  
  final.msg <- paste(msg.good, msg.bad, sep = "\n")
  
  if (usedialog)
    winDialog("ok", final.msg)
  else
    cli::cli_alert_info(final.msg)
}




.messageHeader <- function(hdr)
{
  stopifnot(is.character(hdr))
  
  hdr <- paste0(hdr, ":")
  dashes <- strrep("-", nchar(hdr))
  hdr <- paste(hdr, dashes, sep = '\n')
  paste0(hdr, "\n")
}



## Interactively fixes regions that are bad - this function is primarily
## used for repairing LGA names, since they are so many.
## @param lga.list The vector of LGA names that is being repaired. This vector
## is generated by `.fix_region_internal` and has an attribute called `misspelt`,
## which is the collection of names needing repair.
## @param usedialog Whether to use dialog in prompts (only on Windows)
#' @import utils
.fix_lgas_interactive <- function(lga.list, usedialog = FALSE)
{
  stopifnot(interactive())
  allLgas <- lgas()
  menuopt <- integer()
  skipped <- character()
  bad.values <- attr(lga.list, "misspelt")
  
  # This list doesn't need to be re-created with each loop iteration
  # that's why it's been created here.
  special.options <- list(
    retry = "RETRY",
    skip = "SKIP",
    quit = "QUIT"
  )
  
  for (bad in bad.values) {
    msg.fixWhich <- paste("Fixing", sQuote(bad))
    
    repeat {
      prompt <- paste(msg.fixWhich, "Enter a search term: ", sep = ' - ')
      
      pattern <- if (usedialog)
        winDialogString(prompt, "")
      else
        readline(prompt)
      
      if (pattern == "" || is.null(pattern))
        return()
      
      used.lgas <- grep(pattern, allLgas, value = TRUE, ignore.case = TRUE)
      used.lgas <- sort(used.lgas)
      choices <- c(used.lgas, unlist(unname(special.options)))
      menuopt <-
        menu(
          choices,
          graphics = usedialog,
          "Select the LGA"
        )
      chosen <- choices[menuopt]
      
      if (chosen != special.options$retry)
        break
    }
    
    if (chosen == special.options$quit)
      break
    
    if (chosen == special.options$skip) {
      skipped <- c(skipped, bad)
      next
    }
    
    # Note that pipes were deliberately not used here.
    lga.list <- sub(bad, chosen, lga.list, fixed = TRUE)
    attr.misspelt <- attr(lga.list, "misspelt")
    attr.misspelt <- attr.misspelt[attr.misspelt != bad]
    attr(lga.list, "misspelt") <- attr.misspelt
    attr.regfixed <- attr(lga.list, "regions.fixed")
    attr.regfixed <- c(attr.regfixed, chosen)
    names(attr.regfixed) <- c(names(attr.regfixed), bad)
  }
  
  if (length(skipped)) {
    msg <-
      paste(
        "The following items were skipped and should be fixed manually:",
        paste(skipped, collapse = ", ")
      )
    
    if (usedialog)
      winDialog("ok", msg)
    else
      cli::cli_inform(msg)
  }
  
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
#' @importFrom cli cli_abort
#' @export
fix_region_manual <- function(x, wrong, correct)
{
  arg <- substitute(x)
  
  if (!(inherits(x, "states") && !inherits(x, "lgas"))) {
    
    if (!is.character(x))
      cli_abort(
        "The operation cannot be done on objects of type {sQuote(typeof(x))}"
      )
  }
  
  if ((length(wrong) != length(correct)) && length(correct) > 1L)
    cli_abort(
      "Substitutions must be single or the same number as targetted fixes"
    )
  
  if (length(correct) == 1L) {
    correct <- .assert_region(correct)
    x[x %in% wrong] <- correct
    return(x)
  }
  
  ## In the loop, we will allow exception handling so that execution is
  ## not made clunky when multiple corrections are attempted at once.
  for (i in seq_along(wrong)) {
    iCorrect <- correct[i]
    iWrong <- wrong[i]
    
    if (!match(iWrong, x, nomatch = 0))
      cli_abort("{sQuote(iWrong, q = FALSE)} is not an element of
                {sQuote(arg, q = FALSE)}")
    
    tryCatch({
      iCorrect <- .assert_region(iCorrect)
      x[x %in% iWrong] <- iCorrect
    }, error = function(e) {
      cli::cli_warn(conditionMessage(e))
    })
  }
  # TODO: Check post-conditions and warn if mistakes remain?
  x
}




.assert_region <- function(x) {
  if (!is_state(x) && !is_lga(x))
    cli::cli_abort("{sQuote(x, q = FALSE)} is not a valid region")
  x
}

