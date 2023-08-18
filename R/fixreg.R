# Source file: fixreg.R
#
# GPL-3 License
#
# Copyright (C) 2019-2023 Victor Ordu.

#' Fix Region Names
#' 
#' Correct any misspelt names of administrative regions i.e. States and LGAs
#' 
#' @details The function will look through a character vector and try to 
#' determine if State or LGA names have been wrongly entered. This presupposes
#' that the atomic vector is of type \code{character}. It does not test any
#' missing values in the vector, leaving them untouched.
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
  function(x, interactive = FALSE, quietly = FALSE, graphic = FALSE, ...)
  {
    # TODO: add an optional 'state' argument to fine-tune the matching
    if (!is.logical(interactive) ||
        !is.logical(quietly) ||
        !is.logical(graphic)) {
      cli::cli_abort("Invalid input where logical argument expected")
    }
    if (graphic) {
      if (!interactive)
        cli::cli_warn("'graphic' was reset to FALSE in non-interactive mode")
      
      graphic <- interactive
    }
    vals <- .fix_region_internal(x, lgas(), interactive)
    usedialog <- .Platform$OS.type == "windows" && graphic
    
    if (interactive) { # nocov start
      vals <- .fix_lgas_interactive(vals, usedialog)
    
      if (is.null(vals)) {
        msg <- "The operation was cancelled"
        
        if (usedialog)
          utils::winDialog("ok", msg)
        else
          cli::cli_alert_info(msg)
        
        return(invisible(x))
      } # nocov end
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
  
  invisible(as.character(fix_region(region, ...)))
}




#' Fix Spelling of Regions Manually
#' 
#' Enable users to interactively and directly change to spelling of States
#' and/or Local Government Areas (LGAs)
#' 
#' @param x The object to be modified
#' @param wrong The misspelt element(s) of \code{x}.
#' @param correct The correction that is to be applied to the misspelt 
#' element(s)
#' 
#' @return The (possibly) modified object of class \code{regions} i.e. a
#' \code{states} or \code{lgas} object.
#' 
#' @importFrom cli cli_abort
#' @export
#' 
#' @examples
#' x <- c("Pankshen", "Pankshin", "Q'uan Pam")
#' is_lga(x)
#' x <- fix_region(x, quietly = TRUE)
#' is_lga(x)
#' fix_region_manual(x, "Q'uan Pam", "Qua'an Pan")
#' all(is_lga(x))
#' 
#' 
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
