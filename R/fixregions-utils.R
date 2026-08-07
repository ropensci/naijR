# Source file: fixregions-utils.R
#
# GPL-3 License
#
# Copyright (C) 2019-2026 Victor Ordu.
#
# Automatically attempts to fix the wrong spellings of regions (States or LGAs).
# At the end of the operation, the object being checked is returned with
# data on the outcome of the attempted fix. These are added on as attributes,
# and these exist temporarily until the entire process of updating spelling
# mistateks is completed in the parent environment.
.fix_region_automatic <- function(regions_object, region_table)
{
  stopifnot(is.object(regions_object))
  region_class <- class(regions_object)
  regions_object <- unclass(regions_object)
  matched <- cant.fix <- fix.status <- character()
  result <- list()
  for (name in regions_object) {
    result <- .get_proper_value(name, region_table)
    matched <- c(matched, result$str)
    fix.status <- c(fix.status, result$fixes)
    cant.fix <- c(cant.fix, result$nofix)
  }
  .get_updated_obj_with_attrs(matched, fix.status, cant.fix, region_class)
}




## Internal function to enable identification of entries that need to
## be fixed and preparing attributes that will enable further processing
## downstream.
#' @importFrom rlang is_string
.get_proper_value <- function(str, regions) {
  stopifnot(exprs = {rlang::is_string(str); inherits(regions, "regions")})
  result <- list()
  if (!is.na(match(str, regions))) {
    result$str <- str
    return(result)
  }
  if (inherits(regions, "states")) {
    abbrFCT <- .toggle_fct_format("abbrev")
    if ( agrepl(str, abbrFCT, max.distance = .defaultDistance()) &&
        identical(toupper(str), abbrFCT)) {
      result$str <- abbrFCT
      result$fixes <- structure(abbrFCT, names = str)
      return(result) 
    }
  }
  # result$str <- .trim_whitespace(result$str)
  ## Now, check for exact matching, case-insensitively
  matched <- 
    grep(paste0("^", str, '$'), regions, value = TRUE, ignore.case = TRUE)
  if (length(matched)) { 
    result$str <- matched
    result$fixes <- structure(matched, names = str)
    return(result)
  }
  ## Otherwise check for approximate matches.
  fixed <- agrep(str, regions, value = TRUE, max.distance = .defaultDistance(),
                 ignore.case = TRUE)
  if (length(fixed) == 1L) {
    result$fixes <- structure(fixed, names = str)
    result$str <- fixed
  }
  else {
    if (length(fixed) > 1L) {
      multimatch <- paste(fixed, collapse = ", ")
      cli::cli_inform(
        "'{str}' approximately matched more than one region - {multimatch}"
      )
    }
    result$nofix <- str
  }
  result
}




## Reduce data for reporting on fixes to only unique instances 
.get_updated_obj_with_attrs <- function(checked, fixed, notfixed, class) {
  stopifnot(
    is.character(checked) || is.character(fixed) || is.character(notfixed)
  )
  if (length(fixed) > 0L && !rlang::is_named(fixed)) {
    cli_abort("'fixed' must be named if length > 0")
  }
  if (length(fixed) > 1L) {
    # we keep only unique values using this method in order to preserve
    # the names, and also bearing in mind that different typos might map
    # to the same true value.
    original <- names(fixed)
    if (anyDuplicated(original)) {
      duplicates <- which(duplicated(original))
      fixed <- fixed[-duplicates]
    }
  }
  attr(checked, "regions.fixed") <- fixed
  attr(checked, "misspelt") <- sort(unique(notfixed))
  structure(checked, class = class)
}


.trim_whitespace <- function(x)
{
  ## First remove spaces around slashes and hyphens
  # str <- gsub("\\s\\/", "/", str)
  # str <- gsub("\\/\\s", "/", str)
  # str <- sub("-\\s", "-", str)
  # sub("^Egbado/", "", str) ## TODO: Address hard-coding
  x
}




# Tells the user about what repairs have been made to the spellings
# @param obj - the checked object, which has attributes with relevant details
# @param usedialog Whether to display a dialog (on Windows only).
.report_on_fixes <- function(obj, usedialog = FALSE)
{
  spell.details <- attributes(obj)
  badspell <- spell.details$misspelt
  hasBadspell <- !identical(badspell, character(0))
  msg.bad <- msg.good <- ""
  if (hasBadspell) {
    hdr.bad <- .messageHeader("Fix(es) not applied")
    nofix.bullets <-
      vapply(badspell, function(x) paste("*", x), character(1))
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
  if (!nchar(msg.good) && !nchar(msg.bad)) {
    return()
  }
  final.msg <- paste(msg.good, msg.bad, sep = "\n")
  if (usedialog) {
    utils::winDialog("ok", final.msg)
  }
  else {
    cli::cli_alert_info(final.msg)
  }
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
## @param lgas.object The vector of LGA names that is being repaired. This vector
## is generated by `.fix_region_automatic` and has an attribute called
## `misspelt`, which is the collection of names needing repair.
## @param usedialog Whether to use dialog in prompts (only on Windows)
.fix_lgas_interactive <- function(lgas.object, usedialog = FALSE)
{
  stopifnot(exprs = {
    is.atomic(lgas.object)
    length(attributes(lgas.object)) > 1L
    is.logical(usedialog)
  })
  skipped <- character()   # instead of NULL, for validation downstream
  for (bad in attr(lgas.object, "misspelt")) {
    result <- .attempt_lga_fix(bad, usedialog)
    if (is.null(result)) {
      return()
    }
    if (identical(result, character())) {    # QUIT
      break
    }
    if (identical(result, bad)) {            # SKIP
      skipped <- c(skipped, bad)
      next
    }
    lgas.object <- .update_attributes(bad, result, lgas.object)
  }
  .suggest_manual_fix(skipped, usedialog)
  lgas.object
}




.attempt_lga_fix <- function(bad, usedialog) {
  allLgas <- lgas()
  specialopts <- list(retry = "RETRY", skip = "SKIP", quit = "QUIT")
  msg.fixWhich <- paste("Fixing", sQuote(bad))
  repeat {
    prompt <- paste(msg.fixWhich, "Enter a search term: ", sep = ' - ')
    pattern <- .collect_searchterm(prompt, usedialog) 
    if (pattern == "" || is.null(pattern)) {
      return(NULL)
    }
    matched <- sort(grep(pattern, allLgas, value = TRUE, ignore.case = TRUE))
    choices <- c(matched, unlist(unname(specialopts)))
    menuopt <- utils::menu(choices, graphics = usedialog, "Select the LGA")
    chosen <- choices[menuopt]
    if (chosen == specialopts$retry) {
      next
    }
    break
  }
  if (chosen == specialopts$quit) {
    return(character())
  }
  if (chosen == specialopts$skip) {
    return(bad)
  }
  chosen
}




# Updates the attributes added to an lgas object in the course of attempting
# to fix spelling mistakes. This attributes are required internally for
# assessing the status of the fixes applied and for reporting to the user
.update_attributes <- function(misspelled, repaired, obj) {
  stopifnot(exprs = {
    rlang::is_string(misspelled)
    rlang::is_string(repaired)
  })
  obj <- sub(misspelled, repaired, obj, fixed = TRUE)
  attr.misspelt <- attr(obj, "misspelt")
  attr(obj, "misspelt") <- attr.misspelt[attr.misspelt != misspelled]
  attr.fixed <- c(attr(obj, "regions.fixed"), repaired)
  names(attr.fixed) <- c(names(attr.fixed), misspelled)
  attr(obj, "regions.fixed") <- attr.fixed
  obj
}




.suggest_manual_fix <- function(skipped, useWinDialog) {
  stopifnot(exprs = {
    is.character(skipped)
    is.logical(useWinDialog)
  })
  if (length(skipped)) {
    msg <- paste0(
      "The following items were skipped: ",
      paste(skipped, collapse = ", "),
      ". Consider using 'fix_region_manual'"
    )
    if (useWinDialog) {
      utils::winDialog("ok", msg)
    }
    else {
      cli::cli_inform(msg)
    }
  }
}




.collect_searchterm <- function(prompt, useWinDialog) {
  result <- if (useWinDialog) {
    utils::winDialogString(prompt, "")
  }
  else {
    readline(prompt)
  }
  result
}




.assert_region <- function(x) {
  if (!is_state(x) && !is_lga(x)) {
    cli::cli_abort("{sQuote(x, q = FALSE)} is not a valid region")
  }
  x
}




.strip_temp_attrs <- function(x)
{
  cl <- class(x)
  attributes(x) <- NULL
  structure(x, class = cl)
}
