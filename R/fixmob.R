# Source file: fixmob.R
#
# GPL-3 License
#
# Copyright (C) 2019-2023 Victor Ordu.

#' Fix mobile numbers
#'
#' Fixes up local mobile phone numbers to a uniform text format.
#'
#' @details This format is specific to that used in a given location - for
#' now the function is useful only for Nigeria mobile numbers, which come in
#' the format expressed by the regex pattern \code{"^0[7-9][0-1][0-9]{8}$"}.
#' 
#' @note There is an option for producing warnings on any mobile number entries
#' that may have been removed from the vector, by setting the option
#' \code{verbose} to \code{TRUE}.
#'
#' @param x A character vector of numerical strings.
#'
#' @return The updated vector, usually the column of a data frame.
#' 
#' @importFrom stats na.exclude
#' @export
#' 
#' @examples
#' fix_mobile("803-123-4567")    # Adds leading '0" and removes separators
#' 
fix_mobile <- function(x) {
  if (!is.null(x) && all(is.na(x)) || is.numeric(x))
    x <- as.character(x)
  
  if (!is.character(x)) 
    cli::cli_abort("Objects of type {sQuote(typeof(x))} are not supported")

  # Existing prefix is removed so we can focus on actual numbers
  x <- stringi::stri_trim_both(x)
  prefix.rgx <- "^(\\+?234\\s?)"
  prefixed <- grepl(prefix.rgx, x)
  hasprefix <- any(prefixed)
  
  if (hasprefix) {
    prefix <- "+234"  # TODO: Extract actual prefixes for vectorized operations
    x <- sub(prefix.rgx, "\\2", x)
  }
  
  x <- .remove_char_O(x)
  
  # Separators are checked and removed
  x <- vapply(x, .process_separators, character(1), USE.NAMES = TRUE)
  
  # place prefix where necessary
  prefix <- if (hasprefix)
    prefix[which(prefixed)[1]]
  else
    "0"
  
  x <- sub("(^\\d{10}$)", paste0(prefix, "\\1"), x)
  like.mobile <- grepl("[7-9][0-1]\\d{8}$", x)
  
  if (getOption("verbose")) {
    outnums <- paste(na.exclude(x)[!like.mobile], collapse = ', ')
    cli::cli_warn("Additional original/transformed number removed: {outnums}")
  }
  
  ifelse(like.mobile, x, NA_character_)
}





# Checks for non-digit characters that could be separators
# specifically, spaces, dashes and dots (- and .).
# NB: The initial thinking was to maintain separators within the numbers, but
# this has be jettisoned. However, with this function, working with these
# separators becomes a lot easier, and if in future it is desirable to maintain 
# the formatting, this could be done more easily.
#
#' @importFrom cli cli_warn
.process_separators <- function(str) {
  stopifnot(is.character(str))
  
  warn_when_verbose <- function() {
    if (getOption("verbose"))
      cli_warn("{sQuote(str)} {msg}")
  }
  
  chars <- charToRaw(str)
  notd <- which(!(chars >= 0x30 & chars <= 0x39))
  sep <- chars[notd]
  
  # When the separators differ, the number is considered unusable. This may also
  # mean that other characters exist that have nothing to do with phone numbers.
  msg <- " was removed"
  
  if (isFALSE(Reduce(identical, sep))) {
    warn_when_verbose()
    return(NA_character_)
  }
  
  # Remove entries that are beyond redemption i.e. too long or too short
  # The separators will be considered with the number of actual digits.
  nsep <- length(sep)
  nlenmax <- nsep + 11L
  nlenmin <- nsep + 10L
  
  if (nchar(str) > nlenmax || nchar(str) < nlenmin) {
    warn_when_verbose()
    return(NA_character_)
  }
  
  if (length(notd))
    chars <- chars[-notd]
  
  rawToChar(chars)
}



# Pre-empts situations where letter 'O' instead of zero,
# making replacements where necessary
.remove_char_O <- function(x) {
  stopifnot(is.character(x))
  orgx <- "O"
  
  if (getOption("verbose")) {
    remv <- x[grepl(orgx, x, ignore.case = TRUE)]
    removed.nums <- paste(remv, collapse = ', ')
    cli::cli_warn("{length(remv)} numbers were removed: {removed.nums}")
  }
  
  gsub(orgx, "0", x, ignore.case = TRUE)
}
