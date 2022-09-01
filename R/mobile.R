#' Fix mobile numbers
#'
#' Fixes up local mobile phone numbers to a uniform text format.
#'
#' @details This format is specific to that used in a given location - for
#' now the function is useful only for Nigeria mobile numbers, which come in
#' the format expressed by the regex pattern \code{"^0[7-9][0-1][0-9]{8}$"}.
#'
#' @param x A character vector of numerical strings.
#'
#' @return The updated vector, usually the column of a data frame.
#' 
#' @importFrom magrittr %>%
#' @importFrom stringi stri_trim_both
#'
#' @export
fix_mobile <- function(x) {
  if (!is.null(x) && all(is.na(x)) || is.numeric(x))
    x <- as.character(x)
  
  if (!is.character(x)) 
    stop(sprintf("Objects of type %s are not supported", sQuote(typeof(x))))

  # Existing prefix is removed so we can focus on actual numbers
  x <- stringi::stri_trim_both(x)
  prefix.rgx <- "^(\\+?234\\s?)"
  prefixed <- grepl(prefix.rgx, x)
  
  if (any(prefixed)) {
    prefix <- "+234"  # TODO: Extract actual prefixes for vectorized operations
    x <- sub(prefix.rgx, "\\2", x)
  }
  
  # Pre-empt a situation where the letter 'O' is mistakenly
  # entered as a leading zero.
  x <- sub("^O", "0", x)
  
  # Separators are checked
  x <- vapply(x, .processSeparators, character(1), USE.NAMES = TRUE)
  
  # place prefix where necessary
  prefix <- if (any(prefixed)) prefix[which(prefixed)[1]] else "0"
  x <- sub("(^\\d{10}$)", paste0(prefix, "\\1"), x)
  
  # remove if it still doesn't look like a mobile number
  ifelse(grepl("[7-9][0-1]\\d{8}$", x), x, NA_character_)
}





# Checks for non-digit characters that could be separators
# specifically, spaces, dashes and dots (- and .).
# NB: The initial thinking was to maintain separators within 
# the numbers, but this has be jettisoned. However, with this 
# function, working with these separators becomes a lot
# easier, and if in future it is desirable to maintain the
# formatting, this could be done more easily.
.processSeparators <- function(str) {
  chars <- charToRaw(str)
  notd <- which(!(chars >= 0x30 & chars <= 0x39))
  sep <- chars[notd]
  
  # When the separators differ, the number
  # is considered unusable. This may also
  # mean that other characters exist that
  # have nothing to do with phone numbers.
  if (isFALSE(Reduce(identical, sep)))
    return(NA_character_)
  
  # Remove entries that are beyond redemption i.e. too long or too short
  # The separators will be considered with the number of actual digits.
  nsep <- length(sep)
  nlenmax <- nsep + 11L
  nlenmin <- nsep + 10L
  
  if (nchar(str) > nlenmax || nchar(str) < nlenmin)
    return(NA_character_)
  
  if (length(notd))
    chars <- chars[-notd]
  
  rawToChar(chars)
}
