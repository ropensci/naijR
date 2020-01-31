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
#' @export
fix_mobile <- function(x) {
  if (is.numeric(x)) {
    # options(scipen = 999)
    x <- as.character(x)
  }
  # Remove entries that are beyond redemption i.e. too long or too short
  x <- ifelse(nchar(x) > 11 | nchar(x) < 10, NA_character_, x)
  
  # Add a leading '0' if there are 10 digits
  x <- as.character(x)
  x <- sub("(^[0-9]{10}$)", "0\\1", x)
  
  # Remove those that still don't look like local mobile numbers (NG)
  ifelse(grepl("^0[7-9][0-1][0-9]{8}$", x), x, NA_character_)
}
