#' Fix up mobile numbers
#'
#' Fixes up mobile numbers to a uniform text format.
#'
#' @details This format is specific to that used in a given location - for
#' now the function is useful only for Nigeria mobile numbers which come in
#' the format expressed by the regex pattern \code{"^0[7-9][0-1][0-9]{8}$"}.
#'
#' @param column A character vector of numerical string characters.
#'
#' @importFrom dplyr %>%
#' @importFrom stringr str_replace
#'
#' @return An update vector, usually the column of a data frame.
#'
#' @export
fix_mobile <- function(column) {
  if (is.numeric(column)) {
    options(scipen = 999)
    column <- as.character(column)
  }
  # Remove entries that are beyond redemption i.e. too long or too short
  column <-
    ifelse(nchar(column) > 11 | nchar(column) < 10, NA_character_, column)
  
  # Add a leading '0' if there are 10 digits
  column <- column %>%
    as.character() %>%
    str_replace("(^[0-9]{10}$)", "0\\1")
  
  # Remove those that still don't look like local mobile numbers (NG)
  column <-
    ifelse(grepl("^0[7-9][0-1][0-9]{8}$", column), column, NA_character_)
}
