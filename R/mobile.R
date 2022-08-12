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
#'
#' @export
fix_mobile <- function(x) {
  if (!is.null(x) && all(is.na(x)) || is.numeric(x))
    x <- as.character(x)
  
  if (!is.character(x)) 
    stop(sprintf("Objects of type %s are not supported", sQuote(typeof(x))))
  
  # Remove entries that are beyond redemption i.e. too long or too short
  # then add a leading '0' if there are 10 digits
  # and then remove those that still don't look like local mobile numbers (NG)
  x %>% 
    ifelse(nchar(.) > 11 | nchar(.) < 10, NA_character_, .) %>% 
    sub("(^[0-9]{10}$)", "0\\1", .) %>% 
    ifelse(grepl("^0[7-9][0-1][0-9]{8}$", .), ., NA_character_)
}
