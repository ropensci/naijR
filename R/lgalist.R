#' Names of Local Government Areas in Nigeria
#' 
#' @param 
#' @return Local Government Areas in Nigeria
#' @details 
#' @examples 
lgalist <- function(state = NULL)
{
  d <- data("lga-by-state.csv")
  if (is.null(state))
    lg <- d[, 2]
  else
  {
    sbd <- d[d$state == as.character(state), ]
    lg <- sbd[, 2] 
  }
  lg
}
