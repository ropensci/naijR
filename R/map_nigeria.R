#' Map of Nigeria
#' 
#' @param fill Whether to fill the plot or not
#' 
#' @param col The colour of the filled portion of the plot.
#' 
#' @param adjoin Whether to display borders of neighbouring countries.
#' 
#' @export
map_ng <- function(adjoin = FALSE, fill = TRUE, col = "lightgrey")
{
  db <- "worldHires"
  maps::map(db, "Nigeria", fill = fill, col = col)
  if(adjoin) {
    ct <- c("Cameroon", "Chad", "Niger", "Benin")
    jn <- function(xx) {
      maps::map(db, xx, add = TRUE)
    }
    jn(ct)
  }
}