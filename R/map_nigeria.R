map_nigeria <- function(fill = TRUE, col = "lightgrey", adjoin = FALSE)
{
  require(maps)
  require(mapdata)
  db <- "worldHires"
  map(db, "Nigeria", fill = fill, col = col)
  if(adjoin) {
    adjoin.cntry <- c("Cameroon", "Chad", "Niger", "Benin")
    join <- function(xx) {
      map(db, xx, add = TRUE)
    }
    join(adjoin.cntry)
  }
}