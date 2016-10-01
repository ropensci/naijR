#' Names of Local Government Areas in Nigeria
#' 
#' @param 
#' @return Local Government Areas in Nigeria
#' @details 
#' @examples 
lgalist <- function(state = NULL)
  {
  ld <- load("data/lgas_nigeria.rda")
  for (i in 1:2)
    ld[, i] <- as.character(ld[, i])
  st <- states()
  st <- grep(state, ignore.case = TRUE, value = TRUE)
  ld <- ld[, 2 == st]
  lgs <- ld
  print(lgs)
  }

# algorithm
# print out LGAs in the country
# print out LGAs by State
#