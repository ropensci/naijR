# admin.R
# Create package data for administrative regions from raw data
#
# Dependencies ----
library(usethis)
library(cli)
devtools::load_all()

check_exist <- function(obj) {
  fname <- paste0(deparse(substitute(obj)), ".rda")
  file.exists(here::here("data", fname))
}

getLGAlist <- function(state) {
  require(WikidataR)
  cli_inform("* {state}")
  
  if (!identical(state, "Federal Capital Territory"))
    state <- paste(state, "State")
  
  stateitem <- find_item(state)
  stateid <- stateitem[[1]]$id
  statedata <- get_item(stateid)
  lgids <- statedata[[1]]$claims$P150$mainsnak$datavalue$value$id
  lgaobjlist <- get_item(lgids)
  
  statelgas <-
    vapply(lgaobjlist, 
           function(x) x$label$en$value, 
           character(1))
  
  data.frame(lga = statelgas)
}

# States ----
# We'll include ISO 3166-2 codes for the States
#
states_nigeria <- ISOcodes::ISO_3166_2 |>
  subset(startsWith(Code, "NG"), select = Code:Name) |>
  transform(Name = sub("^Abuja\\s", "", Name)) |>
  setNames(c("isocode", "state")) |>
  .__addGPZ("state", "gpz")

cli_inform("Saving object with {nrow(states_nigeria)} States")
use_data(states_nigeria, overwrite = check_exist(states_nigeria))


# Local Government Areas ----
cli_inform("Fetching list of LGAs from external source:")

# used `sapply` without simplification so we can capture the names
lgas_by_state <-
  sapply(states_nigeria$state, getLGAlist, simplify = FALSE)
lgadata <- purrr::list_rbind(lgas_by_state, names_to = "state")

lgas_nigeria <- lgadata |>
  merge(states_nigeria, by = "state") |>
  subset(select = -isocode)

cli_inform("Saving object with {nrow(lgas_nigeria)} LGAs")
use_data(lgas_nigeria, overwrite =  check_exist(lgas_nigeria))
