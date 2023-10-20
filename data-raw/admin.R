# Source file: admin.R
#
# GPL-3 License
#
# Copyright (c) 2020-2023 Victor Ordu

# Create package data for administrative regions from raw data
#
# Dependencies ----
devtools::load_all()

check_exist <- function(obj) {
  fname <- paste0(deparse(substitute(obj)), ".rda")
  file.exists(here::here("data", fname))
}

getLGAlist <- function(state) {
  cli::cli_inform("* {state}")
  
  if (!identical(state, "Federal Capital Territory"))
    state <- paste(state, "State")
  
  stateitem <- WikidataR::find_item(state)
  stateid <- stateitem[[1]]$id
  statedata <- WikidataR::get_item(stateid)
  lgids <- statedata[[1]]$claims$P150$mainsnak$datavalue$value$id
  lgaobjlist <- WikidataR::get_item(lgids)
  
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

cli::cli_inform("Saving object with {nrow(states_nigeria)} States")
usethis::use_data(states_nigeria, overwrite = check_exist(states_nigeria))


# Local Government Areas ----
cli::cli_inform("Fetching list of LGAs from external source:")

# used `sapply` without simplification so we can capture the names
lgas_by_state <-
  sapply(states_nigeria$state, getLGAlist, simplify = FALSE)
lgadata <- purrr::list_rbind(lgas_by_state, names_to = "state")

lgas_nigeria <- lgadata |>
  merge(states_nigeria, by = "state") |>
  subset(select = -isocode)

cli::cli_inform("Saving object with {nrow(lgas_nigeria)} LGAs")
usethis::use_data(lgas_nigeria, overwrite =  check_exist(lgas_nigeria))
