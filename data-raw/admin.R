# Source file: admin.R
#
# GPL-3 License
#
# Copyright (c) 2020-2023 Victor Ordu

# Create package data for administrative regions from raw data
# 
# This script will, amongst other things, download LGA data from Wikidata.
# Since this source is open-access and modifiable, internal check have been
# provided within the package infrastructure. Also, in the event of such
# modifications, the source can be edited prior to running this script.
# NOTE: This script takes a long time to run, so don't be in a hurry to 
# replace the existing data. (TODO: Make provision for caching!)

library(purrr)
library(cli)
library(usethis)
library(ISOcodes)

local({
  # Adds a column for the geo-political zones to an
  # existing data frame that has a character column
  # of the States.
  # Arguments:
  # - data: The data frame
  # - statehdr: The name of the column with the States.
  # - zonehdr: The proposed name of the column with the zones
  #
  # Returns the modified data frame
  add_gpz <- \(data, statehdr, zonehdr) {
    stopifnot(exprs = {
      is.data.frame(data)
      is.character(statehdr)
      is.character(zonehdr)
    })
    
    data[[zonehdr]] <- NA_character_
    statelist <- get_all_states()
    
    for (gpz in names(statelist)) {
      rgx <- paste(statelist[[gpz]], collapse = "|")
      index <- grep(rgx, data[[statehdr]])
      data[[zonehdr]][index] <- gpz
    }
    
    data
  }
  
  rda_exists <- \(obj) {
    fname <- paste0(deparse(substitute(obj)), ".rda")
    file.exists(here::here("data", fname))
  }
  
  get_lga_list <- \(state) {
    require(WikidataR)
    cli_inform("* {state}")
    fct <- "Federal Capital Territory"
    
    state.in.full <-
      if (identical(state, fct))
        fct
    else
      paste(state, "State")
    
    tryCatch({
      stateitem <- find_item(state.in.full)
    },
    error = \(e) {
      cli_alert("Could not find {state.in.full}")
      cli_warn(conditionMessage(e))
    })
    
    stateid <- stateitem[[1]]$id
    
    tryCatch({
      statedata <- get_item(stateid)
    },
    error = \(e) {
      cli_alert("Item ID for {state.in.full} could not be fetched")
      cli_warn(conditionMessage(e))
    })
    
    lgids <- statedata[[1]]$claims$P150$mainsnak$datavalue$value$id
    
    tryCatch({
      lgaobjlist <- get_item(lgids)
    },
    error = \(e) {
      cli_alert("Could not fetch list of LGAs for {state.in.full}")
      cli_warn(conditionMessage(e))
    })
    
    statelgas <-
      vapply(lgaobjlist, \(x) x$label$en$value, character(1))
    
    if (length(statelgas) == 0L)
      cli_abort("LGAs could not be read for {state}")
    
    data.frame(lga = statelgas)
  }
  
  # States ----
  # We include ISO 3166-2 codes for the States
  states_nigeria <- ISO_3166_2 |>
    subset(startsWith(Code, "NG"), select = Code:Name) |>
    transform(Name = sub("^Abuja\\s", "", Name)) |>
    stats::setNames(c("isocode", "state")) |>
    add_gpz("state", "gpz")
  
  cli_inform("Saving object with {nrow(states_nigeria)} States")
  use_data(states_nigeria, overwrite = rda_exists(states_nigeria))
  
  
  # Local Government Areas ----
  cli_inform("Fetching list of LGAs from external source:")
  
  # NoTE: `sapply()` is used because we need to capture names succinctly
  lgas_nigeria <- states_nigeria$state |>
    sapply(get_lga_list, simplify = FALSE) |>
    list_rbind(names_to = "state") |>
    merge(states_nigeria, by = "state") |>
    subset(select = -isocode)
  
  LGAtotal <- 774L
  LGAdownloaded <- nrow(lgas_nigeria)
  
  if (LGAdownloaded != LGAtotal)
    cli_abort("Only {LGAdownloaded}/{LGAtotal} LGAs were downloaded")
  
  cli_inform("Saving LGA data")
  use_data(lgas_nigeria, overwrite = rda_exists(lgas_nigeria))
})
