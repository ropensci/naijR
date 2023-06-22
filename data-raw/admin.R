# admin.R
# Create package data for administrative regions from raw data
#
# Dependencies ----
suppressPackageStartupMessages(library(here))
library(usethis)
devtools::load_all()

check_exist <- function(obj) {
    fname <- paste0(deparse(substitute(obj)), ".rda")
    file.exists(here("data", fname))
}

# States ----
# We'll include ISO 3166-2 codes for the States
# 
states_nigeria <- ISOcodes::ISO_3166_2 |>
  subset(startsWith(Code, "NG"), select = Code:Name) |>
  transform(Name = sub("^Abuja\\s", "", Name)) |>
  setNames(c("isocode", "state")) |>
  .__addGPZ("state", "gpz")

use_data(states_nigeria, overwrite = check_exist(states_nigeria))


# Local Government Areas ----
file <- here("data-raw", "list_of_local_government_areas_of_nigeria-1729j.csv")

lgas_nigeria <- read.csv(file) |>
  subset(select = -SNo) |>
  transform(
    State = sub("(\\s)State", "", State) |>
            sub("Nassarawa", "Nasarawa", x = _) |>
            sub("FCT", "Federal Capital Territory", x = _)
  ) |>
  setNames(c("lga", "state")) |>
  merge(states_nigeria, by = "state") |>
  subset(select = -isocode)

use_data(lgas_nigeria, overwrite =  check_exist(lgas_nigeria))
