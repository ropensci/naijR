# states_nigeria.R

# We'll include ISO 3166-2 codes for the States
library(ISOcodes)

data("ISO_3166_2")
iso <- ISO_3166_2

states_nigeria <- subset(iso, startsWith(Code, "NG"), select = Code:Name)
states_nigeria$Name <- sub("^Abuja\\s", "", states_nigeria$Name)

if (!identical(names(states_nigeria), c("Code", "Name")))
  stop("The 'states_nigeria' dataset as not saved due to a column mismatch")

statedata <- here::here("data", "states_nigeria.rda")
usethis::use_data(states_nigeria, overwrite = file.exists(statedata))
