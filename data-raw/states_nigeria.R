# iso.R

# Inclusion of ISO 3166-2 codes for national subdivisions i.e. States
library(ISOcodes)
data("ISO_3166_2")

iso <- ISO_3166_2
ng <- grep("^NG", iso$Code)
states_ng <- iso[ng, c("Code", "Name")]
states_ng$Name <- sub("^Abuja\\s", "", states_ng$Name)
