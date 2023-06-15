# Scan and fix mismatches between LGAs in main data and the shapefile
devtools::load_all()

mismatch <- sapply(states(), \(x) try(.__lga_mismatch(x)), USE.NAMES = TRUE)
mismatch
