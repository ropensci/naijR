# Source file: shpfileobj.R 
# 
# GPL-3 License
# 
# Copyright (c) 2020-2023 Victor Ordu


# Preparation of the `ShapefileProps` objects used in the package
#
# The spatial data used in this packages is prepared before hand by 
# defining a special class of object called `ShapefileProps`. This 
# object contains the spatial data, as well as other fields that are
# used in retrieving data requested by functions. These objects are 
# loaded into the package namespace once the package itself is
# loaded. Whenever this script is run, the objects will be recreated.
datafile <- here::here("R/sysdata.rda")

if (file.exists(datafile))
  file.remove(datafile)

devtools::load_all()
regionlevels <- c("state", "lga")
namefields <- structure(c("admin1Name", "STATE"), names = regionlevels)

for (i in regionlevels) {
  
  args <- if (identical(i, "state")) {
    list(states, gpz = NULL, all = TRUE, warn = TRUE)
  }
  else if (identical(i, "lga")) {
    list(region = NA_character_, strict = FALSE, warn = TRUE)
  }
  else
    stop(sprintf("No argument list"))
  
  regionfun <- paste0(i, "s")
  allregions <- do.call(regionfun, args = args)
  obj <- do.call("ShapefileProps", args = list(region = allregions))
  assign(paste0("shp.", i), obj, envir = globalenv())
}

shpobjs <- grep("^shp\\.", ls(), value = TRUE)
save(list = shpobjs, file = datafile)
