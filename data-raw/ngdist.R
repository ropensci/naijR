# Source file: ngdist.R
#
# GPL-3 License
#
# Copyright (c) 2024-2026 Victor Ordu

# The creation of a distance matrix that contains the distances between State
# capitals of Nigeria (in kilometres)
local({
  devtools::load_all()
  distdata <- readr::read_csv(here::here("data-raw", "KM_CHART_NG.csv"))
  distdata <- tibble::column_to_rownames(distdata, var = "...1")
  ngdist <- as.dist(as.matrix(distdata))
  usethis::use_data(ngdist, overwrite = .__rda_exists(ngdist))
})
