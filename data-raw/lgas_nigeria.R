# Processing LGA dataset

library(magrittr, warn.conflicts = FALSE)

dir <- here::here('data-raw')
file <- file.path(dir, "list_of_local_government_areas_of_nigeria-1729j.csv")
lgas_nigeria <- read.csv(file, stringsAsFactors = FALSE)
str(lgas_nigeria)

lastTwoRows <- c(775:776)
lgas_nigeria[lastTwoRows, ]
lgas_nigeria <- lgas_nigeria[-lastTwoRows, ]
tail(lgas_nigeria)

lgas_nigeria <- lgas_nigeria[, !colnames(lgas_nigeria) %in% c("SNo", "X")]
lgas_nigeria <- within(lgas_nigeria, {
  State <- State %>% 
    as.factor() %>% 
    sub("State", "", .) %>% 
    gsub("\\s*", "", .)
})

colnames(lgas_nigeria) <- c("lga", "state")
head(lgas_nigeria)

usethis::use_data(lgas_nigeria, overwrite = file.exists("data/lgas_nigeria.rda"))
