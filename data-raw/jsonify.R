devtools::load_all()

if (!requireNamespace("magrittr"))
  install.packages("magrittr", repos = "https://cran.rstudio.com")

library(magrittr)

dir <- here::here("data-raw")

lgalist <- 
  read.csv(file.path(dir, "nglga.csv")) %>% 
  transform(state = sub(" State", "", state)) %>%     # note whitespace removed
  split(.[["state"]]) %>% 
  lapply(extract2, i = "lga")

lgajson <- lgalist %>%
  jsonlite:::toJSON(pretty = TRUE)

cat(lgajson, file = file.path(dir, "lgas.json"))
