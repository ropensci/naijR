dir <- here::here('data-raw')
file <- file.path(dir, "list_of_local_government_areas_of_nigeria-1729j.csv")
lgas_nigeria <- read.csv(file, stringsAsFactors = FALSE)
lgas_nigeria <- lgas_nigeria[, !colnames(lgas_nigeria) %in% c("SNo", "X")]

lgas_nigeria <- within(lgas_nigeria, {
  State <- State |>
    as.factor() |> 
    (\(x) sub("State", "", x))() |>
    (\(x) gsub("\\s*", "", x))() |>
    (\(x) sub("Nassarawa", "Nasarawa", x))() |>
    (\(x) sub("AkwaIbom", "Akwa Ibom", x))() |>
    (\(x) sub("CrossRiver", "Cross River", x))() |>
    (\(x) sub("FCT", "Federal Capital Territory", x))()
})

colnames(lgas_nigeria) <- c("lga", "state")
write.csv(lgas_nigeria, file = file.path(dir, "nglga.csv"), row.names = FALSE)
