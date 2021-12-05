# Processing LGA dataset

dir <- here::here('data-raw')
file <- file.path(dir, "list_of_local_government_areas_of_nigeria-1729j.csv")
lgas_nigeria <- read.csv(file, stringsAsFactors = FALSE)

lastTwoRows <- c(775:776)
lgas_nigeria <- lgas_nigeria[-lastTwoRows, ]

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

ss <- list(
  nc = c("Benue", "Kogi", "Kwara", "Nasarawa", "Niger", "Plateau"),
  ne = c("Adamawa", "Bauchi", "Borno", "Gombe", "Taraba", "Yobe"),
  nw = c("Jigawa", "Kaduna", "Kano", "Katsina", "Kebbi", "Sokoto", "Zamfara"),
  se = c("Abia", "Anambra", "Ebonyi", "Enugu", "Imo"),
  ss = c("Akwa Ibom", "Bayelsa", "Cross River", "Delta", "Edo", "Rivers"),
  sw = c("Ekiti", "Lagos", "Ogun", "Ondo", "Osun", "Oyo"),
  fct = "Federal Capital Territory"
  )

lgas_nigeria$gpz <- NA

for (n in names(ss)) {
  rgx <- paste(ss[[n]], collapse = "|")
  matched <- grepl(rgx, lgas_nigeria$state)
  lgas_nigeria$gpz[matched] <- n
}

usethis::use_data(lgas_nigeria, overwrite = file.exists("data/lgas_nigeria.rda"))
