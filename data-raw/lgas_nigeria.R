# Processing LGA dataset
lgas_nigeria <- read.csv(here::here("data-raw", "nglga.csv"))

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
