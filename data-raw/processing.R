# processing sandbox

################################################################################
# Processing LGA dataset
lgas_nigeria <- read.csv("list_of_local_government_areas_of_nigeria-1729j.csv",
                         stringsAsFactors = FALSE)
str(lgas_nigeria)
lgas_nigeria <- lgas_nigeria[-c(775:776), ]
nrow(lgas_nigeria)
lgas_nigeria <- lgas_nigeria[, -c(1, 4)]
ncol(lgas_nigeria)
lgas_nigeria$State <- as.factor(lgas_nigeria$State)
str(lgas_nigeria)
head(lgas_nigeria)
colnames(lgas_nigeria) <- c("lga", "state")
head(lgas_nigeria)
lgas_nigeria$state <- stringr::str_replace(lgas_nigeria$state, "State", "")
lgas_nigeria$state <- stringr::str_trim(lgas_nigeria$state)
head(lgas_nigeria)
devtools::use_data(lgas_nigeria)
#######################################################################################