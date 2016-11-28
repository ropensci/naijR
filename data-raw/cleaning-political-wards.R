# cleaning RA-LGA-ANALYSIS-NATIONWIDE.csv
library(stringr)

ward_data <- read.csv("data-raw/RA-LGA-ANALYSIS-NATIONWIDE.csv",
                      stringsAsFactors = F, na.strings = "")
dim(ward_data)
str(ward_data)

apply(ward_data, 2, function(x) sum(is.na(x)))
sum(complete.cases(ward_data))
ward_data <- ward_data[- which(is.na(ward_data$WARD.NAME)), ]


ward_data <- tidyr::fill(ward_data, LGA.NAME)
colnames(ward_data) <- gsub("([A-Z]+)(.NAME)", "\\1", colnames(ward_data))
colnames(ward_data)[2] <- str_to_title(colnames(ward_data)[2])

devtools::use_data(ward_data)
