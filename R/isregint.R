# Source file: isregint.R
#
# GPL-3 License
#
# Copyright (C) 2019-2023 Victor Ordu.

# Checks whether an object has all its elements as States or LGAs
.all_are_regions <- function(x) {
  stopifnot(isFALSE(is.null(x)))
  all(is_state(x)) || all(is_lga(x))
}




.some_are_regions <- function(x) {
  stopifnot(isFALSE(is.null(x)))
  isFALSE(.all_are_regions(x)) && (any(is_state(x)) || any(is_lga(x)))
}
