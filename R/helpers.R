# Internal helper functions
# This file exists for helper functions that are used across multiple scripts.


# Sets the Levenshtein distance being used package-wide for functions that
# carry out partial matching
.pkgLevDistance <- function() 2L