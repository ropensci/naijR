totalerr <- 
  "reconstructing 'x' with `states()` or `lgas()` for a more reliable fix"

test_that("input is validated before fixing state names", {
  errchr <- "'x' is not a character vector"
  warn0 <- "'x' has length 0L or only missing values"
  
  expect_error(fix_region(99), errchr)
  expect_error(fix_region(NA), errchr)
  expect_error(fix_region(c(NA, NA, NA)), errchr)
  expect_error(fix_region(NULL), errchr)
  expect_error(fix_region(TRUE), errchr)
  expect_error(fix_region(""), "'x' only has empty strings")
  expect_warning(try(fix_region(c("Ogin", "", "Abia")), silent = TRUE),
                 "Tried to fix empty strings - may produce errors")
  expect_warning(fix_region(NA_character_), warn0)
  expect_warning(fix_region(character()), warn0)
  expect_type(fix_region(matrix(states())), "character") ## preserve class??
})


test_that("Messaging is clear when fixing regions via character vectors", {
  # Function for creating regular expressions for matching messages
  .msgfunc <- function(x) {
    stopifnot(grepl("^(.+)(\\s=>\\s)(.+)$", x))
    sprintf("Successful fix\\(es\\)\\:\\n\\-+\\n\\*\\s%s", x)
  } #                                                 ^
  #                                               Note place-holder
  
  lg <- c("Fufure", "Demsa", "Fufore", "Machika", "Ganye", "Noman", "Fufure")
  correctLga <- lg[2]
  misspeltLga <- lg[3]
  bothlga <- c(correctLga, misspeltLga)
  multi.lga <- readRDS("data/mispelt-lga.rds")
  change1 <- "Fufore => Fufure"
  change2 <- "Fafure => Fufure"
  msg1 <- .msgfunc(change1)
  msg2 <- .msgfunc(change2)
  
  # ----
  
  expect_silent(fix_region(lgas(correctLga)))
  expect_message(fix_region(lgas(misspeltLga, warn = FALSE)))
  expect_message(fix_region(lgas(bothlga, warn = FALSE)), msg1)
  expect_message(fix_region(lgas(c(bothlga, "Fufore"), warn = FALSE)), msg1)
  expect_message(fix_region(lgas(c(bothlga, "Fafure"), warn = FALSE)),
                 sprintf("%s\\n\\*\\s%s", change1, change2))
  expect_error(fix_region(misspeltLga), totalerr, fixed = TRUE)
  expect_warning(fix_region(lgas(lg, warn = FALSE)), 
                 "approximately matched more than one region")
  expect_message(suppressWarnings(fix_region(lgas(lg, warn = FALSE))),
                 sprintf("%s.+Noman => Numan", change1))
  expect_message(fix_region(lgas(multi.lga, warn = FALSE)),
                 change1)
})


test_that("various cases for fixing state names", {
  ss <- states()
  ss2 <- states(c("Oyo", "Legos"), warn = FALSE)
  ssx <- states(c("xxx", "Benue"), warn = FALSE)
  ss.us <- c("kentucky", "Bornu", "Abia")
  fedcap <- "Federal Capital Territory"
  
  expect_equivalent(fix_region(ss), ss)
  expect_error(fix_region('Fct'), totalerr, fixed = TRUE)
  expect_error(fix_region('Kane'), totalerr, fixed = TRUE)
  expect_error(fix_region('plateau'), totalerr, fixed = TRUE)
  expect_identical(fix_region('FCT'), fedcap)
  expect_identical(fix_region(states('Fct', warn = FALSE)), states(fedcap))
  expect_identical(fix_region(states('Kane', warn = FALSE)), states("Kano"))
  expect_identical(fix_region(states('plateau', warn = FALSE)), states('Plateau'))
  
  fixed2 <- suppressMessages(fix_region(ss2))
  expect_identical(fixed2, states(c("Oyo", "Lagos"), warn = FALSE))
  expect_length(fixed2, 2L)
  
  fixed.x <- suppressMessages(fix_region(ssx))
  expect_length(fixed.x, 2L)
  expect_identical(fix_region(ss.us), c("kentucky", "Borno", "Abia"))
  expect_length(fix_region(ss.us), 3L)
})




test_that("Misspelt LGA can be fixed (limited)", {
  result <- fix_region(lgas(c("Amuwo Odofin", "Lagos Island"), warn = FALSE))
  
  expect_equivalent(result, c("Amuwo-Odofin", "Lagos Island"))
  
})
