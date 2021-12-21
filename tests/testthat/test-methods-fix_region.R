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
  # expect_type(fix_region(matrix(states())), "character") ## preserve class??
})


test_that("Messaging is clear when fixing regions via character vectors", {
  lg <- c("Fufure", "Demsa", "Fufore", "Machika", "Ganye", "Noman", "Fufure")
  correctLga <- lg[2]
  misspeltLga <- lg[3]
  bothlga <- c(correctLga, misspeltLga)
  hdr.rgx <- "Successful fix\\(es\\)\\:.+\\*\\s"
  fuf.rgx <- "Fufore => Fufure"
  
  expect_silent(fix_region(lgas(correctLga)))
  expect_message(fix_region(lgas(misspeltLga, warn = FALSE)), fuf.rgx)
  expect_message(fix_region(bothlga), fuf.rgx)
  expect_warning(fix_region(lgas(lg, warn = FALSE)), 
                 "approximately matched more than one region")
  expect_message(suppressWarnings(fix_region(lgas(lg))),
                 paste0(fuf.rgx, ".+Noman => Numan"))
  expect_message(fix_region(lgas(bothlga, warn = FALSE)),
                 paste0(hdr.rgx, fuf.rgx))
  expect_message(suppressWarnings(fix_region(lg)),
                 paste0(hdr.rgx, fuf.rgx, "\\n"))  # TODO: Check again
  expect_message(fix_region(c("Owerri north", "Owerri West")),
                 paste0(hdr.rgx, "Owerri north => Owerri North"))
})


test_that("various cases for fixing state names", {
  ss <- states()
  ss2 <- states(c("Oyo", "Legos"), warn = FALSE)
  ssx <- states(c("xxx", "Benue"), warn = FALSE)
  ss.us <- c("kentucky", "Bornu", "Abia")
  
  expect_equivalent(fix_region(ss), ss)
  expect_error(fix_region('Fct'), "Incorrect region name")
  expect_error(fix_region('Kane'))
  expect_error(fix_region('plateau'))
  
  fixed2 <- suppressMessages(fix_region(ss2))
  expect_identical(fixed2, states(c("Oyo", "Lagos"), warn = FALSE))
  expect_length(fixed2, 2L)
  
  fixed.x <- suppressMessages(fix_region(ssx))
  expect_length(fixed.x, 2L)
  
  expect_identical(fix_region(ss.us),
                   c("kentucky", "Borno", "Abia"))
  expect_length(fix_region(ss.us), 3L)
})




test_that("Misspelt LGA can be fixed (limited)", {
  result <- fix_region(lgas(c("Amuwo Odofin", "Lagos Island"), warn = FALSE))
  
  expect_equivalent(result, c("Amuwo-Odofin", "Lagos Island"))
  expect_error(fix_region("Legos Island"))
  expect_error(fix_region(c("Amuwo Odofin", "Legos Island")), 
               "Incorrect region name") # both misspelt!
  
})



test_that("outputs", {
  expect_invisible(fix_region(c("Fufore", "Demsa")))
})




test_that("regions can be fixed manually", {
  bs <- states(c("Oyo", "Lagos", "Abya"), warn = FALSE)
  bl <- lgas(c("Damboo", "Biu", "Hawl", "Shank", "Damboe"), warn = FALSE)
  lag <-  "Lagos"
  errTyp <- "The operation cannot be done on objects of type"
  output2 <- lgas(c("Damboo", "Biu", "Hawul", "Shani", "Damboe"), warn = FALSE)
  wrong2 <- c("Hawl", "Shank")
  
  expect_error(fix_region_manual(bs, lag, "Legos"),
               "'Legos' is not a valid region")
  for (elem in list(999L, NULL, NA, TRUE, pi))
    expect_error(fix_region_manual(elem, lag, "Lagos"), errTyp)
  # expect_error(fix_region_manual("TRUE", lag, "Lagos"))
  expect_identical(fix_region_manual(bs, "Abya", "Abia"),
                   states(c("Oyo", "Lagos", "Abia")))
  expect_identical(fix_region_manual(bl, c("Damboo", "Damboe"), "Damboa"),
                   lgas(c("Damboa", "Biu", "Hawl", "Shank", "Damboa"), 
                        warn = FALSE))
  expect_identical(fix_region_manual(bl, wrong2, c("Hawul", "Shani")), output2)
  expect_warning(fix_region_manual(bl, wrong2, c("Hawul", "FakeLG")), 
               "'FakeLG' is not a valid region")
  expect_error(
    fix_region_manual(
      bl, 
      c("Hawl", "shank"),         # used the wrong case in element #2
      c("Hawul", "FakeLG")),
    regexp = "'shank' is not an element of 'bl'"
  )   
  
})
