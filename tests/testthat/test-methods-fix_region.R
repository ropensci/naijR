test_that("input is validated before fixing state names", {
  
  errchr <- "'x' is not a character vector"
  expect_error(fix_region(99), errchr)
  expect_error(fix_region(NA), errchr)
  expect_error(fix_region(c(NA, NA, NA)), errchr)
  expect_error(fix_region(NULL), errchr)
  expect_error(fix_region(TRUE), errchr)
  
  
  expect_error(fix_region(""), "'x' only has empty strings")
  expect_warning(try(fix_region(c("Ogin", "", "Abia")), silent = TRUE),
                 "Tried to fix empty strings - may produce errors")
  
  warn0 <- "'x' has length 0L or only missing values"
  expect_warning(fix_region(NA_character_), warn0)
  expect_warning(fix_region(character()), warn0)
  
  expect_type(fix_region(matrix(states())), "character") ## preserve class??
})


test_that("various cases for fixing state names", {
  ss <- states()
  ss2 <- suppressWarnings(states(c("Oyo", "Legos")))
  ssx <- suppressWarnings(states(c("xxx", "Benue")))
  ss.us <- c("kentucky", "Bornu", "Abia")
  
  expect_equivalent(fix_region(ss), ss)
  expect_identical(fix_region('Fct'), "Federal Capital Territory")
  expect_identical(fix_region('Kane'), "Kano")
  expect_identical(fix_region('plateau'), 'Plateau')
  
  fixed2 <- suppressMessages(fix_region(ss2))
  expect_identical(fixed2, states(c("Oyo", "Lagos")))
  expect_length(fixed2, 2L)
  
  fixed.x <- suppressMessages(fix_region(ssx))
  expect_match(attr(fixed.x, 'misspelt'), "xxx")
  expect_length(fixed.x, 2L)
  
  expect_identical(fix_region(ss.us), 
                   c("kentucky", "Borno", "Abia"))
  expect_length(fix_region(ss.us), 3L)
})




test_that("Misspelt LGA can be fixed (limited)", {
  result <- suppressWarnings(
    fix_region(lgas(c("Amuwo Odofin", "Lagos Island"))))
  
  expect_equivalent(result, c("Amuwo-Odofin", "Lagos Island"))
  
})
