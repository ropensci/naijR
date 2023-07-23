test_that("State names shared with LGAs can be coerced into 'lgas' objects", {
  lgaclass <- lg.cl
  
  expect_s3_class(as_lga("Bauchi"), lgaclass)
  expect_s3_class(as_lga("Gombe"), lgaclass)
})


test_that("LGA names shared with States can be coerced into 'states' objects", {
  stateclass <- st.cl
  
  expect_s3_class(as_state("Kogi"), stateclass)
  expect_s3_class(as_state("Ebonyi"), stateclass)
})

test_that("Regions without synonyms are not treated after validation", {
  err <- "Expected a character vector"
  err2 <- "To coerce a region with synonyms, use a vector of length 1L"
  err3 <- "The object does not possess State/LGA synonyms"
  wrn <- "Object was stripped down to mode 'character'"
  int <- 999L
  dbl <- 999
  lgl <- TRUE
  twoRegs <- c("Katsina", "Ebonyi")
  
  expect_error(as_state(int), err)
  expect_error(as_lga(int), err)
  expect_error(as_state(dbl), err)
  expect_error(as_lga(dbl), err)
  expect_error(as_state(lgl), err)
  expect_error(as_lga(lgl), err)
  expect_error(as_lga(twoRegs), err2)
  expect_error(as_state(twoRegs), err2)
  expect_error(as_lga("Kano"), err3)
  expect_error(as_state("Kano"), err3)
  expect_error(as_state(states("Kano")), err3)
  expect_error(as_lga(lgas("Michika")), err3)
  expect_warning(as_state(states("Katsina")), wrn)
  expect_error(as_lga(lgas("Ebonyi")), err2)
  
  kt.st <- suppressWarnings(as_state(states("Katsina")))
  expect_length(class(kt.st), 3L)
})
