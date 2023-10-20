test_that("State names shared with LGAs can be coerced into 'lgas' objects", {
  lgaclass <- "lgas"
  
  expect_s3_class(as_lga("Bauchi"), lgaclass)
  expect_s3_class(as_lga("Gombe"), lgaclass)
})


test_that("LGA names shared with States can be coerced into 'states' objects", {
  stateclass <- "states"
  
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

test_that("LGA and States can be disambiguated", {
  ## Input validation
  err.typecheck <- "Expected an object of class `lgas`"
  
  expect_error(disambiguate_lga(42L), err.typecheck)
  expect_error(disambiguate_lga(NULL), err.typecheck)
  expect_error(disambiguate_lga(pi), err.typecheck)
  expect_error(disambiguate_lga(TRUE), err.typecheck)
  expect_error(disambiguate_lga(list()), err.typecheck)
  expect_error(disambiguate_lga(data.frame()), err.typecheck)
  expect_error(disambiguate_lga(matrix(letters, 2)), err.typecheck)
  expect_error(disambiguate_lga("FCT"), err.typecheck)
  expect_error(disambiguate_lga("Ekiti"), err.typecheck)
  expect_error(disambiguate_lga(states("Ekiti")), err.typecheck)
  
  ## Logic
  nasstr <- "Nasarawa"
  benstr <- "Benue"
  obilga <- suppressWarnings(lgas("Obi"))
  obiben <- disambiguate_lga(obilga, benstr)
  obinas <- disambiguate_lga(obilga, nasstr)
  myattr <- "State"
  
  expect_s3_class(obiben, "lgas")
  expect_identical(attr(obiben, myattr), benstr)
  expect_error(disambiguate_lga(c(obilga, "Tarka")),
               "only done for objects with one element")
  expect_identical(attr(obinas, myattr), nasstr)
  expect_error(disambiguate_lga(obilga, "Imo"), "is not in [[:alpha:]]+ State")
  
  if (!interactive())
    expect_error(disambiguate_lga(obiben),
                 "This operation can only be done in interactive mode")
})
