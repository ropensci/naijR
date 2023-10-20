# Test an object for States
test_that("input is validated", {
  expect_warning(is_state(pi))
  expect_error(is_state(NULL), "Expected a non-null atomic vector as input")
  expect_length(suppressWarnings(is_state(pi)), 1L)
  expect_false(is_state(character()))
})

test_that("States can be identified in an object", {
  ss.good <- ss.bad <- ss.na <- states()
  
  ss.bad[c(5, 7, 19)] <- c("big", "bad", "wolf")
  ind.nas <- c(6, 19, 33)
  ss.na[ind.nas] <- NA
  
  good <- is_state(ss.good)
  bad <- is_state(ss.bad)
  has.na <- suppressWarnings(is_state(ss.na))
  
  all.good <- all(good)
  not.all.good <- all(bad)
  nas.but.good <- all(has.na, na.rm = TRUE)
  
  expect_length(all.good, 1L)
  expect_true((all.good))
  expect_length(not.all.good, 1L)
  expect_false(not.all.good)
  expect_length(good, length(ss.good))
  expect_length(bad, length(ss.bad))
  expect_length(has.na, length(ss.na))
  expect_length(good, 37L)
  expect_equal(sum(good), 37L)
  expect_length(bad, 37L)
  expect_equal(sum(bad), 34L)
  expect_true(nas.but.good)
  expect_warning(is_state(ss.na), "Invalid entries were replaced with NAs")
  expect_length(has.na, 37)
  expect_equal(sum(has.na), NA_integer_)
  expect_equal(sum(has.na, na.rm = TRUE), 34L)
  expect_true(anyNA(has.na))
  expect_equal(sum(nas.but.good), 1L)
  expect_equal(length(nas.but.good), 1L)
  expect_equal(sum(is.na(has.na)), length(ind.nas))
})

test_that("Different representations of the FCT are handled", {
  ss <- c(
    "Zamfara",
    "Niger",
    "Borno",
    "Kaduna",
    "Jigawa",
    "Taraba",
    "Benue",
    "Ogun",
    "Ekiti",
    "Rivers",
    "Lagos",
    "Delta",
    "Cross River",
    "Sokoto",
    "Kano",
    "Fct",
    "Oyo",
    "Edo",
    "Osun",
    "Plateau",
    "Abia",
    "Akwa Ibom",
    "Bauchi",
    "Ondo",
    "Gombe",
    "Kogi",
    "Adamawa",
    "Katsina",
    "Enugu",
    "Yobe",
    "Ebonyi",
    "Imo",
    "Kwara",
    "Anambra",
    "Nasarawa",
    "Bayelsa",
    "Kebbi"
  )
  
  expect_true(is_state("FCT"))
  expect_true(is_state("Federal Capital Territory"))
  expect_false(sum(is_state(ss)) == length(ss))
  expect_false(is_state("Fct"))
})





test_that("Mispelt LGA are discovered", {
  ibd <- c(
    'Akinyele',
    'Egbeda',
    'Ibadan North',
    'Ibadan North East',         # misspelt
    'Ibadan North West',         # misspelt
    'Ibadan South East',         # misspelt
    'Ibadan South West',         # misspelt
    'Iddo',                      # misspelt
    'Lagelu',
    'Oluyole',
    'Onu-Ara'                    # misspelt
  )
  
  xx <- is_lga(ibd)
  err <- "x should be of type 'character'"
  expect_false(all(xx))
  expect_error(is_lga(NULL), err)
  expect_error(is_lga(42), err)
  expect_error(is_lga(cars), err)
  expect_error(is_lga(c(TRUE, FALSE)), err)
  expect_type(xx, 'logical')
})





test_that("is_lga recognises LGAs", {
  anlga <- "Amuwo-Odofin"
  veclga <- c("Akira-Uba", "Hawul", NA)
  
  
  expect_true(is_lga(anlga))
  expect_false(all(is_lga(veclga)))
})



test_that("State/LGA synonyms are discernible", {
  rr <- lgas_like_states()
  
  for (i in rr) {
    expect_true(is_state(i))
    expect_true(is_lga(i))
    expect_true(is_state(states(i)))
    expect_true(all(is_lga(lgas(i))))
    expect_true(suppressWarnings(is_lga(lgas(i, strict = TRUE))))
  }
})


test_that("Warnings on multiple LGAs", {
  nas <- "Nasarawa"
  wrn <- "'Nasarawa' LGA is found in 2 States"
  
  expect_silent(lgas(nas))
  expect_warning(lgas(nas, strict = TRUE), wrn)
  expect_warning(lgas(nas, strict = TRUE, warn = FALSE), wrn)
})
