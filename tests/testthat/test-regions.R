# Copyright (C) 2019-2021 Victor Ordu.
# 
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>
library(naijR)

test_that("input is validated", {
  expect_error(states(gpz = 99))
  expect_error(states(gpz = NA))
  expect_error(states(gpz = "gpz"))
  expect_error(states(sorted = 99))
  expect_error(states(sorted = NULL))
  expect_error(states(sorted = NA))
  expect_error(states(sorted = "n"))
  expect_error(states(sorted = "sorted"))
  expect_error(states(full.names = 88))
  expect_error(states(full.names = "full"))
  expect_error(states(full.names = NA))
  expect_error(states(full.names = NULL))
})

test_that("Internal object listing states is created and retrievable", {
  default <- .getAllStates()
  notDefault <- .getAllStates(FALSE)
  
  expect_type(default, 'list')
  expect_named(default)
  expect_identical(names(default), c('nc', 'ne', 'nw', 'se', 'ss', 'sw', 'fct'))
  expect_type(notDefault, 'character')
  expect_null(names(notDefault))
})


# Test an object for States
test_that("input is validated", {
  expect_warning(is_state(pi))
  expect_error(is_state(NULL), "Expected a non-null atomic vector as input")
  expect_length(suppressWarnings(is_state(pi)), 1L)
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


test_that("input is validated before fixing state names", {
  errchr <- "'x' is not an object of class 'character'"

  expect_error(fix_region(99), errchr)
  expect_error(fix_region(NA), errchr)
  expect_error(fix_region(c(NA, NA, NA)), errchr)
  expect_warning(fix_region(NA_character_), "'x' has only missing values")
  expect_error(fix_region(NULL), errchr)
  expect_error(fix_region(TRUE), errchr)
  expect_error(fix_region(matrix(states())), errchr)
})


test_that("various cases for fixing state names", {
  ss <- states()
  ss2 <- c("oyo", "Legos")
  ssx <- c("xxx", "Benue")
  ss.us <- c("kentucky", "Bornu", "Abia")
  
  expect_identical(fix_region(ss), ss)
  expect_identical(fix_region('Fct'), "Federal Capital Territory")
  expect_identical(fix_region('Kane'), "Kano")
  expect_identical(fix_region('plateau'), 'Plateau')
  expect_identical(fix_region(ss2), c("Oyo", "Lagos"))
  expect_length(fix_region(ss2), 2L)
  expect_identical(fix_region(ssx), c(NA_character_, "Benue"))
  expect_length(fix_region(ssx), 2L)
  expect_identical(fix_region(ss.us), c(NA_character_, "Borno", "Abia"))
  expect_length(fix_region(ss.us), 3L)
})


test_that("FCT abbreviations are well handled", {
  fct_full <- 'Federal Capital Territory'
  expect_equivalent(.toggleFct(fct_full), 'FCT')
  expect_equivalent(.toggleFct('FCT'), fct_full)
})



## ---- LGA related tests ---- 

test_that("illegal input is caught early", {
  chErr <- "Expected an object of type 'character'"
  
  expect_error(lgas("Saarland"))
  expect_error(lgas("Maryland"),
               "One or more elements is not a valid region in Nigeria",
               fixed = TRUE)
  expect_error(lgas(888), chErr)
  expect_error(lgas(NULL), chErr)
  expect_error(lgas(TRUE), chErr)
  expect_error(lgas(3.14), chErr)
})


test_that("LGAs are returned correctly", {
  res <- lgas("Plateau")
  res2 <- lgas(nam <- c("Borno", "Abia"))
  
  expect_match(res, "Pankshin", all = FALSE)
  expect_length(res, 17L)
  expect_type(res, "character")
  expect_is(res, "character")
  expect_null(names(res))
  expect_is(res2, "list")
  expect_type(res2, "list")
  expect_named(res2, nam)
  expect_length(res2, 2L)
  expect_warning(lgas(c("Oyo West", "Obomo Ngwa")), 
                 "One or more elements is not an LGA")
})

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


test_that("Mispelt LGA are discovered", {
  xx <- is_lga(ibd)
  err <- "x should be of type 'character'"
  expect_false(all(xx))
  expect_error(is_lga(NULL), err)
  expect_error(is_lga(42), err)
  expect_error(is_lga(cars), err)
  expect_error(is_lga(c(TRUE, FALSE)), err)
  expect_type(xx, 'logical')
  expect_is(matrix(is_lga(lgas())), 'matrix')
  
})


test_that("is_lga recognises LGAs", {
  anlga <- "Amuwo-Odofin"
  veclga <- c("Akira-Uba", "Hawul", NA)
  
  
  expect_true(is_lga(anlga))
  expect_false(all(is_lga(veclga)))
})




test_that("Misspelt LGA can be fixed (limited)", {
  result <- suppressWarnings(
    fix_region(lgas(c("Amuwo Odofin", "Lagos Island"))))
  
  expect_equivalent(result, c("Amuwo-Odofin", "Lagos Island"))

  })
