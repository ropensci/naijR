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
  
  expect_error(fix_state(99), errchr)
  expect_error(fix_state(NA), errchr)
  expect_error(fix_state(c(NA, NA, NA)), errchr)
  expect_warning(fix_state(NA_character_), "'x' has only missing values")
  expect_error(fix_state(NULL), errchr)
  expect_error(fix_state(TRUE), errchr)
  expect_error(fix_state(matrix(states())), errchr)
})


test_that("various cases for fixing state names", {
  ss <- states()
  ss2 <- c("oyo", "Legos")
  ssx <- c("xxx", "Benue")
  ss.us <- c("kentucky", "Bornu", "Abia")
  
  expect_identical(fix_state(ss), ss)
  expect_identical(fix_state('Fct'), "Federal Capital Territory")
  expect_identical(fix_state('Kane'), "Kano")
  expect_identical(fix_state('plateau'), 'Plateau')
  expect_identical(fix_state(ss2), c("Oyo", "Lagos"))
  expect_length(fix_state(ss2), 2L)
  expect_identical(fix_state(ssx), c(NA_character_, "Benue"))
  expect_length(fix_state(ssx), 2L)
  expect_identical(fix_state(ss.us), c(NA_character_, "Borno", "Abia"))
  expect_length(fix_state(ss.us), 3L)
})


test_that("FCT abbreviations are well handled", {
  fct_full <- 'Federal Capital Territory'
  expect_identical(.toggleFct(fct_full), 'FCT')
  expect_identical(.toggleFct('FCT'), fct_full)
})