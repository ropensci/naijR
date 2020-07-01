# Copyright (C) 2019 Victor Ordu.
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

test_that("Invalid input terminates the function", {
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




test_that("States can be identified in an object", {
  ss.good <- ss.bad <- ss.na <- states()
  ind.bad <- c(5, 7, 19)
  ind.nas <- c(6, 19, 33)
  ss.bad[ind.bad] <- c("big", "bad", "wolf")
  all <- is_state(ss.good)
  all2 <- is_state(ss.bad)
  sel <- is_state(ss.good, test = 'selected')
  sel2 <- is_state(ss.bad, test = 'selected')
  ss.na[ind.nas] <- NA
  minus.nas <- is_state(ss.na, test = 'selected', allow.na = FALSE)
  sel.na <- is_state(ss.na, 'selected')
  
  expect_length(all, 1L)
  expect_true(all)
  expect_length(all2, 1L)
  expect_false(all2)
  expect_length(sel, 37L)
  expect_equal(sum(sel), 37L)
  expect_length(sel2, 37L)
  expect_equal(sum(sel2), 34L)
  expect_true(is_state(ss.na))
  expect_length(is_state(ss.na, test = 'selected'), 37L)
  expect_equal(sum(minus.nas), 34L)
  expect_false(anyNA(minus.nas))
  expect_equal(sum(sel.na), NA_integer_)
  expect_equal(sum(is.na(sel.na)), length(ind.nas))
  expect_true(is_state("FCT"))
  expect_true(is_state("Federal Capital Territory"))
  expect_false(is_state(pi))
  expect_false(is_state(NULL))
  expect_length(is_state(pi), 1L)
})

test_that("is_lga recognises LGAs", {
  anlga <- "Amuwo-Odofin"
  veclga <- c("Akira-Uba", "Hawul", NA)
  
  
  expect_true(is_lga(anlga))
  expect_false
})