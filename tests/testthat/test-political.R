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
  ss <- ss2 <- states()
  ss2[c(5, 7, 19)] <- c("big", "bad", "wolf")
  all <- is_state(ss)
  all2 <- is_state(ss2)
  sel <- is_state(ss, test = 'selected')
  sel2 <- is_state(ss2, test = 'selected')
  
  expect_length(all, 1L)
  expect_true(all)
  expect_length(all2, 1L)
  expect_false(all2)
  expect_length(sel, 37L)
  expect_equal(sum(sel), 37L)
  expect_length(sel2, 37L)
  expect_equal(sum(sel2), 34L)
})