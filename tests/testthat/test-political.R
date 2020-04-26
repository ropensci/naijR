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