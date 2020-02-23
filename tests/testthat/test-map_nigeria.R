# Source file: test-map_nigeria.R 
# 
# GPL-3 License
# 
# Copyright (c) 2020 Victor Ordu
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

test_that("Input is validated", {
  myerr <- "is.logical(show.neighbours) is not TRUE"
  
  expect_error(map_ng(999), myerr, fixed = TRUE)
  expect_error(map_ng(NULL), myerr, fixed = TRUE)
  expect_error(map_ng(NA), "missing value where TRUE/FALSE needed")
  expect_error(map_ng('TRUE'), myerr, fixed = TRUE)
  expect_error(map_ng(pi), myerr, fixed = TRUE)
})



test_that("Use of vector longer than 1", {
  expect_warning(tstmap <- map_ng(c(TRUE, TRUE, FALSE), plot = FALSE),
                 'has length > 1 and only the first element will be used')
  expect_is(tstmap, 'map')
})



test_that("'map' object is properly created", {
  mp1 <- map_ng(plot = FALSE)
  mp2 <- map_ng(show = TRUE, plot = FALSE)
  
  expect_is(mp1, 'map')
  expect_type(mp1, 'list')
  expect_length(mp1, 4L)
  expect_identical(names(mp1), c("x", "y", 'range', 'names'))
  expect_identical(mp1$names, 'Nigeria')
  expect_identical(mp1$x, readRDS('data/map_xcoord.rds'))
  expect_identical(mp1$y, readRDS('data/map_ycoord.rds'))
  expect_equal(signif(mp1$range, 7), c(2.681958, 14.67419, 4.272489, 13.89193))
  expect_equal(signif(mp2$range, 7), c(0.1666734, 24.00166, 1.654186, 23.52112))
  expect_identical(mp2$names, c("Chad", "Niger", "Benin", "Nigeria","Cameroon"))
  expect_false(identical(mp1$x, mp2$x))
  expect_false(identical(mp1$y, mp2$y))
  expect_true(length(mp1$x) < length(mp2$x))
  expect_true(length(mp1$y) < length(mp2$y))
  expect_true(all(mp1$x %in% mp2$x))
  expect_true(all(mp1$y %in% mp2$y))
})