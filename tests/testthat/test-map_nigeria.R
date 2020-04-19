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
library(testthat)
library(naijR)

test_that("Input is validated", {
  myerr <- "'arg' must be NULL or a character vector"
  
  expect_error(map_ng(999), myerr)
  expect_error(map_ng(NULL), "Invalid input for 'style'")
  expect_error(map_ng(NA), myerr)
  expect_error(map_ng('TRUE'), 
               "'arg' should be one of \"basic\", \"choropleth\"")
  expect_error(map_ng(pi), myerr)
  expect_warning(map_ng(plot = FALSE, show.neighbours = TRUE), 
                 "Display of neighbouring countries is disabled")
})



test_that("'map' object is properly created", {
  mp1 <- map_ng(plot = FALSE)
  mp2 <- suppressWarnings(map_ng(show = TRUE, plot = FALSE))
  
  expect_is(mp1, 'map')
  expect_type(mp1, 'list')
  expect_s3_class(mp1, 'map')
  expect_length(mp1, 4L)
  expect_identical(names(mp1), c("x", "y", 'range', 'names'))
  expect_length(mp1$names, 41)
  expect_true(identical(mp1$x, mp2$x))
  expect_true(identical(mp1$y, mp2$y))
  expect_false(length(mp1$x) < length(mp2$x))
  expect_false(length(mp1$y) < length(mp2$y))
  expect_true(all(mp1$x %in% mp2$x))
  expect_true(all(mp1$y %in% mp2$y))
  
  rng <- c(2.668534, 14.678820, 4.273007, 13.894420)
  expect_equal(signif(mp1$range, 7), rng)
  expect_equal(signif(mp2$range, 7), rng)
  
  for (i in states()) expect_match(mp2$names, i, all = FALSE)
})


test_that("Data for mapping is retrieved properly", {
  tryToGetMap <- quote(try(.getMapData()))
  success <- eval(tryToGetMap)
  threwErrExcept <- quote(inherits(success, "try-error"))
  
  if (eval(threwErrExcept)) {
    expect_error(eval(tryToGetMap), 
                 "The map data could not be found in 'extdata'")
  }
  expect_false(eval(threwErrExcept))
  expect_is(success, 'map')
})


# test_that("Choropleth map for Nigeria can be created", {
#   set.seed(123)
#   cases <- sample(1:100, 37, replace = T)
#   breaks <- seq(10, 100, 10)
#   res <- map_ng("choropleth", cases, breaks, plot = FALSE)
#   
#   expect_is(res, 'map')
# })
