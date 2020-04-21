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
  myerr1 <- "One or more elements of 'state' is not a Nigerian state"
  
  expect_error(map_ng(999), myerr1)
  expect_error(map_ng(NULL), myerr1)
  expect_error(map_ng(NA), myerr1)
  expect_error(map_ng('TRUE'), myerr1)
  expect_error(map_ng(pi), myerr1)
  expect_error(map_ng(style = 'choropleth'), 
               "'var' and 'breaks' must be supplied to plot chropleth maps")
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


test_that("Data for mapping are retrieved properly", {
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


local({
  set.seed(4)
  vals <- sample(0:6, 37, TRUE)
  brks <- seq(0, 6, 2)
  
  test_that("Internal function for preparing colours is validated", {
    df <- data.frame(1)
    mt <- matrix(1:3)
    err1 <- "is\\.atomic\\(x\\) is not TRUE"
    err2 <- "Expected dim\\(x\\) to evaluate to NULL"
    
    expect_error(.prepareChoroplethColors(),
                 'argument "x" is missing, with no default')
    expect_error(.prepareChoroplethColors(NULL),
                 'argument "brk" is missing, with no default')
    expect_error(.prepareChoroplethColors(df, brks), err1)
    expect_error(.prepareChoroplethColors(vals, df), 
                 "is\\.atomic\\(brk\\) is not TRUE")
    expect_error(.prepareChoroplethColors(df, df), err1)
    expect_error(.prepareChoroplethColors(mt, brks), err2)
    expect_warning(.prepareChoroplethColors(vals, c(1:3)))
    expect_warning(.prepareChoroplethColors(vals, mt))
    expect_error(.prepareChoroplethColors(mt, mt), err2)
  })
  
  
  test_that("Expected colours and related data are prepared", {
    cho <- .prepareChoroplethColors(vals, brks)
    cols <-
      c(
        "#BDBDBD", "#BDBDBD", "#BDBDBD", "#636363", "#BDBDBD", "#BDBDBD", 
        "#636363", "#636363", "#F0F0F0", "#BDBDBD", "#636363", "#636363",
        "#636363", "#F0F0F0", "#F0F0F0", "#BDBDBD", "#636363", "#BDBDBD",
        "#BDBDBD", "#BDBDBD", "#636363", "#636363", "#636363", "#F0F0F0",
        "#636363", "#BDBDBD", "#636363", "#636363", "#636363", "#636363",
        "#636363", "#F0F0F0", "#636363", "#636363", "#BDBDBD", "#636363",
        "#636363"
      )
    
    expect_is(cho, "list")
    expect_type(cho, "list")
    expect_length(cho, 3L)
    expect_named(cho, c("colors", "scheme", "bins"))
    expect_type(cho$colors, 'character')
    expect_type(cho$scheme, 'character')
    expect_type(cho$bins, 'character')
    expect_identical(cho$colors, cols)
    expect_identical(cho$scheme, c("#F0F0F0", "#BDBDBD", "#636363"))
    expect_identical(cho$bins, c("(0,2]", "(2,4]", "(4,6]"))
    expect_length(cho$scheme, 3L)
    expect_length(cho$bins, 3L)
    expect_identical(length(vals), length(cho$colors))
  })
})



