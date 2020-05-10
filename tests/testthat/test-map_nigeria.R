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
library(rlang)






test_that("Input is validated", {
  myerr1 <- "One or more elements of 'state' is not a Nigerian state"
  myerr2 <- "Type of argument supplied to 'state' is invalid."
  expect_error(map_ng(999), myerr2)
  expect_is(map_ng(NULL, plot = FALSE), 'map')
  expect_error(map_ng(NA), myerr2)
  expect_error(map_ng('TRUE'), myerr1)
  expect_error(map_ng(pi), myerr2)
  expect_message(map_ng(plot = FALSE, show.neighbours = TRUE), 
                 "Display of neighbouring countries is disabled")
  # TODO: Add test case for choropleths with too few states
})





test_that("Check on 'state' parameter works", {
  
  expect_equal(.processStateParam(NULL), "Nigeria")
  expect_error(.processStateParam(), 
               "argument \"s\" is missing, with no default")
  expect_length(.processStateParam(states('se')), 5L)
  expect_length(.processStateParam(character()), 37L)
  expect_error(.processStateParam(c("Abia", "Kano", "Lifebuoy")),
               "One or more elements of 'state' is not a Nigerian state")
})





test_that("Decision is made on drawing choropleths", {
  all.states <- states()
  nc.states <- states('nc')
  set.seed(23)
  vals <- lapply(list(all = all.states, nc = nc.states), function(x)
    factor(sample(LETTERS[1:5], length(x), replace = TRUE)))
  all.ints <- sample(1:5, 37L, replace = T)
  
  expect_true(.validateChoroplethParams(state = all.states, val = vals$all))
  expect_true(.validateChoroplethParams(data = data.frame(nc.states, vals$nc)))
  expect_true(.validateChoroplethParams(state = all.states, val = all.ints))
  expect_false(.validateChoroplethParams(state = '.'))
})





test_that("'map' object is properly created", {
  mp1 <- map_ng(plot = FALSE)
  mp2 <- suppressWarnings(map_ng(show.neighbours = TRUE, plot = FALSE))
  mp.lb <- map_ng(plot = FALSE)
  
  expect_is(mp1, 'map')
  expect_is(mp.lb, 'map')
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




test_that("Subnational divisions are plotted", {
  sw <- map_ng(state = states('sw'), plot = FALSE)
  
  expect_length(sw$names, 6L)
  expect_identical(sw$names, c("Ekiti", "Lagos", "Ogun", "Ondo", "Osun", "Oyo"))
})




test_that("Data for mapping are retrieved properly", {
  s <- "Nigeria"
  res <- try(.getMapData(s))
  exceptionThrown <- inherits(res, "try-error")
  
  expect_false(exceptionThrown)
  expect_is(res, 'character')
  expect_is(.getMapData("Abia"), 'map')
  expect_error(.getMapData("Alaska"), 
               "Invalid region(s) for the map: Alaska",
               fixed = TRUE)
  expect_error(.getMapData(c("Unreal", "Imaginary")), 
               "Invalid region(s) for the map: Unreal, Imaginary",
               fixed = TRUE)
})




  
set.seed(4)
df <-
  data.frame(state = states(all = TRUE), value = sample(0:6, 37, TRUE),
             stringsAsFactors = FALSE)
mp <- map_ng(plot = FALSE)
brks <- seq(0, 6, 2)
vals <- df$value
lso <-
  list(
    state = df$state,
    value = df[, 'value'],
    breaks = brks,
    category = LETTERS[seq_len(length(brks))]
  )

test_that("Internal function for preparing colours is validated", {
  mt <- matrix(1:3)
  err1 <- "is\\.atomic\\(x\\) is not TRUE"
  err2 <- "Expected dim\\(x\\) to evaluate to NULL"
  err3 <- 'argument "bins" is missing, with no default'
  err4 <- 'inherits(map, "map") is not TRUE'

  expect_error(.prepareChoroplethOptions(), 
               "argument \"map\" is missing, with no default")
  expect_error(.prepareChoroplethOptions(NULL), err4, fixed = TRUE)
  expect_error(.prepareChoroplethOptions(df, brks), err4, fixed = TRUE)
  expect_error(.prepareChoroplethOptions(vals, df), err4, fixed = TRUE)
  expect_error(.prepareChoroplethOptions(df, df), err4, fixed = TRUE)
  expect_error(.prepareChoroplethOptions(mt, brks), err4, fixed = TRUE)
  expect_error(.prepareChoroplethOptions(vals, c(1:3)), err4, fixed = TRUE)
  expect_error(.prepareChoroplethOptions(vals, mt), err4, fixed = TRUE)
  expect_error(.prepareChoroplethOptions(mt, mt), err4, fixed = TRUE)
  expect_error(.prepareChoroplethOptions())
})





test_that("Hexadecimal colour format is detected internally", {
  trio <- trio_na <- c("#CCCCCC", '#FFFFFF', "#AB7402")
  trio_na[4] <- NA_character_
  
  expect_true(.assertHexColor("#DE458E"))
  expect_false(.assertHexColor("Hex"))
  expect_true(.assertHexColor(trio))
  expect_false(.assertHexColor(trio_na))
})





test_that("Regular expression for checking polygons is built", {
  result <- .regexDuplicatedPolygons("WORD")
  err.char <- "is.character\\(x\\) is not TRUE"
  
  expect_error(.regexDuplicatedPolygons(), 
               "argument \"x\" is missing, with no default")
  expect_error(.regexDuplicatedPolygons(NULL), err.char)
  expect_error(.regexDuplicatedPolygons(numeric()), err.char)
  expect_error(.regexDuplicatedPolygons(logical()), err.char)
  expect_error(.regexDuplicatedPolygons(NA), err.char)
  expect_type(result, "character")
  expect_is(result, "character")
  expect_equal(result, "^(WORD)(.?|\\:\\d*)$")
})





test_that("Colours are reassigned when duplicate polygons exist", {
  mapnames <- c("Kano:1", "Kano:2", "Abia:1", "Abia:2", "Abia:3", "Oyo")
  statenames <- c("Abia", "Kano", "Oyo")
  init.color <- c("#FFFFFF", "#CCCCCC", "#000000")
  fin.color <- .reassignColours(mapnames, statenames, init.color)
  not.ng <- c("Maryland", "Saarland")
  
  expect_length(fin.color, length(mapnames))
  expect_named(fin.color)
  expect_true(any(duplicated(names(fin.color))))
  expect_equal(sum(duplicated(names(fin.color))), 3L)
  expect_error(.reassignColours(mapnames, not.ng, init.color), 
               "is_state\\(states\\) is not TRUE")
  expect_error(
    .reassignColours(mapnames, statenames, rep("NoHexs", length(statenames))),
    ".assertHexColor\\(in.colours\\) is not TRUE")
})





test_that("List of choropleth inputs is properly checked", {
  expect_true(.assertListElements(lso))
})





test_that("Expected colours and related data are prepared", {
  func <- expr(.prepareChoroplethOptions(mp, lso))
  cho <- eval_tidy(func)
  cols <-
    c(
      "#BDBDBD", "#BDBDBD", "#BDBDBD", "#BDBDBD", "#636363", "#BDBDBD", 
      "#BDBDBD", "#636363", "#636363", "#F0F0F0", "#F0F0F0", "#F0F0F0", 
      "#F0F0F0", "#BDBDBD", "#636363", "#636363", "#636363", "#F0F0F0", 
      "#F0F0F0", "#BDBDBD", "#636363", "#BDBDBD", "#BDBDBD", "#BDBDBD", 
      "#636363", "#636363", "#636363", "#F0F0F0", "#636363", "#BDBDBD", 
      "#636363", "#636363", "#636363", "#636363", "#636363", "#F0F0F0", 
      "#636363", "#636363", "#BDBDBD", "#636363", "#636363"
    )

  expect_is(cho, "list")
  expect_type(cho, "list")
  expect_length(cho, 3L)
  expect_named(cho, c("colors", "scheme", "bins"))
  expect_type(cho$colors, 'character')
  expect_type(cho$scheme, 'character')
  expect_type(cho$bins, 'character')
  expect_equivalent(cho$colors, cols)
  expect_identical(cho$scheme, c("#F0F0F0", "#BDBDBD", "#636363"))
  expect_identical(cho$bins, c("[0,2]", "(2,4]", "(4,6]"))
  expect_length(cho$scheme, 3L)
  expect_length(cho$bins, 3L)
  
  func$col <- "brown"
  expect_error(eval_tidy(func), 
               "'brown' is not one of the supported colours or palettes")
})




test_that("State polygon names are not repeated during computations", {
  result <- .getUniqueStateNames(mp)
  
  expect_length(result, 37L)
  expect_error(.getUniqueStateNames(states()))
})



dat <- readRDS('data/pvc2015.rds')
pop.groups <- c(1000000, 2500000, 5000000, 7500000, 10000000)
cat <- c("Small", "Moderate", "Large", "Mega")
test_that("Choropleth mapping succeeds", {
  expect_is(
    map_ng(
      data = dat, 
      value = total.pop, 
      breaks = pop.groups,
      categories = cat,
      plot = FALSE), 
    'map')
  
  # expect_error(
  #   map_ng(
  #     flavour = 'choropleth',
  #     data = dat, 
  #     value = val, 
  #     breaks = pop.groups,
  #     category = cat,
  #     col = 99L,
  #     plot = FALSE),
  #   'error')
  
  # expect_error(
  #   map_ng(
  #     flavour = 'choropleth',
  #     data = dat,
  #     value = val,
  #     breaks = pop.groups,
  #     plot = FALSE,
  #     fill = FALSE
  #   ), 
  #   "Choropleths cannot be drawn when 'fill == FALSE'")
})

test_that("Choropleth colours can be controlled at interface", {
  expect_error(
    map_ng(
      data = dat,
      value = total.pop,
      breaks = pop.groups,
      categories = cat,
      plot = FALSE,
      col = 'black'
    ),
    "'black' is not one of the supported colours or palettes")
  
  expect_is(
    map_ng(
      data = dat,
      value = total.pop,
      breaks = pop.groups,
      categories = cat,
      plot = FALSE,
      col = 'blue'
    ),
    "map")
})





test_that("Duplicated polygon names are made empty", {
  label <- c("A:1", 'A:2', 'A:3', 'B', 'C:1', 'C:2')
  result <- .adjustLabels(label)
  err <- "is.character\\(x\\) is not TRUE"
  
  expect_error(.adjustLabels(NA), err)
  expect_error(.adjustLabels(numeric()), err)
  expect_error(.adjustLabels(logical()), err)
  expect_error(.adjustLabels(NULL), err)
  expect_type(result, 'character')
  expect_is(result, 'character')
  expect_length(result, 6L)
  expect_length(grep("^$", result), 3L)
  expect_false(any(grepl("\\:\\d$", result)))
})







test_that("States' columns are searchable within a data frame", {
  states <- states()
  ss <- .stateColumnIndex(dat, states)
  err <- "is.data.frame\\(dt\\) is not TRUE"
  
  expect_is(ss, 'integer')
  expect_length(ss, 1L)
  expect_error(.stateColumnIndex(), 
               "argument \"dt\" is missing, with no default")
  expect_error(.stateColumnIndex(states, states), err)
  expect_equivalent(.stateColumnIndex(dat, letters), 1L)
  expect_equivalent(.stateColumnIndex(dat, NULL), 1L)
  expect_error(.stateColumnIndex(NULL, states), err)
  expect_error(.stateColumnIndex(mtcars, states), 
               "No column with elements in states.")
})
