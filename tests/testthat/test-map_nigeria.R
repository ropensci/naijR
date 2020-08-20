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
  myerr1 <- "One or more elements of 'region' is not a Nigerian region"
  myerr2 <- "Type of argument supplied to 'region' is invalid."
  expect_error(map_ng(999), myerr2)
  expect_is(map_ng(NULL, plot = FALSE), 'map')
  expect_error(map_ng(NA), myerr2)
  expect_error(map_ng('TRUE'), myerr1)
  expect_error(map_ng(pi), myerr2)
  expect_message(map_ng(plot = FALSE, show.neighbours = TRUE), 
                 "Display of neighbouring countries is disabled")
  # TODO: Add test case for choropleths with too few regions
})

test_that("Check on 'region' parameter works", {
  
  expect_equal(.processStateParam(NULL), "Nigeria")
  expect_error(.processStateParam(), 
               "argument \"s\" is missing, with no default")
  expect_length(.processStateParam(states('se')), 5L)
  expect_length(.processStateParam(character()), 37L)
  expect_error(.processStateParam(c("Abia", "Kano", "Lifebuoy")),
               "One or more elements of 'region' is not a Nigerian region")
})

test_that("Choropleth categories are created", {
  set.seed(50)
  int.val <- sample(1:100, 20)
  br <- c(0, 20, 40, 60, 80, 100)
  c <- .createCategorized(int.val, br)
  d <- .createCategorized(int.val, 5L)
  
  expect_length(c, 20L)
  expect_length(d, 20L)
  expect_type(c, 'integer')
  expect_type(d, 'integer')
  expect_is(c, "factor")
  expect_is(d, "factor")
  expect_length(levels(c), 5L)
  expect_length(levels(d), 5L)
  expect_error(.createCategorized(int.val, br[-6]),
               "Values are out of range of breaks")
  expect_error(.createCategorized(sample(c(TRUE, FALSE), 30, TRUE)),
               "'logical' is not a supported type")
})

test_that("Decision is made on drawing choropleths", {
  all.states <- states()
  nc.states <- states('nc')
  set.seed(23)
  vals <- lapply(list(all = all.states, nc = nc.states), function(x)
    factor(sample(LETTERS[1:5], length(x), replace = TRUE)))
  all.ints <- sample(1:5, 37L, replace = T)
  
  expect_true(.validateChoroplethParams(region = all.states, val = vals$all))
  expect_true(.validateChoroplethParams(data = data.frame(nc.states, vals$nc)))
  expect_true(.validateChoroplethParams(region = all.states, val = all.ints))
  expect_false(.validateChoroplethParams(region = '.'))
})

test_that("Subnational divisions are plotted", {
  sw <- map_ng(region = states('sw'), plot = FALSE)
  
  expect_length(sw$names, 6L)
  expect_identical(sw$names, c("Ekiti", "Lagos", "Ogun", "Ondo", "Osun", "Oyo"))
})

test_that("Data for mapping are retrieved properly", {
  db <- .getMapData("Nigeria")
  
  expect_error(.getMapData(NULL))
  expect_error(.getMapData(42L))
  expect_error(.getMapData(pi))
  expect_error(.getMapData(TRUE))
  expect_is(db, 'character')
  expect_identical(db, "mapdata::worldHires")
  expect_is(.getMapData("Abia"), 'map')
  expect_error(.getMapData("Alaska"), 
               "Invalid region(s) for the map: Alaska",
               fixed = TRUE)
  expect_error(.getMapData(c("Unreal", "Imaginary")), 
               "Invalid region(s) for the map: Unreal, Imaginary",
               fixed = TRUE)
})

test_that("Shapefile data is retrievable",
          expect_is(.getSpatialPolygonsDataFrame(), 'SpatialPolygonsDataFrame'))

set.seed(4)
df <-
  data.frame(region = states(all = TRUE), value = sample(0:6, 37, TRUE),
             stringsAsFactors = FALSE)
mp <- map_ng(plot = FALSE)
brks <- seq(0, 6, 2)
vals <- df$value
lso <-
  list(
    region = df$region,
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
  
  expect_true(.isHexColor("#DE458E"))
  expect_false(.isHexColor("Hex"))
  expect_true(.isHexColor(trio))
  expect_false(.isHexColor(trio_na))
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
               "suppressWarnings(is_state(regions)) is not TRUE",
               fixed = TRUE)
  expect_error(
    .reassignColours(mapnames, statenames, rep("NoHexs", length(statenames))),
    ".isHexColor\\(in.colours\\) is not TRUE")
})

test_that("Appropriate palette is used", {
  p3 <- c("black", "red", "green3", "blue", "cyan", "magenta", "yellow", "gray")
  pal <- .get_R_palette()
  
  expect_equivalent(pal, p3)
})

test_that("Colours are prepared for plotting", {
  expect_error(.processColouring(col = 'brown', 5L), 
               "'brown' is not a supported colour or palette")
})

test_that("List of choropleth inputs is properly checked", {
  expect_true(.assertListElements(lso))
})

test_that("Expected colours and related data are prepared", {
  set.seed(4)
  brks <- seq(0, 6, 2)
  obj <-
    list(
      region = states(),
      value = sample(0:6, 37, TRUE),
      breaks = brks,
      categories = LETTERS[seq_len(length(brks))]
    )
  mp <- map_ng(plot = FALSE)
  cho <- .prepareChoroplethOptions(mp, obj)
  cols <-
    c(
      "#F0F0F0", "#F0F0F0", "#F0F0F0", "#F0F0F0", "#636363", "#BDBDBD", "#F0F0F0",
      "#636363", "#BDBDBD", "#F0F0F0", "#F0F0F0", "#F0F0F0", "#F0F0F0", "#F0F0F0",
      "#636363", "#636363", "#636363", "#F0F0F0", "#F0F0F0", "#BDBDBD", "#636363",
      "#F0F0F0", "#F0F0F0", "#F0F0F0", "#BDBDBD", "#BDBDBD", "#BDBDBD", "#F0F0F0",
      "#636363", "#BDBDBD", "#BDBDBD", "#636363", "#BDBDBD", "#636363", "#BDBDBD",
      "#F0F0F0", "#636363", "#636363", "#F0F0F0", "#636363", "#BDBDBD"
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
})

test_that("State polygon names are not repeated during computations", {
  result <- .getUniqueStateNames(mp)
  
  expect_length(result, 37L)
  expect_error(.getUniqueStateNames(states()))
})

test_that("Choropleth mapping succeeds", {
  pop.groups <- c(1000000, 2500000, 5000000, 7500000, 10000000)
  dat <- readRDS('data/pvc2015.rds')
  val <- dat$total.pop    # use name to test quasiquotation
  dat$alpha <- sample(LETTERS[1:5], nrow(dat), TRUE)  
  cat <- c("Small", "Moderate", "Large", "Mega")
  
  expect_is(
    map_ng(
      data = dat, 
      x = total.pop, 
      breaks = pop.groups,
      categories = cat,
      plot = FALSE), 
    'map')
  
  expect_is(map_ng(
    region = dat$region,
    x = dat$total.pop,
    breaks = pop.groups,
    plot = FALSE
  ),
  'map')
  
  expect_error(
    map_ng(region = dat$state,
           x = dat$total.pop,
           plot = FALSE),
    "Breaks were not provided for the categorization of a numeric type"
  )
  
  expect_error(
    map_ng(
      data = dat,
      x = val,         # no such name in data frame
      breaks = pop.groups,
      category = cat,
      col = 2L,
      plot = FALSE),
    "One or more inputs for generating choropleth options are invalid")

  expect_is(
    map_ng(
      data = dat,
      x = total.pop,
      breaks = pop.groups,
      plot = FALSE,
      fill = FALSE
    ), 'map',
    "Choropleths should be deductively drawn even when `fill == FALSE`")
  
  expect_is(map_ng(data = dat,
                   x = alpha,
                   plot = FALSE),
            'map')
})

test_that("Choropleth colours can be controlled at interface", {
  dat <- readRDS('data/pvc2015.rds')
  func <- expr(map_ng(
    data = dat,
    x = total.pop,
    breaks = c(1000000, 2500000, 5000000, 7500000, 10000000),
    categories = c("Small", "Moderate", "Large", "Mega"),
    plot = FALSE,
    col = NULL
  ))
  mm <- 'map'
  
  expect_is(eval_tidy(func), mm)
  func$col <- "YlOrRd"; expect_is(eval_tidy(func), mm)
  func$col <- 'blue'; expect_is(eval_tidy(func), mm)
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
  dat <- readRDS('data/pvc2015.rds')
  regions <- states()
  ss <- .regionColumnIndex(dat, regions)
  err <- "is.data.frame\\(dt\\) is not TRUE"
  
  expect_is(ss, 'integer')
  expect_length(ss, 1L)
  expect_error(.regionColumnIndex(), 
               "argument \"dt\" is missing, with no default")
  expect_error(.regionColumnIndex(regions, regions), err)
  expect_equivalent(.regionColumnIndex(dat, letters), 1L)
  expect_equivalent(.regionColumnIndex(dat, NULL), 1L)
  expect_error(.regionColumnIndex(NULL, regions), err)
  expect_error(.regionColumnIndex(mtcars, regions), 
               "No column with elements in regions.")
})

test_that("Bounds for plotting points are checked", {
  x <- c(3.000, 4.000, 6.000, 5.993, 5.444, 6.345, 5.744)
  y <- c(8.000, 9.000, 9.300, 10.432, 8.472, 6.889, 9.654)
  m <- map_ng(NULL, plot = FALSE)
  mk <- map_ng("Kwara", plot = FALSE)
  
  expect_true(.xyWithinBounds(m, x, y))
  expect_false(.xyWithinBounds(mk, x, y))
  
  x[4] <- -3; y[4] <- 1000
  expect_false(.xyWithinBounds(m, x, y))
  
})

test_that("'map' object is properly created", {
  mp1 <- map_ng(plot = FALSE)
  mp2 <- suppressWarnings(map_ng(show.neighbours = TRUE, plot = FALSE))
  mp.lb <- map_ng(plot = FALSE)
  mm <- 'map'
  rng <- c(2.668534, 14.678820, 4.273007, 13.894420)
  
  expect_is(mp1, mm)
  expect_is(mp.lb, mm)
  expect_type(mp1, 'list')
  expect_s3_class(mp1, mm)
  expect_length(mp1, 4L)
  expect_identical(names(mp1), c("x", "y", 'range', 'names'))
  expect_length(mp1$names, 41)
  expect_true(identical(mp1$x, mp2$x))
  expect_true(identical(mp1$y, mp2$y))
  expect_false(length(mp1$x) < length(mp2$x))
  expect_false(length(mp1$y) < length(mp2$y))
  expect_true(all(mp1$x %in% mp2$x))
  expect_true(all(mp1$y %in% mp2$y))
  expect_equal(signif(mp1$range, 7), rng)
  expect_equal(signif(mp2$range, 7), rng)
  for (i in states()) 
    expect_match(mp2$names, i, all = FALSE)
})

test_that("Points are mapped", {
  x <- c(3.000, 4.000, 6.000)
  y <- c(6.000, 9.000, 4.300)
  
  expect_is(map_ng(NULL, x = x, y = y, plot = FALSE), 'map')
  
  x[4] <- -3; y[4] <- 1000
  expect_error(map_ng(NULL, x = x, y = y, plot = FALSE), 
               "Coordinates are out of bounds of the map")
})

test_that("Factors can draw choropleth", {
  getSmpl <- function(seed) { set.seed(seed); sample(letters[1:5], 37, T) }
  
  getDblSmpl <- function(seed = round(runif(100, max = 500))) {
    set.seed(seed)
    runif(37, max = 100)
  }
  mapClass <- 'map'
  Fac <- ordered(getSmpl(2))
  expect_is(map_ng(x = Fac, plot = FALSE), mapClass)
  
  Char <- getSmpl(5)
  expect_is(map_ng(x = Char, plot = FALSE), mapClass)
  
  dd <- data.frame(region = states(), Col = Fac, stringsAsFactors = FALSE)
  expect_is(map_ng(data = dd, x = Col, plot = FALSE), mapClass)
  
  expect_is(map_ng(states(), x = Fac, plot = FALSE), mapClass)
  expect_is(map_ng(states(), x = Char, plot = FALSE), mapClass)
  
  Int <- sample(1L:5L, 37, TRUE)
  expect_error(map_ng(states(), x = Int, plot = FALSE),
               'Breaks were not provided for the categorization of a numeric type')
  expect_is(map_ng(states(), x = as.factor(Int), plot = FALSE), mapClass)
  
  brks <- c(0, 40, 60, 100)
  expect_is(map_ng(states(), x = getDblSmpl(), breaks = brks, plot = FALSE), mapClass)
  
  dd$dblCol <- getDblSmpl()
  qq <- quote(map_ng(data = dd, x = dblCol, breaks = brks, plot = FALSE))
  expect_is(eval(qq), mapClass)
  qq$categories <- c("Low", "Medium", "High")
  expect_is(eval(qq), mapClass)
  qq$col <- "green"
  expect_is(eval(qq), mapClass)
})

test_that("Parameters passed via ellipsis work seamlessly", {
  mm <- 'map'
  
  expect_is(map_ng(lwd = 2, plot = FALSE), mm)
  expect_is(map_ng(lwd = 2, col = 2, plot = FALSE), mm)
  expect_is(map_ng(NULL, lwd = 2, col = 2, plot = FALSE), mm)
})

test_that("Shapefile data is retrievable",
          expect_is(.getSpatialPolygonsDataFrame(), 'SpatialPolygonsDataFrame'))
