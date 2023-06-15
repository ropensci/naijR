# Source file: test-map_nigeria.R 
# 
# GPL-3 License
# 
# Copyright (c) 2020-2021 Victor Ordu

test_that("Input is validated", {
  myerr1 <- "One or more elements of 'region' is not a Nigerian region"
  myerr2 <- "Expected a character vector as 'region'"
  
  expect_error(map_ng(999), myerr2)
  expect_error(map_ng(NULL, plot = FALSE), myerr2)
  expect_error(map_ng(NA), myerr2)
  expect_error(map_ng('TRUE'), 
               "Single inputs for 'region' only support the value 'Nigeria'")
  expect_error(map_ng(pi), myerr2)
  # expect_warning(map_ng(plot = FALSE, show.neighbours = c(TRUE, TRUE)),
  #                "Only the first element of 'show.neighbours' was used")
  expect_message(map_ng(plot = FALSE, show.neighbours = TRUE), 
                 "Display of neighbouring regions is temporarily disabled")
  expect_error(map_ng(data = vector()), 
               "A non-NULL input for 'data' must be a data frame")
  expect_error(map_ng(data = data.frame(col = runif(10))),
               "Insufficient variables in 'data' to generate a plot")
  expect_error(map_ng(data.frame()), "A data frame was passed;")
  # TODO: Add test case for choropleths with too few regions
})


test_that("National outline map is plotted", {
  expect_s3_class(map_ng("Nigeria", plot = FALSE), "map")
})

test_that("Geo-political Zones are plotted", {
  sw <- map_ng(region = states(gpz = 'sw'), plot = FALSE)
  
  expect_length(sw$names, 6L)
  expect_identical(sw$names, c("Ekiti", "Lagos", "Ogun", "Ondo", "Osun", "Oyo"))
})

test_that("LGAs are plotted", {
  expect_s3_class(map_ng(lgas("Akinyele"), plot = FALSE), "map")
  expect_s3_class(map_ng(lgas("Owerri North"), plot = FALSE), "map")
})



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



test_that("Choropleth mapping succeeds", {
  pop.groups <- c(1e6, 2.5e6, 5e6, 7.5e6, 1e7)
  dat <- readRDS('data/pvc2015.rds')
  baddat <- readRDS('data/pvc2015_badcolumn.rds')
  val <- dat$total.pop    # use name to test quasiquotation
  dat$alpha <- sample(LETTERS[1:5], nrow(dat), TRUE)  
  cat <- c("Small", "Moderate", "Large", "Mega")
  
  expect_error(suppressWarnings(
    map_ng(
      data = baddat,
      x = total.pop,
      breaks = pop.groups,
      categories = cat,
      plot = FALSE
    )
  ),
  "No column with elements in 'data'")
  
  expect_error(map_ng(
    region = dat$region,
    x = dat$total.pop,
    breaks = pop.groups,
    plot = FALSE
  ),
  "Expected a character vector as 'region'")
  
  expect_error(
    map_ng(region = baddat$state,
           x = dat$total.pop,
           plot = FALSE),
    "One or more elements of 'region' is not a Nigerian region"
  )
  
  expect_error(
    map_ng(
      data = dat,
      x = val,         # no such name in data frame
      breaks = pop.groups,
      category = cat,
      col = 2L,
      plot = FALSE),
    "The column 'val' does not exist in 'data'")

  expect_s3_class(
    map_ng(
      data = dat,
      x = total.pop,
      breaks = pop.groups,
      plot = FALSE,
      fill = FALSE
    ), 'map')
  
  expect_s3_class(map_ng(data = dat,
                         x = alpha,
                         plot = FALSE),
                  'map')
  
  ss <- states()
  expect_s3_class(map_ng(
    region = ss,
    x = runif(length(ss), max = 100),
    breaks = c(0, 40, 60, 100),
    col = 'YlOrRd',
    show.text = FALSE,
    plot = FALSE
  ),
  'map')
  
  expect_s3_class(map_ng(
    region = ss,
    x = sample(LETTERS[1:5], length(ss), TRUE),
    col = "red",
    show.text = FALSE,
    plot = FALSE
  ), "map")
})


test_that("Draw choropleth automatically with 2-column data frames", {
  ds <- data.frame(state = states(), value = sample(LETTERS[1:5], 37, TRUE))
  dl <- data.frame(LGA = lgas(), value = sample(LETTERS[1:5], 774, TRUE))
  
  for (df in list(ds, dl)) 
    expect_s3_class(try(map_ng(data = df, plot = FALSE)), "map")
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
  
  expect_s3_class(eval_tidy(func), mm)
  
  func$col <- "YlOrRd"
  expect_s3_class(eval_tidy(func), mm)
  
  func$col <- 'blue'
  expect_s3_class(eval_tidy(func), mm)
})




test_that("'map' object is properly created", {
  mp1 <- map_ng(plot = FALSE)
  mp2 <- suppressWarnings(map_ng(show.neighbours = TRUE, plot = FALSE))
  mm <- 'map'
  rng <- c(2.668534, 14.678820, 4.273007, 13.894420)
  
  expect_s3_class(mp1, mm)
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
  
  expect_error(map_ng(NULL, x = x, y = y, plot = FALSE),
               "Expected a character vector as 'region'")
  
  x[4] <- -3; y[4] <- 1000
  expect_error(map_ng(NULL, x = x, y = y, plot = FALSE), 
               "Expected a character vector as 'region'")
})




test_that("Factors can draw choropleth", {
  getSmpl <- function(seed) { 
    set.seed(seed)
    sample(letters[1:5], 37, T) 
  }
  
  getDblSmpl <- function(seed = round(runif(100, max = 500))) {
    set.seed(seed)
    runif(37, max = 100)
  }
  
  mapClass <- 'map'
  Fac <- ordered(getSmpl(2))
  expect_s3_class(map_ng(x = Fac, plot = FALSE), mapClass)
  
  Char <- getSmpl(5)
  expect_s3_class(map_ng(x = Char, plot = FALSE), mapClass)
  
  sss <- states()
  dd <- data.frame(region = sss, Col = Fac, stringsAsFactors = FALSE)
  expect_s3_class(map_ng(data = dd, x = Col, plot = FALSE), mapClass)
  
  expect_s3_class(map_ng(sss, x = Fac, plot = FALSE), mapClass)
  expect_s3_class(map_ng(sss, x = Char, plot = FALSE), mapClass)
  
  Int <- sample(1L:5L, 37, TRUE)
  expect_error(map_ng(sss, x = Int, plot = FALSE),
               'Breaks were not provided for the categorization of a numeric type')
  expect_s3_class(map_ng(sss, x = as.factor(Int), plot = FALSE), mapClass)
  
  brks <- c(0, 40, 60, 100)
  expect_s3_class(map_ng(sss, x = getDblSmpl(), breaks = brks, plot = FALSE), mapClass)
  
  dd$dblCol <- getDblSmpl()
  qq <- quote(map_ng(data = dd, x = dblCol, breaks = brks, plot = FALSE))
  expect_s3_class(eval(qq), mapClass)
  qq$categories <- c("Low", "Medium", "High")
  expect_s3_class(eval(qq), mapClass)
  qq$col <- "green"
  expect_s3_class(eval(qq), mapClass)
})




test_that("Parameters passed via ellipsis work seamlessly", {
  mm <- 'map'
  
  expect_s3_class(map_ng(lwd = 2, plot = FALSE), mm)
  expect_s3_class(map_ng(lwd = 2, col = 2, plot = FALSE), mm)
  expect_error(map_ng(NULL, lwd = 2, col = 2, plot = FALSE), 
               "Expected a character vector as 'region'")
})




test_that("All individual plain State maps can be drawn", {
  for (s in states())
    expect_s3_class(map_ng(s, plot = FALSE), "map")
})


test_that("All LGAs within a given State are drawn", {
  for (s in states())
    expect_s3_class(map_ng(lgas(s), plot = FALSE), "map")
})


test_that("All individual LGA maps can be drawn", {
  for (s in states()) {
    lgs <- lgas(s)

    for (lg in lgs) {
      x <- suppressWarnings(lgas(lg))
      state <- attr(x, "State")

      if (length(state) > 1L)
        x <- disambiguate_lga(x, parent = s)

      expect_s3_class(map_ng(x, plot = FALSE), "map")
    }
  }
})



test_that("Map LGAs together as individual blocs", {
  abLga <- lgas("Abia")
  expect_s3_class(map_ng(abLga, plot = FALSE), "map")
  
  ## Do actual plot
  testMap <- "data/test-map.png"
  if (file.exists(testMap))
    file.remove(testMap)
  png(testMap)
  val <- try(map_ng(abLga), silent = TRUE)
  dev.off()
  
  expect_false(inherits(val, "try-error"))
  expect_true(file.exists(testMap))
})



test_that("Number of LGAs matches the number extracted for mapping", {
  # This test case is created for bug fix that involved the creation
  # of wrong LGA coordinates for Borno State -> Kagarko is included
  for (i in states(all = FALSE)) {
    lg <- lgas(i)
    mplg <- map_ng(lg, plot = FALSE)$names
    
    rgx <- .regex_duplicated_poly(lg)
    expect_match(mplg, rgx)
    # expect_true(all(mplg %in% lg))
    # expect_true(all(lg %in% mplg))
  }
})




test_that("Choropleth map can be formed with excluded regions", {
  mapClass <- "map"
  colpal <- "YlOrRd"
  excluded.reg <- c("Abia", "Jigawa")
  green <- "green"
  d <- data.frame(state = states(),
                  total = sample(LETTERS[1:4], 37, TRUE))
  
  expect_s3_class(
    map_ng(
      data = d,
      x = total,
      col = colpal,
      excluded = excluded.reg,
      plot = FALSE
    ),
    mapClass
  )
  
  expect_s3_class(
    map_ng(
      data = d,
      x = total,
      col = colpal,
      excluded = excluded.reg,
      exclude.fill = green,
      plot = FALSE
    ),
    mapClass
  )
  
  expect_s3_class(
    map_ng(
      data = d,
      x = total,
      col = colpal,
      excluded = excluded.reg,
      exclude.fill = green,
      leg.title = "Legend title",
      plot = FALSE
    ),
    mapClass
  )
  
  expect_error(
    map_ng(
      data = d,
      x = total,
      col = colpal,
      excluded = excluded.reg,
      exclude.fill = c(green, "grey"),
      leg.title = "Legend title",
      plot = FALSE
    ),
    paste("Only one colour can be used to denote regions excluded", 
          "from the choropleth colouring scheme"),
    fixed = TRUE
  )
  
  expect_error(
    map_ng(
      data = d,
      x = total,
      col = colpal,
      excluded = excluded.reg,
      exclude.fill = "notacolour",
      leg.title = "Legend title",
      plot = FALSE
    ),
    paste("The colour used for excluded regions must be valid",
          "i.e. an element of the built-in set 'colours()'"),
    fixed = TRUE
  )
  
  expect_error(
    map_ng(
      data = d,
      x = total,
      col = colpal,
      excluded = excluded.reg,
      exclude.fill = 99L,
      leg.title = "Legend title",
      plot = FALSE
    ),
    "Colour indicators of type 'integer' are not supported",
    fixed = TRUE
  )
})




# test_that("Mapping of adjoining States", {
#   expect_error(map_ng(lgas(c("Imo", "Abia"))), 
#                "LGA-level maps for adjoining States are not yet supported")
# })



test_that(
  "Mapping fails when choropleth is plotted with repetitive State levels",
  {
    data("esoph")
    set.seed(87)
    ng.esoph <-  
      transform(esoph, state = sample(states(), nrow(esoph), TRUE))
    
    expect_error(map_ng(data = ng.esoph, x = agegp, plot = FALSE), 
                 "argument lengths differ")
    
    # One record per State (although there are missing states)
    ng.esoph <- ng.esoph[!duplicated(ng.esoph$state), ]
    
    expect_s3_class(
      map_ng(ng.esoph$state, x = ng.esoph$agegp, plot = FALSE),
      "map"
    )
    expect_s3_class(
      map_ng(data = ng.esoph, x = agegp, plot = FALSE),
      "map"
    ) 
  })


test_that("Deprecation messages ahead of next release (current - 0.4.4)", {
  d <- data.frame(state = states(), value = sample(LETTERS[1:5], 37, TRUE))
  # 
  # rlang::with_options(lifecycle_verbosity = "warning", {
  #   expect_warning(map_ng(data = d, leg.x = 8L, plot = FALSE))
  # })
  # 
  # rlang::with_options(lifecycle_verbosity = "warning", {
  #   expect_warning(map_ng(data = d, leg.y = 94L, plot = FALSE))
  # }) 
  # 
  # rlang::with_options(lifecycle_verbosity = "warning", {
  #   expect_warning(map_ng(data = d, leg.orient = "horiz", plot = FALSE))
  # })
})


test_that("Labels are show", {
  expect_s3_class(map_ng(show.text = TRUE, plot = FALSE), "map")
  expect_s3_class(map_ng(states(gpz = "sw"), show.text = TRUE, plot = FALSE),
                  "map")
  expect_s3_class(map_ng(
    states(gpz = "sw"),
    show.text = TRUE,
    col = 4,
    plot = FALSE
  ), "map")
  
})


test_that("Labels can be resized", {
  # expect_error(map_ng(show.text = TRUE, cex = ".75")) 
  expect_s3_class(map_ng(show.text = TRUE, plot = FALSE), "map")
  expect_s3_class(map_ng(show.text = TRUE, cex = .5, plot = FALSE), "map")
})
