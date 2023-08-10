# Source file: test-map_nigeria.R 
# 
# GPL-3 License
# 
# Copyright (c) 2020-2021 Victor Ordu
maptype <- "sf"

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
  expect_s3_class(map_ng("Nigeria", plot = FALSE), maptype)
})

test_that("Geo-political Zones are plotted", {
  sw <- map_ng(region = states(gpz = 'sw'), plot = FALSE)
  
  expect_equal(nrow(sw), 6L)
  expect_identical(sw$admin1Name, 
                   c("Ekiti", "Lagos", "Ogun", "Ondo", "Osun", "Oyo"))
})

test_that("LGAs are plotted", {
  expect_s3_class(map_ng(lgas("Akinyele"), plot = FALSE), maptype)
  expect_s3_class(map_ng(lgas("Owerri North"), plot = FALSE), maptype)
})

set.seed(4)
df <- data.frame(region = states(all = TRUE), value = sample(0:6, 37, TRUE))
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
  dat <- readRDS("data/pvc2015.rds")
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
    ), maptype)
  
  expect_s3_class(map_ng(data = dat,
                         x = alpha,
                         plot = FALSE),
                  maptype)
  
  ss <- states()
  expect_s3_class(map_ng(
    region = ss,
    x = runif(length(ss), max = 100),
    breaks = c(0, 40, 60, 100),
    col = 'YlOrRd',
    show.text = FALSE,
    plot = FALSE
  ),
  maptype)
  
  expect_s3_class(map_ng(
    region = ss,
    x = sample(LETTERS[1:5], length(ss), TRUE),
    col = "red",
    show.text = FALSE,
    plot = FALSE
  ), maptype)
})

test_that("Draw choropleth automatically with 2-column data frames", {
  ds <- data.frame(state = states(), value = sample(LETTERS[1:5], 37, TRUE))
  dl <- data.frame(LGA = lgas(), value = sample(LETTERS[1:5], 774, TRUE))
  
  expect_s3_class(map_ng(data = ds, plot = FALSE), maptype)
  expect_s3_class(suppressWarnings(map_ng(data = dl, plot = FALSE)), maptype)
  expect_warning(map_ng(data = dl, plot = FALSE),
                 "Duplicated LGAs found, but may or may not need a review")
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
  
  mm <- maptype
  
  expect_s3_class(eval_tidy(func), mm)
  
  func$col <- "YlOrRd"
  expect_s3_class(eval_tidy(func), mm)
  
  func$col <- 'blue'
  expect_s3_class(eval_tidy(func), mm)
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
  
  Fac <- ordered(getSmpl(2))
  expect_s3_class(map_ng(x = Fac, plot = FALSE), maptype)
  
  Char <- getSmpl(5)
  expect_s3_class(map_ng(x = Char, plot = FALSE), maptype)
  
  sss <- states()
  dd <- data.frame(region = sss, Col = Fac, stringsAsFactors = FALSE)
  expect_s3_class(map_ng(data = dd, x = Col, plot = FALSE), maptype)
  
  expect_s3_class(map_ng(sss, x = Fac, plot = FALSE), maptype)
  expect_s3_class(map_ng(sss, x = Char, plot = FALSE), maptype)
  
  Int <- sample(1L:5L, 37, TRUE)
  expect_error(map_ng(sss, x = Int, plot = FALSE),
               'Breaks were not provided for the categorization of a numeric type')
  expect_s3_class(map_ng(sss, x = as.factor(Int), plot = FALSE), maptype)
  
  brks <- c(0, 40, 60, 100)
  expect_s3_class(map_ng(sss, x = getDblSmpl(), breaks = brks, plot = FALSE), maptype)
  
  dd$dblCol <- getDblSmpl()
  qq <- quote(map_ng(data = dd, x = dblCol, breaks = brks, plot = FALSE))
  expect_s3_class(eval(qq), maptype)
  qq$categories <- c("Low", "Medium", "High")
  expect_s3_class(eval(qq), maptype)
  qq$col <- "green"
  expect_s3_class(eval(qq), maptype)
})

test_that("Parameters passed via ellipsis work seamlessly", {
  expect_s3_class(map_ng(lwd = 2, plot = FALSE), maptype)
  expect_s3_class(map_ng(lwd = 2, col = 2, plot = FALSE), maptype)
  expect_error(map_ng(NULL, lwd = 2, col = 2, plot = FALSE), 
               "Expected a character vector as 'region'")
})

test_that("All individual plain State maps can be drawn", {
  for (s in states())
    expect_s3_class(map_ng(s, plot = FALSE), maptype)
})

test_that("All LGAs within a given State are drawn", {
  for (s in states()) {
    expect_s3_class(map_ng(lgas(s), plot = FALSE), maptype)
  }
})

test_that("All individual LGA maps can be drawn", {
  for (s in states()) {
    lgs <- lgas(s)

    for (lg in lgs) {
      x <- suppressWarnings(lgas(lg))
      state <- attr(x, "State")

      if (length(state) > 1L)
        x <- disambiguate_lga(x, state = s)

      expect_s3_class(map_ng(x, plot = FALSE), maptype)
    }
  }
})

test_that("Map LGAs together as individual blocs", {
  abiaLga <- lgas("Abia")
  testMap <- "data/test-map.png"
  
  if (file.exists(testMap))
    file.remove(testMap)
  
  png(testMap)
  val <- try(map_ng(abiaLga), silent = TRUE)
  dev.off()
  
  expect_s3_class(map_ng(abiaLga, plot = FALSE), maptype)
  expect_false(inherits(val, "try-error"))
  expect_true(file.exists(testMap))
  
  file.remove(testMap)
})

test_that("Choropleth map can be formed with excluded regions", {
  colpal <- "YlOrRd"
  excluded.reg <- c("Abia", "Jigawa")
  exclusion.color <- "green"
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
    maptype
  )

  expect_s3_class(
    map_ng(
      data = d,
      x = total,
      col = colpal,
      excluded = excluded.reg,
      exclude.fill = exclusion.color,
      plot = FALSE
    ),
    maptype
  )

  expect_s3_class(
    map_ng(
      data = d,
      x = total,
      col = colpal,
      excluded = excluded.reg,
      exclude.fill = exclusion.color,
      leg.title = "Legend title",
      plot = FALSE
    ),
    maptype
  )

  expect_error(
    map_ng(
      data = d,
      x = total,
      col = colpal,
      excluded = excluded.reg,
      exclude.fill = c(exclusion.color, "grey"),
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
    )
  )
})

test_that(
"Mapping fails when choropleth is plotted with repetitive State levels",
  {
    data("esoph")
    set.seed(87)
    ng.esoph <- transform(esoph, state = sample(states(), nrow(esoph), TRUE))
    expect_error(map_ng(data = ng.esoph, x = agegp, plot = FALSE),
                 "Data cannot be matched with map. Aggregate them by States")

    # One record per State (although there are missing states)
    ng.esoph <- ng.esoph[!duplicated(ng.esoph$state), ]

    expect_s3_class(
      map_ng(ng.esoph$state, x = ng.esoph$agegp, plot = FALSE),
      maptype
    )
    expect_s3_class(
      map_ng(data = ng.esoph, x = agegp, plot = FALSE),
      maptype
    )
  })


test_that("Labels are shown", {
  expect_s3_class(map_ng(show.text = TRUE, plot = FALSE), maptype)
  expect_s3_class(map_ng(states(gpz = "sw"), show.text = TRUE, plot = FALSE),
                  maptype)
  expect_s3_class(map_ng(
    states(gpz = "sw"),
    show.text = TRUE,
    col = 4,
    plot = FALSE
  ), maptype)
})

test_that("Labels can be resized", {
  # expect_error(map_ng(show.text = TRUE, cex = ".75")) 
  expect_s3_class(map_ng(show.text = TRUE, plot = FALSE), maptype)
  expect_s3_class(map_ng(show.text = TRUE, cex = .5, plot = FALSE), maptype)
})
