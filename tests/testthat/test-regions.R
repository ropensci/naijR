# Copyright (C) 2019-2021 Victor Ordu.
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

## Class names commonly used in this script
st.cl <- "states"
lg.cl <- "lgas"
reg.cl <- "regions"

## ---- State related tests ----
test_that("input is validated", {
  gpz.no.char <- "argument 'gpz' is not of type 'character'"
  all.no.lgl <- "'all' should be TRUE/FALSE"
  wrn.no.lgl <- "'warn' should be TRUE/FALSE"

  expect_error(states(gpz = 99), gpz.no.char)
  expect_error(states(gpz = NA), gpz.no.char)
  expect_error(states(gpz = "gpz"))
  expect_error(states(all = 99), all.no.lgl)
  expect_error(states(all = NULL), all.no.lgl)
  expect_error(states(all = NA), all.no.lgl)
  expect_error(states(all = "n"), all.no.lgl)
  expect_error(states(warn = 88), wrn.no.lgl)
  expect_error(states(warn = "full"), wrn.no.lgl)
  expect_error(states(warn = NA), wrn.no.lgl)
  expect_error(states(warn = NULL), wrn.no.lgl)
  expect_error(states("sw"),
               "'sw' represents a geopolitical zone, not a State")
})




test_that("'states' object is properly constructed", {
  wrn <- "One or more items is not a State. Spelling error\\?"
  shuffled <- sample(states(), 37L, FALSE)

  expect_warning(states(c("Oyo", "Legos")), wrn)
  expect_silent(states(c("xxx", "Benue"), warn = FALSE))
  expect_warning(states(c("kentucky", "Bornu", "Abia")))
  expect_warning(states(c("Nassarawa", "Oyo")), wrn)
  expect_identical(states(gpz = NULL), sort(shuffled))
  expect_true(is.unsorted(states(c("Benue", "Abia"))))
  expect_length(states(c("Jigawa", "Ebonyi", "Jigawa")), 3L)
})



test_that("FCT can be selectively removed", {
  allstates <- states()
  notall <- states(all = FALSE)
  fct <- "Federal Capital Territory"

  expect_length(notall, 36L)
  expect_length(allstates, 37L)
  expect_true(fct %in% allstates)
  expect_false(fct %in% notall)
})



test_that("printout is correct", {
  expect_snapshot(states())
  expect_snapshot(states("Akwa Ibom"))
  expect_snapshot(states(c("Abia", "Kaduna", "Bauchi")))
  expect_snapshot(states(c("Avia", "Dalta", "Keduna", "Dulta")))
  expect_warning(states(c("Avia", "Dalta", "Keduna", "Dulta")))
})


## ---- LGA related tests ----

test_that("illegal input is caught early", {
  chErr <- "Expected an object of type 'character'"
  chErr2 <- "Illegal use of empty string/missing character values"
  
  expect_error(lgas("Saarland"))
  expect_error(lgas("Maryland"), "None of the items is a valid LGA")
  expect_error(lgas(888), chErr)
  expect_error(lgas(NULL), chErr)
  expect_error(lgas(TRUE), chErr)
  expect_error(lgas(3.14), chErr)
  expect_error(lgas(c(NA, NA)), chErr)
})


test_that("LGAs are returned correctly", {
  res <- lgas("Plateau")
  res2 <- lgas(nam <- c("Borno", "Abia"))

  expect_match(res, "Pankshin", all = FALSE)
  expect_length(res, 17L)
  expect_type(res, "character")
  expect_null(names(res))
  expect_type(res2, "list")
  expect_length(res2, 2L)
  expect_warning(lgas(c("Oyo West", "Obomo Ngwa")),
                 "One or more items is not an LGA")
})


test_that("Output is sorted appropriately", {
  expect_false(is.unsorted(lgas()))
  expect_false(is.unsorted(lgas("Akwa Ibom")))
  expect_false(is.unsorted(names(lgas(c("Ebonyi", "Abia")))))
  expect_false(is.unsorted(names(lgas(c("Oyo", "Osun", "Ondo")))))
  expect_true(is.unsorted(lgas(c("Fufore", "Demsa"))))
})


test_that("Correct number of LGAs are returned for each State", {
  ss <- states()
  numLg <- as.integer(table(lgas_nigeria$state))

  expect_equal(sum(numLg), 774L)
  expect_length(lgas(), 774L)

  for (i in seq_along(ss))
    expect_equal(length(lgas(ss[i])), numLg[i], label = ss[i])

  expect_length(lgas(c("Fufore", "Demsa", "Fufore")), 3L)
})


test_that("State/LGAs synonyms are handled", {
  eklga <- lgas("Ekiti", strict = T)
  bau.oyo <- c("Oyo", "Bauchi")

  expect_length(lgas("Oyo"), 33L)
  expect_error(lgas("Oyo", strict = TRUE),
               "There is no LGA Oyo sharing State names")
  expect_error(lgas(bau.oyo, strict = TRUE),
               "There is no LGA Oyo sharing State names")
  expect_length(lgas(bau.oyo), 2)
  expect_type(lgas(bau.oyo), "list")
  expect_length(lgas("Bauchi"), 20L)
  expect_length(lgas("Bauchi", strict = TRUE), 1L)
})

test_that("LGA objects' attributes are appropriately formed", {
  expect_identical(attr(lgas("Abia"), "State"), "Abia")
  expect_length(attr(lgas(c("Kebbi", "Jigawa")), "State"), 2L)
  expect_length(attributes(lgas("Rivers")), 2L)
  expect_length(attributes(lgas()), 1L)

  benuelgafun <- quote(lgas(c("Obi", "Tarka")))
  expect_length(attr(suppressWarnings(eval(benuelgafun)), "State"), 0L)
  expect_warning(eval(benuelgafun), "'Obi' LGA is found in 2 States")
})

test_that("printout for LGAs is correct", {
  expect_snapshot(lgas())
  expect_snapshot(lgas("Akwa Ibom"))
  expect_snapshot(lgas(c('Fufore', 'Demsa', "Fufore")))
  expect_snapshot(lgas(c("Fafure", "Damsa", "Fufore", "Dumsa")))
  expect_warning(lgas(c("Fafure", "Damsa", "Fufore", "Dumsa")))
})

## ---- Internal generics ----
test_that("objects can be concatenated", {
  se <- states(gpz = "se")
  ss <- states(gpz = "ss")
  state.combo <- c(se, ss)
  lga.combo <- c(lgas("Sokoto"), lgas("Kebbi"))

  expect_s3_class(state.combo, st.cl)
  expect_s3_class(state.combo, reg.cl)
  expect_s3_class(lga.combo, lg.cl)
  expect_s3_class(lga.combo, reg.cl)
})

test_that("'regions' objects can be subset/indexed into", {
  ne <- states(gpz = "ne")
  oslg <- lgas("Osun")
  bo <- ne[[3]]
  egb <- oslg[[9]]
  three.states <- ne[c(4, 2)]
  four.lgas <- oslg[1:4]

  expect_s3_class(bo, st.cl)
  expect_s3_class(bo, reg.cl)
  expect_s3_class(bo, "character")
  expect_s3_class(egb, lg.cl)
  expect_s3_class(egb, reg.cl)
  expect_s3_class(egb, "character")
  expect_s3_class(three.states, st.cl)
  expect_s3_class(three.states, reg.cl)
  expect_s3_class(three.states, "character")
  expect_s3_class(four.lgas, lg.cl)
  expect_s3_class(four.lgas, reg.cl)
  expect_s3_class(four.lgas, "character")
})

test_that("missing values in 'regions' are handled", {
  classname <- "exclude"

  # State tests
  staat <- states(c("Imo", "Kaduna", "Adamawa"))
  staat[2:3] <- NA
  nomiss.st <- na.exclude(staat)

  expect_identical(states(), stats::na.exclude(states()))
  expect_s3_class(nomiss.st, classname)
  expect_s3_class(nomiss.st, "states")
  expect_s3_class(nomiss.st, "regions")
  expect_s3_class(nomiss.st, "character")
  expect_false(inherits(states("Lagos"), classname))

  # LGA tests
  ebonyi.lga <- lgas("Ebonyi")
  ebonyi.lga[c(2, 4:5)] <- NA
  nomiss.lg <- stats::na.exclude(ebonyi.lga)

  expect_identical(lgas(), stats::na.exclude(lgas()))
  expect_s3_class(nomiss.lg, classname)
  expect_s3_class(nomiss.lg, "lgas")
  expect_s3_class(nomiss.lg, "regions")
  expect_s3_class(nomiss.lg, "character")
  expect_false(inherits(lgas("Shomolu"), classname))
})



test_that("Warning is issued when 'Abuja' is used as a State", {
  x <- c("Jigawa", "Kebbi", "Nasarawa", "Abuja")
  y <- c(x, "Nassarawa")  # misspelt

  expect_warning(states(x),
                 "'Abuja' in position(s) 4 is not a State",
                 fixed = TRUE)
  expect_length(capture_warnings(states(x)), 1)
  expect_length(capture_warnings(states(y)), 2)
})

test_that("State names shared with LGAs can be coerced into 'lgas' objects", {
  lgaclass <- "lgas"
  
  expect_s3_class(as_lga("Bauchi"), lgaclass)
  expect_s3_class(as_lga("Gombe"), lgaclass)
})


test_that("LGA names shared with States can be coerced into 'states' objects", {
  stateclass <- "states"
  
  expect_s3_class(as_state("Kogi"), stateclass)
  expect_s3_class(as_state("Ebonyi"), stateclass)
  # expect_s3_class(as_state(factor("Kogi")), stateclass)
})

test_that("Regions without synonyms are not treated after validation", {
  err <- "is.character(obj) is not TRUE"
  err2 <- "To coerce a region with synonyms, use a vector of length 1L"
  err3 <- "The object does not possess State/LGA synonyms"
  wrn <- "Object was stripped down to mode 'character'"
  int <- 999L
  dbl <- 999
  lgl <- TRUE
  twoRegs <- c("Katsina", "Ebonyi")
  
  expect_error(as_state(int), err, fixed = TRUE)
  expect_error(as_lga(int), err, fixed = TRUE)
  expect_error(as_state(dbl), err, fixed = TRUE)
  expect_error(as_lga(dbl), err, fixed = TRUE)
  expect_error(as_state(lgl), err, fixed = TRUE)
  expect_error(as_lga(lgl), err, fixed = TRUE)
  expect_error(as_lga(twoRegs), err2)
  expect_error(as_state(twoRegs), err2)
  expect_error(as_lga("Kano"), err3)
  expect_error(as_state("Kano"), err3)
  expect_error(as_state(states("Kano")), err3)
  expect_error(as_lga(lgas("Michika")), err3)
  expect_warning(as_state(states("Katsina")), wrn)
  expect_error(as_lga(lgas("Ebonyi")), err2)
  
  kt.st <- suppressWarnings(as_state(states("Katsina")))
  expect_length(class(kt.st), 3L)
})

test_that("LGA and States can be disambiguated", {
  ## Input validation
  err.typecheck <- "Expected an object of class `lgas`"
  
  expect_error(disambiguate_lga(42L), err.typecheck)
  expect_error(disambiguate_lga(NULL), err.typecheck)
  expect_error(disambiguate_lga(pi), err.typecheck)
  expect_error(disambiguate_lga(TRUE), err.typecheck)
  expect_error(disambiguate_lga(list()), err.typecheck)
  expect_error(disambiguate_lga(data.frame()), err.typecheck)
  expect_error(disambiguate_lga(matrix(letters, 2)), err.typecheck)
  expect_error(disambiguate_lga("FCT"), err.typecheck)
  expect_error(disambiguate_lga("Ekiti"), err.typecheck)
  expect_error(disambiguate_lga(states("Ekiti")), err.typecheck)
  
  ## Logic
  nasstr <- "Nasarawa"
  benstr <- "Benue"
  obilga <- suppressWarnings(lgas("Obi"))
  obiben <- disambiguate_lga(obilga, benstr)
  obinas <- disambiguate_lga(obilga, nasstr)
  myattr <- "State"
  
  expect_s3_class(obiben, "lgas")
  expect_identical(attr(obiben, myattr), benstr)
  expect_error(disambiguate_lga(c(obilga, "Tarka")),
               "only done for objects with one element")
  expect_identical(attr(obinas, myattr), nasstr)
  expect_error(disambiguate_lga(obilga, "Imo"), "is not in [[:alpha:]]+ State")
  
  if (!interactive())
    expect_error(disambiguate_lga(obiben),
                 "This operation can only be done in interactive mode")
})

# Test an object for States
test_that("input is validated", {
  expect_warning(is_state(pi))
  expect_error(is_state(NULL), "Expected a non-null atomic vector as input")
  expect_length(suppressWarnings(is_state(pi)), 1L)
  expect_false(is_state(character()))
})

test_that("States can be identified in an object", {
  ss.good <- ss.bad <- ss.na <- states()
  
  ss.bad[c(5, 7, 19)] <- c("big", "bad", "wolf")
  ind.nas <- c(6, 19, 33)
  ss.na[ind.nas] <- NA
  
  good <- is_state(ss.good)
  bad <- is_state(ss.bad)
  has.na <- suppressWarnings(is_state(ss.na))
  
  all.good <- all(good)
  not.all.good <- all(bad)
  nas.but.good <- all(has.na, na.rm = TRUE)
  
  expect_length(all.good, 1L)
  expect_true((all.good))
  expect_length(not.all.good, 1L)
  expect_false(not.all.good)
  expect_length(good, length(ss.good))
  expect_length(bad, length(ss.bad))
  expect_length(has.na, length(ss.na))
  expect_length(good, 37L)
  expect_equal(sum(good), 37L)
  expect_length(bad, 37L)
  expect_equal(sum(bad), 34L)
  expect_true(nas.but.good)
  expect_warning(is_state(ss.na), "Invalid entries were replaced with NAs")
  expect_length(has.na, 37)
  expect_equal(sum(has.na), NA_integer_)
  expect_equal(sum(has.na, na.rm = TRUE), 34L)
  expect_true(anyNA(has.na))
  expect_equal(sum(nas.but.good), 1L)
  expect_length(nas.but.good, 1L)
  expect_equal(sum(is.na(has.na)), length(ind.nas))
})

test_that("Different representations of the FCT are handled", {
  ss <- c(
    "Zamfara",
    "Niger",
    "Borno",
    "Kaduna",
    "Jigawa",
    "Taraba",
    "Benue",
    "Ogun",
    "Ekiti",
    "Rivers",
    "Lagos",
    "Delta",
    "Cross River",
    "Sokoto",
    "Kano",
    "Fct",
    "Oyo",
    "Edo",
    "Osun",
    "Plateau",
    "Abia",
    "Akwa Ibom",
    "Bauchi",
    "Ondo",
    "Gombe",
    "Kogi",
    "Adamawa",
    "Katsina",
    "Enugu",
    "Yobe",
    "Ebonyi",
    "Imo",
    "Kwara",
    "Anambra",
    "Nasarawa",
    "Bayelsa",
    "Kebbi"
  )
  
  expect_true(is_state("FCT"))
  expect_true(is_state("Federal Capital Territory"))
  expect_false(sum(is_state(ss)) == length(ss))
  expect_false(is_state("Fct"))
})





test_that("Mispelt LGA are discovered", {
  ibd <- c(
    'Akinyele',
    'Egbeda',
    'Ibadan North',
    'Ibadan North East',         # misspelt
    'Ibadan North West',         # misspelt
    'Ibadan South East',         # misspelt
    'Ibadan South West',         # misspelt
    'Iddo',                      # misspelt
    'Lagelu',
    'Oluyole',
    'Onu-Ara'                    # misspelt
  )
  
  xx <- is_lga(ibd)
  err <- "x should be of type 'character'"
  expect_false(all(xx))
  expect_error(is_lga(NULL), err)
  expect_error(is_lga(42), err)
  expect_error(is_lga(cars), err)
  expect_error(is_lga(c(TRUE, FALSE)), err)
  expect_type(xx, 'logical')
})





test_that("is_lga recognises LGAs", {
  anlga <- "Amuwo-Odofin"
  veclga <- c("Akira-Uba", "Hawul", NA)
  
  
  expect_true(is_lga(anlga))
  expect_false(all(is_lga(veclga)))
})



test_that("State/LGA synonyms are discernible", {
  rr <- lgas_like_states()
  
  for (i in rr) {
    expect_true(is_state(i))
    expect_true(is_lga(i))
    expect_true(is_state(states(i)))
    expect_true(all(is_lga(lgas(i))))
    expect_true(suppressWarnings(is_lga(lgas(i, strict = TRUE))))
  }
})


test_that("Warnings on multiple LGAs", {
  nas <- "Nasarawa"
  wrn <- "'Nasarawa' LGA is found in 2 States"
  
  expect_silent(lgas(nas))
  expect_warning(lgas(nas, strict = TRUE), wrn)
  expect_warning(lgas(nas, strict = TRUE, warn = FALSE), wrn)
})


test_that("correct output of regions are printed", {
  expect_snapshot(print(states()))
  expect_snapshot(print(lgas("Abia")))
  expect_snapshot(print(lgas(c("Abia", "Imo"))))
})