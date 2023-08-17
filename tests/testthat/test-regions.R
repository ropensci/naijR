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
})




test_that("'states' object is properly constructed", {
  wrn <- "One or more items is not a State. Spelling error\\?"
  shuffled <- sample(states(), 37L, FALSE)
  
  expect_warning(states(c("Oyo", "Legos")), wrn)
  expect_silent(states(c("xxx", "Benue"), warn = FALSE))
  expect_warning(states(c("kentucky", "Bornu", "Abia")))
  expect_warning(states(c("Nassarawa", "Oyo")), wrn)
  expect_identical(states(gpz = NULL), sort(shuffled))
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






## ---- LGA related tests ---- 

test_that("illegal input is caught early", {
  chErr <- "Expected an object of type 'character'"
  
  expect_error(lgas("Saarland"))
  expect_error(lgas("Maryland"), "None of the items is a valid LGA")
  expect_error(lgas(888), chErr)
  expect_error(lgas(NULL), chErr)
  expect_error(lgas(TRUE), chErr)
  expect_error(lgas(3.14), chErr)
})


test_that("LGAs are returned correctly", {
  res <- lgas("Plateau")
  res2 <- lgas(nam <- c("Borno", "Abia"))
  
  expect_match(res, "Pankshin", all = FALSE)
  expect_length(res, 17L)
  expect_type(res, "character")
  expect_null(names(res))
  expect_type(res2, "list")
  expect_named(res2, nam)
  expect_length(res2, 2L)
  expect_warning(lgas(c("Oyo West", "Obomo Ngwa")), 
                 "One or more items is not an LGA")
})

test_that("Correct number of LGAs are returned for each State", {
  ss <- states()
  numLg <- as.integer(table(lgas_nigeria$state))
  
  expect_equal(sum(numLg), 774L)
  expect_length(lgas(), 774L)
  
  for (i in seq_along(ss)) 
    expect_equal(length(lgas(ss[i])), numLg[i], label = ss[i])
})


test_that("State/LGAs synonyms are handled", {
  eklga <- lgas("Ekiti", strict = T)

  expect_length(lgas("Oyo"), 33L)
  expect_error(lgas("Oyo", strict = TRUE),
               "There is no LGA Oyo sharing State names")
  expect_length(lgas("Bauchi"), 20L)
  expect_length(lgas("Bauchi", strict = TRUE), 1L)
})

test_that("LGA objects' attributes appropriately", {
  expect_identical(attr(lgas("Abia"), "State"), "Abia")
  expect_length(attr(lgas(c("Kebbi", "Jigawa")), "State"), 2L)
  expect_length(attributes(lgas("Rivers")), 2L)
  expect_length(attributes(lgas()), 1L)
  
  benuelgafun <- quote(lgas(c("Obi", "Tarka")))
  expect_length(attr(suppressWarnings(eval(benuelgafun)), "State"), 0L)
  expect_warning(eval(benuelgafun), "'Obi' LGA is found in 2 States")
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
  nomiss.st <- stats::na.exclude(staat)
  
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
