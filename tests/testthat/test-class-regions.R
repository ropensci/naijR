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
library(naijR)

## ---- State related tests ----
test_that("input is validated", {
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




test_that("'states' object is constructed", {
  wrn <- "One or more items is not a State. Spelling error\\?"
  expect_warning(states(c("Oyo", "Legos")), wrn)
  expect_silent(states(c("xxx", "Benue"), warn = FALSE))
  expect_warning(states(c("kentucky", "Bornu", "Abia")))
  expect_warning(states(c("Nassarawa", "Oyo")), wrn)
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
  expect_is(res, "character")
  expect_null(names(res))
  expect_is(res2, "list")
  expect_type(res2, "list")
  expect_named(res2, nam)
  expect_length(res2, 2L)
  expect_warning(lgas(c("Oyo West", "Obomo Ngwa")), 
                 "One or more items is not an LGA")
})


test_that("Regions without synonyms are not treated after validation", {
  err <- "Expected a character vector"
  err2 <- "To coerce a region with synonyms, use a vector of length 1L"
  err3 <- "The object does not possess State/LGA synonyms"
  wrn <- "Object was stripped down to mode 'character'"
  int <- 999L
  dbl <- 999
  lgl <- TRUE
  twoRegs <- c("Katsina", "Ebonyi")
  
  expect_error(as_state(int), err)
  expect_error(as_lga(int), err)
  expect_error(as_state(dbl), err)
  expect_error(as_lga(dbl), err)
  expect_error(as_state(lgl), err)
  expect_error(as_lga(lgl), err)
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

test_that("State names shared with LGAs can be coerced into 'lgas' objects", {
  lgaclass <- "lgas"
  
  expect_s3_class(as_lga("Bauchi"), lgaclass)
  expect_s3_class(as_lga("Gombe"), lgaclass)
})


test_that("LGA names shared with States can be coerced into 'states' objects", {
  stateclass <- "states"
  
  expect_s3_class(as_state("Kogi"), stateclass)
  expect_s3_class(as_state("Ebonyi"), stateclass)
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
  expect_length(lgas("Oyo"), 33L)
  expect_error(lgas("Oyo", strict = TRUE),
               "strict can only be set to TRUE where State/LGA syonnyms exist")
  expect_length(lgas("Bauchi"), 20L)
  expect_length(lgas("Bauchi", strict = TRUE), 1L)
})


test_that("LGA objects' attributes are set when appropriate", {
  expect_identical(attr(lgas("Abia"), "State"), "Abia")
  expect_length(attr(lgas(c("Kebbi", "Jigawa")), "State"), 2L)
  expect_length(attributes(lgas("Rivers")), 2L)
  expect_length(attributes(lgas()), 1L)
})