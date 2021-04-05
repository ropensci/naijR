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


test_that("illegal input is caught early", {
  chErr <- "Expected an object of type 'character'"
  
  expect_error(lgas_ng("Saarland"))
  expect_error(lgas_ng("Maryland"),
               "One or more elements of 'ng.state' is not a State in Nigeria",
               fixed = TRUE)
  expect_error(lgas_ng(888), chErr)
  expect_error(lgas_ng(NULL), chErr)
  expect_error(lgas_ng(TRUE), chErr)
  expect_error(lgas_ng(3.14), chErr)
})


test_that("LGAs are returned correctly", {
  res <- lgas_ng("Plateau")
  res2 <- lgas_ng(nam <- c("Borno", "Abia"))
  
  expect_match(res, "Pankshin", all = FALSE)
  expect_length(res, 17L)
  expect_type(res, "character")
  expect_is(res, "character")
  expect_null(names(res))
  expect_is(res2, "list")
  expect_type(res2, "list")
  expect_named(res2, nam)
  expect_length(res2, 2L)
})

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


test_that("Mispelt LGA are discovered", {
  xx <- is_lga(ibd)
  err <- "x should be of type 'character'"
  expect_false(all(xx))
  expect_error(is_lga(NULL), err)
  expect_error(is_lga(42), err)
  expect_error(is_lga(cars), err)
  expect_error(is_lga(c(TRUE, FALSE)), err)
  expect_type(xx, 'logical')
  expect_is(matrix(is_lga(lgas_ng())), 'matrix')
  
})


test_that("is_lga recognises LGAs", {
  anlga <- "Amuwo-Odofin"
  veclga <- c("Akira-Uba", "Hawul", NA)


  expect_true(is_lga(anlga))
  expect_false(all(is_lga(veclga)))
})