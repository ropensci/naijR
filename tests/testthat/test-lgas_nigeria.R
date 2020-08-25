# Copyright (C) 2019 Victor Ordu.
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
  expect_error(lgas_ng("Saarland"))
  expect_error(lgas_ng("Maryland"),
               "One or more elements of 'ng.state' is not a State in Nigeria",
               fixed = TRUE)
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

# test_that("is_lga recognises LGAs", {
#   anlga <- "Amuwo-Odofin"
#   veclga <- c("Akira-Uba", "Hawul", NA)
#   
#   
#   expect_true(is_lga(anlga))
#   expect_false
# })