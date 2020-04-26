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

          ##################################################
          ##################################################
          ##                  DISCLAIMER                  ##
          ##   The phone numbers that were used to write  ##
          ##   these unit tests are purely a figment of   ##
          ##   the author's imagination and are not in    ##
          ##   any way intended to represent any actual   ##
          ##   phone numbers belonging to anyone, living  ##
          ##   or dead. If any number here looks like a   ##
          ##   number you know, please notify us via      ##
          ##   email (victorordu at outlook.com) to have  ##
          ##   it removed from the source code.           ##
          ##################################################
          ##################################################
          
test_that("Input corner cases are checked", {
  err1 <- "Objects of type .+ are not supported"
  
  expect_error(fix_mobile(NULL), err1)
  expect_error(fix_mobile(NA), err1)
  expect_error(fix_mobile(data.frame(x = c(09012343829, 08132348321))), err1)
  expect_error(fix_mobile(), "argument \"x\" is missing, with no default")
})

test_that("Wrong mobile numbers are repaired or removed.", {
  
  init.numbers <-
    c(
      "123456789",
      "0123456789",
      "8000000001",
      "9012345678",
      "07098765432",
      "08123456789",
      "09064321987",
      "O8055577889",
      "070456789011",
      "07031356890",    # http://www.5starsmsng.com/nigerian-phone-prefixes
      "07061356890",
      "08031356890",
      "08061356890",
      "08101356890",
      "08131356890",
      "08141356890",
      "08161356890",
      "09031356890",
      "09061356890",
      "07051356890",
      "08051356890",
      "08071356890",
      "08111356890",
      "08151356890",
      "09051356890",
      "07011356890",
      "07081356890",
      "08021356890",
      "08081356890",
      "08121356890",
      "09021356890",
      "09071356890",
      "09011356890",
      "08091356890",
      "08171356890",
      "08181356890",
      "09081356890",
      "09091356890",
      "07028356890",
      "07029356890",
      "08191356890",
      "07025356890",
      "07026356890",
      "07041356890",
      "07027356890",
      "07091356890",
      "07071356890",
      "08042318843",
      "07021356890"
    )
  
  fin.numbers <- fix_mobile(init.numbers)
  
  expect_type(fin.numbers, "character")
  expect_true(is.na(fin.numbers[1]))
  expect_true(all(nchar(na.omit(fin.numbers)) == 11))
  expect_equal(fin.numbers[1], NA_character_)
  expect_equal(fin.numbers[2], NA_character_)
  expect_identical(fin.numbers[3], "08000000001")
  expect_identical(fin.numbers[4], "09012345678")
  expect_identical(fin.numbers[5], "07098765432")
  expect_identical(fin.numbers[6], "08123456789")
  expect_identical(fin.numbers[7], "09064321987")
  for (i in 10:length(init.numbers))
    expect_identical(fin.numbers[i], init.numbers[i])
  expect_equal(fin.numbers[8], NA_character_)
  expect_equal(fin.numbers[9], NA_character_)
})


test_that("Numbers read from MS Excel are appropriately treated", {
  df <- as.data.frame(readxl::read_xlsx("data/numbers.xlsx"))
  fx <- fix_mobile(df[, 1])
  
  expect_true(all(nchar(fx) == 11))
  expect_false(anyNA(fx))
  expect_length(fx, nrow(df))
  expect_type(fx, "character")
})

test_that("Return values for different kinds of legal input", {
  pops <- '08037239837'
  
  expect_identical(fix_mobile(9), NA_character_)
  expect_identical(fix_mobile('test-case'), NA_character_)
  expect_identical(fix_mobile(08037239837), pops)
  expect_identical(fix_mobile(8037239837), pops)
})
