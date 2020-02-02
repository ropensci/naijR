library(naijR)

test_that("Input corner cases are checked", {
  err1 <- "Objects of type .+ are not supported"
  
  expect_error(fix_mobile(NULL), err1)
  expect_error(fix_mobile(NA), err1)
  expect_error(fix_mobile(data.frame(x = c(09012343829, 08132348321))), err1)
  expect_error(fix_mobile(), "argument \"x\" is missing, with no default")
})

test_that("Wrong mobile numbers are repaired or removed.", {
  numbers <-
    c(
      "123456789",
      "0123456789",
      "8000000001",
      "9012345678",
      "07098765432",
      "08123456789",
      "09064321987",
      "O8055577889",
      "070456789011"
    )
  
  numbers <- fix_mobile(numbers)
  
  expect_type(numbers, "character")
  expect_true(is.na(numbers[1]))
  expect_true(all(nchar(na.omit(numbers)) == 11))
  expect_equal(numbers[1], NA_character_)
  expect_equal(numbers[2], NA_character_)
  expect_identical(numbers[3], "08000000001")
  expect_identical(numbers[4], "09012345678")
  expect_identical(numbers[5], "07098765432")
  expect_identical(numbers[6], "08123456789")
  expect_identical(numbers[7], "09064321987")
  expect_equal(numbers[8], NA_character_)
  expect_equal(numbers[9], NA_character_)
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
