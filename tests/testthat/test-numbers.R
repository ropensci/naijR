library(naijR)

test_that("Wrong mobile numbers are repaired or removed.", {
  numbers <-
    as.data.frame(
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
    )
  numbers <- fix_mobile(numbers)
  
  # Tests proper
  expect_type(numbers, "character")
  expect_true(is.na(numbers[1]))
  expect_true(all(nchar(na.omit(numbers)) == 11))
})