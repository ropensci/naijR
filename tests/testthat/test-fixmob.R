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
          ##   email (victorordu at outlook dot com) to   ##
          ##   have it removed from the source code.      ##
          ##################################################
          ##################################################

# Mobile No.pattern: http://www.5starsmsng.com/nigerian-phone-prefixes
init.numbers <-
  c(
    "123456789",
    "0123456789",
    "8000000001",
    "9012345678",
    "07098765432",
    "08123456789",
    "09064321987",
    "O8055577889",   # starts with letter 'O'
    "07O2135689o",   # has lower & upper case 'O'
    "070456789011",
    "07031356890",
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

          
test_that("Input corner cases are checked", {
  err1 <- "Objects of type .+ are not supported"
  
  expect_error(fix_mobile(NULL), err1)
  expect_type(fix_mobile(NA), "character")
  expect_identical(fix_mobile(NA), NA_character_)
  expect_identical(fix_mobile(c("8034510441", NA))[2], NA_character_)
  expect_error(fix_mobile(data.frame(x = c(09012343829, 08132348321))), err1)
  expect_error(fix_mobile(), "argument \"x\" is missing, with no default")
  expect_error(fix_mobile(c(TRUE, FALSE)), err1)
})


test_that("Wrong mobile numbers are repaired or removed.", {
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
  expect_identical(fin.numbers[8], "08055577889")
  expect_identical(fin.numbers[9], "07021356890")
  expect_equal(fin.numbers[10], NA_character_)
  
  for (i in 11:length(init.numbers))
    expect_identical(fin.numbers[i], init.numbers[i])
})


test_that("Numbers read from MS Excel are appropriately treated", {
  df <- readRDS("data/numbers.rds")
  fx <- fix_mobile(df$Number)
  
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


test_that("general formatting of numbers can be maintained", {
  expect_identical(fix_mobile("803 451 0441"), "08034510441")
  expect_identical(fix_mobile("703-452-1234"), "07034521234")
})


test_that("country code is recognized", {
  num <- "2348059874323"
  numx <- paste0("+", num)
  
  expect_identical(fix_mobile(num), numx)
  expect_identical(fix_mobile(numx), numx)
})


test_that("dealing with whitespace", {
  num <- "08043456623"
  
  expect_identical(fix_mobile(" 08043456623 "), num)
  expect_identical(fix_mobile(" 08043456623"), num)
  expect_identical(fix_mobile("08043456623 "), num)
  expect_identical(fix_mobile("     08043456623"), num)
})


test_that("verbosity on number removal is activated", {
  local_edition(2)
  
  op <- options(verbose = TRUE)
  expect_warning(fix_mobile(init.numbers), "2 numbers were removed") 
  options(op)
})
