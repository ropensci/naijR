# tests/testthat/test-ng_distance.R
test_that("input is validated", {
  expect_error(ng_distance(123, "Lagos"))
  expect_error(ng_distance("Lagos", TRUE))
  expect_error(ng_distance(), "'a' and 'b' must both be supplied")
  expect_error(ng_distance("Lagos", "Kaduna", unit = "Fahrenheit"),
               'should be one of "km", "miles"')
})

test_that("ng_distance returns correct value for known pair", {
  expect_equal(ng_distance("Lagos", "Abuja"), 761)          # replace with your actual value
  expect_equal(ng_distance("Kano", "lagos"), 1139)           # case insensitivity
})

test_that("unit conversion to miles works", {
  expect_equal(ng_distance("Abuja", "Kano", unit = "miles"), 246.1)
})

test_that("case insensitivity works", {
  expect_equal(ng_distance("PORt HarCOurt", "EnUGu"),
               ng_distance("Port Harcourt", "Enugu"))
})

test_that("error on unknown city", {
  expect_error(ng_distance("Lagos", "FakeCity"), "not found")
  expect_error(ng_distance("Invalid", "Abuja"), "not found")
})

test_that("same city returns 0", {
  expect_equal(ng_distance("Abuja", "Abuja"), 0)
  expect_equal(ng_distance("ibadan", "Ibadan"), 0)
})

test_that("output is numeric scalar with 1 decimal place", {
  d <- ng_distance("Jos", "Maiduguri")
  expect_type(d, "double")
  expect_length(d, 1)
  expect_equal(d, round(d, 1))
})