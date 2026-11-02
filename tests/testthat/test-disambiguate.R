test_that("inputs are validated", {
  expect_snapshot(disambiguate_lga("Bauchi"), error = TRUE)
  expect_snapshot(disambiguate_lga(lgas(c("Bauchi", "Kogi"))), error = TRUE)
  obilga <- suppressWarnings(lgas("Obi"))
  if (!interactive()) {
    expect_snapshot(disambiguate_lga(obilga), error = TRUE)
  }
  attr(obilga, "State") <- c("Benue", "Nassarawa")  # misspelled 
  expect_snapshot(disambiguate_lga(obilga), error = TRUE)
  attr(obilga, "State") <- c("Benue", "Nasarawa")
  local_mocked_bindings(menu = function(choices, graphics, title) 1L)
  expect_no_error(disambiguate_lga(obilga))
  expect_identical(attr(disambiguate_lga(obilga), "State"), "Benue")
  local_mocked_bindings(menu = function(choicde, graphics, title) 2L)
  expect_identical(attr(disambiguate_lga(obilga), "State"), "Nasarawa")
  expect_snapshot(disambiguate_lga(obilga, state = "Abia"), error = TRUE)
  })