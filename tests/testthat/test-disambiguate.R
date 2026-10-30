test_that("inputs are validated", {
  expect_snapshot(disambiguate_lga("Bauchi"), error = TRUE)
  expect_snapshot(disambiguate_lga(lgas(c("Bauchi", "Kogi"))), error = TRUE)
  obilga <- suppressWarnings(lgas("Obi"))
  if (!interactive()) {
    expect_snapshot(disambiguate_lga(obilga), error = TRUE)
  }
  expect_snapshot(disambiguate_lga(obilga, state = "Abia"), error = TRUE)
  })