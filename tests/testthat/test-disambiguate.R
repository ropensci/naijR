test_that("inputs are validated", {
  expect_snapshot(disambiguate_lga("Bauchi"), error = TRUE)
  expect_snapshot(disambiguate_lga(lgas(c("Bauchi", "Kogi"))), error = TRUE)
  obilga <- suppressWarnings(lgas("Obi"))
  attr(obilga, "State") <- c("Benue", "Nassarawa")  # misspelled 
  expect_snapshot(disambiguate_lga(obilga), error = TRUE)
  attr(obilga, "State") <- c("Benue", "Nasarawa")
  mock_menu <- function(opt) {
    function(choices, graphics, title) {
      opt
    }
  }
  local_mocked_bindings(
    menu = mock_menu(1L),
    interactive = function() TRUE
  )
  expect_no_error(disambiguate_lga(obilga))
  expect_identical(attr(disambiguate_lga(obilga), "State"), "Benue")
  local_mocked_bindings(
    menu = mock_menu(2L),
    interactive = function() TRUE
  )
  expect_identical(attr(disambiguate_lga(obilga), "State"), "Nasarawa")
  expect_snapshot(disambiguate_lga(obilga, state = "Abia"), error = TRUE)
  })