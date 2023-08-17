totalerr <- "consider reconstructing 'x'"

test_that("input is validated before fixing state names", {
  errchr <- "'x' is not a character vector"
  warn0 <- "'x' has length 0L or only missing values"
  
  expect_error(fix_region(99), errchr)
  expect_error(fix_region(NA), errchr)
  expect_error(fix_region(c(NA, NA, NA)), errchr)
  expect_error(fix_region(NULL), errchr)
  expect_error(fix_region(TRUE), errchr)
  expect_error(fix_region(""), "'x' only has empty strings")
  expect_warning(try(fix_region(c("Ogin", "", "Abia")), silent = TRUE),
                 "Tried to fix empty strings - may produce errors")
  expect_warning(fix_region(NA_character_), warn0)
  expect_warning(fix_region(character()), warn0)
  expect_warning(fix_region(factor()), warn0)
  expect_error(fix_region(lgas(), 9))
  expect_error(fix_region(lgas(), quietly = 9))
  expect_error(fix_region(lgas(), graphic = 9))
  expect_type(fix_region(matrix(states())), "character") ## preserve class??
})


test_that("Messaging clear when fixing via character vectors or factors", {
  # Function for creating regular expressions for matching messages
  .msgfunc <- function(x) {
    stopifnot(grepl("^(.+)(\\s=>\\s)(.+)$", x))
    sprintf("Successful fix\\(es\\)\\:\\n\\-+\\n\\*\\s%s", x)
  } #                                                 ^
  #                                               Note place-holder
  
  # data ---
  multi.lga <- readRDS("data/mispelt-lga.rds")
  
  # messages ---
  change1 <- "Fufure => Fufore"
  change2 <- "Fafure => Fufore"
  msg1 <- .msgfunc(change1)
  msg2 <- .msgfunc(change2)
  morethanone <- "approximately matched more than one region"
  
  # character vectors ---
  lg.chr <- 
    c("Fufure", "Demsa", "Fufure", "Machika", "Ganye", "Noman", "Fufure")
  correctLga.chr <- lg.chr[2]
  misspeltLga.chr <- lg.chr[3]
  bothlga.chr <- c(correctLga.chr, misspeltLga.chr)
  
  expect_no_warning(fix_region(lgas(correctLga.chr)))
  expect_message(fix_region(lgas(misspeltLga.chr)))
  expect_message(fix_region(lgas(bothlga.chr)), msg1)
  expect_message(fix_region(lgas(c(bothlga.chr, "Fufore"))), 
                 msg1)
  expect_message(fix_region(lgas(c(bothlga.chr, "Fafure"))),
                 "not applied.+Fafure")
  expect_error(fix_region(misspeltLga.chr), totalerr)
  expect_message(fix_region(lgas(lg.chr)), morethanone)
  expect_message(suppressWarnings(fix_region(lgas(lg.chr))),
                 sprintf("%s.+Noman => Numan", change1))
  expect_message(fix_region(lgas(multi.lga)), change1)
  
  # factors ---
  lg.fac <- factor(lg.chr)
  correctLga.fac <- droplevels(lg.fac[2])
  misspeltLga.fac <- droplevels(lg.fac[3])
  bothlga.fac <- c(correctLga.fac, misspeltLga.fac)
  
  expect_silent(fix_region(lgas(correctLga.fac)))
  expect_message(fix_region(lgas(misspeltLga.fac)))
  expect_message(fix_region(lgas(bothlga.fac)), msg1)
  expect_message(fix_region(lgas(
    c(bothlga.fac, factor("Fufore")), 
    warn = FALSE)), msg1)
  expect_error(fix_region(misspeltLga.fac), totalerr, fixed = TRUE)
  expect_message(fix_region(lgas(lg.fac)), morethanone)
  expect_message(suppressWarnings(fix_region(lgas(lg.fac))),
                 sprintf("%s.+Noman => Numan", change1))
  expect_message(fix_region(lgas(multi.lga)),
                 change1)
  
})


test_that("various cases for fixing state names", {
  ss <- states()
  ss2 <- states(c("Oyo", "Legos"), warn = FALSE)
  ssx <- states(c("xxx", "Benue"), warn = FALSE)
  ss.us <- c("kentucky", "Bornu", "Abia")
  fctup <- "Federal Capital Territory"
  fctlw <- "FCT"
  
  expect_equal(fix_region(ss), ss, ignore_attr = TRUE)
  expect_error(fix_region('Fct'), totalerr, fixed = TRUE)
  expect_error(fix_region('Kane'), totalerr, fixed = TRUE)
  expect_error(fix_region('plateau'), totalerr, fixed = TRUE)
  expect_identical(fix_region(c(fctup, fctlw)), rep(fctup, 2))
  expect_identical(fix_region(states(c(fctup, fctlw)))[2], states(fctup))
  expect_identical(fix_region(states(fctlw, "Kano"))[1], states(fctlw))
  # expect_identical(fix_region('FCT'), fedcap)
  # expect_identical(fix_region(states('Fct')), states(fedcap))
  expect_identical(fix_region(states('Kane')), states("Kano"))
  expect_identical(fix_region(states('plateau')), states('Plateau'))
  expect_error(fix_region(states(c("Owerri north", "Owerri West"))))
  
  fixed2 <- suppressMessages(fix_region(ss2))
  expect_identical(fixed2, states(c("Oyo", "Lagos")))
  expect_length(fixed2, 2L)
  expect_error(fix_region(ssx))
  expect_error(fix_region(ss.us))
})




test_that("Misspelt LGAs can be fixed (limited)", {
  dt <- readRDS("data/taraba-lga.rds")
  tar.lgas <- lgas(dt, warn = FALSE)
  fixed <- suppressMessages(fix_region(tar.lgas))
  
  expect_length(attr(fixed, "misspelt"), 0L)
  
  expect_equal(
    fix_region(lgas(c("Amuwo Odofin", "Lagos Island"), warn = FALSE)), 
    c("Amuwo-Odofin", "Lagos Island"),
    ignore_attr = TRUE
  )
  
})



test_that("outputs", {
  lgs <- c("Fufore", "Demsa")
  
  expect_invisible(fix_region(lgs))
  expect_warning(fix_region(lgs, interactive = FALSE, graphic = TRUE),
                 "'graphic' was reset to FALSE in non-interactive mode")
  expect_silent(fix_region(lgs, quietly = TRUE))
})




test_that("regions can be fixed manually", {
  bs <- states(c("Oyo", "Lagos", "Abya"), warn = FALSE)
  bl <- lgas(c("Damboo", "Biu", "Hawl", "Shank", "Damboe"), warn = FALSE)
  lag <-  "Lagos"
  errTyp <- "The operation cannot be done on objects of type"
  output2 <- lgas(c("Damboo", "Biu", "Hawul", "Shani", "Damboe"), warn = FALSE)
  wrong2 <- c("Hawl", "Shank")
  
  expect_error(fix_region_manual(bs, lag, "Legos"),
               "'Legos' is not a valid region")
  for (elem in list(999L, NULL, NA, TRUE, pi))
    expect_error(fix_region_manual(elem, lag, "Lagos"), errTyp)
  # expect_error(fix_region_manual("TRUE", lag, "Lagos"))
  expect_identical(fix_region_manual(bs, "Abya", "Abia"),
                   states(c("Oyo", "Lagos", "Abia")))
  expect_identical(fix_region_manual(bl, c("Damboo", "Damboe"), "Damboa"),
                   lgas(c("Damboa", "Biu", "Hawl", "Shank", "Damboa"), 
                        warn = FALSE))
  expect_identical(fix_region_manual(bl, wrong2, c("Hawul", "Shani")), output2)
  expect_warning(fix_region_manual(bl, wrong2, c("Hawul", "FakeLG")), 
               "'FakeLG' is not a valid region")
  expect_error(
    fix_region_manual(
      bl, 
      c("Hawl", "shank"),         # used the wrong case in element #2
      c("Hawul", "FakeLG")),
    regexp = "'shank' is not an element of 'bl'"
  )   
  expect_error(
    fix_region_manual(bl, "Hawl", c("Hawul", "FakeLG")),
    "Substitutions must be single or the same number as targetted fixes"
  )
})


test_that("No warning when constructors are nested with fix_* functions", {
  expect_error(fix_region(states('Fct')))
  expect_warning(states("Fct"))
  expect_no_warning(fix_region(lgas(c("Legos Island",  "Amuwo-Odofin"))))
  expect_warning(lgas(c("Legos Island",  "Amuwo-Odofin")))
})
