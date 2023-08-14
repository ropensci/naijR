# Source file: regionsint.R
#
# GPL-3 License
#
# Copyright (C) 2019-2023 Victor Ordu.

# Internal functions for regions.R

# For States ----
## Provides some uniformity in the messaging b/w States & LGAs
.warn_on_misspelling <- function(region.type) {
  region.type <- match.arg(region.type, c("state", "lga"))
  
  regionstr <- switch(
    region.type, 
    state = "a State", 
    lga = "an LGA"
  )
  
  cli::cli_warn("One or more items is not {regionstr}. Spelling error?")
}




get_all_states <- function(named = TRUE)
{
  stopifnot(exprs = {
    length(named) == 1L
    is.logical(named)
    ! is.na(named)
  })
  
  states.by.zone <- stateList()
  
  if (!named) {
    s <- sort(unlist(states.by.zone, use.names = FALSE))
    return(s)
  }
  
  names(states.by.zone) <- sub("\\.state", "", names(states.by.zone))
  states.by.zone
}



# Subsets the table of LGAs, returning a data frame 
# with rows filtered by only the given LGAs
.subset_states_by_lga <- function(l)
{
  stopifnot(is.character(l))
  with(lgas_nigeria, state[lga %in% l])
}




.list_states_by_lga <- function(l)
{
  stopifnot(all(is_lga(l)))
  ss <- lapply(l, .subset_states_by_lga)
  names(ss) <- l
  ss
}




.subset_lgas_by_state <- function(s)
{
  stopifnot(is.character(s))
  with(lgas_nigeria, lga[state %in% s])
}




.list_lgas_by_state <- function(s) {
  stopifnot(all(is_state(s)))
  ll <- lapply(s, .subset_lgas_by_state)
  names(ll) <- s
  ll
}



# For LGAs ----
# Do not warn if this function is used inside a call to `fix_region`
.is_nested_fix_dont_warn <- function() {
  check_nesting_func <- function(funcall) {
    funs <- as.list(funcall)
    any(nest.func %in% funs)
  }
  nest.func <- c("fix_region", "disambiguate_lga")
  
  ## Check to pre-empt any future removal of these functions
  if (!sum(vapply(nest.func, exists, logical(1))))  
    cli::cli_abort("The nesting function does not exist")
  
  found <- vapply(sys.calls(), check_nesting_func, logical(1))
  any(found)
}




.has_mix_of_non_lga <- function(x) {
  stopifnot(is.character(x))
  matches <- .bools_partial_lga_matches(x)
  
  if (.all_are_not_lgas(x))
    return(FALSE)
  
  sum(matches) < length(x)
}




.all_are_not_lgas <- function(x) {
  stopifnot(is.character(x))
  sum(.bools_partial_lga_matches(x)) == 0L
}




.has_misspelt_lgas <- function(x) {
  stopifnot(is.character(x))
  matches <- .bools_exact_lga_matches(x)
  
  if (.all_are_not_lgas(x))
    return(FALSE)
  
  sum(matches) < length(x)
}




.bools_exact_lga_matches <- function(x) {
  stopifnot(is.character(x))
  grepl(.lgas_regex(x), lgas())
}




.bools_partial_lga_matches <- function(x) {
  stopifnot(is.character(x))
  
  agrepl(.lgas_regex(x),
         lgas(),
         fixed = FALSE,
         max.distance = .pkgLevDistance())
}



# Sets the Levenshtein distance being used package-wide for functions that
# carry out partial matching
.pkgLevDistance <- function() {1L}



.lgas_regex <- function(x) {
  stopifnot(is.character(x))
  paste0("^", paste(x, collapse = "|"), "$")
}
