# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

**Maintainer's General Instructions:**[Source](https://x.com/svpino/status/2028545907499872690)

1. Before writing any code, describe your approach and wait for approval.
2. If the requirements I give you are ambiguous, ask clarifying questions before writing any code.
3. After you finish writing any code, list the edge cases and suggest test cases to cover them.
4. If a task requires changes to more than 3 files, stop and break it into smaller tasks first.
5. When there’s a bug, start by writing a test that reproduces it, then fix it until the test passes.
6. Every time I correct you, reflect on what you did wrong and come up with a plan to never make the same mistake again.

## Project Overview

naijR is an R package (rOpenSci-reviewed, on CRAN) providing convenience functions and geopolitical data for Nigeria. Key domains: administrative regions (States/LGAs), geospatial mapping, phone number correction, and inter-city distances.

## Common Commands

```bash
# Check package (full R CMD check)
R CMD check .

# Run all tests
Rscript -e 'testthat::test_local()'

# Run a single test file
Rscript -e 'testthat::test_file("tests/testthat/test-regions.R")'

# Build documentation (roxygen2)
Rscript -e 'roxygen2::roxygenise()'

# Lint
Rscript -e 'lintr::lint_package()'

# Install locally for testing
R CMD INSTALL .
```

Tests use testthat edition 3 (configured in DESCRIPTION).

## Architecture

### S3 Class System

The package defines two core S3 classes: `states` and `lgas`. These have custom methods for `print`, `[`, `[[`, `c`, and `na.exclude`. The `fix_region` generic dispatches on these classes plus a default method.

### Source File Organization

Files ending in `int` contain **internal** (non-exported) helper functions for the corresponding public module:

- `regions.R` / `regionsint.R` — `states()`, `lgas()`, constructors, validators, coercion (`as_state`, `as_lga`, `is_state`, `is_lga`)
- `map.R` / `mapint.R` — `map_ng()` and internal mapping helpers (uses `sf`, `maps`, `mapdata`)
- `fixreg.R` / `fixregint.R` — `fix_region()` S3 methods and `fix_region_manual()` for correcting misspelled region names
- `fixmob.R` — `fix_mobile()` for Nigerian phone number normalization
- `distances.R` — `ng_distance()` for road distances between state capitals
- `disambi.R` — `disambiguate_lga()` for States/LGAs that share names
- `fct.R` — Helpers for handling Federal Capital Territory (FCT) special cases
- `helpers.R` — Shared internal utilities
- `regdata.R` — Documentation for package datasets
- `zzz.R` — `.onLoad` hook (sets choropleth colour options)

### Data

- `data/states_nigeria.rda`, `data/lgas_nigeria.rda` — Exported datasets of States and LGAs
- `data/ngdist.rda` — Road distance matrix between state capitals
- `R/sysdata.rda` — Internal data (shapefile objects `shp.state`, `shp.lga` used by mapping)
- `data-raw/` — Source shapefiles and scripts for regenerating `.rda` files

### Key Conventions

- Uses `cli` for user-facing warnings/errors (`cli_abort`, `cli_warn`)
- Uses `rlang` for non-standard evaluation and internal error signaling
- `stringi::stri_trans_totitle` for case-insensitive name matching
- Linting uses default `lintr` rules (`.lintr` config)
- README.md is generated from README.Rmd — edit the `.Rmd` file, not the `.md`
- `R/util.R` is excluded from the package build (in `.Rbuildignore`)

### Branches

- `master` — main/release branch
- `dev` — development branch
- CI runs R-CMD-check on both `master` and `dev`
