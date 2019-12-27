
<!-- README.md is generated from README.Rmd. Please edit that file -->

# naijR

An R package on Nigeria and for Nigeria

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/BroVic/naijR.svg?branch=master)](https://travis-ci.org/BroVic/naijR)
[![Codecov test
coverage](https://codecov.io/gh/BroVic/naijR/branch/master/graph/badge.svg)](https://codecov.io/gh/BroVic/naijR?branch=master)
<!-- badges: end -->

The goal of **naijR** is to make it easier for R users to work with data
related to Nigeria.

## Installation

The development version of this package can be downloaded and installed
from [GitHub](https://github.com/) with:

``` r
# If necessary, first do 'install.packages("remotes")'
remotes::install_github("BroVic/naijR")
```

## Examples

### States

To create a list of all the States of the Nigerian Federation, simply
call `states()`

``` r
library(naijR, quietly = TRUE)  # attach to R session
states()
#>  [1] "Benue"       "Kogi"        "Kwara"       "Nasarawa"    "Niger"      
#>  [6] "Plateau"     "Adamawa"     "Bauchi"      "Borno"       "Gombe"      
#> [11] "Taraba"      "Yobe"        "Jigawa"      "Kaduna"      "Kano"       
#> [16] "Katsina"     "Kebbi"       "Sokoto"      "Zamfara"     "Abia"       
#> [21] "Anambra"     "Ebonyi"      "Enugu"       "Imo"         "Akwa Ibom"  
#> [26] "Bayelsa"     "Cross River" "Delta"       "Edo"         "Rivers"     
#> [31] "Ekiti"       "Lagos"       "Ogun"        "Ondo"        "Osun"       
#> [36] "Oyo"
```

States from a given geo-political zone can also be selected

``` r
states(gpz = "ne")  # i.e. North-East
#> [1] "Adamawa" "Bauchi"  "Borno"   "Gombe"   "Taraba"  "Yobe"
```

For other capabilities of this function, see `?states()`

### Local Government Areas

This is a basic example that shows how to very quickly fetch the names
of Local Government Areas within a given State:

``` r
lgas_ng("Imo")
#>  [1] "Aboh Mbaise"      "Ahiazu Mbaise"    "Ehime Mbano"      "Ezinihitte"      
#>  [5] "Ideato North"     "Ideato South"     "Ihitte/Uboma"     "Ikeduru"         
#>  [9] "Isiala Mbano"     "Isu"              "Mbaitoli"         "Ngor Okpala"     
#> [13] "Njaba"            "Nkwerre"          "Nwangele"         "Obowo"           
#> [17] "Oguta"            "Ohaji/Egbema"     "Okigwe"           "Orlu"            
#> [21] "Orsu"             "Oru East"         "Oru West"         "Owerri Municipal"
#> [25] "Owerri North"     "Owerri West"      "Unuimo"
```

To list all the LGAs in Nigeria, call the same function without any
parameters:

``` r
n <- length(lgas_ng())
sprintf("Nigeria has a total of %i Local Government Areas", n)
#> [1] "Nigeria has a total of 774 Local Government Areas"
```

Want to create a function to check how many LGAs a particular State has?

``` r
how_many_lgas <- function(state) {
  require(naijR)
  stopifnot(state %in% states())
  cat(sprintf("No. of LGAs in %s State:", state),
      length(lgas_ng(state)),
      fill = TRUE)
}

how_many_lgas("Sokoto")
#> No. of LGAs in Sokoto State: 23
how_many_lgas("Ekiti")
#> No. of LGAs in Ekiti State: 16
```

## Feedback

For bug reports or feature requests, submit an
[issue](https://github.com/BroVic/naijR/issues/new).
