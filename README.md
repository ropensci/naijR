
<!-- README.md is generated from README.Rmd. Please edit that file -->

# naijR

An R package on Nigeria and for Nigeria

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/DevSolutionsLtd/naijR.svg?branch=master)](https://travis-ci.org/DevSolutionsLtd/naijR)
<!-- badges: end -->

The goal of naijR is to make it easier for people R users to work with
data related to R, as well as to draw the attention of Nigerian workers
to R’s superb facilities for data analysis and modelling.

## Installation

<!-- You can install the released version of naijR from [CRAN](https://CRAN.R-project.org) with: -->

<!-- ``` r -->

<!-- install.packages("naijR") -->

<!-- ``` -->

The development version of this package can be downloaded and installed
from [GitHub](https://github.com/) with:

``` r
# If necessary, first do 'install.packages("devtools")'
devtools::install_github("DevSolutionsLtd/naijR")
```

## Example

This is a basic example that shows how to very quickly fetch the names
of Local Government Areas within a given State:

``` r
library(naijR, quietly = TRUE)
lgas_ng("Imo")
#>  [1] "Aboh Mbaise"      "Ahiazu Mbaise"    "Ehime Mbano"     
#>  [4] "Ezinihitte"       "Ideato North"     "Ideato South"    
#>  [7] "Ihitte/Uboma"     "Ikeduru"          "Isiala Mbano"    
#> [10] "Isu"              "Mbaitoli"         "Ngor Okpala"     
#> [13] "Njaba"            "Nkwerre"          "Nwangele"        
#> [16] "Obowo"            "Oguta"            "Ohaji/Egbema"    
#> [19] "Okigwe"           "Orlu"             "Orsu"            
#> [22] "Oru East"         "Oru West"         "Owerri Municipal"
#> [25] "Owerri North"     "Owerri West"      "Unuimo"
```
