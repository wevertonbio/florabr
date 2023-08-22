
<!-- README.md is generated from README.Rmd. Please edit that file -->

# florabr

<!-- badges: start -->
<!-- badges: end -->

[Brazilian Flora
2020](http://floradobrasil.jbrj.gov.br/reflora/listaBrasil/PrincipalUC/PrincipalUC.do?lingua=en)
is the most comprehensive work to reliably document Brazilian plant
diversity. It involves the work of hundreds of taxonomists, integrating
data from plant and fungi collected in Brazil during the last two
centuries. The database contains detailed and standardized morphological
descriptions, illustrations, nomenclatural data, geographic
distribution, and keys for the identification of all native and
non-native plants found in Brazil.

The florabR package includes a collection of functions designed to
retrieve, filter and spatialize data from the Brazilian Flora 2020
dataset.

## Installation

You can install the development version of florabr from
[GitHub](https://github.com/) with:

``` r
if(!require(devtools)){
    install.packages("devtools")
}

if(!require(florabR)){
devtools::install_github('wevertonbio/florabR')}

library(florabR)
```
