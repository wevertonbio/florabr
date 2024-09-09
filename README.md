
<!-- README.md is generated from README.Rmd. Please edit that file -->

# florabr <img src="man/figures/logo.png" align="right" width="20%" height="20%"/>

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/florabr)](https://CRAN.R-project.org/package=florabr)
[![downloads](https://cranlogs.r-pkg.org/badges/grand-total/florabr)](https://cranlogs.r-pkg.org:443/badges/grand-total/florabr)
[![R-CMD-check](https://github.com/wevertonbio/florabr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/wevertonbio/florabr/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/wevertonbio/florabr/branch/main/graph/badge.svg)](https://app.codecov.io/gh/wevertonbio/florabr?branch=main)
[![License](https://img.shields.io/badge/license-GPL%20(%3E=%203)-lightgrey.svg?style=flat)](http://www.gnu.org/licenses/gpl-3.0.html)
<!-- badges: end -->

[Flora e Funga do Brasil](https://floradobrasil.jbrj.gov.br/consulta/) is
the most comprehensive work to reliably document Brazilian plant, algae and
fungi diversity. It involves the work of hundreds of taxonomists, integrating
data from plant (including algae) and fungi collected in Brazil during the last
two centuries. The database contains detailed and standardized morphological
descriptions, illustrations, nomenclatural data, geographic distribution, and
keys for the identification of all native and non-native plants and fungi found
in Brazil.

The florabr package includes a collection of functions designed to retrieve,
filter and spatialize data from the Flora e Funga do Brasil dataset.

## Package citation
Trindade, W. C. (2024). florabr: An R package to explore and spatialize species distribution using Flora e Funga do Brasil. Applications in Plant Sciences, e11616. [10.1002/aps3.11616](https://doi.org/10.1002/aps3.11616)

## Overview of functions

### Download and import Flora e Funga do Brasil Dataset

- `check_version()`: Check if you have the latest version of Flora e Funga do
  Brasil dataset available.
- `get_florabr()`: Download the latest version of Flora e Funga do Brasil
  database.
- `load_florabr()`: Load Flora e Funga do Brasil database.
- `solve_discrepancies()`: Resolve discrepancies between species and
  subspecies/varieties information.

### Check names

- `check_names()`: Check species names.
- `get_binomial()`: Extract the binomial name (Genus + specific epithet)
  from a complete Scientific Name.
- `get_synonym()`: Retrieve synonyms for species.

### Select or subset species

- `get_attributes()`: Get available attributes to filter species.
- `select_by_vernacular()`: Search for taxa using vernacular names.
- `select_species()`: Select species based on its characteristics and
  distribution.
- `subset_species()`: Extract a subset of species from Flora e Funga do Brasil
  database.

### Spatialize distribution and use Flora e Funga do Brasil to flag records

- `get_pam()`: Get a presence-absence matrix of species.
- `filter_florabr()`: Identify records outside natural ranges according
  to Flora e Funga do Brasil.
- `get_spat_occ()`: Get Spatial polygons (SpatVectors) of species based
  on its distribution (States and Biomes) according to Flora e Funga do Brasil

## Installation

### Install stable version from CRAN

To install the stable version of `florabr` use:

``` r
install.packages("florabr")
```

<br>

### Install development version from GitHub

You can install the development version of florabr from
[GitHub](https://github.com/wevertonbio/florabr) with:

``` r
if(!require(devtools)){
    install.packages("devtools")
}

if(!require(florabr)){
devtools::install_github('wevertonbio/florabr')}

library(florabr)
```

Before downloading the data available in the Flora e Funga do Brasil, we need to
create a folder to save the data:

``` r
#Creating a folder in a temporary directory
#Replace 'file.path(tempdir(), "florabr")' by a path folder to be create in your computer
my_dir <- file.path(file.path(tempdir(), "florabr"))
dir.create(my_dir)
```

You can now utilize the `get_florabr` function to retrieve the most
recent version of the data:

``` r
get_florabr(output_dir = my_dir, #directory to save the data
            data_version = "latest", #get the most recent version available
            overwrite = T) #Overwrite data, if it exists
```

You also have the option to download an older, specific version of the Flora e
Funga do Brasil dataset. To explore the available versions, please refer to
[this link](https://ipt.jbrj.gov.br/jbrj/resource?r=lista_especies_flora_brasil).
For downloading a particular version, simply replace ‘latest’ with the
desired version number. For example:

``` r
get_florabr(output_dir = my_dir, #directory to save the data
            data_version = "393.385", #Version 393.385, published on 2023-07-21
            overwrite = T) #Overwrite data, if it exists
```

## Loading data

In order to use the other functions of the package, you need to load the
data into your environment. To achieve this, utilize the
`load_florabr()` function. By default, the function will automatically
search for the latest available version in your directory. However, you
have the option to specify a particular version using the *data_version*
parameter. Additionally, you can choose between two versions of the
data: the ‘short’ version (containing the 19 columns required for run
the other functions of the package) or the ‘complete’ version (with all
original 39 columns). The function imports the ‘short’ version by
default.

``` r
#Load the short version
bf <- load_florabr(data_dir = my_dir,
                   data_version = "Latest_available",
                   type = "short") #short
```

## Package website

See the package website (<https://wevertonbio.github.io/florabr/>) for
further functions explanation and vignettes.
