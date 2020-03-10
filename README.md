
<!-- README.md is generated from README.Rmd. Please edit that file -->

# covid19ita <img src='man/figures/logo.png' align="right" height="120" />

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/covid19ita)](https://CRAN.R-project.org/package=covid19ita)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/UBESP-DCTV/covid19ita?branch=master&svg=true)](https://ci.appveyor.com/project/UBESP-DCTV/covid19ita)
[![Travis Build
Status](https://travis-ci.com/UBESP-DCTV/covid19ita.svg?branch=master)](https://travis-ci.com/UBESP-DCTV/covid19ita)
[![Codecov test
coverage](https://codecov.io/gh/UBESP-DCTV/covid19ita/branch/master/graph/badge.svg)](https://codecov.io/gh/UBESP-DCTV/covid19ita?branch=master)
<!-- badges: end -->

The goal of covid19ita is to provide a platform for the monitoring of
Covid-19 infection diffusion in Italy.

## Installation

You can install the development version of `{covid19ita}` from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("UBESP-DCTV/covid19ita")
```

## Data

The package provide R versions of the three official Italian COVID-19
datasets at national, regional and provincial level of detail.

Last data update: 2020-03-10 11:10:28.

Access the data with:

``` r
library(covid19ita)
data(covid_stato)    # national level data
data(covid_regioni)  # regional level data
data(covid_province) # provincial level data
```

You can access to the data documentation with any of:

``` r
?covid_stato
?covid_regioni
?covid_province

?`covid-ita`
```

## Data attribution

License:
[CC-BY-4.0](https://creativecommons.org/licenses/by/4.0/deed.en) -
[Visualizza
licenza](https://github.com/pcm-dpc/COVID-19/blob/master/LICENSE)

Dataset author/editor: [Dipartimento della Protezione
Civile](http://www.protezionecivile.it/)

Data provided by [Ministero della Salute](http://www.salute.gov.it/)

Original data processing and management by the [Dipartimento della
Protezione Civile](http://www.protezionecivile.it/)

## Code of Conduct

Please note that the ‘covid19ita’ project is released with a
[Contributor Code of Conduct](CODE_OF_CONDUCT.md). By contributing to
this project, you agree to abide by its terms.
