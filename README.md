
<!-- README.md is generated from README.Rmd. Please edit that file -->

# covid19ita

<img src='man/figures/logo-laims.jpg' align='right' height='120' /><img src='man/figures/logo-ubep.png' align='right' height='120' />

<!-- badges: start -->

[![R build
status](https://github.com/UBESP-DCTV/covid19ita/workflows/R-CMD-check/badge.svg)](https://github.com/UBESP-DCTV/covid19ita/actions)
[![Codecov test
coverage](https://codecov.io/gh/UBESP-DCTV/covid19ita/branch/master/graph/badge.svg)](https://codecov.io/gh/UBESP-DCTV/covid19ita?branch=master)

[![CRAN
status](https://www.r-pkg.org/badges/version/covid19ita)](https://CRAN.R-project.org/package=covid19ita)
[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)

[![CII Best
Practices](https://bestpractices.coreinfrastructure.org/projects/3892/badge)](https://bestpractices.coreinfrastructure.org/projects/3892)
<!-- badges: end -->

Lo scopo del pacchetto `{covid19ita}` è quello di fornire una
piattaforma per il monitoraggio della diffusione dell’infezione da
CoViD-19 in Italia.

The goal of `{covid19ita}` is to provide a platform for the monitoring
of COVID-19 infection diffusion in Italy.

## Installazione/Installation

È possibile installare la versione di sviluppo di `{covid19ita}` dal
sorgente su [GitHub](https://github.com/) tramite l’istruzione:

You can install the development version of `{covid19ita}` from the
source on [GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("UBESP-DCTV/covid19ita")
```

> Nota per poter utilizzare gli accessi riservati sulla propria
> installazione locale e richiesto il pacchetto `{covid19.icuve}`
> riservato all’uso esclusivo di chi ne ha diritto. Nel caso ci si
> trovasse in tale situazione e si volesse una installazione completa
> utilizzare l’istruzione seguente.
>
> ``` r
> remotes::install_github("UBESP-DCTV/covid19.icuve")
> remotes::install_github("UBESP-DCTV/covid19ita")
> ```
>
> Se autorizzati, il sistema completo verrà installato/aggiornato
> regolarmente.

## Shiny App

<img src='inst/app/www/covid19ita_screen.png' align="center"/>

**Ultimo aggiornamento/Last app update**: 2020-12-29 23:46:22.

> È possibile visitare la Shiny App `covid19ita` ospitata e operante sui
> server UBEP ([Unità di Biostatistica, Epidemiologia e Sanità
> Pubblica](https://ubesp.jimdofree.com/)) [**QUI (versione
> italiana)**](https://r-ubesp.dctv.unipd.it/shiny/covid19ita/)

È inoltre possibile eseguire una versione dell’app locale sul proprio
computer tramite:

> You can visit the `covid19ita` Shiny App hosted and running on the
> UBEP ([Unit of Biostatistics, Epidemiology, and Public
> Health](https://ubesp.jimdofree.com/)) servers [**HERE (English
> version)**](https://r-ubesp.dctv.unipd.it/shiny/covid19italy/).

You can also run a local version of the app from your computer with:

``` r
library(covid19ita)
run_app()
```

## Data

Il pacchetto `{covid19ita}` contiene le versioni R delle tre [basi di
dati ufficiali](https://github.com/pcm-dpc/COVID-19/) con dettaglio a
livello nazionale, regionale e provinciale.

The `{covid19ita}` package provide R versions of the three [official
Italian COVID-19 datasets](https://github.com/pcm-dpc/COVID-19/) at
national, regional and provincial level of detail.

**Ultimo aggiornamento dati/Last data update**: 2020-12-29 23:53:38.

È possibile accedere ai dati tramite:

Access the data with:

``` r
library(covid19ita)
data(dpc_covid19_ita_andamento_nazionale) # national level data
data(dpc_covid19_ita_regioni)             # regional level data
data(dpc_covid19_ita_andamento_province)  # provincial level data
```

È possibile accedere anche alla documentazione dei dati in uno dei
seguenti modi:

You can access to the data documentation with any of:

``` r
?dpc_covid19_ita_andamento_nazionale
?dpc_covid19_ita_regioni
?dpc_covid19_ita_province

?dpc_covid19_ita
```

## Attribuzione dei dati/Data attribution

**Licenza/License**:
[CC-BY-4.0](https://creativecommons.org/licenses/by/4.0/deed.en) -
[Visualizza licenza/View
license](https://github.com/pcm-dpc/COVID-19/blob/master/LICENSE)

**Autore dei dati/Dataset author/editor**: [Dipartimento della
Protezione Civile](http://www.protezionecivile.it/)

**Fornitore dei dati/Data provided by**: [Ministero della
Salute](http://www.salute.gov.it/)

**Dati originariamente gestiti e processati da/Original data processing
and management by**: [Dipartimento della Protezione
Civile](http://www.protezionecivile.it/)

## Richiesta di funzionalità/Feature request

Per richiedere funzionalità aggiuntive è possibile aprire un
[issue](https://github.com/UBESP-DCTV/covid19ita/issues).

If you need some more features, please open an
[issue](https://github.com/UBESP-DCTV/covid19ita/issues).

## Bug reports

Nel caso in cui si incontrasse un bug nelle funzioni del pacchetto, per
favore si riporti un [reprex](https://github.com/tidyverse/reprex)
all’interno di un
[issue](https://github.com/UBESP-DCTV/covid19ita/issues).

If you encounter a bug in the package’s functions, please file a
[reprex](https://github.com/tidyverse/reprex) (minimal reproducible
example) in a [issue](https://github.com/UBESP-DCTV/covid19ita/issues).

Se si incontra un bug nell’applicazione, aprire un
[issue](https://github.com/UBESP-DCTV/covid19ita/issues) descrivendo: -
Passaggi per riprodurre/vedere il problema - Descrizione del problema
dettagliata - Descrizione di cosa ci si sarebbe aspettato

If you encounter a bug in the app, please describe: - The steps to
reproduce/visualize the problem - The problem in detail - The behavior
you expected

## Codice di condotta/Code of Conduct

Il progetto `covid19ita` è sviluppato con un [Codice di condotta per chi
contribuisce](CODE_OF_CONDUCT.md). Dando il tuo contributo a questo
progetto, accetti di rispettarne i termini.

Please note that the `covid19ita` project is released with a
[Contributor Code of Conduct](CODE_OF_CONDUCT.md). By contributing to
this project, you agree to abide by its terms.
