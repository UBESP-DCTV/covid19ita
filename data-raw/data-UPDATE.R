library(tidyverse)
source(here::here("data-raw", "data-dpc_covid19_ita.R"), local = TRUE)
data_dpc()


source(here::here("data-raw", "data-INTERNAL.R"), local = TRUE)
source(here::here("data-raw", "data-EXPORTED.R"), local = TRUE)

rm(list = ls(all.names = TRUE))

devtools::check_man()
devtools::check()
