# DPC CoViD-19 Ita ------------------------------------------------

## to update it:
## source(here::here("data-raw", "data-dpc_covid19_ita.R"))
##

# data to update --------------------------------------------------

data_dpc()



# data to include -------------------------------------------------


dpc_covid19_ita_andamento_nazionale <- readr::read_rds(
  here::here("data-raw/dpc_covid19_ita_andamento_nazionale.rds")
)
dpc_covid19_ita_regioni <- readr::read_rds(
  here::here("data-raw/dpc_covid19_ita_regioni.rds")
)
dpc_covid19_ita_province <- readr::read_rds(
  here::here("data-raw/dpc_covid19_ita_province.rds")
)


# Save/Update all -------------------------------------------------


usethis::use_data(
  dpc_covid19_ita_andamento_nazionale,
  dpc_covid19_ita_regioni,
  dpc_covid19_ita_province,
  overwrite = TRUE
)
