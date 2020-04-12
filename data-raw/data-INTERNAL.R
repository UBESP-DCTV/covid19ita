read_data_raw <- function(name) {
  readr::read_rds(
    here::here("data-raw", paste0(name, ".rds"))
  )
}




# DPC CoViD-19 Ita ------------------------------------------------

## to update it:
## source(here::here("data-raw", "data-dpc_covid19_ita.R"))
## data_dpc()
dpc_covid19_ita_andamento_nazionale <- read_data_raw(
  "dpc_covid19_ita_andamento_nazionale"
)
dpc_covid19_ita_regioni <- read_data_raw("dpc_covid19_ita_regioni")
dpc_covid19_ita_province <- read_data_raw("dpc_covid19_ita_province")
last_data_update <-read_data_raw("last_data_update")



# population data -------------------------------------------------

## data_population()
region_population <- read_data_raw("region_population")
residenti_anpr_1084 <- read_data_raw("residenti_anpr_1084")
province_population2019 <- read_data_raw("province_population2019")
province_polygons2019 <- read_data_raw("province_polygons2019")


# plotly help -----------------------------------------------------

## data_plotly_help()
eng_plottly_help_txt <- read_data_raw("eng_plottly_help_txt")
plottly_help_txt <- read_data_raw("plottly_help_txt")


# maps ------------------------------------------------------------

## data_maps()
paletteList.t <- read_data_raw("paletteList.t")


# mortalità -------------------------------------------------------

## data_mortalita()
decessi_genere <- read_data_raw("decessi_genere")
decessi_eta <- read_data_raw("decessi_eta")
decessi_eta_maschi <- read_data_raw("decessi_eta_maschi")
decessi_eta_femmine <- read_data_raw("decessi_eta_femmine")

mort_data_reg_age <- read_data_raw("mort_data_reg_age")
mort_data_reg_sex <- read_data_raw("mort_data_reg_sex")
mort_data_veneto_age <- read_data_raw("mort_data_veneto_age")
mort_data_veneto_sex <- read_data_raw("mort_data_veneto_sex")

comuni_settimana <- read_data_raw("comuni_settimana")
mort_data_comuni <- read_data_raw("mort_data_comuni")




# dictionary ------------------------------------------------------

## data_dictionary()
dictionary <- read_data_raw("dictionary")



# Save/Update all -------------------------------------------------


usethis::use_data(
  # dpc
  dpc_covid19_ita_andamento_nazionale,
  dpc_covid19_ita_regioni,
  dpc_covid19_ita_province,
  last_data_update,

  # population
  region_population,
  residenti_anpr_1084,
  province_population2019,
  province_polygons2019,


  # plotly help
  eng_plottly_help_txt,
  plottly_help_txt,

  # maps
  paletteList.t,


  # mortalità
  decessi_genere,
  decessi_eta,
  decessi_eta_maschi,
  decessi_eta_femmine,
  mort_data_reg_age,
  mort_data_reg_sex,
  mort_data_veneto_age,
  mort_data_veneto_sex,
  mort_data_comuni,


  # dictionary
  dictionary,


  internal = TRUE,
  overwrite = TRUE
)
