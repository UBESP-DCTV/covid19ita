# DPC CoViD-19 Ita ------------------------------------------------

dpc_covid19_ita_andamento_nazionale <- covid19ita:::read_data_raw(
  "dpc_covid19_ita_andamento_nazionale"
)
dpc_covid19_ita_regioni <- covid19ita:::read_data_raw("dpc_covid19_ita_regioni")
dpc_covid19_ita_province <- covid19ita:::read_data_raw("dpc_covid19_ita_province")
last_data_update <- covid19ita:::read_data_raw("last_data_update")



# population data -------------------------------------------------

## data_population()
region_population <- covid19ita:::read_data_raw("region_population")
residenti_anpr_1084 <- covid19ita:::read_data_raw("residenti_anpr_1084")
province_population2019 <- covid19ita:::read_data_raw("province_population2019")
province_polygons2019 <- covid19ita:::read_data_raw("province_polygons2019")


# plotly help -----------------------------------------------------

## data_plotly_help()
eng_plottly_help_txt <- covid19ita:::read_data_raw("eng_plottly_help_txt")
plottly_help_txt <- covid19ita:::read_data_raw("plottly_help_txt")


# maps ------------------------------------------------------------

## data_maps()
palette_list_t <- covid19ita:::read_data_raw("palette_list_t")


# mortalità -------------------------------------------------------

## data_mortalita()
decessi_genere <- covid19ita:::read_data_raw("decessi_genere")
decessi_eta <- covid19ita:::read_data_raw("decessi_eta")
decessi_eta_maschi <- covid19ita:::read_data_raw("decessi_eta_maschi")
decessi_eta_femmine <- covid19ita:::read_data_raw("decessi_eta_femmine")

mort_data_reg_age <- covid19ita:::read_data_raw("mort_data_reg_age")
mort_data_reg_sex <- covid19ita:::read_data_raw("mort_data_reg_sex")
mort_data_veneto_age <- covid19ita:::read_data_raw("mort_data_veneto_age")
mort_data_veneto_sex <- covid19ita:::read_data_raw("mort_data_veneto_sex")

comuni_settimana <- covid19ita:::read_data_raw("comuni_settimana")
mort_data_comuni <- covid19ita:::read_data_raw("mort_data_comuni")




# dictionary ------------------------------------------------------

## data_dictionary()
dictionary <- covid19ita:::read_data_raw("dictionary")



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
  palette_list_t,


  # mortality
  decessi_genere,
  decessi_eta,
  decessi_eta_maschi,
  decessi_eta_femmine,
  mort_data_reg_age,
  mort_data_reg_sex,
  mort_data_veneto_age,
  mort_data_veneto_sex,
  mort_data_comuni,

# data-UPDATE -----------------------------------------------------



  # dictionary
  dictionary,
  internal = TRUE,
  overwrite = TRUE
)
