#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # List the first level callModules here

  ## Header info
  mod_img_header_server("logo_testa", "Covid19.png")
  mod_img_header_server("logo_coda_torino", "Torino.png")
  mod_img_header_server("logo_coda_novara", "Novara.png")
  mod_img_header_server("logo_coda", "Covid19.png")
  mod_info_sidebar_server("summary_today")

  ## Impact
  mod_ind_ita_server("20200315")

  ## plottply help
  mod_help_plot_server("help")

  ## ICUs VE
  mod_icuve_ts_server("icuve_ts_cl")

  ## National
  mod_ts_ita_server("ts_nat_cum", "cum")
  mod_ts_ita_server("ts_nat_inc", "inc")

  # ## Regional
  mod_ts_reg_server("ts_reg_cum_mes", "cum", color_var = "measure")
  mod_ts_reg_server("ts_reg_inc_mes", "inc", color_var = "measure")
  mod_ts_reg_server("ts_reg_cum_reg", "cum", color_var = "region")
  mod_ts_reg_server("ts_reg_inc_reg", "inc", color_var = "region")

  ## Provincial
  mod_ts_prv_server("ts_prv_cum", "cum")
  mod_ts_prv_server("ts_prv_inc", "inc")


  ## In evidenza
  mod_0314_server("dapb")
  mod_0318_friuli_server("20200318_fvg")
  mod_0318_piemonte_server("20200318_piemonte")
  mod_0318_intensive_server("21")
  mod_0320_novara_server("da_novara")
  mod_0320_novara_server("da_vercelli",
    loc = "Vercelli",
    pop = 174904
  )
  mod_0320_novara_server("da_alessandria",
    loc = "Alessandria",
    pop = 428826
  )
  mod_0323_picco_server("picco")
  mod_0325_hosp_server("hosp")
  mod_0328_hosp_server("tot")
  mod_0331_server("ven_pie")
  mod_0404_magnani_server("mortality")
  mod_0406_mort_veneto_server("mort_veneto")
  mod_0415_tamponi_server("tamp_hosp")

  ## Geo-spatial
  mod_maps_server("geo_1")
}
