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

    ## National
    mod_ts_ita_server("ts_nat_cum", "cum")
    mod_ts_ita_server("ts_nat_inc", "inc")

    ## Regional
    mod_ts_reg_server("ts_reg_cum_mes", "cum", color_var = "measure")
    mod_ts_reg_server("ts_reg_inc_mes", "inc", color_var = "measure")
    mod_ts_reg_server("ts_reg_cum_reg", "cum", color_var = "region")
    mod_ts_reg_server("ts_reg_inc_reg", "inc", color_var = "region")

    ## Provincial
    mod_ts_prv_server("ts_prv_cum", "cum")
    mod_ts_prv_server("ts_prv_inc", "inc")


    ## In evidenza
    mod_focus_20200314_server("dapb")
}
