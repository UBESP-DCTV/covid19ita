#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
eng_app_server <- function(input, output, session) {
  # List the first level callModules here

    ## Header info
    mod_img_header_server("logo_testa", "Covid19.png")
    mod_img_header_server("logo_coda_torino", "Torino.png")
    mod_img_header_server("logo_coda_novara", "Novara.png")
    mod_img_header_server("logo_coda", "Covid19.png")
    eng_mod_info_sidebar_server("summary_today")

    ## Impact
    eng_mod_ind_ita_server("20200315")

    ## plottply help
    eng_mod_help_plot_server("help")

    ## National
    eng_mod_ts_ita_server("ts_nat_cum", "cum")
    eng_mod_ts_ita_server("ts_nat_inc", "inc")

    ## Regional
    eng_mod_ts_reg_server("ts_reg_cum_mes", "cum", color_var = "measure")
    eng_mod_ts_reg_server("ts_reg_inc_mes", "inc", color_var = "measure")
    eng_mod_ts_reg_server("ts_reg_cum_reg", "cum", color_var = "region")
    eng_mod_ts_reg_server("ts_reg_inc_reg", "inc", color_var = "region")

    ## Provincial
    eng_mod_ts_prv_server("ts_prv_cum", "cum")
    eng_mod_ts_prv_server("ts_prv_inc", "inc")


    ## In evidenza
    eng_mod_focus_20200314_server("dapb")
    eng_mod_focus_20200318_friuli_server("20200318_fvg")
    eng_mod_focus_20200318_piemonte_server("20200318_piemonte")
    eng_mod_focus_20200318_veneto_intensive_server("21")
    eng_mod_focus_20200320_novara_server("da_novara")
    eng_mod_focus_20200320_novara_server("da_vercelli",
                                     loc = "Vercelli",
                                     pop = 174904
    )
    eng_mod_focus_20200320_novara_server("da_alessandria",
                                     loc = "Alessandria",
                                     pop = 428826
    )
    eng_mod_focus_20200323_picco_server("picco")
    eng_mod_focus_20200325_hosp_server("hosp")
    eng_mod_focus_20200328_hosp_server("tot")
    eng_mod_focus_20200331_server("ven_pie")
}
