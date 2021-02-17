#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic 
  mod_home_server("home_ui_1")
  dataSet <- mod_download_server("download_ui_1")
  group_list <- mod_pdata_server("pdata_ui_1",dataSet=dataSet)
  probes <- mod_probe_server("probe_ui_1",dataSet=dataSet)
  filter <- mod_filter_server("filter_ui_1", dataSet=dataSet, probes=probes)
  normal <- mod_normal_server("normal_ui_1",group_list=group_list,filter=filter)
  mod_heatmap_server("heatmap_ui_1",group_list=group_list,filter=filter,normal=normal)
  mod_release_server("release_ui_1")
}
