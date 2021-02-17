#' release UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_release_ui <- function(id){
  ns <- NS(id)
  tagList(
    includeMarkdown(app_sys("app/www/releases.md"))
  )
}
    
#' release Server Functions
#'
#' @noRd 
mod_release_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
  })
}
    
## To be copied in the UI
# mod_release_ui("release_ui_1")
    
## To be copied in the server
# mod_release_server("release_ui_1")
