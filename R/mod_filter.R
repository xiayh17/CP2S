#' filter UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @rdname mod_filter
#' 
#' @keywords internal
#' @export
#'
#' @importFrom shiny NS tagList 
mod_filter_ui <- function(id){
  ns <- NS(id)
  tagList(
    h3(em(strong("Filter expression matrix based on annotation"))),
    #actionButton("filter","Start Filter"),
    br(),
    DT::dataTableOutput(ns("preview4")) %>% shinycssloaders::withSpinner(type = 6),
    downloadLink(ns("downloadpreview4"), "Download Table")
  )
}
    
#' filter Server Functions
#'
#' @rdname mod_filter
#' 
#' @keywords internal
#' @export
#' @importFrom stats model.matrix
#' @importFrom utils write.csv
#' @importFrom AnnoProbe filterEM
mod_filter_server <- function(id, dataSet="",probes=""){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    # annotate
    genes_expr <- reactive({
      probes_expr <- dataSet$probes_expr()
      probes_anno <- probes$probes_anno()
      filterEM(probes_expr,probes_anno)
    })
    
    # observe({
    #   genes_expr <- genes_expr()
    #   save(genes_expr,file="genes_expr.Rdata")
    # })
    
    #observeEvent(input$filter, {

      # preview data
      output$preview4 <- DT::renderDataTable({
        DT::datatable( genes_expr(), 
                   rownames = TRUE, 
                   extensions = 'Buttons', 
                   options=list(
                     dom = 'Bfrtip',
                     buttons = list(list(extend ='collection',
                                         buttons =  c('csv', 'excel', 'pdf'),text = 'Download View'
                     ),
                     list(extend ='colvis',text = 'Hide Columns'
                     )),
                     scrollX = TRUE,
                     scrollY = TRUE,
                     fixedColumns = TRUE,
                     fixedHeader = TRUE
                   )
        )
        
      })
      
      output$downloadpreview4 <- downloadHandler(
        filename = function() {
          paste("genes_expr-", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
          write.csv(genes_expr(), file)
        })
      
      return(
        list(
          genes_expr = reactive({
            genes_expr()
          })))

    #})
  })
}
    
## To be copied in the UI
# mod_filter_ui("filter_ui_1")
    
## To be copied in the server
# mod_filter_server("filter_ui_1")
