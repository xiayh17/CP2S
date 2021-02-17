#' normal UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @rdname mod_normal
#' 
#' @keywords internal
#' @export
#'
#' @importFrom shiny NS tagList 
mod_normal_ui <- function(id){
  ns <- NS(id)
  tagList(
    h3(em(strong("DEG with limma"))),
    actionButton(ns("norm"),"Start DEG Analysis"),
    #dataTableOutput("preview5")  %>% withSpinner(type = 6) %>% hidden()
    div(id = ns('show5'), shinycssloaders::withSpinner((DT::dataTableOutput(ns("preview5"))),type = 6)) %>% shinyjs::hidden(),
    downloadButton(ns("downloadpreview5"), "Download Table")
  )
}
    
#' normal Server Functions
#'
#' @rdname mod_normal
#' 
#' @keywords internal
#' @export
#' @importFrom stats model.matrix
#' @importFrom utils write.csv
mod_normal_server <- function(id, group_list="", filter=""){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    deg <- reactive({
      group_list <- group_list$group_list()
      genes_expr <- filter$genes_expr()
      design <- model.matrix(~factor(group_list))
      fit=limma::lmFit(genes_expr,design)
      fit=limma::eBayes(fit)
      limma::topTable(fit,coef=2,n=Inf)
    })
    
    # observe({
    #   deg <- deg()
    #   save(deg,file = "deg.Rdata")
    # })
    # preview after press button
    observeEvent(input$norm, {
      shinyjs::show("show5")
      output$preview5 <- DT::renderDataTable({
        DT::datatable( deg(), 
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
    })
    #download the expression of assay data
    output$downloadpreview5 <- downloadHandler(
      filename = function() {
        paste("DEG-data-", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        write.csv(deg(), file)
      }
    )
    
    return(
      list(
        deg = reactive({
          deg()
        })))
  })
}
    
## To be copied in the UI
# mod_normal_ui("normal_ui_1")
    
## To be copied in the server
# mod_normal_server("normal_ui_1")
