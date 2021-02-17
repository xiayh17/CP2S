#' pdata UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @rdname mod_pdata
#' 
#' @keywords internal
#' @export
#'
#' @importFrom shiny NS tagList 
mod_pdata_ui <- function(id){
  ns <- NS(id)
  tagList(
    h3(em(strong("Group of Clinic infomation"))),
    textInput(ns("gc"),"Which colunm to Group","title"),
    # textInput(ns("g1"),"Group1 keywords","Control"),
    # textInput(ns("g2"),"Group2 Keywords","Diabetes"),
    helpText("Please input your key words for group operation"),
    textInput(ns("g"),"Split Your Group Keywords by ;","Control;Diabetes"),
    actionButton(ns("applyGroup"),"Access Group"),
    textOutput(ns("test")),
    helpText("The robot point out Group of Clinic infomation below"),
    #tableOutput("group") %>% withSpinner(type = 6) %>% hidden(),
    div(id = ns('show3'), shinycssloaders::withSpinner((tableOutput(ns("group"))),type = 6)) %>% shinyjs::hidden(),
    h3(em(strong("Preview Clinic infomation"))),
    helpText("Preview phenotypic data after download"),
    DT::dataTableOutput(ns("preview2")) %>% shinycssloaders::withSpinner(type = 6),
    downloadLink(ns("downloadpreview2"), "Download Table")
  )
}
    
#' pdata Server Functions
#'
#' @rdname mod_pdata
#' 
#' @keywords internal
#' @export
#' @importFrom utils write.csv
#' @importFrom Biobase pData
mod_pdata_server <- function(id,dataSet=""){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    #download <- mod_download_server("download_ui_1")
  phenoDat <- reactive({
    eSet <- dataSet$eSet()
    pData(eSet)
  })
  
  tar <- reactive({
    ncol(phenoDat())
  })
  
  mat <- reactive({
    match(c("title", "geo_accession", "source_name_ch1"), colnames(phenoDat()))
  })
  # preview phenotypic
  output$preview2 <- DT::renderDataTable({
    DT::datatable( phenoDat(), 
               rownames = TRUE, 
               extensions = 'Buttons', 
               options=list(
                 pageLength = 30,
                 #dom = 'Bfrtip',
                 columnDefs = list(list(
                   targets = c(0:(tar()-1))[-mat()], visible = FALSE
                 )),
                 buttons = list(list(extend ='collection',
                                     buttons =  c('csv', 'excel', 'pdf'),text = 'Download View'
                 ),
                 list(extend = 'colvis')),
                 scrollX = TRUE,
                 scrollY = TRUE,
                 fixedColumns = TRUE,
                 fixedHeader = TRUE
               )
    )
  })
  
  output$downloadpreview2 <- downloadHandler(
    filename = function() {
      paste("clincal-data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(phenoDat(), file)
    }
  )
  # group after press button
  group_list <- reactive({
    g <- input$g
    col <- input$gc
    data <- phenoDat()[,col]
    group_pdata(data = data,g = g)
  })
  
  output$test <- renderText({
    print(group_list())
  })
  # observe({
  #   group_list <- group_list()
  #   save(group_list,file="group_list.Rdata")
  # })
  
  observeEvent(input$applyGroup, {

    shinyjs::show("show3")
    # group table
    output$group <- renderTable({
      test <- table(group_list())
      data <- as.data.frame(test)
      colnames(data) <- c("Group","Freq")
      data
    })
  })
  
  return(
    list(
      group_list = reactive({
        group_list()
      })))
  
})
}
  

## To be copied in the UI
# mod_pdata_ui("pdata_ui_1")
    
## To be copied in the server
# mod_pdata_server("pdata_ui_1")