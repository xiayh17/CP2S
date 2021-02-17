#' download UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @rdname mod_download
#' 
#' @keywords internal
#' @export
#' @importFrom shiny NS tagList 
mod_download_ui <- function(id){
  ns <- NS(id)
  tagList(
   h3(em(strong("Input GEO Accession"))),
   br(),
   helpText("Input a GEO Accession.
             Data will be collected with geoChina"),
   textInput(ns("geoacc"), "", "GSE1009"),
   #actionButton(ns("go"), "go"),
   actionButton(ns("applyDownload"), "Start Download"),
   htmlOutput(ns("timer")),
   h3(em(strong("A preview of data after download success"))),
   br(),
   helpText("Preview expression data just downloaded"),
   htmlOutput(ns("dim")),
   br(),
   #dataTableOutput("preview1") %>% withSpinner(type = 6) %>% hidden(),
   #shinyjs::hidden(div(id = 'show1', shinycssloaders::withSpinner((dataTableOutput("preview1")),type = 6))),
   div(id = ns('show1'), shinycssloaders::withSpinner(DT::dataTableOutput(ns("preview1"))),type = 6) %>% shinyjs::hidden(),
   br(),
   downloadLink(ns("downloadpreview1"), "Download Table"),
   h3(em(strong("A boxplot check"))),
   br(),
   helpText("Quick check expression data just downloaded with boxplot"),
   checkboxInput(ns("log"), "log gene expression",
                 value = FALSE),
   actionButton(ns("applyBoxpot1"), "Box plot"),
   #plotOutput("boxplot1") %>% withSpinner(type = 6) %>% hidden()
   #shinyjs::hidden(div(id = ns('show2'), shinycssloaders::withSpinner((plotly::plotlyOutput(ns("boxplot1"))),type = 6))),
   #div(id = ns('show2'), plotly::plotlyOutput(ns("boxplot1")) %>% shinycssloaders::withSpinner(type = 6)) %>% shinyjs::hidden(),
   div(id = ns('show2'), plotOutput(ns("boxplot1")) %>% shinycssloaders::withSpinner(type = 6)) %>% shinyjs::hidden(),
   br(),
   #downloadLink("downloadboxplot1", "Download Plot"),
   br(),
   br()    
  )
}
    
#' download Server Functions
#'
#' @rdname mod_download
#' @export
#' @keywords internal
#' @importFrom utils download.file
#' @importFrom GEOmirror geoChina
#' @importFrom utils write.csv
#' @importFrom Biobase exprs

mod_download_server_core <- function(input, output, session){
  ns <- session$ns
  observeEvent(input$applyDownload, {
      shinyjs::show("show1")
      print(input$geoacc)
  })
  
  # times <- reactiveVal()
  # gset <- reactiveVal()
  
  mydat <- eventReactive(input$applyDownload, {
    gse=toupper(input$geoacc)
    tm <- system.time({
      down=ifelse(as.numeric(gsub('GSE','',gse))<1000,
                  paste0('/GSEnnn/',gse,
                         '_eSet.Rdata'),
                  paste0(
                    gsub('[0-9][0-9][0-9]$','nnn',gse),'/',gse,
                    '_eSet.Rdata'))
      
      path <- "inst/app/data/rdata"
      files <- dir(path,recursive=TRUE,include.dirs = FALSE)
      filesNames <- gsub(".*/|_eSet.Rdata","",files)
      
      if(gse %in% filesNames){
        load(file.path(path,down))
      } else {
        gset <- geoChina(gse)
      }
    })
    list(x=gset,elapsed = tm['elapsed'])
    # times(tm['elapsed'])
    # gset(gset)
  })
  
  # describe data dimension
  output$timer <- renderText({
    paste0("<font color=\"#0275D8\"><b>", round(mydat()$elapsed*1000), "</b></font>",
           " milliseconds" )
  })
  
  eSet <- reactive({
    gset <- mydat()$x
    eSet <- gset[[1]]
  })
  
  # access the expression of assay data
  probes_expr <- reactive({
    eSet <- eSet()
    #save(eSet,file = "eSet.Rdata")
    probes_expr <- exprs(eSet)
    if(input$log) {
      log(probes_expr)
    } else {
      probes_expr
    }
  })
  
  # observe({
  #   probes_expr <- probes_expr()
  #   save(probes_expr,file = 'probes_expr.Rdata')
  # })
  
  # preview data table
  output$preview1 <- DT::renderDataTable({
    DT::datatable( probes_expr(), 
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
  
  GPL <- reactive({
    eSet <- eSet()
    eSet@annotation
  })
    
  # describe data dimension
  output$dim <- renderText({
    paste("Your data have",
          "<font color=\"#0275D8\"><b>",dim(probes_expr())[1], "</b></font>",
          "rows, ",
          "<font color=\"#0275D8\"><b>",dim(probes_expr())[2], "</b></font>",
          "column and GPL is",
          "<font color=\"#0275D8\"><b>",gplLink(GPL(),GPL()), "</b></font>")
  })

  #download the expression of assay data
  output$downloadpreview1 <- downloadHandler(
    filename = function() {
      paste("exp-data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(probes_expr(), file)
    }
  )
  # boxplot1
  observeEvent(input$applyBoxpot1, {
    shinyjs::show("show2")
    probes_expr <- probes_expr()
    # boxplot -----------------------------------------------------------------
    output$boxplot1 <- renderPlot({
      par(mar = c(6,2,2,2))
      boxplot(probes_expr,las=2)
    })
    # boxplot in plotly -------------------------------------------------------
    # data <- as.data.frame(probes_expr())
    # res1 <- tidyr::gather(data) #宽转长
    # output$boxplot1 <- plotly::renderPlotly({
    #   #
    #   #library(plotly)
    #   plotly::plot_ly(res1,y=~value,x=~key,color=~key,type = "box") %>%
    #     plotly::layout(
    #      xaxis = list(title = 'Samples'),
    #      yaxis = list(title = 'Expression'))
    # })
  })
  return(
    list(
      eSet = reactive({
        eSet()
      }),
      probes_expr = reactive({
        probes_expr()
      })
    )
    )
}

mod_download_server <- function(id){
  moduleServer( 
    id, 
    mod_download_server_core
  )
}
## To be copied in the UI
# mod_download_ui("download_ui_1")
    
## To be copied in the server
# mod_download_server("download_ui_1")