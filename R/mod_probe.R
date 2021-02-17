#' probe UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @rdname mod_probe
#' 
#' @keywords internal
#' @export
#'
#' @importFrom shiny NS tagList 
mod_probe_ui <- function(id){
  ns <- NS(id)
  tagList(
  h3(em(strong("Select your type"))),
  helpText("bioc ( from bioconductor)"),
  helpText("soft (from GPL soft file)"),
  helpText("pipe (annotate by our pipeline )"),
  helpText("source of probe anntation stored, one of 'pipe', 'bioc', 'soft'"),
  #textInput("type","","bioc"),
  selectInput(inputId = ns("type"),
              label = "Select a type:",
              choices = c('pipe', 'bioc', 'soft'),
              selected = 'bioc'),
  # helpText("choose human or mouse, or rat, default: human"),
  # selectInput(inputId = ns("species"),
  #             label = "Choose species:",
  #             choices = c('human', 'mouse', 'rat'),
  #             selected = 'human'),
  actionButton(ns("anno"),"Start Probe Annotation"),
  h3(em(strong("Preview Probe Anntation"))),
  #dataTableOutput("preview3") %>% withSpinner(type = 6) %>% hidden()
  div(id = ns('show4'), shinycssloaders::withSpinner((DT::dataTableOutput(ns("preview3"))),type = 6)) %>% shinyjs::hidden(),
  #reactable::reactableOutput(ns("preview3")),
  downloadLink(ns("downloadpreview3"), "Download Table")
  )
}
    
#' probe Server Functions
#'
#' @rdname mod_probe
#' 
#' @keywords internal
#' @export
#' @importFrom utils write.csv
#' @importFrom AnnoProbe idmap
#' @importFrom AnnoProbe annoGene
mod_probe_server <- function(id, dataSet=""){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    GPL <- reactive({
      eSet=dataSet$eSet()
      eSet@annotation
    })
    
    species <- reactive({
      eSet=dataSet$eSet()
      species <- eSet@phenoData@data[["organism_ch1"]]
      a <- unique(species)
      if (a == "Homo sapiens") {
        paste0("human")
      } else if (a == "Mus musculus") {
        paste0("mouse") 
      } else if (a == "Rattus norvegicus") {
        paste0("rat")
      }
    })
    
    # annotate
    probes_anno <- reactive({
      GPL <- GPL()
      idmap(GPL,type = input$type)
    })
    
    # observe({
    #   probes_anno <- probes_anno()
    #   save(probes_anno,file = "probes_anno.Rdata")
    # })
    
    observeEvent(input$anno, {
      shinyjs::show("show4")
      # merge more info
      t2 <- reactive({
        #library(AnnoProbe)
        probes_anno <- probes_anno()
        species <- species()
        tmp=AnnoProbe::annoGene(probes_anno$symbol,'SYMBOL',species)
        t2=merge(tmp,probes_anno,by.y='symbol',by.x='SYMBOL')
        t2$NCBI <- ncbiLink(t2$SYMBOL,t2$SYMBOL)
        t2$GeneCards <- geneCardsLink(t2$SYMBOL,t2$SYMBOL)
        t2$ENSEMBL <- ensemblLink(t2$ENSEMBL,t2$ENSEMBL)
        t2[,-match("SYMBOL",colnames(t2))]
      })
      # symbol  链接到genecard数据库
      # preview data
      output$preview3 <- DT::renderDataTable({
        #output$preview3 <- reactable::renderReactable({
        DT::datatable( t2(), escape = FALSE,
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
    
    
    
    output$downloadpreview3 <- downloadHandler(
      filename = function() {
        paste("probes-data-", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        write.csv(t2(), file)
    })
    
    return(
      list(
        probes_anno = reactive({
          probes_anno()
        })))

    
  })
}
    
## To be copied in the UI
# mod_probe_ui("probe_ui_1")
    
## To be copied in the server
# mod_probe_server("probe_ui_1")
