#' heatmap UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @rdname mod_heatmap
#' 
#' @keywords internal
#' @export
#'
#' @importFrom shiny NS tagList 
mod_heatmap_ui <- function(id){
  ns <- NS(id)
  tagList(
    h3(em(strong("Plot Heatmap"))),
    actionButton(ns("ph"),"Plot Heatmap"),
    #plotOutput(ns("hplot")),
    div(id = ns('show6'), shinycssloaders::withSpinner((plotOutput(ns("hplot"))),type = 6)) %>% shinyjs::hidden(),
    h3(em(strong("Plot Volcano"))),
    br(),
    #textInput(ns("style"),"Style of Volcano",1),
    selectInput(inputId = ns("style"),
                label = "Style of Volcano:",
                choices = c(1, 2),
                selected = 1),
    br(),
    textInput(ns("p_thred"),"P Thred",0.05),
    textInput(ns("logFC_thred"),"logFC Thred",1),
    actionButton(ns("pv"),"Plot Volcano"),
    #plotOutput("vplot")  %>% withSpinner(type = 6) %>% hidden()
    div(id = ns('show7'), shinycssloaders::withSpinner((plotOutput(ns("vplot"))),type = 6)) %>% shinyjs::hidden()
  )
}
    
#' heatmap Server Functions
#'
#' @rdname mod_heatmap
#' 
#' @keywords internal
#' @export
#' @importFrom AnnoProbe deg_heatmap
#' @importFrom AnnoProbe deg_volcano
mod_heatmap_server <- function(id,normal="",group_list="",filter=""){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
     # heatmap
    # normal <- mod_normal_server("normal_ui_1")
    # pdata <- mod_pdata_server("pdata_ui_1")
    # filter <- mod_filter_server("filter_ui_1")
    observeEvent(input$ph,{
      shinyjs::show("show6")
      output$hplot <- renderPlot({
        #library(AnnoProbe)
        #load("deg.Rdata")
        DEG <- normal$deg()
        genes_expr <- filter$genes_expr()
        group_list <- group_list$group_list()
        print(deg_heatmap(DEG, genes_expr, group_list, topn = 20))
      })
    })


    need_deg <- reactive({
      #load("deg.Rdata")
      DEG <- normal$deg()
      data.frame(symbols=rownames(DEG), logFC=DEG$logFC, p=DEG$P.Value)
    })

    observeEvent(input$pv,{
      shinyjs::show("show7")
      output$vplot <- renderPlot({
        #library(AnnoProbe)
        need_deg <- need_deg()
        st <- as.numeric(input$style)
        p <- as.numeric(input$p_thred)
        logFC <- as.numeric(input$logFC_thred)
        deg_volcano(need_deg, style = st, p_thred = p, logFC_thred = logFC)
      })
    })
    
  })
}
    
## To be copied in the UI
# mod_heatmap_ui("heatmap_ui_1")
    
## To be copied in the server
# mod_heatmap_server("heatmap_ui_1")
