#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # remove shiny "red" warning messages on GUI
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"
    ),
    # Your application UI logic 
    fluidPage(
    #h1("CP2S"),
    # hiden
    shinyjs::useShinyjs(),
    # load page layout
    shinydashboard::dashboardPage(
    # color of dashboard
    skin = "green",
    # title and size of dashboard head
    shinydashboard::dashboardHeader(title = "Converting Probes into Symbols", titleWidth = 300),
    # sidebar of web
    shinydashboard::dashboardSidebar(width = 300,
                     shinydashboard::sidebarMenu(
                       # add link, picture and text
                       HTML(paste0(
                         "<br>",
                         "<a href='https://mp.weixin.qq.com/s/OPhXbBJQC-3gQ3dr5tLZ7Q' target='_blank'><img style = 'display: block; margin-left: auto; margin-right: auto;' src='www/log.png' width = '186'></a>",
                         "<br>",
                         "<p style = 'text-align: center;'><small><a href='https://www.github.com//xiayh17' target='_blank'>Probes2Symbol logo designer</a></small></p>",
                         "<br>"
                       )),
                       # add menu of sidebar
                       shinydashboard::menuItem("Home", tabName = "home", icon = icon("robot")),
                       shinydashboard::menuItem("Download Data", tabName = "download", icon = icon("cloud-download-alt")),
                       shinydashboard::menuItem("Clinic infomation", tabName = "pdata", icon = icon("diagnoses")),
                       shinydashboard::menuItem("Probe Annotation", tabName = "probe", icon = icon("random", lib = "glyphicon")),
                       shinydashboard::menuItem("Filter expression", tabName = "filter", icon = icon("stats", lib = "glyphicon")),
                       shinydashboard::menuItem("DEG", tabName = "normal", icon = icon("align-right")),
                       shinydashboard::menuItem("Heatmap", tabName = "heatmap", icon = icon("map marked alt")),
                       shinydashboard::menuItem("Release", tabName = "release", icon = icon("code-branch")),
                       HTML(paste0(
                         "<br><br><br><br><br><br><br><br><br>",
                         "<table style='margin-left:auto; margin-right:auto;'>",
                         "<tr>",
                         "<td style='padding: 5px;'><a href='https://space.bilibili.com/338686099' target='_blank'><i class='iconfont iconbilibili'></i></a></td>",
                         "<td style='padding: 5px;'><a href='https://www.youtube.com/channel/UC67sImqK7V8tSWHMG8azIVA' target='_blank'><i class='fab fa-youtube fa-lg'></i></a></td>",
                         "<td style='padding: 5px;'><a href='www/weixin.png' target='_blank'><i class='iconfont iconweixin'></i></a></td>",
                         "</tr>",
                         "</table>",
                         "<br>"),
                         HTML(paste0(
                           "<script>",
                           "var today = new Date();",
                           "var yyyy = today.getFullYear();",
                           "</script>",
                           "<p style = 'text-align: center;'><small>&copy; - <a href='http://www.biotrainee.com' target='_blank'>biotrainee.com</a> - <script>document.write(yyyy);</script></small></p>")
                         ))
                     )

    ), # end dashboardSidebar

    shinydashboard::dashboardBody(
      
      shinydashboard::tabItems(
        
        shinydashboard::tabItem("home", mod_home_ui("home_ui_1")),
        shinydashboard::tabItem("download", mod_download_ui("download_ui_1")),
        shinydashboard::tabItem("pdata", mod_pdata_ui("pdata_ui_1")),
        shinydashboard::tabItem("probe", mod_probe_ui("probe_ui_1")),
        shinydashboard::tabItem("filter", mod_filter_ui("filter_ui_1")),
        shinydashboard::tabItem("normal", mod_normal_ui("normal_ui_1")),
        shinydashboard::tabItem("heatmap", mod_heatmap_ui("heatmap_ui_1")),
        shinydashboard::tabItem("release", mod_release_ui("release_ui_1"))
        
          )# end tabItems
        ) # end dashboardBody
      ) # end dashboardPage
    ) # end fluidPage
  ) # end taglist
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
  # google analysis
  tags$head(includeHTML((app_sys("app/www/google-analytics.html"))))
  # icon
  #golem::use_external_css_file("style.css")
  #golem::use_external_css_file("iconfont.css")
  # favicon
  #golem::favicon(ico = "www/favicon.ico")
  tags$head(
    golem::favicon(
    ),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'CP2S'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  ) # end tags$head


}

