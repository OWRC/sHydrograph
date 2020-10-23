##########################################################
#################### sHydrograph ######################### 
######## A Shiny interface to the YPDT database. #########
##########################################################
# Well hydrograph tool
#
# By M. Marchildon
# v.0.6.1.1
# Oct 2020
##########################################################

source("pkg/packages.R", local = TRUE)


shinyApp(
  ui <- fluidPage(
    useShinyjs(),
    tags$head(includeCSS("pkg/styles.css")),
    tags$head(tags$script(HTML(jscode.mup))),
    inlineCSS(appLoad),
    
    # Loading message
    div(
      id = "loading-content",
      div(class='space300'),
      h2("Loading..."),
      div(img(src='ORMGP_logo_no_text_bw_small.png')), br(),
      shiny::img(src='loading_bar_rev.gif')
    ),

    # The main app
    hidden(
      div(
        id = "app-content",
        list(tags$head(HTML('<link rel="icon", href="favicon.png",type="image/png"/>'))),
        div(style="padding: 1px 0px; height: 0px", titlePanel(title="", windowTitle="sHydrograph")),
        navbarPage(
          title=div(img(src="ORMGP_logo_no_text_short.png", height=11), "sHydrograph v0.6.1"),
          source(file.path("ui", "ui_hydrograph.R"), local = TRUE)$value,
          source(file.path("ui", "ui_trends.R"), local = TRUE)$value,
          source(file.path("ui", "ui_stats.R"), local = TRUE)$value,
          source(file.path("ui", "ui_data_table.R"), local = TRUE)$value#,
          # source(file.path("ui", "ui_about.R"), local = TRUE)$value
        )
      )
    )
  ),
  
  server <- function(input, output, session){
    
    ###################
    ### Parameters & methods:
    source("pkg/headers.R", local = TRUE)$value
    source("pkg/collect_interval.R", local = TRUE)$value
    
    ###################
    ### Load station ID:
    ### http://shinyapps.canadacentral.cloudapp.azure.com:3838/sHydrograph/?i=
    # Here are a few to try out: 1) Cannington OW99-2D (Int ID = -1261492764); 2) Aurora MW 1 (Int ID = -373572324); 
    #                            3) NVCA - Earl Rowe (IntID = -498465806); 4) Port Perry OW 5-3 (Int ID = -224406311)
    collect_interval(-373572324) #(-373572324) #(-498465806) #(-373572324) #(-1261492764) #(148842) #(-224406311) #(-130212055) #
    # observe({
    #   query <- parseQueryString(session$clientData$url_search)
    #   # print(query)
    #   if (!is.null(query[['i']]) & !is.na(as.numeric(query[['i']]))) {
    #     # collect_interval(strtoi(query[['i']]))
    #     if ('t' %in% query) {
    #       collect_interval(strtoi(query[['i']]),strtoi(query[['t']]))
    #     } else {
    #       collect_interval(strtoi(query[['i']]))
    #     }
    #   } else {
    #     showNotification(paste0("Error: URL invalid."))
    #   }
    # })
    
    ### sources:
    source(file.path("srv/temporal", "srv_main.R"), local = TRUE)$value
    source(file.path("srv/trends", "srv_annual.R"), local = TRUE)$value
    source(file.path("srv/trends", "srv_hilow.R"), local = TRUE)$value
    source(file.path("srv/stats", "srv_extreme.R"), local = TRUE)$value
    source(file.path("srv", "srv_data_table.R"), local = TRUE)$value 

    session$onSessionEnded(stopApp)
  }
)
