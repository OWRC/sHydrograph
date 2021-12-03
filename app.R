##########################################################
#################### sHydrograph ######################### 
######## A Shiny interface to the YPDT database. #########
##########################################################
# Well hydrograph tool
#
# By M. Marchildon
# v.1.2
# Nov. 2021
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
          title=div(img(src="ORMGP_logo_no_text_short.png", height=11), "sHydrograph v1.2"),
          source(file.path("ui", "ui_hydrograph.R"), local = TRUE)$value,
          source(file.path("ui", "ui_trends.R"), local = TRUE)$value,
          source(file.path("ui", "ui_stats.R"), local = TRUE)$value#,
          # source(file.path("ui", "ui_about.R"), local = TRUE)$value
        )
      )
    )
  ),
  
  server <- function(input, output, session){
    old <- Sys.time() 
    
    ###################
    ### Parameters & methods:
    source("srv/sources.R", local = TRUE)
    
    ###################
    ### Load station ID:
    # Here are a few to try out: 1) Cannington OW99-2D (Int ID = -1261492764); 2) Aurora MW 1 (Int ID = -373572324); 
    #                            3) NVCA - Earl Rowe (IntID = -498465806); 4) Port Perry OW 5-3 (Int ID = -224406311)
    # collect_interval(-847483645) #(730800020) #() #(283459923) #(40977) #(6994) #(148405,5) #(-2056054271,5) #(-224406311) #(-1261492764) #(-498465806) #(1697639961) #(1099646144) #(-373572324) #("test/-847483645.json") #(148842) #(83764) #(-1741125310,3) #
    # collect_interval_loc(-2087373503) #(148720,3) 8275
    observe({
      query <- parseQueryString(session$clientData$url_search)
      if ( !is.null(query$l) ) {
        if ( is.na(as.numeric(query$l)) ) {
          showNotification(paste0("Error: URL invalid."))
        } else {
          if ( !is.null(query$t) ) {
            if ( is.na(as.numeric(query$t)) ) {
              showNotification(paste0("Error: URL invalid."))
            } else {
              collect_interval_loc(strtoi(query$l),strtoi(query$t))
            }
          } else {
            collect_interval_loc(strtoi(query$l))
          }
        }
      } else if ( !is.null(query$i) ) {
        if ( is.na(as.numeric(query$i)) ) {
          showNotification(paste0("Error: URL invalid."))
        } else {
          if ( !is.null(query$t) ) {
            if ( is.na(as.numeric(query$t)) ) {
              showNotification(paste0("Error: URL invalid."))
            } else {
              collect_interval(strtoi(query$i),strtoi(query$t))
            }
          } else {
            collect_interval(strtoi(query$i))
          }
        }
      } else {
        showNotification(paste0("Error: URL invalid."))
      }
    })

    source("srv/observables.R", local = TRUE)$value
    print(Sys.time() - old)
    session$onSessionEnded(stopApp)
  }
)
