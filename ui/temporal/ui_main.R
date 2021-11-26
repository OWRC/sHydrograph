fluidPage(
  title = 'sHydrograph',
  fluidRow(
    htmlOutput("hdr.raw")
  ), hr(),
  fluidRow(
    sidebarPanel(
      dateRangeInput("dt.rng",label='Select date range:'),
      pickerInput(
        inputId = "pck.raw",
        label = "Choose interval:",
        choices = NULL,
        options = list(
          `actions-box` = TRUE,
          size = 10,
          `selected-text-format` = "count > 3"
        ),
        multiple = TRUE
      ),
      # selectInput("cmbData", "Choose data type:", choices=NULL),
      checkboxGroupInput("chkData", "Choose data type:", choices=NULL),
      hr(),
      checkboxInput("chkScrn", "show screen elevations", value=FALSE),   
      # hr(),
      # h4("Selected data range:"),
      # dateRangeInput("dt.rng",label=NULL), #,label='selected data range:'),
      # htmlOutput('info.main'), 
      br(),
      shiny::includeMarkdown("md/notes.md"),
      width = 2
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Raw view", br(), 
                           fluidRow(dygraphOutput("plt.raw")),
                           br(), htmlOutput('info.main'),
                           fluidRow(formattableOutput('tabsum'))
                  ),
                  tabPanel("Faceted", br(),
                           fluidRow(plotOutput("plt.print", height = "600px")), 
                           fluidRow(shiny::includeMarkdown("md/rightclick.md"))
                  ),
                  tabPanel("Map", br(),
                           fluidRow(leafletOutput("main.map", height = "600px"))
                  ),
                  tabPanel("Disclaimer", shiny::includeMarkdown("md/disclaimer.md"))
      ),
      width = 10
    )
  )
)
