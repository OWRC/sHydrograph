fluidPage(
  title = 'sHydrograph',
  fluidRow(
    htmlOutput("hdr.raw"),
    h3('daily-aggregated data shown')
  ), hr(),
  fluidRow(
    sidebarPanel(
      
      dateRangeInput("dt.rng",label='Select date range:'),
      div(
        style="display:inline-blockl; float:right",
        actionButton("raw.but.1yr", "1yr"),
        actionButton("raw.but.5yr", "5yr"),
        actionButton("raw.but.10yr", "10yr"),
        # actionButton("raw.but.20yr", "20yr"),
        actionButton("raw.but.all", "all"),br(),br()
      ),
      
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
                  tabPanel("Printable", br(),
                           fluidRow(plotOutput("plt.print", height = "600px")), 
                           fluidRow(shiny::includeMarkdown("md/rightclick.md"))
                  ),
                  tabPanel("Quick viewer", br(), 
                           fluidRow(shiny::includeMarkdown("md/rawview.md")),
                           fluidRow(dygraphOutput("plt.raw")), 
                           br(), htmlOutput('info.main'),
                           fluidRow(formattableOutput('tabsum'))
                  ),
                  tabPanel("Map", br(),
                           fluidRow(leafletOutput("main.map", height = "600px"))
                  ),
                  tabPanel("Please read", shiny::includeMarkdown("md/disclaimer.md"))
      ),
      width = 10
    )
  )
)
