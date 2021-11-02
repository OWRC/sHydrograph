fluidPage(
  title = 'sHydrograph',
  fluidRow(
    htmlOutput("hdr.raw")
  ), hr(),
  fluidRow(
    sidebarPanel(
      # selectInput("cmbData", "Choose data type:", choices=NULL),
      checkboxGroupInput("chkData", "Choose data type:", choices=NULL),
      hr(),
      checkboxInput("chkScrn", "show screen elevations", value=FALSE),   
      hr(),
      h4("Selected data range:"),
      dateRangeInput("dt.rng",label=NULL), #,label='selected data range:'),
      htmlOutput('info.main'), br(),
      shiny::includeMarkdown("md/notes.md"),
      width = 2
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Viewer", br(), dygraphOutput("plt.raw")),
                  tabPanel("Printable", br(),
                           fluidRow(plotOutput("plt.print")), 
                           fluidRow(shiny::includeMarkdown("md/rightclick.md"))
                  ),
                  tabPanel("Disclaimer", shiny::includeMarkdown("md/disclaimer.md"))
      ),
      width = 10
    )
  )
)
