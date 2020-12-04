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
      checkboxInput("chkScrn", "show screen elevations", value=TRUE),      
      htmlOutput('info.main'),
      width = 2
    ),
    mainPanel(
      dygraphOutput("plt.raw"),
      width = 10
    )
  )
)
