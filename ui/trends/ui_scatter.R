
fluidPage(
  title = 'Scatter plot',
  fluidRow(
    htmlOutput("hdr.sc")
  ), hr(),
  fluidRow(
    sidebarPanel(
      dateRangeInput("rng.sc",label='select date range:'),
      selectInput("cmbX.sc", "Choose x-axis:", choices=NULL),
      selectInput("cmbY.sc", "Choose y-axis:", choices=NULL),
      #   htmlOutput('info.main'),
      width = 3
    ),
    mainPanel(
      plotOutput("plt.sc", height = 800,
                 dblclick = "plt.sc_dblclick",
                 brush = brushOpts(
                   id = "plt.sc_brush",
                   resetOnNew = TRUE
                 )
      ),
      width = 9
    )
  )
)
