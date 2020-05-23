
fluidPage(
  title = 'sHydrograph annual summary',
  fluidRow(
    htmlOutput("hdr.an")
  ), hr(),
  fluidRow(
    sidebarPanel(
      dateRangeInput("an.rng", label = "Choose date range"),
      radioButtons("radio.an", "Choose data type:",choices=c("dummy")),
      width=3
    ),
    mainPanel(
      plotOutput("plt.an.tot"), hr(),
      plotOutput("plt.an.diff"), br(),
      shiny::includeMarkdown("md/rightclick.md") 
    )
  )
)