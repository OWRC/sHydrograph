
fluidPage(
  title = 'sHydrograph hi-low summary',
  fluidRow(
    htmlOutput("hdr.hl")
  ), hr(),
  fluidRow(
    sidebarPanel(
      dateRangeInput("hl.rng", label = "Choose date range"),
      radioButtons("radio.hl", "Choose data type:",choices=c("dummy")),
      width=3
    ),
    mainPanel(
      plotOutput("plt.hl.all"), hr(),
      plotOutput("plt.hl.dens"), br(),
      shiny::includeMarkdown("md/rightclick.md") 
    )
  )
)