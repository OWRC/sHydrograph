
fluidPage(
  title = 'Seasonal summary',
  fluidRow(
    htmlOutput("hdr.se")
  ), hr(),
  fluidRow(
    sidebarPanel(
      dateRangeInput("se.rng", label = "Choose date range"),
      radioButtons("radio.se", "Choose data type:",choices=c("dummy")),
      width=3
    ),
    mainPanel(
      plotOutput("plt.se", height='600px'), br(),
      shiny::includeMarkdown("md/rightclick.md") 
    )
  )
)