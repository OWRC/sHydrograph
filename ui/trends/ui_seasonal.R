
fluidPage(
  title = 'Seasonal summary',
  fluidRow(
    htmlOutput("hdr.se")
  ), hr(),
  fluidRow(
    sidebarPanel(
      # dateRangeInput("se.rng", label = "Choose date range"),
      selectInput("int.se", "Choose interval:", choices=NULL),
      radioButtons("radio.se", "Choose data type:",choices=c("dummy")), br(),
      shiny::includeMarkdown("md/notes.md"),
      width=3
    ),
    mainPanel(
      plotOutput("plt.se", height='600px'), br(),
      shiny::includeMarkdown("md/rightclick.md") 
    )
  )
)