
fluidPage(
  title = 'Annual summary',
  fluidRow(
    htmlOutput("hdr.an")
  ), hr(),
  fluidRow(
    sidebarPanel(
      dateRangeInput("an.rng", label = "Choose date range"),
      selectInput("int.an", "Choose interval:", choices=NULL),
      radioButtons("radio.an", "Choose data type:",choices=c("dummy")), br(),
      shiny::includeMarkdown("md/notes.md"),
      width=3
    ),
    mainPanel(
      plotOutput("plt.an.tot"), hr(),
      plotOutput("plt.an.diff"), br(),
      shiny::includeMarkdown("md/rightclick.md") 
    )
  )
)