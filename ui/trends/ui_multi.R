
fluidPage(
  title = 'Multivariate summary',
  fluidRow(
    htmlOutput("hdr.mv")
  ), hr(),
  fluidRow(
    sidebarPanel(
      dateRangeInput("mv.rng", label = "Choose date range"),
      selectInput("int.mv", "Choose interval:", choices=NULL),
      radioButtons("radio.mv", "Choose data type:",choices=c("under construction")), br(),
      shiny::includeMarkdown("md/notes.md"),
      width=3
    ),
    mainPanel(
      h3("Site under construction")
      # plotOutput("plt.mv.all"), hr(),
      # plotOutput("plt.mv.dens"), br(),
      # shiny::includeMarkdown("md/rightclick.md") 
    )
  )
)