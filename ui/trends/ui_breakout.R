
fluidPage(
  title = 'Seasonal summary',
  fluidRow(
    htmlOutput("hdr.bko")
  ), hr(),
  fluidRow(
    sidebarPanel(
      selectInput("int.bko", "Choose interval:", choices=NULL),
      radioButtons("radio.bko", "Choose data type:",choices=c("dummy")), br(),
      shiny::includeMarkdown("md/rightclick.md"), br(),
      shiny::includeMarkdown("md/notes.md"),
      width=3
    ),
    mainPanel(
      plotOutput("plt.bko", height='650px'), hr(),
      shiny::includeMarkdown("md/breakout.md") 
    )
  )
)