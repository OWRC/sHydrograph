fluidPage(
  title = 'sHydrograph distribution',
  fluidRow(
    htmlOutput("hdr.distr.m")
  ), hr(),
  fluidRow(
    sidebarPanel(
      selectInput("int.distr.m", "Choose interval:", choices=NULL),
      radioButtons("radio.distr.m", "Choose data type:",choices=c("dummy")), br(),
      shiny::includeMarkdown("md/notes.md"),
      width=3
    ),
    mainPanel(
      plotOutput('distr.m.h', height='600px'), br(),
      shiny::includeMarkdown("md/rightclick.md"),
      width=9
    )
  )
)