fluidPage(
  title = 'sHydrograph distribution',
  fluidRow(
    htmlOutput("hdr.distr.m")
  ), hr(),
  fluidRow(
    sidebarPanel(
      radioButtons("radio.distr.m", "Choose data type:",choices=c("dummy")),
      width=3
    ),
    mainPanel(
      plotOutput('distr.m.h', height='600px'), br(),
      shiny::includeMarkdown("md/rightclick.md"),
      width=9
    )
  )
)