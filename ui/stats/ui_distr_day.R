fluidPage(
  title = 'sHydrograph distribution',  
  fluidRow(
    htmlOutput("hdr.distr.d")
  ), hr(),
  fluidRow(
    sidebarPanel(
      radioButtons("radio.distr.d", "Choose data type:",choices=c("dummy")),
      width=3
    ),
    mainPanel(
      plotOutput('distr.d.h', height='600px'), br(),
      shiny::includeMarkdown("md/rightclick.md"),
      width=9
    )
  )
)