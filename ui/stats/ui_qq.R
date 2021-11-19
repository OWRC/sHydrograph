fluidPage(
  title = 'sHydrograph distribution',
  fluidRow(
    htmlOutput("hdr.qq")
  ), hr(),
  fluidRow(
    sidebarPanel(
      selectInput("int.qq", "Choose interval:", choices=NULL),
      radioButtons("radio.qq", "Choose data type:",choices=c("dummy")),
      selectInput('freq.qq', 'Choose distribution model', c('Uniform','Normal','Exponential'), selected='Normal'),
      shiny::includeMarkdown("md/notes.md"),
      htmlOutput('info.qq')
    ),
    mainPanel(
      column(6, plotOutput('distr.qq.distr')),
      column(6, plotOutput('distr.qq')), br(),
      shiny::includeMarkdown("md/rightclick.md")
    )
  )
)