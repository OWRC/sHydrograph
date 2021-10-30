fluidPage(
  title = 'sHydrograph distribution',  
  fluidRow(
    htmlOutput("hdr.distr.gam")
  ), hr(),
  fluidRow(
    sidebarPanel(
      radioButtons("radio.distr.gam", "Choose data type:",choices=c("dummy")),
      numericInput('distr.gam.k','smoothing (basis dimension of the spline-k)',12,min=0,max=3000),
      # checkboxInput('distr.gam.pnts', 'categorize by year', value=TRUE),
      checkboxInput('distr.gam.norm', 'normalize/de-trend'),
      width=3
    ),
    mainPanel(
      # tabsetPanel(type = "tabs",
      #             tabPanel("Rigorous approach", plotOutput('distr.gam', height='600px')),
      #             tabPanel("Naive approach", plotOutput('distr.naive', height='600px'))
      # ),
      plotOutput('distr.gam', height='600px'),
      # textOutput("distr.gam.out"), 
      # plotOutput("distr.gam.hl", height='300px'), 
      br(),
      shiny::includeMarkdown("md/rightclick.md"),
      shiny::includeMarkdown("md/gam.md"),
      width=9
    )
  )
)