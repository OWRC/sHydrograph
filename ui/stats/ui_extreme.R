fluidPage(
  title = 'sHydrograph frequency of annual extremes',
  fluidRow(
    htmlOutput("hdr.ax")
  ), hr(),
  fluidRow(
    sidebarPanel(
      radioButtons("radio.ax", "Choose data type:",choices=c("dummy")), hr(),
      h5('Click "Regenerate" after making changes below'),
      selectInput('ax.mnmx', 'extreme', c('Annual maximum'='max','Annual minimum'='min')),
      selectInput('ax.freq', 'flow frequency model', c('Log Pearson III'='lp3',
                                                       'Generalized Extreme Value'='gev',
                                                       'Weibull'='wei',
                                                       'Gumbel'='gum',
                                                       'three-parameter lognormal'='ln3')),
      numericInput('ax.rsmpl','number of boostrap resamples',10000,min=1000,max=100000),
      numericInput('ax.ci','confidence interval',0.9,min=0.05,max=0.999,step=0.01),
      actionButton('ax.regen',"Regenerate"),
      width=3
    ),
    mainPanel(
      plotOutput('ax.h', height='600px'),
      column(6,
             plotOutput('ax.dist', height='200px')
      ),
      column(6,
             plotOutput('ax.hist', height='200px')
      ), br(),
      shiny::includeMarkdown("md/rightclick.md"),
      shiny::includeMarkdown("md/extreme.md"),
      width=9
    )
  )
)
