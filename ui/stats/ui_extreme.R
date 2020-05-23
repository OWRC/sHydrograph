fluidPage(
  title = 'sHydrograph frequency of annual extremes',
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
    column(9, plotOutput('ax.h')),
    column(9, plotOutput('ax.dist'),
    shiny::includeMarkdown("md/rightclick.md"))
  ), br(),
  shiny::includeMarkdown("md/extreme.md")
)
