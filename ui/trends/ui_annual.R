
fluidPage(
  title = 'Annual summary',
  fluidRow(
    htmlOutput("hdr.an")
  ), hr(),
  fluidRow(
    sidebarPanel(
      # dateRangeInput("an.rng", label = "Choose date range"),
      pickerInput(
        inputId = "pck.an",
        label = "1. Choose interval(s):",
        choices = NULL,
        options = list(
          `actions-box` = TRUE,
          size = 10,
          `selected-text-format` = "count > 3"
        ),
        multiple = TRUE
      ),
      selectInput('sel.an', 'Choose statistic:', c('Mean','Maximum','Median','Minimum'), selected='Mean'),
      checkboxGroupInput("chk.an", "Choose data type:", choices=NULL), br(),
      shiny::includeMarkdown("md/rightclick.md"), br(),
      shiny::includeMarkdown("md/notes.md"),
      width=3
    ),
    mainPanel(
      h4("Absolute"),
      fluidRow(plotOutput("plt.an.tot", height = "600px")), hr(),
      # plotOutput("plt.an.tot")
      h4("Realtive"),
      # plotOutput("plt.an.diff"), br(),
      fluidRow(plotOutput("plt.an.diff", height = "600px"))
      # shiny::includeMarkdown("md/rightclick.md") 
    )
  )
)