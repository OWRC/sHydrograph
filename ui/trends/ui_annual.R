
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
        label = "Choose interval:",
        choices = NULL,
        options = list(
          `actions-box` = TRUE,
          size = 10,
          `selected-text-format` = "count > 3"
        ),
        multiple = TRUE
      ),
      checkboxGroupInput("chk.an", "Choose data type:", choices=NULL), br(),
      shiny::includeMarkdown("md/notes.md"),
      width=3
    ),
    mainPanel(
      plotOutput("plt.an.tot"), hr(),
      plotOutput("plt.an.diff"), br(),
      shiny::includeMarkdown("md/rightclick.md") 
    )
  )
)