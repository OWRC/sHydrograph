

fluidPage(
  title = 'Cumulative distribution function',
  fluidRow(
    htmlOutput("hdr.cdf")
  ), hr(),
  fluidRow(
    sidebarPanel(
      pickerInput(
        inputId = "pck.cdf",
        label = "Choose interval:",
        choices = NULL,
        options = list(
          `actions-box` = TRUE,
          size = 10,
          `selected-text-format` = "count > 3"
        ),
        multiple = TRUE
      ),
      checkboxGroupInput("chk.cdf", "Choose data type:", choices=NULL),
      checkboxInput("chklog.cdf", "log-transformed", value=FALSE), 
      hr(),
      shiny::includeMarkdown("md/notes.md"),
      width = 3
    ),
    mainPanel(
      fluidRow(plotOutput("plt.cdf", height='600px')), 
      fluidRow(shiny::includeMarkdown("md/rightclick.md")),
      width = 9
    )
  )
)


