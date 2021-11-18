tabPanel("Data table",
  sidebarPanel(
   dateRangeInput("tab.rng", label = "Select date range"),
   radioButtons("tabRad", label = "Table format",
                choices = list("original query (normalized)" = 1, "spread (re-organized)" = 2), 
                selected = 1), br(),
   downloadButton("tabCsv", "Download csv.."), br(),
   shiny::includeMarkdown("md/notes.md"),
   width=2
  ),
  mainPanel(
   dataTableOutput('tabts'),
   width=10
  )
)