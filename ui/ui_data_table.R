tabPanel("Data table",
  sidebarPanel(
   dateRangeInput("tabRng", label = "Date range"),
   radioButtons("tabRad", label = "Table format",
                choices = list("normalized" = 1, "spread" = 2), 
                selected = 1), br(),
   downloadButton("tabCsv", "Download csv.."),
   width=2
  ),
  mainPanel(
   dataTableOutput('tabts'),
   width=10
  )
)