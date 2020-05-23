tabPanel("Data table",
  sidebarPanel(
   dateRangeInput("tabRng", label = "Date range"),
   br(), downloadButton("tabCsv", "Download csv.."),
   width=2
  ),
  mainPanel(
   dataTableOutput('tabts'),
   width=10
  )
)