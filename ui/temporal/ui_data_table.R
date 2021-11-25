tabPanel("Data table",
  sidebarPanel(
   dateRangeInput("tab.rng", label = "Select date range:"),
   pickerInput(
     inputId = "pck.tab",
     label = "Choose interval:",
     choices = NULL,
     options = list(
       `actions-box` = TRUE,
       size = 10,
       `selected-text-format` = "count > 3"
     ),
     multiple = TRUE
   ),
   radioButtons("tabRad", label = "Table format",
                choices = list("original query (normalized)" = 1, "spread (re-organized)" = 2), 
                selected = 1), br(),
   downloadButton("tabCsv", "Download csv.."), br(),
   shiny::includeMarkdown("md/notes.md"),
   width=2
  ),
  mainPanel(
   dataTableOutput('tabiids'),
   dataTableOutput('tabts'),
   width=10
  )
)