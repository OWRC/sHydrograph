
tabPanel("Query Download", icon = icon("download"),
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
   checkboxGroupInput("chk.tab", "Choose data type:", choices=NULL),
   radioButtons("tab.spread", label = "Table format",
                choices = list("raw original DB query (normalized)" = 1, 
                               "spread (column-wise organization)" = 2, 
                               "spread, daily-aggregated data only (includes interpolated climate)" = 3), 
                selected = 1), 
   br(),
   h5('Download table as shown'),
   downloadButton("tabCsv", "Download csv.."), br(), br(),
   shiny::includeMarkdown("md/notes.md"),
   width=2
  ),
  mainPanel(
   dataTableOutput('tabiids'),
   dataTableOutput('tabts'),
   width=10
  )
)
