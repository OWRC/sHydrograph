
tabPanel("Scatter plot",
         sidebarPanel(
           dateRangeInput("rng.sca",label='Select date range:'),
           selectInput("cmbX.sca", "Choose x-axis:", choices=NULL),
           selectInput("cmbY.sca", "Choose y-axis:", choices=NULL),
           checkboxInput("chk.11.sca", "add 1:1 line", value=FALSE), 
           width = 3
         ),
         mainPanel(
           plotOutput("plt.sca", height = 800,
                      dblclick = "plt.sca_dblclick",
                      brush = brushOpts(
                        id = "plt.sca_brush",
                        resetOnNew = TRUE
                      )
           ),
           width = 9
         )
)

