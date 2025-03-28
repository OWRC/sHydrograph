
navbarMenu("Hydrograph",
           tabPanel("Hydrograph",
                    source(file.path("ui/temporal", "ui_main.R"), local = TRUE)$value
           ),
           tabPanel("Scatter plot",
                      source(file.path("ui/temporal", "ui_scatter.R"), local = TRUE)$value
           ),
           tabPanel("Cumulative distribution function",
                    source(file.path("ui/temporal", "ui_cdf.R"), local = TRUE)$value
           )
)

# tabPanel("Hydrograph",
#   source(file.path("ui/temporal", "ui_main.R"), local = TRUE)$value
# )
