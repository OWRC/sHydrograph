
navbarMenu("Hydrograph",
           tabPanel("Hydrograph",
                    source(file.path("ui/temporal", "ui_main.R"), local = TRUE)$value
           ),
           tabPanel("Scatter plot",
                      source(file.path("ui/temporal", "ui_scatter.R"), local = TRUE)$value
           ),
           tabPanel("Data download",
                      source(file.path("ui/temporal", "ui_data_table.R"), local = TRUE)$value
           )
)

# tabPanel("Hydrograph",
#   source(file.path("ui/temporal", "ui_main.R"), local = TRUE)$value
# )
