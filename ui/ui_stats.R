navbarMenu("Statistics",
           tabPanel("Annual extremes",
                    source(file.path("ui/stats", "ui_extreme.R"), local = TRUE)$value
           )
)