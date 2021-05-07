navbarMenu("Statistics",
           tabPanel("Annual extremes",
                    source(file.path("ui/stats", "ui_extreme.R"), local = TRUE)$value
           ),tabPanel("GAM",
                    source(file.path("ui/stats", "ui_distr_day.R"), local = TRUE)$value
           ),tabPanel("Monthly densities",
                    source(file.path("ui/stats", "ui_distr_mon.R"), local = TRUE)$value
           )
)