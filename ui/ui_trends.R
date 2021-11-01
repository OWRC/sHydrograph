navbarMenu("Trends",
           tabPanel("Annual summary",
                    source(file.path("ui/trends", "ui_annual.R"), local = TRUE)$value
           ),
           tabPanel("Seasonal summary",
                    source(file.path("ui/trends", "ui_seasonal.R"), local = TRUE)$value
           ),
           # tabPanel("Hi-Low summary",
           #          source(file.path("ui/trends", "ui_hilow.R"), local = TRUE)$value
           # ),
           tabPanel("Multivariate analysis",
                    source(file.path("ui/trends", "ui_multi.R"), local = TRUE)$value
           ),
           tabPanel("Scatter plot",
                    source(file.path("ui/trends", "ui_scatter.R"), local = TRUE)$value
           )
)