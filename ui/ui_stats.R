navbarMenu("Statistics",
           tabPanel("Annual extremes",
                    source(file.path("ui/stats", "ui_extreme.R"), local = TRUE)$value
           ),tabPanel("Annual variability",
                    source(file.path("ui/stats", "ui_distr_gam.R"), local = TRUE)$value
           ),tabPanel("Monthly densities",
                    source(file.path("ui/stats", "ui_distr_mon.R"), local = TRUE)$value
           ),tabPanel("QQ plot",
                    source(file.path("ui/stats", "ui_qq.R"), local = TRUE)$value
           )
)