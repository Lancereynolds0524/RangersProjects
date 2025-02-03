
ui <- fluidPage(theme = shinytheme("cosmo"), 
  
  #libractionButton("debug", 'Debug'),
  
  titlePanel("MiLB Pitcher Evaluations"),
  
  actionButton("go", "Save Report"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("YEAR", "Year:", choices = possible_pitchers %>% filter(YEAR >= 2021) %>% distinct(YEAR), selected = 2024),
      selectInput("ORGANIZATIONS_ABBREV", "Organization:", choices = NULL),
      selectInput("COMPETITION_LEVEL_ABBREV", "Comp Level:", choices = NULL),
      selectInput("pitchers", "Pitcher Name:", choices = NULL),
      width = 2
    ),
    mainPanel(
      div(id = "screenshotdiv",
          fluidRow(
            column(6,
                   withSpinner(gt_output(outputId = "table1"), type = 6, color.background = "white", color = "#A91111"))
            ,
            column(6,
                   withSpinner(plotOutput(outputId = "role_pie_chart"), type = 6, color.background = "white", color = "#A91111"))
            ,
            column(12,
                   withSpinner(gt_output(outputId = "table2"), type = 6, color.background = "white", color = "#A91111"))
            ,
            column(12,
                   withSpinner(gt_output(outputId = "table3"), type = 6, color.background = "white", color = "#A91111"))
            ,
            column(12,
                   withSpinner(gt_output(outputId = "table4"), type = 6, color.background = "white", color = "#A91111"))
          )),
    width = 10
    )
  )
)
