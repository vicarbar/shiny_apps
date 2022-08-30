library(shinydashboard)
library(shinycssloaders)
library(tidyr)
library(plotly)

ui <- fluidPage(
  title = "Shiny Application",
  theme = bslib::bs_theme(4),
  
  # Header row formatting
  fluidRow(style = "background-color:#5acedd",
    column(
      width = 2,
      # Personal logo img with link to my personal website https://vicarbar.github.io
      HTML('<a href = "https://vicarbar.github.io"><img src = "personal_logo.png", width = "100%", height = "100%"></a>')
    ),
    column(
      width = 4,
      offset = 3,
      h1(
        "Sleep EDA App"
      )
    )
  ),
  
  # Configuration layout and formatting
  fluidRow(
    column(
      width = 3,
      dateRangeInput(
        inputId = "date_range",
        label = "Select the date range you want to explore",
        start = "2022-01-12",
        end = "2022-07-31"
      )
    ),
    column(
      width = 3,
      offset = 1,
      selectInput(
        inputId = "weekdays",
        label = "Select the weekdays you want to explore",
        choices = c("All", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"),
        multiple = FALSE
      )
    ),
    column(
      width = 3,
      offset = 1,
      selectInput(
        inputId = "type_graph",
        label = "Select what you want to explore",
        choices = c("Sleep time series", "Sleep phases series", "Sleep phases (mean)", "Correlation"),
        multiple = FALSE
      )
    )
  ),
  fluidRow(
    column(
      width = 3,
      tableOutput(
        outputId = "table"
      )
    ),
    column(
      width = 8,
      offset = 1,
      plotlyOutput("graph") %>% withSpinner(color="#0dc5c1")
    )
  )
)
  