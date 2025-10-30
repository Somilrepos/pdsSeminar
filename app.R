suppressPackageStartupMessages({
  library(shiny)
})

source('codes/shiny_app/app.R', local = TRUE)

shinyApp(ui, server)

