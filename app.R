library(shiny)

source("ui.R");
source("server.R");
shinyApp(ui, server, options = list(sanitize.errors = F))