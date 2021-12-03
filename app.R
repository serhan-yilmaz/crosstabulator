library(shiny)

source("ui.R");
source("server.R");
options(shiny.sanitize.errors = F)
shinyApp(ui, server, options = list(sanitize.errors = F))