## ---- shiny-drag-vars-to-plot -------------------------------------------
## Example shiny app to create a plot from sortable inputs

library(shiny)
library(htmlwidgets)
library(sortable)
library(magrittr)
library(dplyr)

#library(table1)
#library(furniture)
library(Gmisc)
library(Hmisc)
library(htmlTable)
library(survival)
library(kableExtra)
library(boot)
library(flextable)
library(officer)
library(tidyverse)
source("table1.R")

source("ui.R");
source("server.R");
shinyApp(ui, server)



