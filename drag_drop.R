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

melanoma2 <- melanoma

# Factor the basic variables that
# we're interested in
melanoma2$status <- 
  factor(melanoma2$status, 
         levels=c(2,1,3),
         labels=c("Alive", # Reference
                  "Melanoma death", 
                  "Non-melanoma death"))

melanoma2$sex <- 
  factor(melanoma2$sex, levels=c(1,0),
         labels=c("Male", 
                  "Female"))

melanoma2$ulcer <- 
  factor(melanoma2$ulcer, levels=c(0,1),
         labels=c("Absent", 
                  "Present"))

#label(melanoma2$sex)       <- "Sex"
#label(melanoma2$age)       <- "Age"
#label(melanoma2$ulcer)     <- "Ulceration"
#label(melanoma2$thickness) <- "Thickness"

#units(melanoma2$age)       <- "years"
#units(melanoma2$thickness) <- "mm"

u_values = sapply(melanoma2, function(x) length(unique(x)))
print(u_values)

colnames_to_tags <- function(df){
  lapply(
    colnames(df),
    function(co) {
      tag(
        "p",
        list(
          class = class(df[, co]),
          tags$span(class = "glyphicon glyphicon-move"),
          tags$strong(co)
        )
      )
    }
  )
}


ui <- fluidPage(
  fluidRow(
    class = "panel panel-heading",
    div(
      class = "panel-heading",
      h3("Tabulator")
    ),
    fluidRow(
      class = "panel-body",
      column(
        width = 2,
        tags$div(
          class = "panel panel-default",
          tags$div(class = "panel-heading", "Variables"),
          tags$div(
            class = "panel-body",
            id = "sort1",
            colnames_to_tags(melanoma2)
          )
        )
      ),
      column(
        width = 2,
        # analyse as x
        tags$div(
          class = "panel panel-default",
          tags$div(
            class = "panel-heading",
            tags$span(class = "glyphicon glyphicon-stats"),
            "X: Variables of interest (drag here)"
          ),
          tags$div(
            class = "panel-body",
            id = "sort2"
          )
        ),
        # analyse as y
        tags$div(
          class = "panel panel-default",
          tags$div(
            class = "panel-heading",
            tags$span(class = "glyphicon glyphicon-stats"),
            "Y: The outcome variable (drag here)"
          ),
          tags$div(
            class        #plotOutput("plot"),
 = "panel-body",
            id = "sort3"
          )
        )
        
      ),
      column(
        width = 6,
        #tableOutput("mtcars_kable")
        htmlOutput("tableout"),
        uiOutput("download_ui"),
        #downloadButton("downloadData", label = "Download"),
      )
    )
  ),
  sortable_js(
    "sort1",
    options = sortable_options(
      group = list(
        name = "sortGroup1",
        put = TRUE
      ),
      swap = TRUE,
      swapClass = "sortable-swap-highlight",
      sort = FALSE,
      onSort = sortable_js_capture_input("sort_vars")
    )
  ),
  sortable_js(
    "sort2",
    options = sortable_options(
      group = list(
        group = "sortGroup1",
        put=TRUE,
        #put = htmlwidgets::JS("function (to) { return to.el.children.length < 1; }"),
        pull = TRUE
      ),
      swap = TRUE,
      swapClass = "sortable-swap-highlight",
      onSort = sortable_js_capture_input("sort_x")
    )
  ),
  sortable_js(
    "sort3",
    options = sortable_options(
      group = list(
        group = "sortGroup1",
        put = htmlwidgets::JS("function (to) { return to.el.children.length < 1; }"),
        pull = TRUE
      ),
      swap = TRUE,
      swapClass = "sortable-swap-highlight",
      onSort = sortable_js_capture_input("sort_y")
    )
  )
)

server <- function(input, output) {
  
  output$variables <- renderPrint(input[["sort_vars"]])
  output$analyse_x <- renderPrint(input[["sort_x"]])
  output$analyse_y <- renderPrint(input[["sort_y"]])
  
  x <- reactive({
    x <- input$sort_x
    if (is.character(x)) x %>% trimws()
    #print(x)
  })
  
  y <- reactive({
    input$sort_y %>% trimws()
  })
  
  output$plot <-
    renderPlot({
      validate(
        need(x(), "Drag a variable to x"),
        need(y(), "Drag a variable to y")
      )
      #print((u_values[y()]<10))
      dat <- melanoma2[, c(x(), y())]
      names(dat) <- c("x", "y")
      plot(y ~ x, data = dat, xlab = x(), ylab = y())
    })
  
  preparetable <- reactive({
    validate(
      need(x(), ""),
      need(y(), "")
    )
    validate(
      need((length(y()) != 0) && (u_values[y()]<10), "")
    )
    x <- table1(melanoma2, y(), x());
    
    a <- rep(NA, sum(x$n.rgroup))
    t <- 0
    for (i in 1:length(x$rgroup)){
      q <- x$n.rgroup[i];
      for (j in 1:q){
        t <- t + 1
        a[t] <- x$rgroup[i]
      }
    }
    
    row_names <- rownames(x$table);
    x$df <- as.data.frame(x$table);
    x$df <- x$df %>% add_column(row_names, .after = 0)
    names(x$df)[1] <- "Statistic"
    x$df <- x$df %>% add_column(a, .after = 0)
    names(x$df)[1] <- "Variable"
    x$df <- x$df %>% mutate_all(funs(str_replace_all(., "&plusmn;", "±")))
    
    return (x)
  })
  
  output$tableout <- function() {
    validate(
      need(x(), "Drag a variable to x"),
      need(y(), "Drag a variable to y")
    )
    validate(
      need((length(y()) != 0) && (u_values[y()]<10), "The variable in y must be categorical.")
    )
    validate(
      need(preparetable(), "Table is not ready yet.")
    )
    
    x <- preparetable()
    myft <- flextable(x$df)
    myft <- width(myft, width = 1)
    myft <- merge_v(myft, j = "Variable")
    myft <- align(myft, align = "center", part = "body")
    myft <- align(myft, align = "center", part = "header")
    myft <- fit_to_width(myft, 9)
    x$myft <- myft
    
    save_as_docx(myft, path = "abcd.docx")
    
    
    return(x$html)
  }
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('table-', Sys.Date(), '.docx', sep='')
    },
    content = function(con) {
      print(con)
      library(flextable)
      library(officer)
      x <- preparetable()
      myft <- flextable(x$df)
      myft <- width(myft, width = 1)
      myft <- merge_v(myft, j = "Variable")
      myft <- align(myft, align = "center", part = "body")
      myft <- align(myft, align = "center", part = "header")
      myft <- fit_to_width(myft, 9)
      x$myft <- myft
      
      save_as_docx(myft, path = "abcd.docx")
      save_as_docx(myft, path = con)
    }
  )
  
  output$download_ui <- renderUI({
    req(preparetable())
    downloadButton("downloadData", label = "Export to Word")
  })
  
}
shinyApp(ui, server)