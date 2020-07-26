## ---- shiny-drag-vars-to-plot -------------------------------------------
## Example shiny app to create a plot from sortable inputs

library(shiny)
library(htmlwidgets)
library(sortable)
library(magrittr)
library(dplyr)
library(boot)
#library(table1)
library(furniture)
library(survival)
library(kableExtra)

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
      h3("Dragging variables to define a plot")
    ),
    fluidRow(
      class = "panel-body",
      column(
        width = 3,
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
        width = 3,
        # analyse as x
        tags$div(
          class = "panel panel-default",
          tags$div(
            class = "panel-heading",
            tags$span(class = "glyphicon glyphicon-stats"),
            "Analyze as x (drag here)"
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
            "Analyze as y (drag here)"
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
        htmlOutput("mtcars_kable")
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
    print(x)
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
      dat <- melanoma2[, c(x(), y())]
      names(dat) <- c("x", "y")
      plot(y ~ x, data = dat, xlab = x(), ylab = y())
    })
  
  
  output$mtcars_kable <- function() {
    validate(
      need(x(), "Drag a variable to x")
    )
    #req(input$mpg)
    #print(y())
    
    
    xval <- x();
    yval <- y();
    
    datax <- melanoma2[xval]
    
    #print(length(yval))
    
    #table1(melanoma2, splitby=~status, output="html") %>%
    # kable_styling("striped")
    
    #table1(~ sex + age + ulcer + thickness | status, data=melanoma2, overall="Total", topclass="Rtable1-grid Rtable1-shade Rtable1-times") 
    
    #knit_print(a)
    if(!(length(yval)==0)){
      datax["splitvariable"] <- melanoma2[yval]
      datax <- datax %>% group_by(splitvariable)
    }
    table1(datax, test = TRUE, na.rm = FALSE, output="html") 
    
    #%>%
    #  kable_styling("striped")
    
    #print(table)
    #mtcars %>%
    #  mutate(car = rownames(.)) %>%
    #  select(car, everything()) %>%
    #  filter(mpg <= 15) %>%
    #  knitr::kable("html") %>%
    #  kable_styling("striped", full_width = F) %>%
    #  add_header_above(c(" ", "Group 1" = 5, "Group 2" = 6))
    
    
  }
  
}
shinyApp(ui, server)