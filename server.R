library(sortable)
library(magrittr)
library(Gmisc)
library(Hmisc)
library(htmlwidgets)
library(htmlTable)
#library(survival)
#library(kableExtra)
library(boot)
library(flextable)
library(officer)
library(haven)
library(tools)
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

melanoma2$ulcer[3] <- NA

#label(melanoma2$sex)       <- "Sex"
#label(melanoma2$age)       <- "Age"
#label(melanoma2$ulcer)     <- "Ulceration"
#label(melanoma2$thickness) <- "Thickness"

dm431 <- read.csv("data/dm431.csv", header = T)
hbp_study <- read.csv("data/hbp_study.csv", header = T)
hbp330 <- read.csv("data/hbp330.csv", header = T)

CapStr <- function(y) {
  c <- strsplit(y, " ")[[1]]
  paste(toupper(substring(c, 1,1)), substring(c, 2),
        sep="", collapse=" ")
}

CleanStr <- function(y) {
  iconv(y, from = 'UTF-8', to = 'ASCII//TRANSLIT')
}


colnames_to_tags <- function(df){
  r <- names_to_tags(df, colnames(df))
  return (r)
}

names_to_tags <- function(df, names){
    r <- lapply(
      names,
      function(co) {
        if(length(unique(df[, co])) < 10){
          sty = "color:red"; 
        }else{
          sty = "color:black"; 
        }
        tag(
          "p",
          list(
            #class = class(df[, co]),
            tags$span(class = "glyphicon glyphicon-move"),
            tags$strong(co, style=sty)
          )
        )
      }
    )
    return (r)
  }

server <- function(input, output) {
  
  #output$variables <- renderPrint(input[["sort_vars"]])
  #output$analyse_y <- renderPrint(input[["sort_y"]])
  
  myvalue <- reactiveVal("melanoma")
  ready <- reactiveVal(FALSE)
  
  upload_name <- reactiveVal("")
  
  reactive_dataset <- reactive({
    switch (myvalue(),
            "melanoma" = melanoma2,
            "mtcars" = mtcars,
            "hbp_study" = hbp_study,
            "hbp330" = hbp330,
            "dm431" = dm431, 
            "upload" = upload_dataset()
    )
  })
  
  preprocessed_dataset <- reactive({
    dataset <- reactive_dataset()
    cat("Dataset changing to: ", myvalue(), "\n", file = stderr())
    ready(TRUE)
    
    P <- list()
    P$dataset <- reactive_dataset()
    colnames(P$dataset) <- lapply(colnames(P$dataset), CleanStr);
    colnames(P$dataset) <- lapply(colnames(P$dataset), CapStr);
    c_names <- colnames(P$dataset);
    P$u_values <- lapply(P$dataset, function(x) length(unique(x)))
    
    cnames <- colnames(P$dataset)
    valids <- rep(TRUE, ncol(P$dataset))
    for (i in 1:ncol(P$dataset)) {
      cat("Variable: ", c, "\n", file = stderr())
      c <- c_names[i]
      q <- P$dataset[[c]]
      if((P$u_values[i] < 10) && !is.factor(q)){
        cat("Factorized variable: ", c, "\n", file = stderr())
        P$dataset[[c]] <- factor(P$dataset[[c]])
        #print(P$dataset[c])
      }
      if(P$u_values[i] >= 10 && is.character(q)){
        valids[i] <- FALSE
      }
      
      if(P$u_values[i] == nrow(P$dataset)){
        if(isTRUE(all(q == as.integer(q)))){
          valids[i] <- FALSE
        }
      }
      
    }
    
    
    #print(valids)
    
    P$dataset <- P$datase[valids]
    P$u_values <- lapply(P$dataset, function(x) length(unique(x)))
    
    cat("Dataset changed to: ", myvalue(), "\n", file = stderr())
    
    #ready(TRUE)
    return (P)
  })
  
  x <- reactive({
    req(preprocessed_dataset())
    message("X updated")
    x <- input$sort_x
    if (is.character(x)) x %>% trimws()
  })
  
  y <- reactive({
    req(preprocessed_dataset())
    message("Y updated")
    y_vars <- input$sort_y %>% trimws();
    
    a <- fo_sort_y()
    if((length(fo_sort_y()) == length(y_vars)) 
       && all(fo_sort_y() == y_vars)){
      ready(TRUE)
    }
    return (y_vars)
  })
  
  preparetable <- reactive({
    validate(
      need(x(), ""),
      need(y(), ""),
      need(ready(), "")
    )
    P <- preprocessed_dataset()
    message(cat("Prepare table: Status 0"))
    validate(need(P, ""))
    validate(
      need((length(y()) != 0) && (P$u_values[y()]<10), "")
    )
    message(cat("Prepare table is starting..."))
    #print("Prepare table is updated.")
    #x <- list(a='b');
    
    x <- table1(P$dataset, y(), x());
    message(cat("Prepare table: Status A"))
    
    x$yval <- y()
    
    a <- rep(NA, sum(x$n.rgroup))
    t <- 0
    for (i in 1:length(x$rgroup)){
      q <- x$n.rgroup[i];
      for (j in 1:q){
        t <- t + 1
        a[t] <- x$rgroup[i]
      }
    }
    
    message(cat("Prepare table: Status B"))
    
    row_names <- rownames(x$table);
    x$df <- as.data.frame(x$table);
    x$df <- x$df %>% add_column(row_names, .after = 0)
    names(x$df)[1] <- "Statistic"
    x$df <- x$df %>% add_column(a, .after = 0)
    names(x$df)[1] <- "Variable"
    x$df <- x$df %>% mutate_all(funs(str_replace_all(., "&plusmn;", "±")))
    x$df <- x$df %>% mutate_all(funs(str_replace_all(., "&lt;", "<")))
    
    message(cat("Prepare table has ended."))
    
    return (x)
  })
  
  output$tableout <- function() {
    validate(
      need(x(), "Drag a variable to x"),
      need(y(), "Drag a variable to y"),
      need(ready(), "")
    )
    P <- preprocessed_dataset()
    message(cat("Tableout: Status 0."))
    validate(need(P, ""))
    validate(
      need((length(y()) != 0) && (P$u_values[y()]<10), "The variable in y must be categorical (shown red).")
    )
    message(cat("Tableout: Status 1."))
    validate(
      need(preparetable(), "Table is not ready yet.")
    )
    message(cat("Tableout: Status 2."))
    x <- preparetable()
    message(cat("Tableout: Status 3."))
    return(x$html)
  }
  
  observeEvent(input$buttonA, {
    switch (myvalue(),
            "melanoma" = myvalue("hbp330"),
            "hbp330" = myvalue("dm431"),
            "dm431" = myvalue("melanoma"),
            "upload" = myvalue("melanoma"), 
    )
  })
  
  # observeEvent(input$sort_y, {
  #   if(fo_sort_names()$y == (input$sort_y %>% trimws())){
  #     ready(TRUE)
  #   }
  #   })
  
  prepareFlexTable <- reactive({
    validate(
      need(preparetable(), "")
    )
    library(flextable)
    library(officer)
    x <- preparetable()
    headings = x$headings
    headings = append(headings, c("Variable", "Statistic"), after = 0)
    cols <- colnames(x$df);
    
    headerlist = list();
    mainheader = list()
    for(i in 1:length(headings)){
      headerlist[cols[i]] <- headings[i]
      if(i <= 2 || i == length(headings)){
        mainheader[cols[i]] <- "";
      } else {
        mainheader[cols[i]] <- x$yval
      }
    }
    
    row_names <- x$df[1]
    
    myft <- flextable(x$df)
    myft <- set_header_labels(myft, values=headerlist);
    
    blist <- vector()
    if(nrow(row_names) >= 2){
      value <- row_names[1,];
      for(i in 1:(nrow(row_names)-1)){
        if((value != row_names[i+1,])){
          blist <- c(blist, i)
        }
        value <- row_names[i+1,];
      }
      if(length(blist) != 0){
        myft <- hline(myft, i = blist, j = 1:length(headings), border = fp_border(color="gray"));
      }
    }
    myft <- add_header(myft, values = mainheader, top = TRUE)
    myft <- fontsize(myft, i = 1, j = 1:length(headings), size=13, part = "header");
    myft <- bold(myft, i = 1, j = 1:length(headings), part = "header");
    myft <- hline(myft, i = 1, j = 1:length(headings), part = "header", border = fp_border(color = "black", width=2) )
    #myft <- hline_top(myft, j = 1:length(headings), part = "header", border = fp_border(color = "red") )
    myft <- merge_h(myft, i = 1, part = "header")
    myft <- merge_v(myft, j = "Variable")
    myft <- vline(myft, j = c(2,length(headings)-1), i = 1:nrow(row_names), border = fp_border(color="black"), part = "body");
    myft <- vline(myft, j = c(2,length(headings)-1), i = 2, border = fp_border(color="black"), part = "header");
    myft <- width(myft, width = 1)
    myft <- align(myft, align = "center", part = "body")
    myft <- align(myft, align = "center", part = "header")
    myft <- fit_to_width(myft, 15)
    return(myft)
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('table-', Sys.Date(), '.docx', sep='')
    },
    content = function(con) {
      message(con)
      library(flextable)
      library(officer)
      save_as_docx(prepareFlexTable(), path = con)
    }
  )
  
  output$downloadData_pptx <- downloadHandler(
    filename = function() {
      paste('table-', Sys.Date(), '.pptx', sep='')
    },
    content = function(con) {
      message(con)
      library(flextable)
      library(officer)
      
      x <- preparetable()
      headings = x$headings
      myft <- prepareFlexTable()
      myft <- fix_border_issues(myft, part = "all")
      save_as_pptx(myft, path = con)
    }
  )
  
  output$download_ui <- renderUI({
    req(prepareFlexTable())
    splitLayout(
      downloadButton("downloadData", label = "Export to Word"),
      fillRow(),
      downloadButton("downloadData_pptx", label = "Export to Powerpoint")
    )
  })
  
  output$current_dataset_ui <- renderUI({
    dname = myvalue();
    if(dname == "upload"){
      dname = upload_name()
    }
    tags$div(
      tags$b("Current Dataset:", style = "margin-bottom:0px;"),
      tags$em(paste("", dname))
    )
  })
  
  fo_sort_y <- reactive({
    req(preprocessed_dataset())
    return(fo_sort_names()$y)
    })
  
  fo_sort_names <- reactive({
      req(preprocessed_dataset())
      Q <- list()
      if(myvalue() == "melanoma"){
        Q$x = c("Age", "Status", "Ulcer");
        Q$y = c("Sex");
      } else {
        df <- preprocessed_dataset()$dataset
        cnames <- colnames(df);
        Q$y = c();
        for(i in 1:length(cnames)){
          if(length(unique(df[, i])) < 10){
            Q$y = c(cnames[i]);
            break;
          }
        }
        cnames = setdiff(cnames, Q$y);
        Q$x = cnames[1:min(3, length(cnames))]
        
      }
      return(Q)
    })
  
  sort_tags <- reactive({
    req(preprocessed_dataset())
    Q = fo_sort_names()
    cnames <- colnames(preprocessed_dataset()$dataset);
    others = setdiff(cnames, c(Q$x, Q$y));
    Q$x = names_to_tags(preprocessed_dataset()$dataset, Q$x)
    Q$y = names_to_tags(preprocessed_dataset()$dataset, Q$y)
    Q$other = names_to_tags(preprocessed_dataset()$dataset, others)
    
    return(Q)
  })
  
  output$sort1_ui <- renderUI({
    req(preprocessed_dataset())
    tags$div(
      class = "panel panel-default",
      style = "margin-bottom: 12px", 
      tags$div(class = "panel-heading", "Variables"),
      tags$div(
        class = "panel-body",
        style = "overflow: auto; max-height: 279px; margin-bottom: 1px", #height: 275px; 
        id = "sort1",
       # colnames_to_tags(preprocessed_dataset()$dataset)
        sort_tags()$other
      ),
      sortable_js(
        "sort1",
        options = sortable_options(
          group = list(
            name = "sortGroup1",
            put = TRUE
          ),
          swap = FALSE,
          swapClass = "sortable-swap-highlight",
          sort = FALSE,
          onSort = sortable_js_capture_input("sort_vars"),
          onLoad = sortable_js_capture_input("sort_vars")
        )
      )
    )
  })
  
  output$sort2_ui <- renderUI({
    req(preprocessed_dataset())
    tags$div(
      class = "panel panel-default",
      style = "margin-bottom: 18px", 
      tags$div(
        class = "panel-heading",
        tags$span(class = "glyphicon glyphicon-stats"),
        "X: Variables of interest (drag here)"
      ),
      tags$div(
        class = "panel-body",
        style = "overflow: auto; max-height: 129px;", #height: 280px; #max-height: 150px;
        id = "sort2",
        sort_tags()$x
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
          swap = FALSE,
          swapClass = "sortable-swap-highlight",
          onSort = sortable_js_capture_input("sort_x"),
          onLoad = sortable_js_capture_input("sort_x")
        )
      )
    )
  })
  
  output$sort3_ui <- renderUI({
    req(preprocessed_dataset())
    tags$div(
      class = "panel panel-default",
      style = "margin-bottom: 12px", 
      tags$div(
        class = "panel-heading",
        tags$span(class = "glyphicon glyphicon-stats"),
        "Y: Stratification variable (drag here)"
      ),
      tags$div(
        class = "panel-body",
        style = "max-height: 50px;", #height: 280px; 
        id = "sort3",
        sort_tags()$y
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
          onSort = sortable_js_capture_input("sort_y"),
          onLoad = sortable_js_capture_input("sort_y")
        ))
    )
  })
  
  observeEvent(input$file1, {
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    upload_dataset()
    })
  
  upload_dataset <- reactive({
      inFile <- input$file1
      if (is.null(inFile))
        return(NULL)
      fileInfo <- input$file1
      ext = file_ext(inFile$datapath)
      switch(ext, 
             "csv" = x <- read.csv(inFile$datapath, header = T), #input$header),
             "sav" = x <- data.frame(read_sav(inFile$datapath)),
             validate(
               need(FALSE, "Invalid file type.")
             )
      )
      myvalue("upload")
      upload_name(fileInfo$name)
      message(cat("Dataset is uploaded: ", fileInfo$name))
      return(x)
    })
  
  output$contents <- renderTable({
    preprocessed_dataset()$dataset
  })
  
}

# wellPanel

