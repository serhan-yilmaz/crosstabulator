
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

CapStr <- function(y) {
  c <- strsplit(y, " ")[[1]]
  paste(toupper(substring(c, 1,1)), substring(c, 2),
        sep="", collapse=" ")
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
            class = class(df[, co]),
            tags$span(class = "glyphicon glyphicon-move"),
            tags$strong(co, style=sty)
          )
        )
      }
    )
    return (r)
  }

server <- function(input, output) {
  
  output$variables <- renderPrint(input[["sort_vars"]])
  output$analyse_x <- renderPrint(input[["sort_x"]])
  output$analyse_y <- renderPrint(input[["sort_y"]])
  
  myvalue <- reactiveVal("melanoma")
  upload_name <- reactiveVal("")
  
  
  reactive_dataset <- reactive({
    switch (myvalue(),
            "melanoma" = melanoma2,
            "mtcars" = mtcars,
            "upload" = upload_dataset()
    )
  })
  
  preprocessed_dataset <- reactive({
    dataset <- reactive_dataset()
    
    P <- list()
    P$dataset <- reactive_dataset()
    colnames(P$dataset) <- lapply(colnames(P$dataset), CapStr);
    c_names <- colnames(P$dataset);
    P$u_values <- lapply(P$dataset, function(x) length(unique(x)))
    
    cnames <- colnames(P$dataset)
    valids <- rep(TRUE, ncol(P$dataset))
    for (i in 1:ncol(P$dataset)) {
      c <- c_names[i]
      q <- P$dataset[[c]]
      if((P$u_values[i] < 10) && !is.factor(q)){
        cat("Factorized variable: ", c, "\n")
        P$dataset[[c]] <- factor(P$dataset[[c]])
        #print(P$dataset[c])
      }
      if(P$u_values[i] >= 10 && is.character(q)){
        valids[i] <- FALSE
      }
    }
    print(valids)
    
    P$dataset <- P$datase[valids]
    P$u_values <- lapply(P$dataset, function(x) length(unique(x)))
    
    return (P)
  })
  
  x <- reactive({
    print("X updated")
    x <- input$sort_x
    if (is.character(x)) x %>% trimws()
  })
  
  y <- reactive({
    print("Y updated")
    input$sort_y %>% trimws()
  })
  
  preparetable <- reactive({
    validate(
      need(x(), ""),
      need(y(), "")
    )
    P <- preprocessed_dataset()
    validate(need(P, ""))
    validate(
      need((length(y()) != 0) && (P$u_values[y()]<10), "")
    )
    #print("Prepare table is updated.")
    #x <- list(a='b');
    
    print(class(P$dataset))
    print(y())
    
    x <- table1(P$dataset, y(), x());
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
    
    row_names <- rownames(x$table);
    x$df <- as.data.frame(x$table);
    x$df <- x$df %>% add_column(row_names, .after = 0)
    names(x$df)[1] <- "Statistic"
    x$df <- x$df %>% add_column(a, .after = 0)
    names(x$df)[1] <- "Variable"
    x$df <- x$df %>% mutate_all(funs(str_replace_all(., "&plusmn;", "±")))
    x$df <- x$df %>% mutate_all(funs(str_replace_all(., "&lt;", "<")))
    
    return (x)
  })
  
  output$tableout <- function() {
    validate(
      need(x(), "Drag a variable to x"),
      need(y(), "Drag a variable to y")
    )
    P <- preprocessed_dataset()
    validate(need(P, ""))
    validate(
      need((length(y()) != 0) && (P$u_values[y()]<10), "The variable in y must be categorical (shown red).")
    )
    validate(
      need(preparetable(), "Table is not ready yet.")
    )
    x <- preparetable()
    return(x$html)
  }
  
  observeEvent(input$buttonA, {
    switch (myvalue(),
            "melanoma" = myvalue("mtcars"),
            "mtcars" = myvalue("melanoma"),
            "upload" = myvalue("melanoma"), 
    )
  })
  
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
    print(row_names)
    print(nrow(row_names))
    
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
      print(con)
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
      print(con)
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
  
  
  
  fo_sort_names <- reactive({
      req(preprocessed_dataset())
      Q <- list()
      if(myvalue() == "melanoma"){
        Q$x = c("Age", "Sex", "Ulcer");
        Q$y = c("Status");
      } else {
        Q$x = c();
        Q$y = c();
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
      tags$div(class = "panel-heading", "Variables"),
      tags$div(
        class = "panel-body",
        style = "overflow: auto; max-height: 300px;", #height: 280px; 
        id = "sort1",
        sort_tags()$other
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
      tags$div(
        class = "panel-heading",
        tags$span(class = "glyphicon glyphicon-stats"),
        "X: Variables of interest (drag here)"
      ),
      tags$div(
        class = "panel-body",
        style = "overflow: auto; max-height: 170px;", #height: 280px; 
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
          swap = TRUE,
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
      tags$div(
        class = "panel-heading",
        tags$span(class = "glyphicon glyphicon-stats"),
        "Y: The outcome variable (drag here)"
      ),
      tags$div(
        class = "panel-body",
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
          onLoad = sortable_js_capture_input("sort_y"),
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
             "csv" = x <- read.csv(inFile$datapath, header = input$header),
             "sav" = x <- data.frame(read_sav(inFile$datapath)),
             validate(
               need(FALSE, "Invalid file type.")
             )
      )
      myvalue("upload")
      upload_name(fileInfo$name)
      print(cat("Dataset is uploaded: ", fileInfo$name))
      return(x)
    })
  
  output$contents <- renderTable({
    preprocessed_dataset()$dataset
  })
  
}

# wellPanel

