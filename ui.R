ui <- fluidPage(
  title = "CrossTabulator", 
  tags$head(
    tags$link(rel="shortcut icon", href="favicon.png"),
    tags$meta(name="description", content="A web application to create demographics tables for use in scientific journals"),
    includeHTML(("www/google-analytics.html")),
  ),
  #tags$link(rel = "icon", type = "image/png", href = "favicon.png"),
  fluidRow(
    class = "panel panel-heading",
    div(
      class = "panel-heading",
      #style = "margin:0px; padding:0px;",
      style = "margin-bottom:0px; padding-bottom:0px;",
      
     # div(
       # style = "position: relative",
        #style = "float: left;",
        div(
        img(src='crosstabulator_logo.png', align = "left", style = "height: 30px;")
        ),
        tags$br(),
        tags$br(style = "display: block; content: \"\"; margin-top: 16px;"),
        #tags$br(),
        #tags$span
      
      #tags$br(),
      #tags$hr(style = "margin:8px 0px 8px 0px;"),
      #h3("CrossTabulator", style = "margin-bottom:3.5px;"),
      tags$span("A web application to create demographics tables for use in scientific journals", 
             style = "font-size: 16.5px; margin-bottom:0px; padding-bottom:0px;")
     # )
      #h4()
    ),
    fluidRow(
      class = "panel-body",
      column(
        width = 6,
        fluidRow(
          column(
            width = 4,
            tags$div(
              class = "panel panel-default",
              style = "margin:0px; margin-bottom: 12px;",
              tags$div(class = "panel-heading", "Input Data"),
              tags$div(
                class = "panel-body",
                style = "padding-bottom:10px; padding-top:10px; margin:0px;  height: 280px;",
                id = "fileInput",
                actionButton("buttonA", "Sample Dataset"),
                tags$hr(style = "margin:8px 0px 8px 0px;"),
                fileInput("file1", "Upload File*:",
                          accept = c(".csv", ".sav")
                ),
                tags$div(
                  style = "margin-bottom:4px;",
                  HTML('Accepted Types: <br> .csv or .sav'),
                ),
                #tags$p("Accepted Types: .csv or .sav"),
                tags$style(".shiny-input-container {margin-bottom: 0px} #file1_progress { margin-bottom: 3px } .checkbox { margin-top: 0px}"),
                #checkboxInput("header", "Header", TRUE),
                tags$style(".checkbox {margin-bottom: 0px;}"),
                tags$hr(style = "margin: 5px 0px 4px 0px;"),
                uiOutput("current_dataset_ui")
              )
            )
          ), 
          column(
            width = 8, 
            fluidRow(
              column(
                width = 6,
                uiOutput("sort1_ui")
              ),
              column(
                width = 6,
                # analyse as x
                uiOutput("sort2_ui")
                ,
                # analyse as y
                uiOutput("sort3_ui")
              )
            )
          )
        ),
        tags$div(
          class = "panel panel-default",
          style = "margin:0px; margin-bottom:5px;",
          #tags$div(class = "panel-heading", "About"),
          tags$div(
            class = "panel-body",
            style = "padding-bottom:10px; padding-top:10px; margin:0px;", #  height: 78px;
            #tags$p(),
            "For questions or feature suggestions, please contact:",
            tags$br(),
            tags$a("Serhan Yilmaz", href="http://www.serhanyilmaz.com"),
            "<serhan.yilmaz@case.edu>",
            tags$br(),
            tags$a("Ahmet Mert Hacialiefendioglu", href="https://www.linkedin.com/in/hacialiefendioglu/"),
            "<ahmetmert53@gmail.com>",
          )
        ),
        tags$p(
          "* The uploaded data and/or the resulting tables are only kept during the current session and are removed once the browser is closed. Please make sure to save your results before exiting.", style = "font-size: 12px")
      ),
      column(
        width = 6,
        tabsetPanel(
          tabPanel(
            "Cross-Tabulation",
            fluidRow(
              column(
                width = 12,
                htmlOutput("tableout"),
                uiOutput("download_ui")
                #downloadButton("downloadData", label = "Download"),
              )
            )
          ),
          tabPanel(
            "Data View",
            div(style = 'overflow: auto; max-height:380px;', tableOutput("contents"))
          )
      ),
    )
    ),
  )
)