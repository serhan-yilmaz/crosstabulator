on_ready <- paste(
  "$(function() {",
  "$(document).on('shiny:connected', function(e) {",
  "Shiny.setInputValue('initialized', 1);",
  "});",
  "",
  "});",
  sep = "\n"
)

ui <- fluidPage(
  title = "CrossTabulator", 
  tags$head(
    tags$link(rel="shortcut icon", href="favicon.png"),
    tags$meta(name="description", content="A web application to create demographics tables for use in scientific journals"),
    tags$meta(name="keywords", content="cross tabulation, summary statistics, data analysis, demographics table"),
    includeHTML(("www/google-analytics.html")),
    tags$script(on_ready),
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
            "<ahmetmert@hotmail.it>",
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
          ),
          tabPanel(
            "FAQ",
            tags$div(
              class = "panel-body",
              style = "padding-bottom:10px; padding-top:10px; margin:0px;", #  height: 78px;
              #tags$p(),
              tags$h3("Frequently Asked Questions (FAQ)", style="font-weight:normal;"),
              tags$h4("What is the purpose behind CrossTabulator?", style="font-weight:bold; margin-bottom:5px;"),
              tags$p(style="text-align:justify;", "In quantitative research (particularly in social science), it is important to provide an overview of the study population (e.g., demographics of human participants) through summary statistics of the study data. With crosstabulator, we aim to provide an easy and accurate way to create such descriptive tables for researchers who do not want to deal with programming or lack the necessary technical knowledge."), 
              #tags$br(),
              tags$h4("How can I use CrossTabulator on my data?", style="font-weight:bold; margin-bottom:5px;"),
              tags$p(style="text-align:justify;", "To use CrossTabulator with your data, simply click on the upload button and, in the opened window, select the data file that you wish to analyze in your computer. Note that, currently two types of file formats (.csv and .sav) are accepted as input."), 
              #tags$br(),
              tags$h4("Can I export the generated tables?", style="font-weight:bold; margin-bottom:5px;"),
              tags$p(style="text-align:justify;", "Yes, the generated tables can be downloaded in several modifiable formats including Word (.docx), Powerpoint (.pptx), Latex, HTML and CSV."), 
              #tags$br(),
              tags$h4("Is it safe to use CrossTabulator for sensitive data?", style="font-weight:bold; margin-bottom:5px;"),
              tags$p(style="text-align:justify;", "Beyond the session duration, CrossTabulator does not store any of the uploaded data files nor the generated tables. But, for highly sensitive or classified data, we recommend running CrossTabulator locally on your own computer to avoid man in the middle attacks."), 
              #tags$br(),
              tags$h4("How to run CrossTabulator locally?", style="font-weight:bold; margin-bottom:5px;"),
              tags$p(style="text-align:justify;", 
                     "To run CrossTabulator locally, download the source code and follow the instructions in ", 
                     tags$a(href="https://github.com/serhan-yilmaz/crosstabulator", "Github page.")), 
              # tags$br(),
            )
          )
      ),
    )
    ),
  )
)