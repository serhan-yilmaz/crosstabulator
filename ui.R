ui <- fluidPage(
  fluidRow(
    class = "panel panel-heading",
    div(
      class = "panel-heading",
      h3("CrossTabulator")
    ),
    fluidRow(
      class = "panel-body",
      column(
        width = 2,
        tags$div(
          class = "panel panel-default",
          style = "margin:0px;",
          tags$div(class = "panel-heading", "Input Data"),
          tags$div(
            class = "panel-body",
            style = "padding-bottom:10px; padding-top:10px; margin:0px;  height: 300px;",
            id = "fileInput",
            actionButton("buttonA", "Sample Dataset"),
            tags$hr(style = "margin:8px 0px 8px 0px;"),
            fileInput("file1", "Upload File:",
                      accept = c(".csv", ".sav")
            ),
            tags$div(
              style = "margin-bottom:4px;",
              HTML('Accepted Types: <br> .csv or .sav'),
            ),
            #tags$p("Accepted Types: .csv or .sav"),
            tags$style(".shiny-input-container {margin-bottom: 0px} #file1_progress { margin-bottom: 3px } .checkbox { margin-top: 0px}"),
            checkboxInput("header", "Header", TRUE),
            tags$style(".checkbox {margin-bottom: 0px;}"),
            tags$hr(style = "margin: 5px 0px 4px 0px;"),
            uiOutput("current_dataset_ui")
          )
        )
      ), 
      column(
        width = 4, 
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
            div(style = 'overflow: auto; max-height:400px;', tableOutput("contents"))
          )
      ),
    )
    ),
  )
)