ui <- fluidPage(
  fluidRow(
    class = "panel panel-heading",
    div(
      class = "panel-heading",
      h3("CrossTabulator")
    ),
    tags$div(
      class = "panel panel-default",
      style = "margin:0px;",
      tags$div(class = "panel-heading", "File Input"),
      tags$div(
        class = "panel-body",
        style = "padding-bottom:10px; padding-top:10px; margin:0px;",
        id = "fileInput",
        fileInput("file1", "Choose CSV File",
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv"),
                  width = "600px"
        ),
        tags$style(".shiny-input-container {margin-bottom: 0px} #file1_progress { margin-bottom: 0px } .checkbox { margin-top: 0px}"),
        tags$hr(style = "margin:0px 0px 10px 0px;"),
        checkboxInput("header", "Header", TRUE),
        tags$style(".checkbox {margin-bottom: 0px;}"),
      )
    ),
    fluidRow(
      class = "panel-body",
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
      )
      ,
      column(
        width = 6,
        #tableOutput("mtcars_kable")
        htmlOutput("tableout"),
        uiOutput("download_ui"),
        actionButton("buttonA", "Switch Dataset")
        #downloadButton("downloadData", label = "Download"),
      )
    ),
    fluidRow(
      mainPanel(
        div(style = 'overflow: auto; max-height:400px;', tableOutput("contents"))
      )
    )
  )
)