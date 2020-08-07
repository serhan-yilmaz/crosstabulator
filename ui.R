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
        uiOutput("sort1_ui")
      ),
      column(
        width = 2,
        # analyse as x
        uiOutput("sort2_ui")
        ,
        # analyse as y
        uiOutput("sort3_ui")
      ),
      column(
        width = 6,
        #tableOutput("mtcars_kable")
        htmlOutput("tableout"),
        uiOutput("download_ui"),
        actionButton("buttonA", "An action button")
        #downloadButton("downloadData", label = "Download"),
      )
    )
  )
)