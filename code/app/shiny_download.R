##SHINY DOWNLOADS
downloadTab <- function (id) {
    ns <- NS(id)
    tabPanel("DOWNLOADS",
             column(2,
                    div(style = "text-align: center;",
                        h3("About")
                    ),
                    br(),
                    
                    # Methods
                    div(style = "text-align: center;",
                        HTML('<a href="methods/index.html" target="_blank"><img src="noun_Document.png" width="50" height="50"></a>'),
                        h4("Methods")
                    ),
                    br(),
                    
                    # Preprint
                    div(style = "text-align: center;",
                        HTML('<a href="https://www.biorxiv.org/content/10.1101/2020.07.17.208751v1" target="_blank"><img src="noun_Document.png" width="50" height="50"></a>'),
                        h4("Preprint")
                    ),
                    br(),
                    
                    # Code
                    div(style = "text-align: center;",
                        HTML('<a href="https://github.com/heurekalabsorg/ddh" target="_blank"><img src="GitHub-Mark-64px.png" width="50" height="50"></a>'),
                        h4("Code")
                    ),
                    br(),
                    
                    # X/Twitter
                    div(style = "text-align: center;",
                        HTML('<a href="https://www.twitter.com/ddhypothesis" target="_blank"><img src="2021 Twitter logo - black.png" width="50" height="50"></a>'),
                        h4("X/Twitter")
                    )
             ),
             column(10,
                    downloadReportPanel(ns("download"))
                    )
    )
}

downloadTabServer <- function (id, data, privateMode) {
  moduleServer(
    id,
    function(input, output, session) {
      downloadReportPanelServer("download", data, privateMode)
      
    }
  )
}
