geneNetworkGraph <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(h4(textOutput(ns("text_graph")))),
    uiOutput(ns("network_graph"))
  )
}

geneNetworkGraphServer <- function(id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$text_graph <- renderText({paste0("Co-essentiality graph of ", 
                                              ifelse(data()$subtype == "pathway",
                                                     paste0("GSID:", data()$query),
                                                     str_c(data()$content, collapse = ", ")
                                              )
      )
      })
      
      #establish reactive value
      rv <- reactiveValues(degree = 2, 
                           threshold = 10,
                           corr_type = "positive")
      
      #update value upon call
      observeEvent(input$update, {
        rv$degree <- input$deg
        rv$threshold <- input$threshold
        rv$corr_type <- input$corr_type
      })
      
      networkGraph <- reactive({
        make_graph(input = data(), 
                   threshold = rv$threshold, 
                   deg = rv$degree, 
                   corr_type = rv$corr_type,
                   cell_line_var = "dependency",
                   displayHeight = '80vh', 
                   displayWidth = '100%', 
                   tooltipLink = TRUE) %>% 
          visLegend(position = "right", width = .25, zoom = F) #.5 fixes cut-off, but makes it too wide
      })

      # conditional UI panels for pathways
      output$hidden_condition_pathways <- renderText({
        logic_pathways <- data()$subtype != 'pathway'
        return(logic_pathways)
      })
      
      output$network_graph <- renderUI({
        sidebarLayout(
          sidebarPanel(sliderInput(inputId = session$ns("deg"),
                                   label = "Filter connections (<)",
                                   value = 2, min = 1, max = 10),
                       sliderInput(inputId = session$ns("threshold"),
                                   label = paste0("# Related ", 
                                                  ifelse(data()$subtype == "gene",
                                                         "genes", 
                                                         ifelse(data()$subtype == "pathway", "pathways",
                                                                "cell lines"))
                                                  ),
                                   value = 10, min = 10, max = 20),
                       # conditional UI panels for pathways
                       shinyjs::useShinyjs(),
                       shinyjs::hidden(div(id = session$ns("hidden_condition_pathways"))),
                       conditionalPanel(paste0("output.", session$ns("hidden_condition_pathways")),
                                        selectInput(inputId = session$ns("corr_type"),
                                                    label = "Associations",
                                                    choices = c("Positive" = "positive",
                                                                "Negative" = "negative",
                                                                "Positive and Negative" = "both"),
                                                    selected = "Positive")
                                        ),
                       actionButton(inputId = session$ns("update"), 
                                    label = "Update", 
                                    width = "100%"),
                       tags$br(),
                       width = 3),
          
          mainPanel(visNetworkOutput(outputId = session$ns("graph"), height = "70vh") %>% # 70vh corresponds to 70% the size of the viewing port
                      withSpinnerColor(plot_type = data()$type), #see shiny_helper.R
                    width = 9)
        )
      })
      
      output$graph <- 
        renderVisNetwork({
        if(data()$type == "gene") {
          shiny::validate(
            shiny::need(c("setup_graph") %in% data()$validate, "No dependency data for this gene"))
        } else if(data()$type == "pathway") {
          shiny::validate(
            shiny::need(c("setup_graph") %in% data()$validate, "No dependency data for this pathway"))
        } else if(data()$type == "cell") {
          shiny::validate(
            shiny::need(c("cell_dependency_sim") %in% data()$validate, "No dependency data for this cell line"))
        }
        networkGraph()
      })
    }
  )
}

geneNetworkGraphExp <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(h4(textOutput(ns("text_graph")))),
    uiOutput(ns("network_graph"))
  )
}

geneNetworkGraphExpServer <- function(id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$text_graph <- renderText({paste0("Co-expression network graph of ", 
                                              ifelse(data()$subtype == "pathway",
                                                     "pathway genes",
                                                     str_c(data()$content, collapse = ", ")
                                              )
      )
        })
      #establish reactive value
      rv <- reactiveValues(degree = 2, 
                           threshold = 10,
                           corr_type = "Positive" )
      
      #update value upon call
      observeEvent(input$update, {
        rv$degree <- input$deg
        rv$threshold <- input$threshold
        rv$corr_type <- input$corr_type
      })
      
      networkGraph <- reactive({
        make_graph(input = data(), 
                   threshold = rv$threshold, 
                   deg = rv$degree, 
                   corr_type = rv$corr_type, 
                   cell_line_var = "expression",
                   displayHeight = '80vh', 
                   displayWidth = '100%', 
                   tooltipLink = TRUE) %>% 
          visLegend(position = "right", width = .25, zoom = F) #.5 fixes cut-off, but makes it too wide
      })
      
      output$network_graph <- renderUI({
        sidebarLayout(
          sidebarPanel(sliderInput(inputId = session$ns("deg"),
                                   label = "Filter connections (<)",
                                   value = 2, min = 1, max = 10),
                       sliderInput(inputId = session$ns("threshold"),
                                   label = paste0("# Related ", 
                                                  ifelse(data()$type == "gene",
                                                         "gene", "cell line")),
                                   value =10, min = 10, max = 20),
                       selectInput(inputId = session$ns("corr_type"),
                                   label = "Associations",
                                   choices = c("Positive", "Negative", "Positive and Negative"),
                                   selected = "Positive"),
                       actionButton(inputId = session$ns("update"), 
                                    label = "Update", 
                                    width = "100%"),
                       tags$br(),
                       width = 3),
          
          mainPanel(visNetworkOutput(outputId = session$ns("graph"), height = "70vh") %>% # 70vh corresponds to 70% the size of the viewing port
                      withSpinnerColor(plot_type = data()$type), #see shiny_helper.R
                    width = 9)
        )
      })
      
      output$graph <- renderVisNetwork({
        if(data()$type == "gene") {
          shiny::validate(
            shiny::need(c("gene_master_top_table", "gene_master_bottom_table") %in% data()$validate, "No expression data for this gene"))
        } else if(data()$type == "cell") {
          shiny::validate(
            shiny::need(c("cell_expression_sim") %in% data()$validate, "No expression data for this cell line"))
        }
        networkGraph()
      })
    }
  )
}

####

compoundNetworkGraph <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(h4(textOutput(ns("text_compound_graph")))),
    sidebarLayout(
      sidebarPanel(sliderInput(inputId = ns("deg"),
                               label = "Filter connections (<)",
                               value = 2, min = 1, max = 10),
                   sliderInput(inputId = ns("threshold"),
                               label = "# Related genes",
                               value =10, min = 10, max = 20),
                   selectInput(inputId = ns("corr_type"),
                               label = "Correlations",
                               choices = c("Positive", "Negative", "Positive and Negative"),
                               selected = "Positive"),
                   actionButton(inputId = ns("update"), 
                                label = "Update", 
                                width = "100%"),
                   tags$br(),
                   width = 3),
      mainPanel(visNetworkOutput(outputId = ns("compound_graph"), height = "70vh") %>% # 70vh corresponds to 70% the size of the viewing port
                  withSpinnerColor(plot_type = "compound"), #see shiny_helper.R
                width = 9)
    )
  )
}

compoundNetworkGraphServer <- function(id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$text_compound_graph <- renderText({paste0("Network graph of ", 
                                                       ifelse(data()$subtype == "pathway",
                                                              "pathway genes",
                                                              str_c(data()$content, collapse = ", ")
                                                       )
      )
        })
      #establish reactive value
      rv <- reactiveValues(degree = 2, 
                           threshold = 10,
                           corr_type = "Positive" )
      
      #update value upon call
      observeEvent(input$update, {
        rv$degree <- input$deg
        rv$threshold <- input$threshold
        rv$corr_type <- input$corr_type
      })
      
      networkCompoundGraph <- reactive({
        make_graph(input = data(), 
                   threshold = rv$threshold, 
                   deg = rv$degree, 
                   corr_type = rv$corr_type, 
                   displayHeight = '80vh', 
                   displayWidth = '100%', 
                   tooltipLink = TRUE) %>% 
          visLegend(position = "right", width = .25, zoom = F) #.5 fixes cut-off, but makes it too wide
      })
      
      output$compound_graph <- renderVisNetwork({
        shiny::validate(
          shiny::need(c("compound_prism_cor_nest") %in% data()$validate, "No viability data for this compound"))
        networkCompoundGraph()
      })
    }
    
  )
}

geneBipartiteGraph <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(h4(textOutput(ns("text_graph")))),
    sidebarLayout(
      sidebarPanel(sliderInput(inputId = ns("threshold"),
                               label = "# Related genes",
                               value =10, min = 10, max = 20),
                   selectInput(inputId = ns("corr_type"),
                               label = "Correlations",
                               choices = c("Positive", "Negative", "Positive and Negative"),
                               selected = "Positive"),
                   checkboxInput(inputId = ns("simplify"), 
                                 label = "Simplify",
                                 value = TRUE),
                   actionLink(inputId = ns("censor_click"), "Censor"), # conditional panel for censor list
                   conditionalPanel(condition = paste0("input['", ns("censor_click"), "'] != 0"), 
                                    checkboxGroupInput(
                                      inputId = ns("censor"), 
                                      label = NULL, 
                                      choices = c("Water", "Adenosine triphosphate", "ADP", "NAD", "NADH", "Oxygen"), 
                                      selected = c("Water"))),
                   actionButton(inputId = ns("update"), 
                                label = "Update", 
                                width = "100%"),
                   tags$br(),
                   width = 3),
      mainPanel(visNetworkOutput(outputId = ns("graph")) %>%
                  withSpinnerColor(plot_type = "gene"), #see shiny_helper.R
                width = 9)
    )
  )
}

geneBipartiteGraphServer <- function(id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$text_graph <- renderText({paste0("Bipartite network graph of ", 
                                              ifelse(data()$subtype == "pathway",
                                                     "pathway genes",
                                                     str_c(data()$content, collapse = ", ")
                      
                                                                             )
      )
        })
      
      #event to store the censor 'click'
      observeEvent(input$censor_click, { 
      })
      #establish reactive value
      rv <- reactiveValues(threshold = 10,
                           corr_type = "Positive", 
                           collapsed = TRUE)
      
      #update value upon call
      observeEvent(input$update, {
        rv$threshold <- input$threshold
        rv$corr_type <- input$corr_type
        rv$collapsed <- input$simplify
        rv$censor <- input$censor
      })
      
      bipartiteNetworkGraph <- reactive({
        make_bipartite_graph(input = data(), 
                             threshold = rv$threshold, 
                             corr_type = rv$corr_type, 
                             collapsed = rv$collapsed, 
                             censor = rv$censor) %>% 
          visLegend(position = "right", width = .2, zoom = F) #.5 fixes cut-off, but makes it too wide
      })
      
      output$graph <- renderVisNetwork({
        shiny::validate(
          shiny::need(c("compound_hmdb_proteins") %in% data()$validate, "No compound data for this gene"))
        bipartiteNetworkGraph()
      })
    }
    
  )
}

compoundBipartiteGraph <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(h4(textOutput(ns("text_graph")))),
    fluidRow(visNetworkOutput(outputId = ns("graph"))  %>% 
               withSpinnerColor(plot_type = "compound") #see shiny_helper.R
    )
  )
}

compoundBipartiteGraphServer <- function(id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$text_graph <- renderText({paste0("Bipartite network graph of ", 
                                              ifelse(data()$subtype == "pathway",
                                                     "pathway genes",
                                                     str_c(data()$content, collapse = ", ")
                                              )
      )
        })
      
      bipartiteNetworkGraph <- reactive({
        make_bipartite_graph(input = data()) %>% 
          visLegend(position = "right", width = .2, zoom = F) #.5 fixes cut-off, but makes it too wide
      })
      
      output$graph <- renderVisNetwork({
        shiny::validate(
          shiny::need(c("compound_hmdb_metabolites") %in% data()$validate, "No viability data for this compound"))
        bipartiteNetworkGraph()
      })
    }
  )
}

