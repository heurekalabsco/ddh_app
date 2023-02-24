#LOAD LIBRARIES-----
source(here::here("code", "install_libraries.R"))

#DDH PARAMS-----
source(here::here("code", "app_params.R"), local = TRUE)

#ESTABLISH PRIVATE-----
source(here::here("code", "private.R"))

#DOWNLOAD/LOAD DATA-----
# Need to set the cache before loading any data
# content_cache <- cachem::cache_mem()
source(here::here("code", "data.R"))

#FUNCTIONS-----
#common functions
source(here::here("code", "fun_search.R"), local = TRUE)

#SHINY FUNCTIONS-----
source(here::here("code", "shiny_helper.R"), local = TRUE)
source(here::here("code", "shiny_tables.R"), local = TRUE)
source(here::here("code", "shiny_plots.R"), local = TRUE)
source(here::here("code", "shiny_graphs.R"), local = TRUE)
source(here::here("code", "shiny_reports.R"), local = TRUE)
source(here::here("code", "shiny_text.R"), local = TRUE)
source(here::here("code", "shiny_cards.R"), local = TRUE)
source(here::here("code", "shiny_download.R"), local = TRUE)
source(here::here("code", "shiny_search.R"), local = TRUE)

# HEAD----
head_tags <- tags$head(includeCSS("styles.css")) # includeHTML("gtag.html") 

### universal elements
main_title <- HTML('<a href="." style="color:black;">DATA-DRIVEN HYPOTHESIS</a>')
window_title <- "Data-Driven Hypothesis | Accelerate Scientific Discovery"

ddhNavbarPage <- function(..., formContent = NULL, id = NULL) {
  title_with_form <- tagList(
    main_title,
    tags$div(class="ddh-nav-form", formContent)
  )
  navbarPage(title = title_with_form, id = id, windowTitle = window_title, ...)
}

### list of all pages rendered by this app
page_names <- list(
  home="home",
  search="search",
  gene="gene",
  pathway="pathway",
  gene_list="gene_list", 
  cell="cell", 
  lineage="lineage",
  cell_list="cell_list",
  compound="compound",
  moa="moa",
  metabolite="metabolite",
  compound_list="compound_list"
)

#HOME PAGE----
homePage <- function (id) {
  ns <- NS(id)
  tagList(
    head_tags,
    HTML('<center><br><br><img src="ddh_logo.png", width = "338" ></center>'),
    tags$div(
      tags$br(),
      HTML("<center>Data-driven hypothesis is a resource for predicting functional relationships for thousands of genes across the human genome to accelerate scientific discovery.</center>"), 
      tags$br(),
      tags$br()),
    HTML("<center>"),
    querySearchInput(ns("search")), 
    exampleSearchesLink(ns("examples")), 
    ", ", 
    HTML("<a href='/methods/start-here.html'>read the manual</a>"),
    ", or",
    getLuckyLink(ns("lucky")),
    HTML("</center>"),
    exampleSearchesPanel(ns("examples"))
  )
}

homePageServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      querySearchServer("search")
      getLuckyServer("lucky")
      exampleSearchesLinkServer("examples")
      exampleSearchesPanelServer("examples")
    }
  )
}

##Search----
# module to input search term and navigate to the search screen
querySearchInput <- function(id) {
  ns <- NS(id)
  searchInput(
    inputId = ns("gene_or_pathway"),
    placeholder = "genes, pathways, or GO number", #change to genes, cells, or compounds
    btnSearch = icon("search")
  )
}

querySearchServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      observeEvent(input$gene_or_pathway, {
        if (input$gene_or_pathway != '') {
          updateQueryString(paste0("?show=search&query=", input$gene_or_pathway), mode="push")
        }
      })
    }
  )
}

##EXAMPLE SEARCHES----
# module to display a list of example searches

exampleSearchesLink <- function(id) {
  ns <- NS(id)
  actionLink(inputId = ns("example_click"), "See example searches")
}

exampleSearchesLinkServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      observeEvent(input$example_click, {}) # event to store the 'click'
    }
  )
}

exampleSearchesPanel <- function(id) {
  ns <- NS(id)
  source(here::here("code", "examples.R")) #pull out so methods can use it
  notZeroConditionalPanel(ns("example_click"), #toggle?
                          tagList(
                            tags$br(),
                            h3("Examples"),
                            HTML(examples)#,
                            #browsePathwaysLink(ns("pathways")),
                            #browsePathwaysPanel(ns("pathways"))
                          )
  )
}

exampleSearchesPanelServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      browsePathwaysLinkServer("pathways")
      browsePathwaysPanelServer("pathways")
    }
  )
}

##LUCKY GENE----
# module to display a random interesting gene and navigate to the detail screen for that gene
getLuckyLink <- function(id) {
  ns <- NS(id)
  htmlOutput(ns("get_lucky"), inline = TRUE)
}

surprise <- function(data_gene_surprise) {
  gene_symbol <- sample(data_gene_surprise[[1]], 1)
  gene_symbol_url <- paste0("?show=gene&query=", gene_symbol)
  return(gene_symbol_url)
}

getLuckyServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      output$get_lucky <- renderUI({
        tags$a(href = surprise(gene_surprise), "get lucky")
      })
    }
  )
}

# SEARCH PAGE ----
searchPage <- function (id) {
  ns <- NS(id)
  tagList(
    head_tags,
    ddhNavbarPage(formContent=querySearchInput(ns("search"))),
    h3(textOutput("search_title")),
    div(div(h3("Results", class="panel-title"), class="panel-heading"),
        div(uiOutput(ns("genes_search_result")), class="panel-body"),
        class="bg-info panel panel-default"
    )
  )
}



searchPageServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      querySearchServer("search")
      output$search_title <- renderText({
        query_string <- getQueryString()
        paste0("Search results for '", query_string$query, "'")
      })
      output$genes_search_result <- renderUI({
        query_str <- getQueryString()$query
        search_result <- search_query(search_index, query_str)
        search_index_rows <- search_result$rows
        if (nrow(search_index_rows) > 0) {
          if (search_result$multi_query) {
            apply(search_index_rows, 1, function (x) { multi_query_result_row(x, search_result$multi_items)})
          } else {
            apply(search_index_rows, 1, query_result_row)
          }
        } else {
          "No results found."
        }
      })
    }
  )
}



# PAGE MODULES-----
source(here::here("code", "page_gene.R"), local = TRUE) ### GENE PAGE ----
source(here::here("code", "page_cell.R"), local = TRUE) ### CELL PAGE ----
source(here::here("code", "page_compound.R"), local = TRUE) ### COMPOUND PAGE ----

# Create output for our router in main UI of Shiny app.
ui <- shinyUI(
  fluidPage(
    uiOutput("pageUI")
  )
)

pages <- list(
  home=homePage(page_names$home),
  search=searchPage(page_names$search),
  gene=genePage(page_names$gene, subtype = "gene"),
  pathway=genePage(page_names$pathway, subtype = "pathway"),
  gene_list=genePage(page_names$gene_list, subtype = "gene_list"),
  cell=cellPage(page_names$cell, subtype = "cell"),
  lineage=cellPage(page_names$lineage, subtype = "lineage"),
  lineage_subtype=cellPage(page_names$lineage_subtype, subtype = "lineage_subtype"),
  cell_list=cellPage(page_names$cell_list, subtype = "cell_list"),
  compound=compoundPage(page_names$compound, subtype = "compound"),
  moa=compoundPage(page_names$moa, subtype = "moa"),
  metabolite=compoundPage(page_names$metabolite, subtype ="metabolite"),
  compound_list=compoundPage(page_names$compound_list, subtype = "compound_list")
)

server <- shinyServer(function(input, output, session) {
  options(shiny.usecairo=TRUE) # ensure high quality images
  output$pageUI <- renderUI({
    query_string <- getQueryString()
    show_page <- query_string$show
    if (is.null(show_page)) {
      show_page <- page_names$home
    }
    pages[show_page]
  })
  homePageServer(page_names$home)
  searchPageServer(page_names$search)
  genePageServer(page_names$gene, subtype = "gene")
  genePageServer(page_names$pathway, subtype = "pathway")
  genePageServer(page_names$gene_list, subtype = "gene_list")
  cellPageServer(page_names$cell, subtype = "cell")
  cellPageServer(page_names$lineage, subtype = "lineage")
  cellPageServer(page_names$lineage_subtype, subtype = "lineage_subtype")
  cellPageServer(page_names$cell_list, subtype = "cell_list")
  compoundPageServer(page_names$compound, subtype = "compound")
  compoundPageServer(page_names$metabolite, subtype = "metabolite")
  compoundPageServer(page_names$moa, subtype = "moa")
  compoundPageServer(page_names$compound_list, subtype = "compound_list")
  # session$onSessionEnded(function() {
  #   delete_tmp_zip_directory(session)
  # })
})

shinyApp(ui, server)
