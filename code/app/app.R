#LOAD LIBRARIES-----
source(here::here("code", "app", "install_libraries.R"))

#DDH PARAMS-----
source(here::here("code", "app", "app_params.R"), local = TRUE)

#ESTABLISH PRIVATE-----
source(here::here("code", "app", "private.R"))

#DOWNLOAD/LOAD DATA-----
# Need to set the cache before loading any data
# content_cache <- cachem::cache_mem()
source(here::here("code", "app", "data.R"))

#FUNCTIONS-----
# ddh::load_ddh_colors() # No need anymore
source(here::here("code", "app", "fun_search.R"), local = TRUE)

#SHINY FUNCTIONS-----
source(here::here("code", "app", "shiny_helper.R"), local = TRUE)
source(here::here("code", "app", "shiny_tables.R"), local = TRUE)
source(here::here("code", "app", "shiny_plots.R"), local = TRUE)
source(here::here("code", "app", "shiny_graphs.R"), local = TRUE)
source(here::here("code", "app", "shiny_reports.R"), local = TRUE)
source(here::here("code", "app", "shiny_text.R"), local = TRUE)
source(here::here("code", "app", "shiny_cards.R"), local = TRUE)
source(here::here("code", "app", "shiny_download.R"), local = TRUE)
source(here::here("code", "app", "shiny_search.R"), local = TRUE)

# HEAD----
if(privateMode == TRUE) {
  head_tags <- tags$head(includeCSS("styles.css"),
                         tags$script(src="outseta.js"),
                         tags$script(src="gtag_com.js"))
} else {
  head_tags <- tags$head(includeCSS("styles.css"),
                         tags$script(src="gtag_org.js"))
}

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
    placeholder = dplyr::if_else(privateMode == TRUE, private_searchbox, "genes, pathways, or a custom list"), #"genes, cells, or compounds"
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
  source(here::here("code", "app", "examples.R")) #pull out so methods can use it
  notZeroConditionalPanel(ns("example_click"), #toggle?
                          tagList(
                            tags$br(),
                            h3("Examples"),
                            HTML(examples),
                            HTML(example_pathways),
                            tags$ul(tags$li(browsePathwaysLink(ns("pathways")))),
                            browsePathwaysPanel(ns("pathways"))
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

##Browse Pathways----
# module that displays a table of pathways when an link is clicked

browsePathwaysLink <- function (id) {
  ns <- NS(id)
  actionLink(inputId = ns("pathway_click"), "Browse pathways")
}

browsePathwaysLinkServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      observeEvent(input$pathway_click, {}) #event to store the 'click'
    }
  )
}

browsePathwaysPanel <- function (id) {
  ns <- NS(id)
  notZeroConditionalPanel(ns("pathway_click"),
                          tags$br(),
                          h4("GO Biological Processes"),
                          DT::dataTableOutput(outputId = ns("pathway_table")))
}

browsePathwaysPanelServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      output$pathway_table <- DT::renderDataTable({
        DT::datatable(make_pathway_browse_table() %>% 
                        #remove some columns for now, for simplicity
                        dplyr::select(-tidyselect::any_of(c("gs_geoid", "gs_exact_source", "gs_url", "gs_pmid"))) %>% 
                        dplyr::arrange(gs_name) %>% 
                        #dplyr::mutate(gs_pmid = purrr::map_chr(gs_pmid, pubmed_linkr, number_only = TRUE)) %>% #from shiny_helper.R
                        dplyr::rename(Pathway = gs_name, 
                                      Description = gs_description, 
                                      ID = gs_id, 
                                      #PMID = gs_pmid, 
                                      #GEO = gs_geoid, 
                                      #Source = gs_exact_source, 
                                      #URL = gs_url, 
                                      `Pathway Size` = pathway_size), 
                      escape = FALSE,
                      options = list(pageLength = 10))
      })      
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
    div(
      div(id = "search_results_div", class="panel-heading",
        h3("Search Results", make_tooltip("These are the search results from your query. Click on any link below to see the data about it.")
           , class="panel-title")),
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
source(here::here("code", "app", "page_gene.R"), local = TRUE) ### GENE PAGE ----
source(here::here("code", "app", "page_cell.R"), local = TRUE) ### CELL PAGE ----
source(here::here("code", "app", "page_compound.R"), local = TRUE) ### COMPOUND PAGE ----
source(here::here("code", "app", "waiting_quotes.R"), local = TRUE) ### WAITING QUOTES ----

# Create output for our router in main UI of Shiny app.
ui <- shinyUI(
  fluidPage(
    waiter::useWaiter(),
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
  options(shiny.usecairo = TRUE) # ensure high quality images
  
  # call the waiter
  waiter::waiter_show(html = tagList(
    waiter::spin_loaders(id = 3, color = "#2EC09C", style = NULL),
    h4(HTML(paste0('<p style="font-family:Roboto Slab; color:#2EC09C; ">', sample(waiting_quotes, 1), '</p>'))) # font-size: 20px;
  ), color = "#ffffff")
  
  # serve the page
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
  
  # stop the waiter
  waiter::waiter_hide()
  
})

shinyApp(ui, server)

