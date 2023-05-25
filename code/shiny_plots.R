# GENE QUERY-----
## GENE----
## Ideogram --------------------------------------------------------
ideogramPlot <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(h4(textOutput(ns("ideogram_plot_text")))),
    uiOutput(outputId = ns("ideogram_plot"), height = "auto"),
    tags$br(),
    fluidRow(ddh::make_legend("make_ideogram"))
  )
}

ideogramPlotServer <- function (id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$ideogram_plot_text <- renderText({
        paste0("Chromosomal location for ", stringr::str_c(data()$content, collapse = ", "))
      })
      output$ideogram_plot <- renderUI({
        #check to see if data are there
        shiny::validate(
          shiny::need(c("gene_location") %in% data()$validate, "No data found for this gene."))
        #check to see if image exists
        img_path <- ddh::load_image(input = data(), fun_name = "make_ideogram", card = FALSE)
        if(!is.null(img_path)) {
          uiOutput(outputId = session$ns("ideogram_plot_image"), width = "100%")
        } else {
          plotOutput(outputId = session$ns("ideogram_plot_render"), 
                     width = "100%") %>%
            withSpinnerColor(plot_type = data()$type)
        }
      })
      output$ideogram_plot_image <- renderUI({
        div(tags$img(src = load_image(input = data(), fun_name = "make_ideogram", card = FALSE), 
                     width = "100%"))
      })
      output$ideogram_plot_render <- renderPlot({
        make_ideogram(input = data())
      })
    }
  )
}

## PROTEIN----
## protein size --------------------------------------------------------
proteinSizePlot <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(h4(textOutput(ns("protein_size_plot_text")))),
    uiOutput(outputId = ns("protein_size_plot"), height = "auto"),
    tags$br(),
    fluidRow(ddh::make_legend("make_proteinsize"))
  )
}

proteinSizePlotServer <- function (id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$protein_size_plot_text <- renderText({paste0("Protein size for ", str_c(data()$content, collapse = ", "))})
      output$protein_size_plot <- renderUI({
        #check to see if data are there
        shiny::validate(
          shiny::need(c("universal_proteins") %in% data()$validate, "No data found."))
        #check to see if image exists
        img_path <- ddh::load_image(input = data(), fun_name = "make_proteinsize", card = FALSE)
        if(!is.null(img_path)) {
          uiOutput(outputId = session$ns("protein_size_plot_image"), width = "100%")
        } else {
          plotOutput(outputId = session$ns("protein_size_plot_render"), 
                     width = "100%") %>%
            withSpinnerColor(plot_type = data()$type)
        }
      })
      output$protein_size_plot_image <- renderUI({
        div(tags$img(src = load_image(input = data(), fun_name = "make_proteinsize", card = FALSE), 
                     width = "100%"))
      })
      output$protein_size_plot_render <- renderPlot({
        make_proteinsize(input = data())
      },
      height = function() length(data()$content) * 120 + 60)
    }
  )
}

## protein structure --------------------------------------------------------
proteinStructurePlot <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(uiOutput(outputId = ns("protein_structure"), height = "auto")),
    tags$br(),
    fluidRow(ddh::make_legend("make_structure"))
  )
}

proteinStructurePlotServer <- function (id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$text_protein_structure <- renderText({paste0(str_c(data()$content[[1]], " Predicted Structure", collapse = ", "))})
      output$protein_structure <- renderUI({
        div(
          tags$img(src = make_structure(input = data(), card = FALSE), 
                   width = "100%",
                   alt = glue::glue('Protein structure rendering in the style of a black and white line drawing'))
        )
      })
    }
  )
}

## protein 3D structure --------------------------------------------------------
proteinStructurePlot3d <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(h4(textOutput(ns("text_protein_structure3d")))),
    tags$br(),
    fluidRow(
      column(8,
             r3dmol::r3dmolOutput(outputId = ns("protein_structure3D"),
                                  height = "600px") %>% 
               withSpinnerColor(plot_type = "protein") #see shiny_helper.R
      ),
      column(4,
             uiOutput(ns("gene_ui")),
             h4("Visualization parameters:"),
             shinyWidgets::prettySwitch(inputId = ns("color3dstructure"), 
                                        "Color structure", value = FALSE),
             
             shinyWidgets::prettySwitch(inputId = ns("ribbon3dstructure"), 
                                        "Ribbon structure", value = FALSE),
             
             shinyWidgets::prettySwitch(inputId = ns("selection3dstructure"), 
                                        "Select a region", value = FALSE),
             
             conditionalPanel(condition = paste0("input['", ns("selection3dstructure"), "']"),
                              textInput(ns("residue"), 
                                        label = "Highlight residues:",
                                        placeholder = "e.g., 1:10"),
                              textInput(ns("chain"), 
                                        label = "Chain:",
                                        placeholder = "e.g., A")
             ),
             actionButton(inputId = ns("update3d"), 
                          label = "Update plot")
      )
    ),
    tags$br(),
    fluidRow(ddh::make_legend("make_structure3d"),
             actionLink(inputId = ns("pdb_table_click"), " View more PDB models for my protein/s")),
    ## TABLE
    conditionalPanel(condition = paste0("input['", ns("pdb_table_click"), "'] != 0"),
                     fluidRow(h4(textOutput(ns("title_structure3d_table")))),
                     DT::dataTableOutput(outputId = ns("structure3d_table"))
    )
  )
}

proteinStructurePlot3dServer <- function (id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      
      output$gene_ui <- renderUI({
        
        gene_options <- 
          ddh::make_structure3d_table(input = data()) %>%
          dplyr::pull(id) %>%
          unique()
        
        selectizeInput(session$ns("gene3dstructure"),
                       "Protein",
                       choices = gene_options[order(match(gene_options, data()$content))]
        )
      })
      
      ## TABLE
      output$title_structure3d_table <- renderText({paste0("PDB table for ", str_c(input$gene3dstructure, collapse = ", "))})
      output$structure3d_table <- DT::renderDataTable({
        shiny::validate(
          shiny::need(c("gene_pdb_table") %in% data()$validate, "No PDB data found."))
        
        DT::datatable(
          ddh::make_structure3d_table(input = data()) %>%
            dplyr::mutate(pdb = pdb_linkr(pdb)) %>% 
            dplyr::select('Query' = id, 
                          'PDB ID' = pdb,
                          'Name' = title, 
                          'Organism' = organism) %>% 
            dplyr::filter(Query == input$gene3dstructure),
          escape = FALSE,
          options = list(pageLength = 10),
          selection = c("single"),
        )
      })
      
      ## PLOT
      rv <- reactiveValues(#gene3dstructure = NULL,
                           pdb3dstructure = NULL,
                           color3dstructure = FALSE,
                           ribbon3dstructure = FALSE,
                           selection3dstructure = FALSE,
                           residue = 1:10,
                           chain = "A")
      
      #update value upon call
      observeEvent(input$update3d, {
        
        if(!is.null(input$structure3d_table_rows_selected)){
          pdb3dstructure <- 
            ddh::make_structure3d_table(input = data()) %>% 
            dplyr::slice(input$structure3d_table_rows_selected) %>% 
            dplyr::pull(pdb)
        } else {
          pdb3dstructure <- NULL
        }
        
        #rv$gene3dstructure <- input$gene3dstructure 
        rv$pdb3dstructure <- pdb3dstructure
        rv$color3dstructure <- input$color3dstructure
        rv$ribbon3dstructure <- input$ribbon3dstructure
        rv$selection3dstructure <- input$selection3dstructure
        rv$residue <- input$residue
        rv$chain <- input$chain
      })
      
      plot3dprotein <- reactive({
        ddh::make_structure3d(input = data(),
                              #gene_id = rv$gene3dstructure,
                              pdb_id = rv$pdb3dstructure,
                              color = rv$color3dstructure,
                              ribbon = rv$ribbon3dstructure,
                              selection = rv$selection3dstructure,
                              resi = rv$residue,
                              chain = rv$chain)
      })
      
      output$text_protein_structure3d <- renderText({paste0("Predicted 3D Structure for ", str_c(input$gene3dstructure, collapse = ", "))})
      output$protein_structure3D <- r3dmol::renderR3dmol({
        shiny::validate(
          shiny::need(c("gene_pdb_table") %in% data()$validate, "No data found."))
        plot3dprotein()
      })
    }
  )
}

## AA radial plot --------------------------------------------------------
radialPlot <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(h4(textOutput(ns("text_radial_plot")))),
    shinyWidgets::prettySwitch(inputId = ns("mean_relative"), 
                               "Show relative frequency to the mean", 
                               value = TRUE),
    fluidRow(plotOutput(outputId = ns("radial_plot"), height = "auto") %>% 
               withSpinnerColor(plot_type = "protein") #see shiny_helper.R
    )
  )
}

radialPlotServer <- function (id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$text_radial_plot <- renderText({paste0("Amino Acid Signature for ", str_c(data()$content, collapse = ", "))})
      output$radial_plot <- renderPlot({
        shiny::validate(
          shiny::need(c("universal_proteins") %in% data()$validate, "No data found."))
        make_radial(input = data(),
                    relative = input$mean_relative,
                    cluster = FALSE,
                    barplot = FALSE)
      },
      height = 550)
    }
  )
}

## AA bar plot --------------------------------------------------------
AABarPlot <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(ddh::make_legend("make_radial"), # this belongs to RADIAL PLOT
             actionLink(inputId = ns("aa_bar_click"), " View bar plot of amino acid signatures")),
    tags$br(), 
    conditionalPanel(condition = paste0("input['", ns("aa_bar_click"), "'] != 0"), 
                     fluidRow(h4(textOutput(ns("text_aa_bar_plot")))),
                     shinyWidgets::prettySwitch(inputId = ns("bar_mean_relative"), 
                                                "Show relative frequency to the mean", 
                                                value = TRUE),
                     fluidRow(plotOutput(outputId = ns("aa_bar_plot"), height = "auto") %>% 
                                withSpinnerColor(plot_type = "protein") #see shiny_helper.R
                     ),
                     tags$br(),
                     fluidRow(ddh::make_legend("make_radial_bar"))
    )
  )
}

AABarPlotServer <- function (id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$text_aa_bar_plot <- renderText({paste0("Amino Acid Signature for ", str_c(data()$content, collapse = ", "))})
      output$aa_bar_plot <- renderPlot({
        shiny::validate(
          shiny::need(c("universal_proteins") %in% data()$validate, "No data found."))
        make_radial(input = data(),
                    relative = input$bar_mean_relative,
                    cluster = FALSE,
                    barplot = TRUE)
      },
      height = 550)
    }
  )
}

## cluster AA radial plot --------------------------------------------------------
clusterRadialPlot <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(h4(textOutput(ns("cluster_text_radial_plot")))),
    shinyWidgets::prettySwitch(inputId = ns("cluster_mean_relative"), 
                               "Show relative frequency to the mean", 
                               value = TRUE),
    fluidRow(plotOutput(outputId = ns("cluster_radial_plot"), height = "auto") %>% 
               withSpinnerColor(plot_type = "protein") #see shiny_helper.R
    ),
    tags$br()
  )
}

clusterRadialPlotServer <- function (id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$cluster_text_radial_plot <- renderText({
        
        clust_num <- 
          ddh::get_data_object(object_names = data()$content,
                               dataset_name = "gene_signature_clusters",
                               pivotwider = TRUE) %>%
          dplyr::pull(clust) %>%
          unique()
        
        shiny::validate(
          shiny::need(c("gene_signature_clusters") %in% data()$validate, "No data found."))
        
        title_text <- paste0("Amino Acid Signature for Cluster ", 
                             str_c(clust_num, collapse = ", "))
        
        return(title_text)
      })
      output$cluster_radial_plot <- renderPlot({
        shiny::validate(
          shiny::need(c("universal_proteins") %in% data()$validate, "No data found."))
        make_radial(input = data(),
                    relative = input$cluster_mean_relative,
                    cluster = TRUE,
                    barplot = FALSE)
      },
      height = 550)
    }
  )
}

## cluster AA bar plot --------------------------------------------------------
clusterAABarPlot <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(ddh::make_legend("make_radial"), # this belongs to RADIAL PLOT
             actionLink(inputId = ns("aa_bar_cluster_click"), " View bar plot of cluster amino acid signatures")),
    tags$br(), 
    conditionalPanel(condition = paste0("input['", ns("aa_bar_cluster_click"), "'] != 0"), 
                     fluidRow(h4(textOutput(ns("cluster_text_aa_bar_plot")))),
                     shinyWidgets::prettySwitch(inputId = ns("cluster_bar_mean_relative"), 
                                                "Show relative frequency to the mean", 
                                                value = TRUE),
                     fluidRow(plotOutput(outputId = ns("cluster_aa_bar_plot"), height = "auto") %>% 
                                withSpinnerColor(plot_type = "protein") #see shiny_helper.R
                     ),
                     tags$br(),
                     fluidRow(ddh::make_legend("make_radial_bar"))
    )
  )
}

clusterAABarPlotServer <- function (id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$cluster_text_aa_bar_plot <- renderText({
        
        clust_num <- make_clustering_table(input = data()) %>%
          dplyr::pull(clust) %>%
          unique()
        
        shiny::validate(
          shiny::need(c("gene_signature_clusters") %in% data()$validate, "No data found."))
        
        title_text <- paste0("Amino Acid Signature for Cluster ", 
                             str_c(clust_num, collapse = ", "))
        
        return(title_text)
      })
      output$cluster_aa_bar_plot <- renderPlot({
        shiny::validate(
          shiny::need(c("gene_signature_clusters") %in% data()$validate, "No data found."))
        make_radial(input = data(),
                    relative = input$cluster_bar_mean_relative,
                    cluster = TRUE,
                    barplot = TRUE)
      },
      height = 550)
    }
  )
}

## UMAP PLOT --------------------------------------------------------
UMAPPlot <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(actionLink(inputId = ns("umap_click"), " View UMAP embeddings plot")),
    tags$br(), 
    conditionalPanel(condition = paste0("input['", ns("umap_click"), "'] != 0"),
                     fluidRow(h4(textOutput(ns("text_umap_plot")))),
                     checkboxInput(inputId = ns("show_all_umap"), 
                                   label = "Show only selected clusters", value = FALSE),
                     checkboxInput(inputId = ns("labels_umap"), 
                                   label = "Show labels", value = FALSE),
                     fluidRow(plotOutput(outputId = ns("umap_plot"), height = "auto") %>% 
                                withSpinnerColor(plot_type = "protein") #see shiny_helper.R
                     ),
                     tags$br(),
                     fluidRow(ddh::make_legend("make_umap_plot"))
    )
  )
}

UMAPPlotServer <- function (id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$text_umap_plot <- renderText({
        shiny::validate(
          shiny::need(c("gene_signature_clusters") %in% data()$validate, "No data found."))
        
        clust_num <- ddh::get_cluster(input = data())
        
        title_text <- glue::glue('UMAP Embeddings for Cluster {stringr::str_c(clust_num, collapse = ", ")}')
        
        return(title_text)
      })
      output$umap_plot <- renderPlot({
        shiny::validate(
          shiny::need(c("universal_proteins") %in% data()$validate, "No data found."))
        
        clust_num <- ddh::get_cluster(input = data())
        
        make_umap_plot(input = data(),
                       show_subset = input$show_all_umap,
                       labels = input$labels_umap)
      },
      height = 550)
    }
  )
}

## cluster enrichment --------------------------------------------------------
clusterEnrichmentPlot <- function(id) {
  ns <- NS(id)
  uiOutput(outputId = ns("conditional_clusterenrichmentplot"))
}

clusterEnrichmentPlotServer <- function (id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$conditional_clusterenrichmentplot <- renderUI({
        if(!is.null(data()$content)) {
          sig_clust <- ddh::get_cluster(input = data())
          
          sig_clust_len <- 
            sig_clust %>% 
            length()
        } else {
          sig_clust <- 0
          sig_clust_len <- 0
        }
        
        if((sig_clust_len == 1) & (sig_clust != 0)) {
          tagList(
            fluidRow(h4(textOutput(session$ns("text_cluster_enrich_plot")))),
            selectizeInput(session$ns("ontology_info_plot"),
                           "Ontology",
                           choices = c("Biological Process (GO)" = "BP",
                                       "Molecular Function (GO)" = "MF",
                                       "Cellular Component (GO)" = "CC"), 
                           selected = "BP"),
            sliderInput(session$ns("num_terms"), "Number of terms to show",
                        min = 10, max = 40, value = 20),
            fluidRow(plotOutput(outputId = session$ns("cluster_enrichment_plot"), height = "auto") %>% 
                       withSpinnerColor(plot_type = "protein") #see shiny_helper.R
            ),
            tags$br(),
            fluidRow(ddh::make_legend("make_cluster_enrich"))
          )
        } else {
          return(NULL)
        }
        
      })
      output$text_cluster_enrich_plot <- renderText({
        shiny::validate(
          shiny::need(c("universal_proteins") %in% data()$validate, "No data found."))
        
        clust_num <- ddh::get_cluster(input = data())
        
        if(length(clust_num) == 1) {
          title_text <- paste0("Enrichment Analysis Plot for Cluster ", 
                               clust_num)
        } else{
          title_text <- NULL
        }
        
        return(title_text)
      })
      output$cluster_enrichment_plot <- renderPlot({
        
        clust_num <- ddh::get_cluster(input = data())
        
        shiny::validate(
          shiny::need(c("universal_proteins") %in% data()$validate, "No data found."),
          shiny::need(length(clust_num) == 1, 
                      "More than one cluster identified in the table above. Cluster enrichment analysis is only available for individual gene queries or multiple gene queries belonging to the same cluster."))
        make_cluster_enrich(input = data(),
                            ontology = input$ontology_info_plot,
                            num_terms = input$num_terms)
      },
      height = 550)
    }
  )
}

## protein domain --------------------------------------------------------
proteinDomainPlot <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(h4(textOutput(ns("text_protein_domain_plot")))),
    fluidRow(column(uiOutput(outputId = ns("dom_choice")), width = 6), 
             column(uiOutput(outputId = ns("ptm_choice")), width = 6)
    ),
    fluidRow(plotOutput(outputId = ns("protein_domain_plot"), height = "auto") %>% 
               withSpinnerColor(plot_type = "protein") #see shiny_helper.R
    ),
    tags$br(),
    fluidRow(ddh::make_legend("make_protein_domain")),
    tags$br()
  )
}

proteinDomainPlotServer <- function (id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$text_protein_domain_plot <- renderText({paste0("Protein Domain Plot for ", str_c(data()$content, collapse = ", "))})
      output$dom_choice <- renderUI({
        prots_dr <-
          get_data_object(object_names = data()$content,
                          dataset_name = "gene_protein_domains", 
                          pivotwider = TRUE) %>% 
          dplyr::filter(type %in% c("DOMAIN", "REGION")) %>%
          tidyr::drop_na(description)
        
        selectizeInput(session$ns("dom_var"), "Protein Features (select):", 
                       choices = prots_dr %>% 
                         distinct(description) %>% 
                         pull(description),
                       multiple = TRUE,
                       selected = prots_dr %>%  
                         distinct(description) %>% 
                         dplyr::slice(1) %>% 
                         pull(description)
        ) 
      })
      output$ptm_choice <- renderUI({
        prots_ptm <-
          get_data_object(object_names = data()$content,
                          dataset_name = "gene_protein_domains", 
                          pivotwider = TRUE) %>% 
          dplyr::filter(category == "PTM") %>%
          tidyr::drop_na(description)
        
        selectizeInput(session$ns("ptm_var"), "PTMs (select):", 
                       choices = prots_ptm %>%  
                         distinct(description) %>% 
                         pull(description),
                       multiple = TRUE,
                       selected = prots_ptm %>%  
                         distinct(description) %>% 
                         dplyr::slice(1) %>% 
                         pull(description)
        ) 
      })
      output$protein_domain_plot <- renderPlot({
        shiny::validate(
          shiny::need(c("gene_protein_domains") %in% data()$validate, "No data found."))
        make_protein_domain(input = data(),
                            dom_var = input$dom_var,
                            ptm_var = input$ptm_var)
      },
      height = function() length(data()$content) * 120 + 120
      )
    }
  )
}

## LIT----
## pubmed --------------------------------------------------------
pubmedPlot <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(h4(textOutput(ns("pubmed_plot_title")))),
    fluidRow(textOutput(ns("pubmed_plot_text"))),
    uiOutput(ns("pubmed_plot"), height = "auto"), 
    tags$br(),
    fluidRow(ddh::make_legend("make_pubmed"))
  )
}

pubmedPlotServer <- function (id, data, session) {
  moduleServer(
    id,
    function(input, output, session) {
      output$pubmed_plot_title <- renderText({paste0("Publication history for ", str_c(data()$content, collapse = ", "))})
      output$pubmed_plot_text <- renderText({
        num <- make_pubmed_table(input = data())
        glue::glue('{nrow(num)} annotated papers')
      })
      output$pubmed_plot <- renderUI({
        #check to see if data are there
        shiny::validate(
          shiny::need(c("universal_pubmed") %in% data()$validate, "No data found."))
        #check to see if image exists
        img_path <- ddh::load_image(input = data(), fun_name = "make_pubmed", card = FALSE)
        if(!is.null(img_path)) {
          uiOutput(outputId = session$ns("pubmed_plot_image"), width = "100%")
        } else {
          plotOutput(outputId = session$ns("pubmed_plot_render"), height = 550) %>%
            withSpinnerColor(plot_type = data()$type)
        }
      })
      output$pubmed_plot_image <- renderUI({
        div(tags$img(src = load_image(input = data(), fun_name = "make_pubmed", card = FALSE), 
                     width = "100%"))
      })
      output$pubmed_plot_render <- renderPlot({
        make_pubmed(input = data())
      })
    }
  )
}

pubmedCompoundPlot <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(h4(textOutput(ns("pubmed_compound_plot_title")))),
    fluidRow(textOutput(ns("pubmed_compound_plot_text"))),
    uiOutput(ns("pubmed_compound_plot"), height = "auto"), 
    tags$br(),
    fluidRow(ddh::make_legend("make_pubmed"))
  )
}

pubmedCompoundPlotServer <- function (id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$pubmed_compound_plot_title <- renderText({paste0("Publication history for ", str_c(data()$content, collapse = ", "))})
      output$pubmed_compound_plot_text <- renderText({
        num <- make_pubmed_table(input = data())
        glue::glue('{nrow(num)} annotated papers')
      })
      output$pubmed_compound_plot <- renderUI({
        #check to see if data are there
        shiny::validate(
          shiny::need(c("universal_pubmed") %in% data()$validate, "No data found."))
        #check to see if image exists
        img_path <- ddh::load_image(input = data(), fun_name = "make_pubmed", card = FALSE)
        if(!is.null(img_path)) {
          uiOutput(outputId = session$ns("pubmed_compound_plot_image"), width = "100%")
        } else {
          plotOutput(outputId = session$ns("pubmed_compound_plot_render"), height = 550) %>%
            withSpinnerColor(plot_type = data()$type)
        }
      })
      output$pubmed_compound_plot_image <- renderUI({
        div(tags$img(src = load_image(input = data(), fun_name = "make_pubmed", card = FALSE), 
                     width = "100%"))
      })
      output$pubmed_compound_plot_render <- renderPlot({
        make_pubmed(input = data())
      })
    }
  )
}

pubmedCellLinePlot <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(h4(textOutput(ns("pubmed_cell_plot_title")))),
    fluidRow(textOutput(ns("pubmed_cell_plot_text"))),
    uiOutput(ns("pubmed_cell_plot"), height = "auto"), 
    tags$br(),
    fluidRow(ddh::make_legend("make_pubmed"))
  )
}

pubmedCellLinePlotServer <- function (id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$pubmed_cell_plot_title <- renderText({paste0("Publication history for ", str_c(data()$content, collapse = ", "))})
      output$pubmed_cell_plot_text <- renderText({
        num <- make_pubmed_table(input = data())
        glue::glue('{nrow(num)} annotated papers')
      })
      output$pubmed_cell_plot <- renderUI({
        #check to see if data are there
        shiny::validate(
          shiny::need(c("universal_pubmed") %in% data()$validate, "No data found."))
        #check to see if image exists
        img_path <- ddh::load_image(input = data(), fun_name = "make_pubmed", card = FALSE)
        if(!is.null(img_path)) {
          uiOutput(outputId = session$ns("pubmed_cell_plot_image"), width = "100%")
        } else {
          plotOutput(outputId = session$ns("pubmed_cell_plot_render"), height = 550) %>%
            withSpinnerColor(plot_type = data()$type)
        }
      })
      output$pubmed_cell_plot_image <- renderUI({
        div(tags$img(src = load_image(input = data(), fun_name = "make_pubmed", card = FALSE), 
                     width = "100%"))
      })
      output$pubmed_cell_plot_render <- renderPlot({
        make_pubmed(input = data())
      })
    }
  )
}

## EXPRESSION----
## anatograms -----
cellAnatogramPlot <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(h4(textOutput(ns("cell_anatogram_gene_plot_text")))),
    uiOutput(outputId = ns("cell_anatogram_gene_plot"), height = "auto"),
    tags$br(),
    fluidRow(ddh::make_legend("make_cellanatogram"))
  )
}

cellAnatogramPlotServer <- function(id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$cell_anatogram_gene_plot_text <- renderText({
        paste0("Subcellular expression of ", str_c(data()$content, collapse = ", "))
      })
      output$cell_anatogram_gene_plot <- renderUI({
        #check to see if data are there
        shiny::validate(
          shiny::need(c("gene_subcell") %in% data()$validate, "No data found."))
        #check to see if image exists
        img_path <- ddh::load_image(input = data(), fun_name = "make_cellanatogram", card = FALSE)
        if(!is.null(img_path)) {
          uiOutput(outputId = session$ns("cell_anatogram_gene_plot_image"), width = "100%")
        } else {
          plotOutput(outputId = session$ns("cell_anatogram_gene_plot_render"), 
                     width = "100%") %>%
            withSpinnerColor(plot_type = data()$type)
        }
      })
      output$cell_anatogram_gene_plot_image <- renderUI({
        div(tags$img(src = load_image(input = data(), fun_name = "make_cellanatogram", card = FALSE), 
                     width = "100%"))
      })
      output$cell_anatogram_gene_plot_render <- renderPlot({
        make_cellanatogram(input = data())
      })
    }
  )
}

cellAnatogramFacetPlot <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(actionLink(inputId = ns("anato_facet_click"), "View detailed cell anatograms below")),
    tags$br(),
    conditionalPanel(condition = paste0("input['", ns("anato_facet_click"), "'] != 0"),
                     plotOutput(ns("cell_anatogram_gene_facet"))),
    tags$br(),
  )
}

cellAnatogramFacetPlotServer <- function(id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$cell_anatogram_gene_facet <- renderPlot({
        #check to see if data are there
        shiny::validate(
          shiny::need(c("gene_subcell") %in% data()$validate, "No data found."))
        #render plot
        make_cellanatogramfacet(input = data())
      })
    }
  )
}

maleAnatogramPlot <- function(id) {
  ns <- NS(id)
  uiOutput(outputId = ns("male_anatogram_gene_plot"))
}

maleAnatogramPlotServer <- function(id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$male_anatogram_gene_plot <- renderUI({
        #check to see if data are there
        shiny::validate(
          shiny::need(c("gene_tissue") %in% data()$validate, "No data found."))
        #check to see if image exists
        img_path <- ddh::load_image(input = data(), fun_name = "make_male_anatogram", card = FALSE)
        if(!is.null(img_path)) {
          uiOutput(outputId = session$ns("male_anatogram_gene_plot_image"), width = "100%")
        } else {
          plotOutput(outputId = session$ns("male_anatogram_gene_plot_render"), 
                     width = "100%") %>%
            withSpinnerColor(plot_type = data()$type)
        }
      })
      output$male_anatogram_gene_plot_image <- renderUI({
        div(tags$img(src = load_image(input = data(), fun_name = "make_male_anatogram", card = FALSE), 
                     width = "100%"))
      })
      output$male_anatogram_gene_plot_render <- renderPlot({
        make_male_anatogram(input = data(), anatogram = "male")
      })
    }
  )
}

femaleAnatogramPlot <- function(id) {
  ns <- NS(id)
  uiOutput(outputId = ns("female_anatogram_gene_plot"))
}

femaleAnatogramPlotServer <- function(id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$female_anatogram_gene_plot <- renderUI({
        #check to see if data are there
        shiny::validate(
          shiny::need(c("gene_tissue") %in% data()$validate, "No data found."))
        #check to see if image exists
        img_path <- ddh::load_image(input = data(), fun_name = "make_female_anatogram", card = FALSE)
        if(!is.null(img_path)) {
          uiOutput(outputId = session$ns("female_anatogram_gene_plot_image"), width = "100%")
        } else {
          plotOutput(outputId = session$ns("female_anatogram_gene_plot_render"), 
                     width = "100%") %>%
            withSpinnerColor(plot_type = data()$type)
        }
      })
      output$female_anatogram_gene_plot_image <- renderUI({
        div(tags$img(src = load_image(input = data(), fun_name = "make_female_anatogram", card = FALSE), 
                     width = "100%"))
      })
      output$female_anatogram_gene_plot_render <- renderPlot({
        make_female_anatogram(input = data())
      })
    }
  )
}

tissuePlot <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(outputId = ns("tissue_gene_plot"), height = "auto"),
    tags$br(),
    fluidRow(ddh::make_legend("make_tissue"))
  )
}

tissuePlotServer <- function(id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$tissue_gene_plot <- renderUI({
        #check to see if data are there
        shiny::validate(
          shiny::need(c("gene_tissue") %in% data()$validate, "No data found."))
        #check to see if image exists
        img_path <- ddh::load_image(input = data(), fun_name = "make_tissue", card = FALSE)
        if(!is.null(img_path)) {
          uiOutput(outputId = session$ns("tissue_gene_plot_image"), width = "100%")
        } else {
          plotOutput(outputId = session$ns("tissue_gene_plot_render"), 
                     width = "100%") %>%
            withSpinnerColor(plot_type = data()$type)
        }
      })
      output$tissue_gene_plot_image <- renderUI({
        div(tags$img(src = load_image(input = data(), fun_name = "make_tissue", card = FALSE), 
                     width = "100%"))
      })
      output$tissue_gene_plot_render <- renderPlot({
        make_tissue(input = data())
      }, height = 1000)
    }
  )
}

## expression --------
cellGeneExpressionPlot <- function(id) {
  ns <- NS(id)
  tagList(
    h3(textOutput(outputId = ns("expression_gene_plot_title"))),
    uiOutput(ns("expression_gene_plot")),
    tags$br(),
    fluidRow(ddh::make_legend("make_cellexpression"))
  )
}

cellGeneExpressionPlotServer <- function(id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$expression_gene_plot_title <- renderText({paste0("Expression values for ", str_c(data()$content, collapse = ", "))})
      output$expression_gene_plot <- renderUI({
        #check to see if data are there
        shiny::validate(
          shiny::need(c("universal_expression_long") %in% data()$validate, "No data found."))
        #check to see if image exists
        img_path <- ddh::load_image(input = data(), fun_name = "make_cellexpression", card = FALSE)
        if(!is.null(img_path)) {
          uiOutput(outputId = session$ns("expression_gene_plot_image"), width = "100%")
        } else {
          plotOutput(outputId = session$ns("expression_gene_plot_render"), 
                     width = "100%") %>%
            withSpinnerColor(plot_type = data()$type)
        }
      })
      output$expression_gene_plot_image <- renderUI({
        div(tags$img(src = load_image(input = data(), fun_name = "make_cellexpression", card = FALSE), 
                     width = "100%"))
      })
      output$expression_gene_plot_render <- renderPlot({
        make_cellexpression(input = data(), var = "gene")
      })
    }
  )
}

cellProteinExpressionPlot <- function(id) {
  ns <- NS(id)
  tagList(
    h4(textOutput(outputId = ns("expression_protein_plot_title"))),
    uiOutput(ns("expression_protein_plot")),
    tags$br(),
    fluidRow(ddh::make_legend("make_cellexpression"))
  )
  
}

cellProteinExpressionPlotServer <- function(id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$expression_protein_plot_title <- renderText({paste0("Expression values for ", str_c(data()$content, collapse = ", "))})
      output$expression_protein_plot <- renderUI({
        #check to see if data are there
        shiny::validate(
          shiny::need(c("universal_expression_long") %in% data()$validate, "No data found."))
        #check to see if image exists
        img_path <- NULL #override b/c protein data not stored
        if(!is.null(img_path)) {
          uiOutput(outputId = session$ns("expression_protein_plot_image"), width = "100%")
        } else {
          plotOutput(outputId = session$ns("expression_protein_plot_render"), 
                     width = "100%") %>%
            withSpinnerColor(plot_type = data()$type)
        }
      })
      output$expression_protein_plot_image <- renderUI({
        div(tags$img(src = load_image(input = data(), fun_name = "make_cellexpression", card = FALSE), 
                     width = "100%"))
      })
      output$expression_protein_plot_render <- renderPlot({
        make_cellexpression(input = data(), var = "protein")
      })
    }
  )
}

cellGeneProteinPlot <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(h4(textOutput(ns("text_cell_gene_protein_plot")))),
    fluidRow(plotOutput(outputId = ns("cell_gene_protein_plot")) %>% 
               withSpinnerColor(plot_type = "gene") #see shiny_helper.R
    ),
    tags$br(),
    fluidRow(ddh::make_legend("make_cellgeneprotein"))
  )
}

cellGeneProteinPlotServer <- function(id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$text_cell_gene_protein_plot <- renderText({paste0("Gene vs. protein expression of ", str_c(data()$content, collapse = ", "))})
      output$cell_gene_protein_plot <- renderPlot({
        shiny::validate(
          shiny::need(c("universal_expression_long") %in% data()$validate, "No data found."))
        make_cellgeneprotein(input = data())
      })
    }
  )
}

## DEPENDENCY----
## dependency -----
cellDependenciesPlot <- function(id) {
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    # Scatterplot
    fluidRow(
      column(h4(textOutput(ns("cell_dep_plot_text"))), width = 12)), 
    fluidRow(column(textOutput(ns("essential_num")), width = 9),
             column(actionButton(ns("cell_dep_switch"), label = "Show interactive plot"), width = 3)
    ),
    tags$br(),
    fluidRow(
      div(
        id = ns("cell_deps_static"),
        style = "padding-left:1%",
        uiOutput(outputId = ns("cell_deps_plot"))
      ),
      shinyjs::hidden(
        div(
          id = ns("cell_deps_interactive"),
          style = "padding-left:1%",
          plotlyOutput(outputId = ns("cell_deps_interactive_plot"))
        )
      )
    ),
    tags$br()
  )
}

cellDependenciesPlotServer <- function (id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      
      observeEvent(input$cell_dep_switch, {
        
        if(input$cell_dep_switch %% 2 == 1){
          shinyjs::hide("cell_deps_static")
          shinyjs::show("cell_deps_interactive")
          
          updateActionButton(session, "cell_dep_switch", label = "Show static plot")
          
        } else {
          shinyjs::hide("cell_deps_interactive")
          shinyjs::show("cell_deps_static")
          
          updateActionButton(session, "cell_dep_switch", label = "Show interactive plot")
        }
      })
      
      # Scatterplot
      output$cell_dep_plot_text <- renderText({paste0("Dependency plots generated for ", str_c(data()$content, collapse = ", "))})
      output$essential_num <- renderText({
        get_essential(input = data())
        #paste0("Essential in ", get_essential(input = data()), " cell lines")
      })
      output$cell_deps_plot <- renderUI({
        #check to see if data are there
        shiny::validate(
          shiny::need(c("universal_achilles_long", "cell_expression_meta") %in% data()$validate, "No data found."))
        #check to see if image exists
        img_path <- ddh::load_image(input = data(), fun_name = "make_celldeps", card = FALSE)
        if(!is.null(img_path)) {
          uiOutput(outputId = session$ns("cell_deps_plot_image"), width = "100%")
        } else {
          plotOutput(outputId = session$ns("cell_deps_plot_render"), 
                     width = "100%") %>%
            withSpinnerColor(plot_type = data()$type)
        }
      })
      output$cell_deps_plot_image <- renderUI({
        div(tags$img(src = load_image(input = data(), fun_name = "make_celldeps", card = FALSE), 
                     width = "100%"))
      })
      output$cell_deps_plot_render <- renderPlot({
        make_celldeps(input = data(), 
                      card = FALSE,
                      lineplot = TRUE,
                      scale = 0.3)
      })
      #interactive
      output$cell_deps_interactive_plot <- renderPlotly({
        shiny::validate(
          shiny::need(c("universal_achilles_long", "cell_expression_meta") %in% data()$validate, "No data found."))
        ggplotly(make_celldeps(input = data()), tooltip = "text" #,
                 # width = plot_size_finder("make_celldeps")$plot_width,
                 # height = plot_size_finder("make_celldeps")$plot_height
        )
      })
    }
  )
}

cellDependenciesDensityPlot <- function(id) {
  ns <- NS(id)
  tagList(
    h4(textOutput(outputId = ns("cell_dep_density_title"))),
    uiOutput(ns("density_plot_plot")),
    tags$br(),
    fluidRow(htmlOutput(outputId = ns("density_plot_legend")))
  )
}

cellDependenciesDensityPlotServer <- function(id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$cell_dep_density_title <- renderText({paste0("Dependency density plot for ", str_c(data()$content, collapse = ", "))})
      output$density_plot_plot <- renderUI({
        fluidRow(plotOutput(outputId = session$ns("cell_deps_density"),  height = "auto") %>% 
                   withSpinnerColor(plot_type = data()$type) #see shiny_helper.R
        )
        
      })
      output$cell_deps_density <- renderPlot({
        shiny::validate(
          shiny::need(c("universal_achilles_long", "cell_expression_meta") %in% data()$validate, "No data found."))
        make_cellbins(input = data())
      },
      height = function() length(data()$content) * 90 + 80)
      output$density_plot_legend <- renderText({paste0("<strong>Computed Densities.</strong> Kernel density estimate of dependency scores. Dependency scores across all ", ifelse(data()$type == "gene", "cell lines for queried genes", "genes for queried cell lines"), ", revealing overall influence of a gene on cellular fitness. The interval indicates the 95% quantile of the data, the dot indicates the median dependency score. The gray background highlights weak dependency values between -1 and 1.")})
    }
  )
}

cellDependenciesBarPlot <- function(id) {
  ns <- NS(id)
  tagList(
    h4(textOutput(outputId = ns("cell_dep_barplot_title"))),
    uiOutput(ns("bar_plot_plot")),
    tags$br(),
    fluidRow(htmlOutput(outputId = ns("bar_plot_legend")))
  )
}

cellDependenciesBarPlotServer <- function (id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$cell_dep_barplot_title <- renderText({paste0("Dependency barplot for ", str_c(data()$content, collapse = ", "))})
      output$bar_plot_plot <- renderUI({
        fluidRow(plotlyOutput(outputId = session$ns("cell_bar"),  height = "auto") %>% 
                   withSpinnerColor(plot_type = data()$type) #see shiny_helper.R
        )
      })
      output$cell_bar <- renderPlotly({
        shiny::validate(
          shiny::need(c("universal_achilles_long", "cell_expression_meta") %in% data()$validate, "No data found."))
        ggplotly(make_cellbar(input = data()), tooltip = "text")
      })
      output$bar_plot_legend <- renderText({paste0("<strong>Dependency Bar Plot.</strong> Each bar shows the dependency scores of the queried ", ifelse(data()$type == "gene", "genes in a cell line.", "cell lines in a gene."), " Dependency scores less than -1 indicate a gene that is essential within a cell line. Dependency scores close to 0 mean no changes in fitness when the gene is knocked out. Dependency scores greater than 1 indicate gene knockouts lead to a gain in fitness.")})
    }
  )
}

cellDepsLinPlot <- function(id) {
  ns <- NS(id)
  tagList(
    h4(textOutput(outputId = ns("lineage_title"))),
    tags$br(),
    fluidRow(prettySwitch(inputId = ns("celllin_click_high"), "Show statistically significant"),
             plotOutput(outputId = ns("cell_deps_lin"),  height = "auto") %>% 
               withSpinnerColor(plot_type = "gene") #see shiny_helper.R
    ),
    tags$br(),
    fluidRow(ddh::make_legend("make_lineage"))
  )
}

cellDepsLinPlotServer <- function(id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$lineage_title <- renderText({paste0("Dependency lineage plot for ", str_c(data()$content, collapse = ", "))})
      output$cell_deps_lin <- renderPlot({
        shiny::validate(
          shiny::need(c("universal_achilles_long") %in% data()$validate, "No data found."))
        make_lineage(input = data(), 
                     highlight = input$celllin_click_high)
      },
      height = 550)
      observeEvent(input$sublin_click, { #event to store the 'click'
      })
    }
  )
}

cellDepsSubLinPlot <- function(id) {
  ns <- NS(id)
  tagList(
    h4(textOutput(outputId = ns("sublineage_title"))),
    tags$br(),
    fluidRow(prettySwitch(inputId = ns("cellsublin_click_high"), "Show statistically significant"),
             plotOutput(outputId = ns("cell_deps_sublin"),  height = "auto") %>% 
               withSpinnerColor(plot_type = "gene") #see shiny_helper.R
    ),
    tags$br(),
    fluidRow(ddh::make_legend("make_sublineage"))
  )
}

cellDepsSubLinPlotServer <- function(id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$sublineage_title <- renderText({paste0("Dependency sublineage plot for ", str_c(data()$content, collapse = ", "))})
      output$cell_deps_sublin <- renderPlot({
        shiny::validate(
          shiny::need(c("universal_achilles_long") %in% data()$validate, "No data found."))
        make_sublineage(input = data(),
                        highlight = input$cellsublin_click_high)
      },
      height = 1400)
    }
  )
}

cellDependenciesCorrPlot <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(h4(textOutput(ns("gene_correlation_text")))),
    uiOutput(outputId = ns("gene_correlation_plot"), height = "auto"),
    tags$br(),
    fluidRow(ddh::make_legend("make_correlation"))
  )
}

cellDependenciesCorrPlotServer <- function (id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$gene_correlation_text <- renderText({
        paste0("Gene correlation plot generated for ", str_c(data()$content, collapse = ", "))
      })
      output$gene_correlation_plot <- renderUI({
        #check to see if data are there
        shiny::validate(
          shiny::need(c("gene_achilles_cor_long") %in% data()$validate, "No data found."))
        #check to see if image exists
        img_path <- ddh::load_image(input = data(), fun_name = "make_correlation", card = FALSE)
        if(!is.null(img_path)) {
          uiOutput(outputId = session$ns("gene_correlation_plot_image"), width = "100%")
        } else {
          plotOutput(outputId = session$ns("gene_correlation_plot_render"), 
                     width = "100%") %>%
            withSpinnerColor(plot_type = data()$type)
        }
      })
      output$gene_correlation_plot_image <- renderUI({
        div(tags$img(src = load_image(input = data(), fun_name = "make_correlation", card = FALSE), 
                     width = "100%"))
      })
      output$gene_correlation_plot_render <- renderPlot({
        make_correlation(input = data())
      })
    }
  )
}

cellCoessentialityPlot <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(h4(textOutput(ns("text_cell_coess_plot")))),
    fluidRow(plotOutput(outputId = ns("cell_coessentiality")) %>% 
               withSpinnerColor(plot_type = "cell") #see shiny_helper.R
    ),
    tags$br(),
    fluidRow(ddh::make_legend("make_cell_similarity"))
  )
}

cellCoessentialityPlotServer <- function (id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      
      output$text_cell_coess_plot <- renderText({paste0("Cell co-essentiality plot generated for ", str_c(data()$content, collapse = ", "))})
      output$cell_coessentiality <- renderPlot({
        shiny::validate(
          shiny::need(c("cell_dependency_sim") %in% data()$validate, "No data found."))
        make_cell_similarity(input = data())
      })      
    }
  )
}

## Exp v. Dep Plots -----
expdepPlot <- function(id) {
  ns <- NS(id)
  tagList(
    h4(textOutput(outputId = ns("exdep_plot_title"))),
    uiOutput(ns("exdep_plot_plot")),
    tags$br(),
    fluidRow(htmlOutput(outputId = ns("exdep_plot_legend")))
  )
}

expdepPlotServer <- function (id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$exdep_plot_title <- renderText({paste0("Expression v. dependency plot for ", str_c(data()[[3]], collapse = ", "))})
      output$exdep_plot_plot <- renderUI({
        fluidRow(plotOutput(outputId = session$ns("cellexpdep"),  height = "500px") %>% 
                   withSpinnerColor(plot_type = data()$type) #see shiny_helper.R
        )
      })
      output$cellexpdep <- renderPlot({
        shiny::validate(
          shiny::need(c("universal_achilles_long", "cell_expression_meta") %in% data()$validate, "No data found."))
        make_expdep(input = data())
      })
      output$exdep_plot_legend <- renderText({paste0("<strong>Dependency versus Expression.</strong> Each point shows the dependency value compared to the expression value for ", ifelse(data()$type == "gene", "a cell line given a gene.", "a gene given a cell line."))})
    }
  )
}

# MOLECULAR FEATURES ----------
MolecularFeaturesSegmentPlot <- function(id) {
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    # Scatterplot
    fluidRow(column(h4(textOutput(ns("mol_feat_seg_plot_text"))), width = 12)),
    tags$br(),
    fluidRow(    
      div(
        id = ns("mol_feat_seg_plot_id"),
        style = "padding-left:1%",
        plotOutput(outputId = ns("mol_feat_seg_plot"), width = "100%") %>% 
          withSpinnerColor(plot_type = "gene"),
        actionLink(inputId = ns("segments_table_click"), " View table")
      )
    ),
    conditionalPanel(condition = paste0("input['", ns("segments_table_click"), "'] != 0"),
                     fluidRow(
                       div(
                         id = ns("mol_feat_seg_table_id"),
                         style = "padding-left:1%",
                         h4(textOutput(ns("mol_feat_seg_table_text"))),
                         DT::dataTableOutput(outputId = ns("mol_feat_seg_table"))
                       )
                     )
    )
  )
}

MolecularFeaturesSegmentPlotServer <- function (id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      # Scatterplot
      output$mol_feat_seg_plot_text <- renderText({paste0("Dependency segments for ", 
                                                          str_c(data()$content, collapse = ", "))})
      output$mol_feat_seg_plot <- renderPlot({
        #check to see if data are there
        shiny::validate(
          shiny::need(c("universal_achilles_long") %in% data()$validate, "No data found."))
        #plot
        ddh::gene_molecular_features_segments(input = data())
      })
      output$mol_feat_seg_table_text <- renderText({paste0("Dependency segments table for ", 
                                                           str_c(data()$content, collapse = ", "))})
      output$mol_feat_seg_table <- DT::renderDataTable({
        #check to see if data are there
        shiny::validate(
          shiny::need(c("universal_achilles_long") %in% data()$validate, "No data found."))
        #table
        DT::datatable(ddh::make_gene_molecular_features_segments(input = data()) %>% 
                        dplyr::select(Query, `Cell Line` = cell_name, Segment = group, Lineage = lineage, 
                                      Sublineage = lineage_subtype, Sex = sex, Age = age) %>%
                        dplyr::arrange(dplyr::desc(Segment)) %>% 
                        dplyr::mutate(`Cell Line` = map_chr(`Cell Line`, cell_linkr, type = "cell")),
                      rownames = FALSE,
                      escape = FALSE,
                      options = list(pageLength = 10))
      })
    }
  )
}

# CELL -----
## Cell Image Loader ----------
cellImage <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(imageOutput(outputId = ns("cell_image"), height = "auto") %>% 
               withSpinnerColor(plot_type = "cell") #see shiny_helper.R
    ),
    tags$br(),
    fluidRow(ddh::make_legend("make_cell_image"))
  )
}

cellImageServer <- function (id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$text_cell_title <- renderText({paste0(str_c(data()$content[[1]], " Cell Line Images"))})
      output$text_cell_attribution <- renderText({
        if(file.exists(make_cell_image(input = data()))){
          atcc_id <- 
            cellosaurus %>% 
            dplyr::filter(name %in% data()$content[[1]]) %>% 
            dplyr::pull("ATCC")
          glue::glue('<a href="https://www.atcc.org/products/all/{atcc_id}.aspx#characteristics" target="_blank">Image from ATCC</a>')
        }
      })
      output$cell_image <- renderImage({
        shiny::validate(
          need(file.exists(make_cell_image(input = data())), "No cell image available"))
        list(src= make_cell_image(input = data()), 
             width = "100%")
      }, deleteFile = FALSE)
    }
  )
}

## EXPRESSION----
cellLineGeneProteinPlot <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(h4(textOutput(ns("text_cellLine_gene_protein_plot")))),
    fluidRow(plotOutput(outputId = ns("cellLine_gene_protein_plot")) %>% 
               withSpinnerColor(plot_type = "gene") #see shiny_helper.R
    ),
    tags$br(),
    fluidRow(ddh::make_legend("make_cellgeneprotein"))
  )
}

cellLineGeneProteinPlotServer <- function(id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$text_cellLine_gene_protein_plot <- renderText({paste0("Gene vs. protein expression of ", str_c(data()$content, collapse = ", "))})
      output$cellLine_gene_protein_plot <- renderPlot({
        shiny::validate(
          shiny::need(c("cell_expression_names") %in% data()$validate, "No data found."))
        # ggplotly(
        make_cellgeneprotein(input = data())#, tooltip = c("text")
        # )
      })
    }
  )
}

cellCoexpressionPlot <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(h4(textOutput(ns("text_cell_coexp_plot")))),
    fluidRow(plotOutput(outputId = ns("cell_coexpression")) %>% 
               withSpinnerColor(plot_type = "cell") #see shiny_helper.R
    ),
    tags$br(),
    fluidRow(ddh::make_legend("make_cell_similarity"))
  )
}

cellCoexpressionPlotServer <- function (id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      
      output$text_cell_coexp_plot <- renderText({paste0("Cell co-expression plot generated for ", str_c(data()$content, collapse = ", "))})
      output$cell_coexpression <- renderPlot({
        shiny::validate(
          shiny::need(c("cell_expression_sim") %in% data()$validate, "No data found."))
        make_cell_similarity(input = data(), 
                             similarity = "expression")
      })      
    }
  )
}

## FUNCTIONAL PLOT ----
cellFunctionalPlot <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(h4(textOutput(ns("title_functional_plot")))),
    fluidRow(prettySwitch(ns("remove_equal"), 
                          "Remove pathways represented for the same group of genes",
                          value = FALSE),
             numericInput(inputId = ns("num_genes"),
                          "Minumum number of genes required to represent a pathway",
                          value = 2),
             sliderInput(ns("num_terms"), 
                         "Number of pathways to show",
                         min = 2, max = 40, value = 10)
    ),
    fluidRow(plotlyOutput(outputId = ns("functional_plot"),  height = "auto") %>% 
               withSpinnerColor(plot_type = "cell")
    ), #see shiny_helper.R
    tags$br(),
    fluidRow(ddh::make_legend("make_functional_cell")),
    tags$br()
  )
}

cellFunctionalPlotServer <- function (id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$title_functional_plot <- renderText({paste0("Differential pathway expression plot for ", str_c(data()$content, collapse = ", "))})
      output$functional_plot <- renderPlotly({
        ggplotly(
          make_functional_cell(input = data(),
                               num_pathways = input$num_terms,
                               num_genes = input$num_genes,
                               remove_equivalent_pathways = input$remove_equal),
          tooltip = "text"
        )
      })
    })
}

## CELL METADATA PLOT ----
cellMetadataPlot <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(h4(textOutput(ns("title_metadata_plot")))),
    fluidRow(selectizeInput(inputId = ns("metadata_factor"),
                            "Metadata factor:",
                            choices = c("Lineage" = "lineage",
                                        "Sublineage" = "sublineage")
    ),
    sliderInput(ns("bonferroni_cutoff"), 
                "Bonferroni cutoff",
                min = 0.01, max = 1, value = 0.05)),
    fluidRow(plotOutput(outputId = ns("metadata_plot"),  height = "auto") %>% 
               withSpinnerColor(plot_type = "cell")
    ), #see shiny_helper.R
    tags$br(),
    fluidRow(ddh::make_legend("make_metadata_cell"))
  )
}

cellMetadataPlotServer <- function (id, data, type) {
  moduleServer(
    id,
    function(input, output, session) {
      output$title_metadata_plot <- renderText({paste0("Lineage similarity plot for ", str_c(data()$content, collapse = ", "))})
      output$metadata_plot <- renderPlot({
        shiny::validate(
          shiny::need(c("cell_expression_sim") %in% data()$validate, "No data found."))
        make_metadata_cell(input = data(),
                           cell_line_similarity = type,
                           metadata = input$metadata_factor,
                           bonferroni_cutoff = input$bonferroni_cutoff)
      },
      height = 550)
    })
}

# COMPOUND QUERY -----
##INFO----
## Compound structure plot --------------------------------------------------------
compoundStructure <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(outputId = ns("compound_structure"), height = "auto"),
    tags$br(),
    fluidRow(ddh::make_legend("make_molecule_structure"))
  )
}

compoundStructureServer <- function (id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$compound_structure <- renderUI({
        #check to see if data are there
        shiny::validate(
          need(is.array(make_molecule_structure(input = data())),
               "No structure found for this compound."))
        #check to see if image exists
        img_path <- ddh::load_image(input = data(), fun_name = "make_molecule_structure", card = FALSE)
        if(!is.null(img_path)) {
          uiOutput(outputId = session$ns("compound_structure_image"), width = "100%")
        } else {
          plotOutput(outputId = session$ns("compound_structure_render"), 
                     width = "100%") %>%
            withSpinnerColor(plot_type = data()$type)
        }
      })
      output$compound_structure_image <- renderUI({
        div(tags$img(src = load_image(input = data(), fun_name = "make_molecule_structure", card = FALSE), 
                     width = "100%"))
      })
      output$compound_structure_render <- renderPlot({
        make_molecule_structure(input = data())
      })
    }
  )
}

## DEPENDENCIES----
compoundDependenciesPlot <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(h4(textOutput(ns("text_compound_dep_plot")))),
    fluidRow(textOutput(ns("essential_num_compound"))),
    fluidRow(plotlyOutput(outputId = ns("cell_deps")) %>% 
               withSpinnerColor(plot_type = "compound") #see shiny_helper.R
    ),
    tags$br(),
    fluidRow(ddh::make_legend("make_celldeps"))
  )
}

compoundDependenciesPlotServer <- function (id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$text_compound_dep_plot <- renderText({paste0("Viability plots generated for ", str_c(data()$content, collapse = ", "))})
      output$essential_num_compound <- renderText({
        paste0("Toxic in ", get_essential(input = data()), " cell lines")})
      output$cell_deps <- renderPlotly({
        shiny::validate(
          shiny::need(c("universal_prism_long") %in% data()$validate, "No data found."))
        ggplotly(make_celldeps(input = data()), tooltip = "text")
      })      
    }
  )
}

compoundBinsPlot <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(plotOutput(outputId = ns("compound_bins"),  height = "auto") %>% 
               withSpinnerColor(plot_type = "compound") #see shiny_helper.R
    ),
    tags$br(),
    fluidRow(ddh::make_legend("make_cellbins"))
  )
}

compoundBinsPlotServer <- function(id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$compound_bins <- renderPlot({
        shiny::validate(
          shiny::need(c("universal_prism_long") %in% data()$validate, "")) #""left blank
        make_cellbins(input = data())
      },
      height = function() length(data()$content) * 90 + 80)
    }
  )
}

compoundLinPlot <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(plotOutput(outputId = ns("compound_lin"),  height = "auto") %>% 
               withSpinnerColor(plot_type = "compound") #see shiny_helper.R
    ),
    tags$br(),
    fluidRow(tags$strong(ddh::make_legend("make_lineage"))
    )
  )
}

compoundLinPlotServer <- function(id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$compound_lin <- renderPlot({
        shiny::validate(
          shiny::need(c("universal_prism_long") %in% data()$validate, "No data found."))
        make_lineage(input = data())
      },
      height = 550)
      observeEvent(input$compound_sublin_click, { #event to store the 'click'
      })
    }
  )
}

compoundSubLinPlot <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(actionLink(inputId = ns("compound_sublin_click"), "View plot split into sublineages")),
    tags$br(),
    conditionalPanel(condition = paste0("input['", ns("compound_sublin_click"), "'] != 0"),
                     tags$br(),
                     fluidRow(plotOutput(outputId = ns("compound_sublin"),  height = "auto") %>% 
                                withSpinnerColor(plot_type = "compound") #see shiny_helper.R
                     ))
  )
}

compoundSubLinPlotServer <- function(id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      observeEvent(input$compound_sublin_click, { #event to store the 'click'
      })
      output$compound_sublin <- renderPlot({
        shiny::validate(
          shiny::need(c("universal_prism_long") %in% data()$validate, "No data found."))
        make_sublineage(input = data())
      },
      height = 1400)
    }
  )
}

compoundCorrelationPlot <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(h4(textOutput(ns("text_compoundcorr_plot")))),
    fluidRow(plotOutput(outputId = ns("compound_correlations")) %>% 
               withSpinnerColor(plot_type = "compound") #see shiny_helper.R
    ),
    tags$br(),
    fluidRow(ddh::make_legend("make_correlation"))
  )
}

compoundCorrelationPlotServer <- function (id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$text_compoundcorr_plot <- renderText({paste0("Compound correlation plot generated for ", str_c(data()$content, collapse = ", "))})
      output$compound_correlations <- renderPlot({
        shiny::validate(
          shiny::need(c("compound_prism_cor_nest") %in% data()$validate, "No data found."))
        make_correlation(input = data())
      })      
    }
  )
}

