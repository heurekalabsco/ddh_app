#GENE-----
## Pathways ---------------------
pathwayList <- function (id) {
  ns <- NS(id)
  tagList(
    fluidRow(h4(textOutput(ns("text_pathway_list")))),
    fluidRow(DT::dataTableOutput(outputId = ns("pathway_list")))
  )
}

pathwayListServer <- function(id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$text_pathway_list <- renderText({
        if(data()$subtype != "pathway") {
          paste0("Pathways containing ", str_c(data()$content, collapse = ", "))
        } else {
          "Pathway genes"
        }
      })
      output$pathway_list <- DT::renderDataTable({
        if(data()$subtype != "pathway") {
          shiny::validate(
            shiny::need(c("universal_gene_pathways") %in% data()$validate, "No pathway data for this gene"))
          DT::datatable(make_pathway_table(input = data()) %>% 
                        dplyr::mutate(gs_id = map_chr(gs_id, internal_link), #from fun_helper.R
                                      gs_name = purrr::map_chr(gs_name, clean_pathway_names),
                                      gs_description = purrr::map_chr(gs_description, clean_pathway_descriptions)) %>%
                        dplyr::select(Gene = human_gene_symbol, ID = gs_id, Pathway = gs_name, Description = gs_description,
                                      `Pathway Size` = pathway_size),
                        rownames = FALSE,
                        escape = FALSE,
                        options = list(paging = FALSE, 
                                       searching = FALSE))
        } else {
          DT::datatable(make_pathway_table(input = data()) %>% 
                          dplyr::mutate(Gene = map_chr(Gene, internal_link)),
                        rownames = FALSE,
                        escape = FALSE,
                        options = list(paging = FALSE, 
                                       searching = FALSE))
        }
      })
    }
  )
}

## Protein Sequence ---------------------
proteinSeq <- function (id) {
  ns <- NS(id)
  tagList(
    fluidRow(h4(textOutput(ns("text_protein_sequence")))),
    fluidRow(verbatimTextOutput(ns("protein_sequence"), placeholder = FALSE)),
    fluidRow(h4(textOutput(ns("text_protein_length")))),
    fluidRow(textOutput(outputId = ns("protein_length")))
  )
}

proteinSeqServer <- function (id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$text_protein_sequence <- renderText({paste0("Protein sequence of ", 
                                                         ifelse(data()$subtype == "pathway",
                                                                "pathway genes",
                                                                str_c(data()$content, collapse = ", ")
                                                         )
      )
      })
      output$protein_sequence <- renderText({paste0(paste0(str_c(data()$content, ": ", 
                                                                 make_protein_sequence(input = data())
                                                                 )), collapse = "\n")})
      output$text_protein_length <- renderText({paste0("Protein length of ", 
                                                       ifelse(data()$subtype == "pathway",
                                                              "pathway genes",
                                                              str_c(data()$content, collapse = ", ")
                                                       )
      )
      })
      output$protein_length <- renderText({paste0(paste0(str_c(data()$content, ": ", 
                                                               stringr::str_count(make_protein_sequence(input = data())), 
                                                               " AA")), collapse = ", ")})
    }
  )
}

## Protein Cluster ---------------------
proteinClusterTable <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(h4(textOutput(ns("text_cluster_table")))),
    DT::dataTableOutput(outputId = ns("prot_clust_table"))
  )
}

proteinClusterTableServer <- function(id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$text_cluster_table <- renderText({
        cluster_table <- make_signature_clusters_table(input = data())
        
        prot_members <- table(cluster_table$Cluster)[table(cluster_table$Cluster) != 0]
        
        paste0("Protein members - ", paste0("Cluster ", names(prot_members),
                                            " (", prot_members, " proteins)", 
                                            collapse = ", "))
      })
      
      output$prot_clust_table <- DT::renderDataTable({
        shiny::validate(
          shiny::need(c("gene_signature_clusters") %in% data()$validate, 
                      "No cluster data for this protein"))
        withProgress(message = 'Building a smart clustering table...', {
          DT::datatable(make_signature_clusters_table(input = data()) %>%
                          dplyr::mutate(Gene = map_chr(Gene, internal_link)),
                        rownames = FALSE,
                        escape = FALSE,
                        filter = "none",
                        options = list(pageLength = 8, 
                                       lengthChange = FALSE)
                        )
        })
      })
    }
  )
}

##Pubmed----
# module that displays a table for pubmed
pubmedTable <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(h4(textOutput(ns("text_pubmed_table")))),
    tags$br(),
    fluidRow(DT::dataTableOutput(outputId = ns("pubmed_table")))
  )
}

pubmedTableServer <- function(id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$text_pubmed_table <- renderText({paste0("Publication history table of ", 
                                                     ifelse(data()$subtype == "pathway",
                                                            "pathway genes",
                                                            str_c(data()$content, collapse = ", ")
                                                     )
      )
      })
      output$pubmed_table <- DT::renderDataTable({
        shiny::validate(
          shiny::need(c("universal_pubmed") %in% data()$validate, 
                      "No literature data for this query"))
        withProgress(message = 'Building a smart table...', {
          DT::datatable(make_pubmed_table(input = data()) %>% 
                          dplyr::mutate(pmid = map_chr(pmid, pubmed_linkr, number_only = TRUE) #from fun_helper.R
                          ) %>% 
                          dplyr::mutate(pmcid = map_chr(pmcid, pmc_linkr) #from fun_helper.R
                          ) %>% 
                          dplyr::rename(Name = id, 'Pubmed ID' = pmid, Year = year, PMCID = pmcid),
                        rownames = FALSE,
                        escape = FALSE,
                        options = list(pageLength = 10))
        })
      })
    }
  )
}

##Expression----
# module that displays a table for cell anatogram
cellAnatogramTable <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(h4(textOutput(ns("text_subcell_table")))),
    fluidRow(DT::dataTableOutput(outputId = ns("cellanatogram_table")))
  )
}

cellAnatogramTableServer <- function(id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$text_subcell_table <- renderText({paste0("Subcellular mRNA distribution of ", 
                                                      ifelse(data()$subtype == "pathway",
                                                             "pathway genes",
                                                             str_c(data()$content, collapse = ", ")
                                                      )
      )
      })
      output$cellanatogram_table <- DT::renderDataTable({
        shiny::validate(
          shiny::need(c("gene_subcell") %in% data()$validate, "")) #intentionally left blank; redundant with plot
        DT::datatable(make_cellanatogram_table(input = data()) %>% 
                        dplyr::select(id, main_location, value, reliability) %>% 
                        dplyr::rename(Gene = id, 
                                      Location = main_location,
                                      Expression = value, 
                                      Reliability = reliability),
                      rownames = FALSE,
                      escape = FALSE,
                      options = list(pageLength = 10))
      })
    }
  )
}

tissueTable <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(h4(textOutput(ns("text_tissue_table")))),
    fluidRow(checkboxInput(inputId = ns("tissue_filter_click"), label = "Filter tissue table", value = FALSE)),
    fluidRow(DT::dataTableOutput(outputId = ns("tissueanatogram_table")))
  )
}

tissueTableServer <- function(id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$text_tissue_table <- renderText({paste0("Human tissue expression of ", 
                                                     ifelse(data()$subtype == "pathway",
                                                            "pathway genes",
                                                            str_c(data()$content, collapse = ", ")
                                                     )
      )
      })
      output$tissueanatogram_table <- DT::renderDataTable({
        shiny::validate(
          shiny::need(c("gene_subcell") %in% data()$validate, "No tissue data for this gene"))
        DT::datatable(make_humananatogram_table(input = data()),
                      filter = if(input$tissue_filter_click == FALSE) {'none'} else {'top'},
                      rownames = FALSE,
                      escape = FALSE,
                      options = list(pageLength = 10))
      })
    }
  )
}

##Cell Expression Tables -----
cellGeneExpressionTable <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(h4(textOutput(ns("text_cell_gene_table")))),
    fluidRow(DT::dataTableOutput(outputId = ns("cell_gene_table"))))
}

cellGeneExpressionTableServer <- function (id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$text_cell_gene_table <- renderText({paste0("mRNA abundance of ", 
                                                        ifelse(data()$subtype == "pathway",
                                                               "pathway genes",
                                                               str_c(data()$content, collapse = ", ")
                                                        )
      )
      })
      output$cell_gene_table <- DT::renderDataTable({
        if(data()$type == "gene") {
          shiny::validate(
            shiny::need(c("universal_expression_long") %in% data()$validate, "No expression data for this gene"))
          DT::datatable(
            make_expression_table(input = data(), var = "gene") %>%
              dplyr::mutate(`Cell Line` = map_chr(`Cell Line`, cell_linkr, type = "cell")),
            rownames = FALSE,
            escape = FALSE)
          
        } else if(data()$type == "cell") {
          shiny::validate(
            shiny::need(c("universal_expression_long") %in% data()$validate, "No expression data for this cell line"))
          DT::datatable(
            make_expression_table(input = data(), var = "gene") %>%
              dplyr::mutate(Gene = map_chr(Gene, internal_link)),
            rownames = FALSE,
            escape = FALSE)
        }
      })
    }
  )
}

cellProteinExpressionTable <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(h4(textOutput(ns("text_cell_protein_table")))),
    fluidRow(DT::dataTableOutput(outputId = ns("cell_protein_table"))))
}

cellProteinExpressionTableServer <- function (id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$text_cell_protein_table <- renderText({paste0("Protein abundance of ", 
                                                           ifelse(data()$subtype == "pathway",
                                                                  "pathway genes",
                                                                  str_c(data()$content, collapse = ", ")
                                                           )
      )
      })
      output$cell_protein_table <- DT::renderDataTable({
        if(data()$type == "gene") {
          shiny::validate(
            shiny::need(c("universal_expression_long") %in% data()$validate, "No protein data for this gene"))
          DT::datatable(
            make_expression_table(input = data(), var = "protein") %>%
              dplyr::mutate(`Cell Line` = map_chr(`Cell Line`, cell_linkr, type = "cell")),
            rownames = FALSE,
            escape = FALSE)
          
        } else if(data()$type == "cell") {
          shiny::validate(
            shiny::need(c("universal_expression_long") %in% data()$validate, "No protein data for this cell line"))
          DT::datatable(
            make_expression_table(input = data(), var = "protein") %>%
              dplyr::mutate(Gene = map_chr(Gene, internal_link)),
            rownames = FALSE,
            escape = FALSE)
        }
      })
    }
  )
}

## Dep Table -----
cellDependenciesTable <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(h4(textOutput(ns("text_cell_dep_table")))),
    fluidRow(checkboxInput(inputId = ns("dep_filter_click"), label = "Filter dependency table", value = FALSE)),
    fluidRow(DT::dataTableOutput(outputId = ns("target_achilles"))))
}

cellDependenciesTableServer <- function (id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$text_cell_dep_table <- renderText({paste0("Dependency table of ", 
                                                       ifelse(data()$subtype == "pathway",
                                                              "pathway genes",
                                                              str_c(data()$content, collapse = ", ")
                                                       )
      )
      })
      output$target_achilles <- DT::renderDataTable({
        shiny::validate(
          shiny::need(c("universal_achilles_long") %in% data()$validate, "No dependency data for this gene"))
        DT::datatable(make_dep_table(input = data()) %>%
                        dplyr::mutate(`Cell Line` = map_chr(`Cell Line`, cell_linkr, type = "cell")), 
                      filter = if(input$dep_filter_click == FALSE) {'none'} else {'top'}, 
                      rownames = FALSE,
                      escape = FALSE,
                      options = list(pageLength = 10))
      })
    }
  )
}

compoundDependenciesTable <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(h4(textOutput(ns("text_compound_dep_table")))),
    fluidRow(checkboxInput(inputId = ns("compound_dep_filter_click"), label = "Filter dependency table", value = FALSE)),
    fluidRow(DT::dataTableOutput(outputId = ns("compound_dep_table"))))
}

compoundDependenciesTableServer <- function (id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$text_compound_dep_table <- renderText({paste0("Dependency table of ", 
                                                           ifelse(data()$subtype == "pathway",
                                                                  "pathway genes",
                                                                  str_c(data()$content, collapse = ", ")
                                                           )
      )
      })
      output$compound_dep_table <- DT::renderDataTable({
        shiny::validate(
          shiny::need(c("universal_achilles_long") %in% data()$validate, "No viability data for this compound"))
        DT::datatable(make_dep_table(input = data()), 
                      filter = if(input$compound_dep_filter_click == FALSE) {'none'} else {'top'}, 
                      rownames = FALSE,
                      escape = FALSE,
                      options = list(pageLength = 10))
      })
    }
  )
}
cellLineDependenciesTable <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(h4(textOutput(ns("text_cell_line_dep_table")))),
    fluidRow(checkboxInput(inputId = ns("dep_cell_line_filter_click"), label = "Filter dependency table", value = FALSE)),
    fluidRow(checkboxGroupInput(inputId = ns("vars_essentials"), 
                                "Select columns:",
                                c("Unique Essential", "Pan Essential"),
                                inline = TRUE)),
    fluidRow(DT::dataTableOutput(outputId = ns("cell_line_achilles"))))
}

cellLineDependenciesTableServer <- function (id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$text_cell_line_dep_table <- renderText({paste0("Dependency table of ", 
                                                            ifelse(data()$subtype == "pathway",
                                                                   "pathway genes",
                                                                   str_c(data()$content, collapse = ", ")
                                                            )
      )
      })
      output$cell_line_achilles <- DT::renderDataTable({
        shiny::validate(
          shiny::need(c("universal_achilles_long") %in% data()$validate, "No dependency data for this cell line"))
        DT::datatable(make_dep_table(input = data()) %>%
                        dplyr::rename("Gene" = "gene", "Name" = "approved_name", "Unique Essential" = "unique_essential", "Pan Essential" = "common_essential") %>%
                        dplyr::mutate(Gene = map_chr(Gene, internal_link)) %>%  #from fun_helper.R
                        dplyr::select("Gene", "Name", contains(data()$content), input$vars_essentials),
                      rownames = FALSE,
                      escape = FALSE, 
                      filter = if(input$dep_cell_line_filter_click == FALSE) {'none'} else {'top'}, 
                      options = list(pageLength = 10))
      })
    }
  )
}

cellLineDrugDependenciesTable <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(h4(textOutput(ns("text_cell_line_drug_dep_table")))),
    fluidRow(checkboxInput(inputId = ns("dep_cell_line_drug_filter_click"), label = "Filter dependency table", value = FALSE)),
    fluidRow(checkboxGroupInput(inputId = ns("vars_toxic_drugs"), 
                                "Select columns:",
                                c("Uniquely Toxic"),
                                inline = TRUE)),
    fluidRow(DT::dataTableOutput(outputId = ns("cell_line_drug_prism"))))
}

cellLineDrugDependenciesTableServer <- function (id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$text_cell_line_drug_dep_table <- renderText({paste0("Drug dependency table of ", 
                                                                 ifelse(data()$subtype == "pathway",
                                                                        "pathway genes",
                                                                        str_c(data()$content, collapse = ", ")
                                                                 )
      )
      })
      output$cell_line_drug_prism <- DT::renderDataTable({
        shiny::validate(
          shiny::need(c("universal_achilles_long") %in% data()$validate, "No viability data for this cell line"))
        DT::datatable(make_dep_table(input = data(), var = "drug") %>% 
                        dplyr::rename("Drug" = "name", "MOA" = "moa", "Uniquely Toxic" = "unique_toxic") %>%
                        dplyr::select("Drug", "MOA", "log2fc", input$vars_toxic_drugs), 
                      filter = if(input$dep_cell_line_drug_filter_click == FALSE) {'none'} else {'top'}, 
                      rownames = FALSE,
                      escape = FALSE,
                      options = list(pageLength = 10))
      })
    }
  )
}

## molecular features ------
MolecularFeaturesTable <- function (id) {
  ns <- NS(id)
  tagList(
    fluidRow(h4(textOutput(ns("mol_feat_table_text")))),
    fluidRow(    
      div(
        id = ns("mol_feat_table_id"),
        style = "padding-left:1%",
        DT::dataTableOutput(outputId = ns("mol_feat_table")),
        actionLink(inputId = ns("mol_feat_table_click"), " View plot")
      )
    ),
    conditionalPanel(condition = paste0("input['", ns("mol_feat_table_click"), "'] != 0"),
                     fluidRow(
                       div(
                         id = ns("mol_feat_table_plot_id"),
                         style = "padding-left:1%",
                         h4(textOutput(ns("mol_feat_plot_text"))),
                         plotOutput(outputId = ns("mol_feat_plot"))
                       )
                     )
    )
  )
}

MolecularFeaturesTableServer <- function(id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$mol_feat_table_text <- renderText({paste0("Molecular features associated with with sensitivity to ", 
                                                       ifelse(data()$subtype == "pathway",
                                                              "pathway genes",
                                                              str_c(data()$content, collapse = ", ")
                                                             ),
                                                       " ablation")
      })
      output$mol_feat_table <- DT::renderDataTable({
        #check to see if data are there
        shiny::validate(
          shiny::need(c("universal_achilles_long") %in% data()$validate, "No data found."))
        #render table
        DT::datatable(ddh::make_molecular_features_table(input = data()) %>% 
                        dplyr::mutate(Feature = map_chr(Feature, internal_link)),
                      rownames = FALSE,
                      escape = FALSE,
                      options = list(pageLength = 10))
      })
      # Conditional barplot
      output$mol_feat_plot_text <- renderText({paste0("Molecular features barplot of ", 
                                                      ifelse(data()$subtype == "pathway",
                                                             "pathway genes",
                                                             str_c(data()$content, collapse = ", ")
                                                      )
      )
      })
      output$mol_feat_plot <- renderPlot({
        #check to see if data are there
        shiny::validate(
          shiny::need(c("universal_achilles_long") %in% data()$validate, "No data found."))
        #plot
        ddh::make_molecular_features(input = data())
      })
    }
  )
}

MolecularFeaturesPathwaysTable <- function (id) {
  ns <- NS(id)
  tagList(
    fluidRow(h4(textOutput(ns("mol_feat_pth_table_text")))),
    fluidRow(    
      div(
        id = ns("mol_feat_pth_table_id"),
        style = "padding-left:1%",
        DT::dataTableOutput(outputId = ns("mol_feat_pth_table")),
        actionLink(inputId = ns("mol_feat_pth_table_click"), " View plot")
      )
    ),
    conditionalPanel(condition = paste0("input['", ns("mol_feat_pth_table_click"), "'] != 0"),
                     fluidRow(
                       div(
                         id = ns("mol_feat_pth_table_plot_id"),
                         style = "padding-left:1%",
                         h4(textOutput(ns("mol_feat_pth_plot_text"))),
                         plotOutput(outputId = ns("mol_feat_pth_plot"))
                       )
                     )
    )
  )
}

MolecularFeaturesPathwaysTableServer <- function(id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$mol_feat_pth_table_text <- renderText({paste0("Pathways associated with sensitivity to ", 
                                                           ifelse(data()$subtype == "pathway",
                                                                  "pathway genes",
                                                                  str_c(data()$content, collapse = ", ")
                                                           ),
                                                           " ablation")
      })
      output$mol_feat_pth_table <- DT::renderDataTable({
        #check to see if data are there
        shiny::validate(
          shiny::need(c("universal_achilles_long") %in% data()$validate, "No data found."))
        #render table
        DT::datatable(ddh::make_molecular_features_pathways_table(input = data()),
                      rownames = FALSE,
                      escape = FALSE,
                      options = list(pageLength = 10))
      })
      # Conditional barplot
      output$mol_feat_pth_plot_text <- renderText({paste0("Pathways barplot of ", 
                                                          ifelse(data()$subtype == "pathway",
                                                                 "pathway genes",
                                                                 str_c(data()$content, collapse = ", ")
                                                          )
      )
      })
      output$mol_feat_pth_plot <- renderPlot({
        #check to see if data are there
        shiny::validate(
          shiny::need(c("universal_achilles_long") %in% data()$validate, "No data found."))
        #plot
        ddh::make_molecular_features_pathways(input = data())
      })
    }
  )
}

##Similar----
similarGenesTable <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(h4(textOutput(ns("text_dep_top")))),
    fluidRow(
      # shinyWidgets::prettySwitch(inputId = ns("gls_table_top"), 
      #                            "Show GLS table", 
      #                            value = FALSE),
      column(6, checkboxGroupInput(inputId = ns("vars_dep_top"), 
                                   "Select columns:",
                                   c("R^2", "Z-Score", "Co-publication Count", "Co-publication Index"), # "GLS p-value"
                                   selected = c("R^2", "Co-publication Count"), # "GLS p-value"
                                   inline = TRUE)),
      column(6, fluidRow(sliderInput(inputId = ns("num_sim_genes"),
                                     "Remove genes with >n associations:",
                                     min = 100,
                                     max = 1000,
                                     value = 1000,
                                     step = 100)), 
             fluidRow(column(3, actionButton(inputId = ns("censor"), "Submit")),
                      column(3, actionButton(inputId = ns("reset"), "Reset"))))
    ),
    hr(),
    fluidRow(DT::dataTableOutput(outputId = ns("dep_top")))
  )
}

similarGenesTableServer <- function (id, data) {
  moduleServer(
    id,
    function(input, output, session) { 
      output$text_dep_top <- renderText({paste0(censor_status$num, " genes with similar dependencies as ", 
                                                ifelse(data()$subtype == "pathway",
                                                       "pathway genes",
                                                       str_c(data()$content, collapse = ", ")
                                                       )
                                                )
      })
      #censor reactive values
      censor_status <- reactiveValues(censor = FALSE, 
                                      num_sim_genes = 1000)
      #activate censor
      observeEvent(input$censor, {
        censor_status$censor <- TRUE
        censor_status$num_sim_genes <- input$num_sim_genes
        censor_status$num <- 
          make_censor_table(input = data(), 
                            censor = censor_status$censor, 
                            greater_than = censor_status$num_sim_genes) %>% 
          nrow()
      })
      #reset censoring
      observeEvent(input$reset, {
        censor_status$censor <- FALSE
        censor_status$num_sim_genes <- 1000
        updateSliderInput(session, inputId = "num_sim_genes", value = 1000)
        censor_status$num <- 
          make_censor_table(input = data(), 
                            censor = censor_status$censor, 
                            greater_than = censor_status$num_sim_genes) %>% 
          nrow()
      })
      output$dep_top <- DT::renderDataTable({
        shiny::validate(
          shiny::need(c("gene_master_top_table") %in% data()$validate, "No dependency data for this gene"))
        
        censor_status$num <- make_censor_table(input = data(), 
                                               censor = censor_status$censor, 
                                               greater_than = censor_status$num_sim_genes) %>% 
          nrow()
        
        DT::datatable(
          ddh::make_censor_table(input = data(), 
                                 censor = censor_status$censor, 
                                 greater_than = censor_status$num_sim_genes) %>%
            dplyr::rename("Query" = "id", "Gene" = "gene", "Name" = "gene_name",
                          "R^2" = "r2", "Z-Score" = "z_score", "Co-publication Count" = "concept_count", "Co-publication Index" = "concept_index") %>%
            dplyr::select("Query", "Gene", "Name", input$vars_dep_top) %>%
            dplyr::mutate_if(is.numeric, ~ signif(., digits = 3)) %>% 
            dplyr::mutate(Gene = map_chr(Gene, internal_link, linkout_img = FALSE)),
          rownames = FALSE,
          escape = FALSE,
          options = list(pageLength = 25)
        )
      })
    }
  )
}

GenePathwayEnrichmentTable <- function(id) {
  ns <- NS(id)
  tagList(
    # TABLE
    fluidRow(h4(textOutput(ns("text_gene_path_coessentiality")))),
    fluidRow(DT::dataTableOutput(outputId = ns("genes_pathways"))),
    # PLOT
    # fluidRow(actionLink(inputId = ns("network_click"), "View network plot")),
    tags$br()#,
    # conditionalPanel(condition = paste0("input['", ns("network_click"), "'] != 0"), 
    #                  fluidRow(h4(textOutput(ns("text_network_plot")))),
    #                  # fluidRow(checkboxInput(inputId = ns("labels_network"), 
    #                  #                        label = "Show labels", value = FALSE)),
    #                  fluidRow(plotOutput(outputId = ns("network_pathway_components"), 
    #                                      height = "600px")),
    #                  tags$br(),
    #                  fluidRow(ddh::make_legend("make_gene_pathways_components_network"))
    # )
  )
}

GenePathwayEnrichmentTableServer <- function (id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      # TABLE
      output$text_gene_path_coessentiality <- renderText({paste0("Enriched pathways for ", 
                                                                 ifelse(data()$subtype == "pathway",
                                                                        "pathway genes",
                                                                        str_c(data()$content, collapse = ", ")
                                                                 )
      )
      })
      output$genes_pathways <- DT::renderDataTable({
        shiny::validate(
          shiny::need(c("universal_achilles_long") %in% data()$validate, "No dependency data for this gene"))
        DT::datatable(ddh::make_gene_dependency_enrichment_table(input = data()) %>% 
                        dplyr::mutate_if(is.numeric, ~ signif(., digits = 3)),
                      rownames = FALSE,
                      escape = FALSE,
                      options = list(pageLength = 10))
      })
      # CLICKABLE PLOT
      # output$text_network_plot <- renderText({paste0("Network plot of pathways with similar dependencies as ", str_c(data()$content, collapse = ", "))})
      # output$network_pathway_components <- renderPlot({
      #   
      #   if(!is.null(input$genes_pathways_rows_selected)){
      #     highlight_sel_table <- make_gene_pathways_components(input = data()) %>% 
      #       dplyr::slice(input$genes_pathways_rows_selected)
      #     
      #     highlight_sel <- c(highlight_sel_table$feature1, highlight_sel_table$feature2)
      #   } else {
      #     highlight_sel <- NULL
      #   }
      #   
      #   make_gene_pathways_components_network(input = data(), 
      #                                         highlight = highlight_sel,
      #                                         cutoff = 0.3, # SET TO NULL AFTER NEXT DATA GENERATION
      #                                         fontsize = 4
      #   )
      # })
    }
  )
}

similarCellsTable <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(h4(textOutput(ns("text_cells_dep_top")))),
    fluidRow(checkboxGroupInput(inputId = ns("vars_cells_dep_top"),
                                label = "Select columns:",
                                choices = c("Lineage", "Sub-lineage", "Estimate",
                                            "P.Value", "Bonferroni", "Sex", "Age", "Status"),
                                selected = c("Lineage", "Estimate", "Bonferroni"),
                                inline = TRUE)),
    fluidRow(DT::dataTableOutput(outputId = ns("cells_dep_top")))
  )
}

similarCellsTableServer <- function (id, data) {
  moduleServer(
    id,
    function(input, output, session) { 
      output$text_cells_dep_top <- renderText({paste0(nrow(make_cell_sim_table(input = data())$top_table),
                                                      " cells with similar dependency profiles as ", 
                                                      ifelse(data()$subtype == "pathway",
                                                             "pathway genes",
                                                             str_c(data()$content, collapse = ", ")
                                                             )
                                                      )
        })     
      output$cells_dep_top <- DT::renderDataTable({
        shiny::validate(
          shiny::need(c("cell_dependency_sim") %in% data()$validate, "No dependency data for this cell line"))
        DT::datatable(
          make_cell_sim_table(input = data())$top_table %>%
            dplyr::mutate(cell2_name = map_chr(cell2_name, cell_linkr, type = "cell") #from fun_helper.R
            ) %>%
            dplyr::rename("Query" = "cell1_name", "Cell" = "cell2_name", "Lineage" = "lineage", 
                          "Sub-lineage" = "lineage_subtype", "Estimate" = "coef", 
                          "P.Value" = "pval", "Bonferroni" = "bonferroni",
                          "Sex" = "sex", "Age" = "age", "Status" = "status") %>% 
            dplyr::arrange(Bonferroni) %>% 
            dplyr::select(Query, Cell, input$vars_cells_dep_top) %>% 
            dplyr::mutate_if(is.numeric, ~ signif(., digits = 3)),
          rownames = FALSE,
          escape = FALSE,
          options = list(pageLength = 25))
      })
    }
  )
}

similarExpCellsTable <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(h4(textOutput(ns("text_cells_exp_top")))),
    fluidRow(checkboxGroupInput(inputId = ns("vars_cells_exp_top"),
                                label = "Select columns:",
                                choices = c("Lineage", "Sub-lineage", "Estimate",
                                            "P.Value", "Bonferroni", "Sex", "Age", "Status"),
                                selected = c("Lineage", "Estimate", "Bonferroni"),
                                inline = TRUE)),
    fluidRow(DT::dataTableOutput(outputId = ns("cells_exp_top")))
  )
}

similarExpCellsTableServer <- function (id, data) {
  moduleServer(
    id,
    function(input, output, session) { 
      output$text_cells_exp_top <- renderText({paste0(nrow(make_cell_sim_table(input = data(),
                                                                               similarity = "expression")$top_table),
                                                      " cells with similar gene expression profiles as ", 
                                                      ifelse(data()$subtype == "pathway",
                                                             "pathway genes",
                                                             str_c(data()$content, collapse = ", ")
                                                             )
                                                      )
        })    
      output$cells_exp_top <- DT::renderDataTable({
        shiny::validate(
          shiny::need(c("cell_dependency_exp") %in% data()$validate, "No expression data for this cell line"))
        DT::datatable(
          make_cell_sim_table(input = data(),
                              similarity = "expression")$top_table %>%
            dplyr::mutate(cell2_name = map_chr(cell2_name, cell_linkr, type = "cell") #from fun_helper.R
            ) %>%
            dplyr::rename("Query" = "cell1_name", "Cell" = "cell2_name", "Lineage" = "lineage", 
                          "Sub-lineage" = "lineage_subtype", "Estimate" = "coef", 
                          "P.Value" = "pval", "Bonferroni" = "bonferroni",
                          "Sex" = "sex", "Age" = "age", "Status" = "status") %>% 
            dplyr::arrange(Bonferroni) %>% 
            dplyr::select(Query, Cell, input$vars_cells_exp_top) %>% 
            dplyr::mutate_if(is.numeric, ~ signif(., digits = 3)),
          rownames = FALSE,
          escape = FALSE,
          options = list(pageLength = 25))
      })
    }
  )
}

similarCompoundsTable <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(h4(textOutput(ns("text_compound_dep_top")))),
    fluidRow(checkboxGroupInput(inputId = ns("vars_compound_dep_top"), 
                                "Select columns:",
                                c("Mechanism", "R^2", "Z-Score"), #"Co-publication Count", "Co-publication Index"
                                selected = c("Z-Score"), #, "Co-publication Count"
                                inline = TRUE)),
    fluidRow(DT::dataTableOutput(outputId = ns("compound_dep_top")))
  )
}

similarCompoundsTableServer <- function (id, data) {
  moduleServer(
    id,
    function(input, output, session) { 
      output$text_compound_dep_top <- renderText({paste0(nrow(make_compound_table(input = data(), top = TRUE)), " compounds with similar dependencies as ", str_c(data()$content, collapse = ", "))})      
      output$compound_dep_top <- DT::renderDataTable({
        shiny::validate(
          shiny::need(c("compound_prism_cor_nest") %in% data()$validate, "No data for this compound"))
        DT::datatable(
          make_compound_table(input = data(), top = TRUE) %>%
            dplyr::mutate(name = map_chr(name, drug_linkr), #from fun_helper.R
                          r2 = round(r2, 2), 
                          z_score = round((r2 - mean_virtual_prism_cor)/sd_virtual_prism_cor, 1) 
            ) %>% 
            dplyr::rename("R^2" = "r2", "Z-Score" = "z_score", "Mechanism" = "moa") %>% 
            dplyr::select("Query" = "fav_drug", "Drug" = "name", input$vars_compound_dep_top),
          rownames = FALSE,
          escape = FALSE,
          options = list(pageLength = 25))
      })
    }
  )
}

##Dissimilar----
dissimilarGenesTable <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(h4(textOutput(ns("text_dep_bottom")))),
    fluidRow(checkboxGroupInput(inputId = ns("vars_dep_bottom"), 
                                "Select columns:",
                                c("R^2", "Z-Score", "Co-publication Count", "Co-publication Index"), # "GLS p-value"
                                selected = c("R^2", "Co-publication Count"), # "GLS p-value"
                                inline = TRUE)),
    fluidRow(DT::dataTableOutput(outputId = ns("dep_bottom")))
  )
}

dissimilarGenesTableServer <- function (id, data) {
  moduleServer(
    id,
    function(input, output, session) { 
      output$text_dep_bottom <- renderText({paste0(nrow(make_bottom_table(input = data())), 
                                                   " genes with dissimilar dependencies as ", 
                                                   ifelse(data()$subtype == "pathway",
                                                             "pathway genes",
                                                             str_c(data()$content, collapse = ", ")
                                                             )
                                                      )
        })     
      output$dep_bottom <- DT::renderDataTable({
        shiny::validate(
          shiny::need(c("gene_master_bottom_table") %in% data()$validate, "No dependency data for this gene"))
        
        DT::datatable(
          ddh::make_bottom_table(input = data()) %>%
            dplyr::rename("Query" = "id", "Gene" = "gene", "Name" = "name", 
                          "R^2" = "r2", "Z-Score" = "z_score", "Co-publication Count" = "concept_count", "Co-publication Index" = "concept_index") %>%
            dplyr::select("Query", "Gene", "Name", input$vars_dep_bottom) %>%
            dplyr::mutate_if(is.numeric, ~ signif(., digits = 3)) %>% 
            dplyr::mutate(Gene = map_chr(Gene, internal_link, linkout_img = FALSE)),
          rownames = FALSE,
          escape = FALSE,
          options = list(pageLength = 25))
      })
    }
  )
}

dissimilarCellsTable <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(h4(textOutput(ns("text_cells_dep_bottom")))),
    fluidRow(checkboxGroupInput(inputId = ns("vars_cells_dep_bottom"),
                                label = "Select columns:",
                                choices = c("Lineage", "Sub-lineage", "Estimate",
                                            "P.Value", "Bonferroni", "Sex", "Age", "Status"),
                                selected = c("Lineage", "Estimate", "Bonferroni"),
                                inline = TRUE)),
    fluidRow(DT::dataTableOutput(outputId = ns("cells_dep_bottom")))
  )
}

dissimilarCellsTableServer <- function (id, data) {
  moduleServer(
    id,
    function(input, output, session) { 
      output$text_cells_dep_bottom <- renderText({paste0(nrow(make_cell_sim_table(input = data())$bottom_table), 
                                                         " cells with dissimilar dependency profiles as ",
                                                         ifelse(data()$subtype == "pathway",
                                                             "pathway genes",
                                                             str_c(data()$content, collapse = ", ")
                                                             )
                                                      )
        })   
      output$cells_dep_bottom <- DT::renderDataTable({
        shiny::validate(
          shiny::need(c("cell_dependency_sim") %in% data()$validate, "No dependency data for this cell line"))
        DT::datatable(
          make_cell_sim_table(input = data())$bottom_table %>%
            dplyr::mutate(cell2_name = map_chr(cell2_name, cell_linkr, type = "cell") #from fun_helper.R
            ) %>%
            dplyr::rename("Query" = "cell1_name", "Cell" = "cell2_name", "Lineage" = "lineage", 
                          "Sub-lineage" = "lineage_subtype", "Estimate" = "coef", 
                          "P.Value" = "pval", "Bonferroni" = "bonferroni",
                          "Sex" = "sex", "Age" = "age", "Status" = "status") %>% 
            dplyr::arrange(Bonferroni) %>% 
            dplyr::select(Query, Cell, input$vars_cells_dep_bottom) %>% 
            dplyr::mutate_if(is.numeric, ~ signif(., digits = 3)),
          rownames = FALSE,
          escape = FALSE,
          options = list(pageLength = 25))
      })
    }
  )
}

dissimilarExpCellsTable <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(h4(textOutput(ns("text_cells_exp_bottom")))),
    fluidRow(checkboxGroupInput(inputId = ns("vars_cells_exp_bottom"),
                                label = "Select columns:",
                                choices = c("Lineage", "Sub-lineage", "Estimate",
                                            "P.Value", "Bonferroni", "Sex", "Age", "Status"),
                                selected = c("Lineage", "Estimate", "Bonferroni"),
                                inline = TRUE)),
    fluidRow(DT::dataTableOutput(outputId = ns("cells_exp_bottom")))
  )
}

dissimilarExpCellsTableServer <- function (id, data) {
  moduleServer(
    id,
    function(input, output, session) { 
      output$text_cells_exp_bottom <- renderText({paste0(nrow(make_cell_sim_table(input = data(),
                                                                                  similarity = "expression")$bottom_table),
                                                         " cells with dissimilar expression profiles as ", 
                                                         ifelse(data()$subtype == "pathway",
                                                             "pathway genes",
                                                             str_c(data()$content, collapse = ", ")
                                                             )
                                                      )
        })    
      output$cells_exp_bottom <- DT::renderDataTable({
        shiny::validate(
          shiny::need(c("cell_dependency_exp") %in% data()$validate, "No expression data for this cell line"))
        DT::datatable(
          make_cell_sim_table(input = data(),
                              similarity = "expression")$bottom_table %>%
            dplyr::mutate(cell2_name = map_chr(cell2_name, cell_linkr, type = "cell") #from fun_helper.R
            ) %>%
            dplyr::rename("Query" = "cell1_name", "Cell" = "cell2_name", "Lineage" = "lineage", 
                          "Sub-lineage" = "lineage_subtype", "Estimate" = "coef", 
                          "P.Value" = "pval", "Bonferroni" = "bonferroni",
                          "Sex" = "sex", "Age" = "age", "Status" = "status") %>% 
            dplyr::arrange(Bonferroni) %>% 
            dplyr::select(Query, Cell, input$vars_cells_exp_bottom) %>% 
            dplyr::mutate_if(is.numeric, ~ signif(., digits = 3)),
          rownames = FALSE,
          escape = FALSE,
          options = list(pageLength = 25))
      })
    }
  )
}

dissimilarCompoundsTable <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(h4(textOutput(ns("text_compound_dep_bottom")))),
    fluidRow(checkboxGroupInput(inputId = ns("vars_compound_dep_bottom"), 
                                "Select columns:",
                                c("Mechanism", "R^2", "Z-Score"), #"Co-publication Count", "Co-publication Index"
                                selected = c("Z-Score"), #, "Co-publication Count"
                                inline = TRUE)),
    fluidRow(DT::dataTableOutput(outputId = ns("compound_dep_bottom")))
  )
}

dissimilarCompoundsTableServer <- function (id, data) {
  moduleServer(
    id,
    function(input, output, session) { 
      output$text_compound_dep_bottom <- renderText({paste0(nrow(make_compound_table(input = data()$content, top = FALSE)), " compounds with inverse dependencies as ", str_c(data()$content, collapse = ", "))})      
      output$compound_dep_bottom <- DT::renderDataTable({
        shiny::validate(
          shiny::need(c("compound_prism_cor_nest") %in% data()$validate, "No data for this compound"))
        DT::datatable(
          make_compound_table(input = data()$content, top = FALSE) %>%
            dplyr::mutate(name = map_chr(name, drug_linkr), #from fun_helper.R
                          r2 = round(r2, 2), 
                          z_score = round((r2 - mean_virtual_prism_cor)/sd_virtual_prism_cor, 1) 
            ) %>% 
            dplyr::rename("R^2" = "r2", "Z-Score" = "z_score", "Mechanism" = "moa") %>% 
            dplyr::select("Query" = "fav_drug", "Drug" = "name", input$vars_compound_dep_bottom),
          rownames = FALSE,
          escape = FALSE,
          options = list(pageLength = 25))
      })
    }
  )
}

##CCA-----
geneCCATable <- function (id) {
  ns <- NS(id)
  tagList(
    fluidRow(h4(textOutput(ns("cca_table_text")))),
    fluidRow(
      div(
        id = ns("cca_table_id"),
        style = "padding-left:1%",
        DT::dataTableOutput(outputId = ns("cca_table")),
        actionLink(inputId = ns("cca_table_click"), " View plot")
      )
    ),
    conditionalPanel(condition = paste0("input['", ns("cca_table_click"), "'] != 0"),
                     fluidRow(
                       div(
                         id = ns("cca_table_plot_id"),
                         style = "padding-left:1%",
                         h4(textOutput(ns("cca_plot_text"))),
                         plotOutput(outputId = ns("cca_plot")),
                         fluidRow(ddh::make_legend("make_cca_genes"))
                       )
                     )
    )
  )
}

geneCCATableServer <- function(id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$cca_table_text <- renderText({paste0("Co-essential pathways for ", 
                                                  ifelse(data()$subtype == "pathway",
                                                         "the queried pathway",
                                                         str_c(data()$content, collapse = ", ")
                                                  )
      )
      })
      output$cca_table <- DT::renderDataTable({
        #check to see if data are there
        shiny::validate(
          shiny::need(c("gene_cca_pathway") %in% data()$validate, "No co-essential pathway data available."))
        #render table
        if (data()$subtype != "pathway") {
          DT::datatable(ddh::make_cca_genes_table(input = data()) %>%
                          dplyr::mutate_if(is.numeric, ~ signif(., digits = 3)) %>% 
                          dplyr::rename(Correlation = CC),
                        rownames = FALSE,
                        escape = FALSE,
                        options = list(pageLength = 10)
                        )
        } else {
          DT::datatable(ddh::make_cca_pathways_table(input = data()) %>%
                          dplyr::mutate_if(is.numeric, ~ signif(., digits = 3)) %>% 
                          dplyr::rename(Correlation = CC),
                        rownames = FALSE,
                        escape = FALSE,
                        options = list(pageLength = 10)
                        )
        }
      })
      # Conditional plot
      output$cca_plot_text <- renderText({paste0("Co-essential pathways for ", 
                                                 ifelse(data()$subtype == "pathway",
                                                        "the queried pathway",
                                                        str_c(data()$content, collapse = ", ")
                                                 )
      )
      })
      output$cca_plot <- renderPlot({
        #check to see if data are there
        shiny::validate(
          shiny::need(c("universal_achilles_long") %in% data()$validate, "No data found."))
        #plot
        if (data()$subtype != "pathway") {
          ddh::make_cca_genes(input = data())
        } else {
          ddh::make_cca_pathways(input = data())
        }
      })
    }
  )
}

##Metabolites-----
#module that displays a table for metabolites
metabolitesTable <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(h4(textOutput(ns("title_metabolites_table")))),
    fluidRow(DT::dataTableOutput(outputId = ns("metabolites_table")))
  )
}

metabolitesTableServer <- function(id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$title_metabolites_table <- renderText({paste0("Metabolite table for ", 
                                                           ifelse(data()$subtype == "pathway",
                                                                  "pathway genes",
                                                                  str_c(data()$content, collapse = ", ")
                                                           )
      )
      })
      output$metabolites_table <- DT::renderDataTable({
        if(data()$type == "gene") {
          shiny::validate(
            shiny::need(c("compound_hmdb_proteins") %in% data()$validate, 
                        "No metabolites associated with this query"))
          DT::datatable(ddh::make_metabolite_table(input = data()) %>% 
                          dplyr::mutate(metabolite = map_chr(metabolite, metabolite_linkr)) %>% 
                          dplyr::select('Gene Name' = id, 
                                        'Metabolite' = metabolite),
                        rownames = FALSE,
                        escape = FALSE,
                        options = list(pageLength = 10))
        } else if(data()$type == "cell") {
          shiny::validate(
            shiny::need(c("cell_metabolites") %in% data()$validate, 
                        "No metabolites associated for this query"))
          DT::datatable(make_metabolite_table(input = data()) %>%
                          dplyr::mutate(metabolite = map_chr(metabolite, metabolite_linkr)) %>% 
                          dplyr::select('Cell Line' = id, 
                                        'Metabolite' = metabolite, 
                                        'Value' = value) %>% 
                          mutate(Value = round(Value, 3)),
                        rownames = FALSE,
                        escape = FALSE,
                        options = list(pageLength = 10))
        }
      })
    }
  )
}

##Drug Tables -----
#search gene, find drugs
geneDrugsTable <- function(id) { #GENE QUERY
  ns <- NS(id)
  tagList(
    fluidRow(h4(textOutput(ns("title_gene_drugs_table")))),
    fluidRow(DT::dataTableOutput(outputId = ns("gene_drugs_table")))
  )
}

geneDrugsTableServer <- function (id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$title_gene_drugs_table <- renderText({paste0("Drugs annotated to target ",
                                                          ifelse(data()$subtype == "pathway",
                                                             "pathway genes",
                                                             str_c(data()$content, collapse = ", ")
                                                             )
                                                      )
        })
      output$gene_drugs_table <- DT::renderDataTable({
        shiny::validate(
          shiny::need(c("gene_drugs_table") %in% data()$validate, 
                      "No drugs annotated for this query"))
        DT::datatable(ddh::make_gene_drugs_table(input = data()) %>% 
                        dplyr::mutate(fav_drug = map_chr(fav_drug, drug_linkr), 
                                      moa = map_chr(moa, moa_linkr)) %>% #from fun_helper.R
                        dplyr::rename(Gene = id, Drug = fav_drug, `Mechanism of Action` = moa), 
                      rownames = FALSE,
                      escape = FALSE)
      })
    }
  )
}

cellDrugsTable <- function(id) { 
  ns <- NS(id)
  tagList(
    fluidRow(h4(textOutput(ns("title_cell_drugs_table")))),
    fluidRow(DT::dataTableOutput(outputId = ns("cell_drugs_table")))
  )
}

cellDrugsTableServer <- function (id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$title_cell_drugs_table <- renderText({paste0("Drugs annotated for ", 
                                                          ifelse(data()$subtype == "pathway",
                                                                 "pathway genes",
                                                                 str_c(data()$content, collapse = ", ")
                                                          )
      )
      })
      output$cell_drugs_table <- DT::renderDataTable({
        shiny::validate(
          shiny::need(c("universal_prism_long") %in% data()$validate, 
                      "No drugs annotated for this query"))
        DT::datatable(make_cell_drugs_table(input = data()) %>% 
                        dplyr::mutate(name = map_chr(name, drug_linkr),
                                      log2fc = round(log2fc, 3)) %>% #from fun_helper.R
                        dplyr::rename(`Cell Line` = cell_line, Drug = name, LogFC = log2fc), 
                      rownames = FALSE,
                      escape = FALSE)
      })
    }
  )
}

#cor tables
geneDrugsCorTable <- function(id) { #GENE QUERY
  ns <- NS(id)
  tagList(
    fluidRow(h4(textOutput(ns("title_gene_drugs_cor_table")))),
    fluidRow(checkboxGroupInput(inputId = ns("vars_gene_drugs_cor_table"), 
                                "Select columns:",
                                c("Known", "R^2", "Z-Score"), 
                                selected = c("Z-Score"), 
                                inline = TRUE)),
    fluidRow(DT::dataTableOutput(outputId = ns("gene_drugs_cor_table"))), 
    tags$br(),
    fluidRow(textOutput(ns("text_gene_drugs_cor_table")))
  )
}

geneDrugsCorTableServer <- function (id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$title_gene_drugs_cor_table <- renderText({paste0("Drug correlation table for ", 
                                                      ifelse(data()$subtype == "pathway",
                                                             "pathway genes",
                                                             str_c(data()$content, collapse = ", ")
                                                             )
                                                      )
        })
      output$text_gene_drugs_cor_table <- renderText({paste0("When knocking out ", 
                                                             ifelse(data()$subtype == "pathway",
                                                                    "pathway genes",
                                                                    str_c(data()$content, collapse = ", ")
                                                             ),
                                                             " a subset of cells die. These are the drugs that show the same cell killing profile."
      )
      })
      output$gene_drugs_cor_table <- DT::renderDataTable({
        shiny::validate(
          shiny::need(c("gene_drugs_cor_table") %in% data()$validate, 
                      "No drugs associated with this query"))
        
        #gene_drug_list will grab a char vec of drugs known to target genes, so we can check if corr's are known; added ifelse logic so index grab doesn't break
        gene_drug_tibble <- gene_drugs_table %>% filter(fav_gene %in% data()$content)
        gene_drug_list <- ifelse(nrow(gene_drug_tibble) != 0, gene_drug_tibble %>% unnest(data) %>% pull(fav_drug), "")
        DT::datatable(make_gene_drugs_cor_table(input = data()) %>% 
                        dplyr::mutate(
                          known = case_when(
                            drug %in% gene_drug_list ~ "TRUE", 
                            TRUE ~ "FALSE"),
                          drug = map_chr(drug, drug_linkr) #from fun_helper.R
                        ) %>% 
                        dplyr::rename(Gene = fav_gene, Drug = drug, Mechanism = moa, 'Known' = known, 'Z-Score' = z_score, 'R^2'=r2) %>% 
                        dplyr::select("Gene", "Drug", "Mechanism", input$vars_gene_drugs_cor_table), 
                      rownames = FALSE,
                      escape = FALSE)
      })
    }
  )
}

#CELL-----
cellSummaryTable <- function (id) {
  ns <- NS(id)
  tagList(
    fluidRow(h4(textOutput(ns("cell_sumary_title")))),
    fluidRow(DT::dataTableOutput(outputId = ns("cell_table")))
  )
}

cellSummaryTableServer <- function(id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$cell_sumary_title <- renderText({paste0("Lineage table for ", 
                                                     ifelse(data()$subtype == "pathway",
                                                            "pathway genes",
                                                            str_c(data()$content, collapse = ", ")
                                                     )
      )
      })
      output$cell_table <- DT::renderDataTable({
        shiny::validate(
          shiny::need(c("cell_expression_meta") %in% data()$validate, "No summary data for this cell line"))
        DT::datatable(
          make_cell_line_table(input = data()) %>% 
            dplyr::mutate(`Cell Line` = map_chr(`Cell Line`, cell_linkr, type = "cell")),
          rownames = FALSE,
          escape = FALSE,
          options = list(paging = FALSE, 
                         searching = FALSE,
                         pageLength = 10))
      })      
    }
  )
}

#COMPOUND-----
##Pubmed-----
pubmedCompoundTable <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(h4(textOutput(ns("pubmed_compound_title")))),
    fluidRow(DT::dataTableOutput(outputId = ns("pubmed_compound_table")))
  )
}

pubmedCompoundTableServer <- function(id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$pubmed_compound_title <- renderText({paste0("Literature table for ", 
                                                         ifelse(data()$subtype == "pathway",
                                                                "pathway genes",
                                                                str_c(data()$content, collapse = ", ")
                                                         )
      )
      })
      output$pubmed_compound_table <- DT::renderDataTable({
        shiny::validate(
          shiny::need(c("universal_pubmed") %in% data()$validate, ""))
        withProgress(message = 'Building a smart table...', {
          DT::datatable(make_pubmed_table(input = data()) %>% 
                          dplyr::mutate(pmid = map_chr(pmid, pubmed_linkr, number_only = TRUE) #from fun_helper.R
                          ) %>% 
                          dplyr::mutate(pmcid = map_chr(pmcid, pmc_linkr) #from fun_helper.R
                          ) %>% 
                          dplyr::rename(Name = name, 'Pubmed ID' = pmid, Year = year, PMCID = pmcid),
                        rownames = FALSE,
                        escape = FALSE,
                        options = list(pageLength = 10))
        })
      })
    }
  )
}

pubmedCellLineTable <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(h4(textOutput(ns("pubmed_cell_line_title")))),
    fluidRow(DT::dataTableOutput(outputId = ns("pubmed_cell_line_table")))
  )
}

pubmedCellLineTableServer <- function(id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$pubmed_cell_line_title <- renderText({paste0("Literature table for ", 
                                                          ifelse(data()$subtype == "pathway",
                                                                 "pathway genes",
                                                                 str_c(data()$content, collapse = ", ")
                                                          )
      )
      })
      output$pubmed_cell_line_table <- DT::renderDataTable({
        shiny::validate(
          shiny::need(c("universal_pubmed") %in% data()$validate, ""))
        withProgress(message = 'Building a smart table...', {
          DT::datatable(make_pubmed_table(input = data()) %>% 
                          dplyr::mutate(pmid = map_chr(pmid, pubmed_linkr, number_only = TRUE) #from fun_helper.R
                          ) %>% 
                          dplyr::mutate(pmcid = map_chr(pmcid, pmc_linkr) #from fun_helper.R
                          ) %>% 
                          dplyr::rename(Name = name, 'Pubmed ID' = pmid, Year = year, PMCID = pmcid),
                        rownames = FALSE,
                        escape = FALSE,
                        options = list(pageLength = 10))
        })
      })
    }
  )
}

##Drugs-----
drugGenesTable <- function(id) { #DRUG QUERY
  ns <- NS(id)
  tagList(
    fluidRow(h4(textOutput(ns("title_drug_genes_table")))),
    fluidRow(DT::dataTableOutput(outputId = ns("drug_genes_table"))), 
    tags$br(),
    fluidRow(textOutput(ns("text_drug_genes_table")))
  )
}

drugGenesTableServer <- function (id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$title_drug_genes_table <- renderText({paste0("Gene target table for ", 
                                                          ifelse(data()$subtype == "pathway",
                                                                 "pathway genes",
                                                                 str_c(data()$content, collapse = ", ")
                                                          )
      )
      })
      output$text_drug_genes_table <- renderText({paste0("Genes annotated to be targeted by ", 
                                                         ifelse(data()$subtype == "pathway",
                                                                "pathway genes",
                                                                str_c(data()$content, collapse = ", ")
                                                         )
      )
      })
      output$drug_genes_table <- DT::renderDataTable({
        shiny::validate(
          shiny::need(c("compound_genes_table") %in% data()$validate, "No gene data for this compound"))
        DT::datatable(make_drug_genes_table(data()$content) %>% 
                        dplyr::mutate(fav_gene = map_chr(fav_gene, internal_link)) %>% #from fun_helper.R
                        dplyr::rename(Drug = fav_drug, Gene = fav_gene, Name = approved_name), 
                      rownames = FALSE,
                      escape = FALSE)
      })
    }
  )
}

drugGenesCorTable <- function(id) { #DRUG QUERY
  ns <- NS(id)
  tagList(
    fluidRow(h4(textOutput(ns("title_drug_genes_cor_table")))),
    fluidRow(checkboxGroupInput(inputId = ns("vars_drug_genes_cor_table"), 
                                "Select columns:",
                                c("Known", "R^2", "Z-Score"), 
                                selected = c("Z-Score"), 
                                inline = TRUE)),
    fluidRow(DT::dataTableOutput(outputId = ns("drug_genes_cor_table"))), 
    br(),
    fluidRow(textOutput(ns("text_drug_genes_cor_table")))
  )
}

drugGenesCorTableServer <- function (id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$title_drug_genes_cor_table <- renderText({paste0("Gene correlation table for ", 
                                                              ifelse(data()$subtype == "pathway",
                                                                     "pathway genes",
                                                                     str_c(data()$content, collapse = ", ")
                                                              )
      )
      })
      output$text_drug_genes_cor_table <- renderText({paste0("When ", 
                                                             ifelse(data()$subtype == "pathway",
                                                                    "pathway genes",
                                                                    str_c(data()$content, collapse = ", ")
                                                             ),
                                                             " is/are placed on cells, a subset dies. These are the genes that show the same cell dependency profile."
      )
      })
      output$drug_genes_cor_table <- DT::renderDataTable({
        shiny::validate(
          shiny::need(c("compound_genes_cor_table") %in% data()$validate, "No gene data for this compound"))
        #drug_gene_list will grab a char vec of genes known to be targeted compound, so we can check if corr's are known; added ifelse logic so index grab doesn't break
        drug_gene_tibble <- drug_genes_table %>% filter(fav_drug %in% data()$content)
        drug_gene_list <- ifelse(nrow(drug_gene_tibble) != 0, drug_gene_tibble %>% unnest(data) %>% pull(fav_gene), "")
        DT::datatable(make_drug_genes_cor_table(data()$content) %>% 
                        dplyr::mutate(
                          known = case_when(
                            gene %in% drug_gene_list ~ "TRUE", 
                            TRUE ~ "FALSE"),
                          gene = map_chr(gene, internal_link) #from fun_helper.R
                        ) %>% 
                        dplyr::rename(Drug = fav_drug, Gene = gene, Name = approved_name, 'Known' = known, 'Z-Score' = z_score, 'R^2'=r2) %>% 
                        dplyr::select("Drug", "Gene", "Name", input$vars_drug_genes_cor_table), 
                      rownames = FALSE,
                      escape = FALSE)
      })
    }
  )
}

##Metabolites----
metaboliteGenesTable <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(h4(textOutput(ns("metabolite_genes_text")))),
    fluidRow(DT::dataTableOutput(outputId = ns("metabolite_genes_table")))
  )
}

metaboliteGenesTableServer <- function(id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$metabolite_genes_text <- renderText({paste0("Metabolite table for ", 
                                                         ifelse(data()$subtype == "pathway",
                                                                "pathway genes",
                                                                str_c(data()$content, collapse = ", ")
                                                         )
      )
      })
      output$metabolite_genes_table <- DT::renderDataTable({
        shiny::validate(
          shiny::need(c("compound_hmdb_metabolites") %in% data()$validate, 
                      "No metabolites associated with this query"))
        DT::datatable(make_metabolite_table(input = data()) %>% 
                        dplyr::mutate(gene_name = map_chr(gene_name, internal_link)) %>% 
                        dplyr::select('Metabolite' = metabolite_name, 'Gene Name' = gene_name),
                      rownames = FALSE,
                      escape = FALSE,
                      options = list(pageLength = 10))
      })
    }
  )
}

