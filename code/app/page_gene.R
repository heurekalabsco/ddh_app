#this page is for the master gene query, including genes, pathways of genes, and custom gene lists
#it includes a specific function variable 'type' to change shiny module that is called (and assoc. fun) for the description page
genePage <- function (id, subtype) {
  ns <- NS(id)
  
  #this block is the logic to define the summary_var variable, to display the proper summary module
  if(subtype == "gene"){
    title_var <- geneTitle(ns("title_var"))
    gene_var <- customListText(ns("gene_var"))
    protein_summary <- proteinText(ns("protein_summary"))
    summary_table <- pathwayList(ns("gene_pathways"))
  } else if (subtype == "pathway"){
    title_var <- pathwayTitle(ns("title_var"))
    gene_var <- pathwayText(ns("gene_var"))
    protein_summary <- pathwayText(ns("protein_summary"))
    summary_table <- pathwayList(ns("gene_pathways"))
  } else if (subtype == "gene_list") {
    title_var <- customListTitle(ns("title_var"))
    gene_var <- customListText(ns("gene_var"))
    protein_summary <- proteinText(ns("protein_summary"))
    summary_table <- pathwayList(ns("gene_pathways"))
  } else {
    stop("call your summary argument")
  }
  
  tagList(
    head_tags,
    ddhNavbarPage(
      id=ns("geneNavBar"),
      ## DASHBOARD-----
      tabPanel("SUMMARY",
               title_var,
               cardLayout(
                 actionLink(inputId = ns("link_to_ideogramPlotDash"), ideogramPlotDash(ns("ideogramdash"))),
                 actionLink(inputId = ns("link_to_sequencePlotDash"), sequenceDash(ns("sequencedash"))),
                 actionLink(inputId = ns("link_to_pubmedGenePlotDash"), pubmedPlotDash(ns("pubmedgenedash"))),
                 actionLink(inputId = ns("link_to_cellAnatogramPlotDash"), cellAnatogramPlotDash(ns("cellanatogramdash"))),
                 actionLink(inputId = ns("link_to_cellExpressionPlotDash"), cellExpressionPlotDash(ns("cellexpressiondash"))),
                 actionLink(inputId = ns("link_to_tissueAnatogramPlotDash"), tissueAnatogramPlotDash(ns("tissueanatogramdash"))),
                 actionLink(inputId = ns("link_to_cellDependenciesPlotDash"), cellDependenciesPlotDash(ns("depdash"))),
                 actionLink(inputId = ns("link_to_cellDependenciesTableDash"), cellDependenciesTableDash(ns("deptabledash"))),
                 private(actionLink(inputId = ns("link_to_geneDrugsCorTableDash"), geneDrugsCorTableDash(ns("genedrugscortabledash")))),
                 private(actionLink(inputId = ns("link_to_geneMolecularFeaturesTableDash"), geneMolecularFeaturesTableDash(ns("molfeattabledash"))))
               )
      ),
      ## INFO (person)-----
      navbarMenu("INFO",
                 tabPanel(title = "Gene", value = "about_gene",
                          shinyjs::useShinyjs(),
                          #summary
                          fluidRow(
                            div(
                              id = ns("gene_summary_tabcard"),
                              style = "padding-left:1%",
                              column(8, gene_var), # summary variable for alt descriptions
                              column(4, ideogramPlot(ns("chromo")))
                            )
                          ),
                          tags$hr(),
                          # cards in a fluid row
                          fluidRow(
                            cardLayout(
                              barcodeTab(ns("barcode_tabcard")),
                              actionLink(inputId = ns("go_click"), pathwaysTableTab(ns("gotab"))) 
                            )
                          ),
                          tags$br(),
                          #conditional for go table
                          fluidRow(
                            shinyjs::hidden(
                              div(
                                id = ns("go_tabcard"),
                                style = "padding-left:1%",
                                #instead of modules here, module logic is above
                                summary_table,
                                tags$br()
                              )
                            )
                          )
                 ), #end tab panel
                 tabPanel(title = "Protein", value = "about_protein",
                          shinyjs::useShinyjs(),
                          #summary
                          fluidRow(
                            div(
                              id = ns("protein_summary_tabcard"),
                              style = "padding-left:1%",
                              column(8, protein_summary),
                              column(4, proteinStructurePlot(ns("structure")))
                            )
                          ),
                          #"ADD: seq, blastP link, pfam, "
                          tags$hr(),
                          # cards in a fluid row
                          fluidRow(
                            cardLayout(
                              actionLink(inputId = ns("size_click"), sizePlotTab(ns("sizetab"))), 
                              actionLink(inputId = ns("sequence_click"), sequencePlotTab(ns("sequencetab"))), 
                              actionLink(inputId = ns("signature_click"), signaturePlotTab(ns("signaturetab"))), 
                              actionLink(inputId = ns("structure_click"), structurePlotTab(ns("structuretab")))
                            )
                          ),
                          tags$br(),
                          #conditional for size
                          fluidRow(
                            shinyjs::hidden(
                              div(
                                id = ns("size_tabcard"),
                                style = "padding-left:1%",
                                proteinSizePlot(ns("protein_size")),
                                tags$br()
                              )
                            )
                          ), 
                          #conditional for sequence
                          fluidRow(
                            shinyjs::hidden(
                              div(
                                id = ns("sequence_tabcard"),
                                style = "padding-left:1%",
                                proteinSeq(ns("protein_seq")),
                                proteinDomainPlot(ns("protein_domain_plot")),
                                tags$br()
                              )
                            )
                          ), 
                          #conditional for signature
                          fluidRow(
                            shinyjs::hidden(
                              div(
                                id = ns("signature_tabcard"),
                                style = "padding-left:1%",
                                private_msg(),
                                private(AABarPlot(ns("aa_bar_plot"))),
                                private(radialPlot(ns("radial_plot"))),
                                tags$br(),
                                fluidRow(
                                  div(
                                    id = ns("signature_tabcard_umap"),
                                    style = "padding-left:1%",
                                    column(7, private(UMAPPlot(ns("umap_plot")))),
                                    column(5, private(proteinClusterTable(ns("prot_clust_table"))))
                                  )
                                ),
                                private(clusterRadialPlot(ns("cluster_radial_plot"))),
                                private(clusterEnrichmentPlot(ns("cluster_enrichment_plot"))),
                                tags$br()
                              )
                            )
                          ),
                          #conditional for structure
                          fluidRow(
                            shinyjs::hidden(
                              div(
                                id = ns("structure_tabcard"),
                                style = "padding-left:1%",
                                private_msg(),
                                private(proteinStructurePlot3d(ns("protein_structure_plot3d"))),
                                tags$br()
                              )
                            )
                          )
                 ), #end tab panel
                 tabPanel(title = "Literature", value = "about_literature",
                          fluidRow(
                            div(
                              id = ns("literature_tabcard"),
                              style = "padding-left:1%",
                              pubmedPlot(ns("pubmed")), 
                              pubmedTable(ns("pubmed")),
                              tags$br()
                              )
                            )
                          )
      ),
      ## EXPRESSION (place)-----
      navbarMenu(title = "EXPRESSION",
                 tabPanel("Sub-cellular", value = "expression_sub",
                          fluidRow(
                            div(
                              id = ns("subcellular_tabcard"),
                              style = "padding-left:1%",
                              cellAnatogramPlot(ns("exp")),
                              cellAnatogramFacetPlot(ns("exp")),
                              cellAnatogramTable(ns("exp")),
                              tags$br()
                            )
                          )
                 ), 
                 tabPanel("Cell Line", value = "expression_cell", 
                          shinyjs::useShinyjs(),
                          # summary plot
                          fluidRow(
                            div(
                              id = ns("cell_line_expression_tabcard"),
                              style = "padding-left:1%",
                              cellGeneExpressionPlot(ns("cell_gene"))
                            )
                          ),
                          tags$hr(),
                          # cards in a fluid row
                          fluidRow(
                            cardLayout(
                              actionLink(inputId = ns("gene_exp_table_click"), cellGeneExpressionTableTab(ns("gene_exp_table_tab"))), 
                              actionLink(inputId = ns("protein_exp_plot_click"), cellProteinExpressionPlotTab(ns("protein_exp_plot_tab"))), 
                              actionLink(inputId = ns("protein_exp_table_click"), cellProteinExpressionTableTab(ns("protein_exp_table_tab"))), 
                              actionLink(inputId = ns("gene_protein_plot_click"), cellGeneProteinPlotTab(ns("gene_protein_plot_tab")))
                            )
                          ),
                          tags$br(),
                          #conditional for cellGeneExpressionTableTab
                          fluidRow(
                            shinyjs::hidden(
                              div(
                                id = ns("gene_exp_table_tabcard"),
                                style = "padding-left:1%",
                                cellGeneExpressionTable(ns("cell_gene_table")),
                                tags$br()
                              )
                            )
                          ), 
                          #conditional for cellProteinExpressionPlot
                          fluidRow(
                            shinyjs::hidden(
                              div(
                                id = ns("protein_exp_plot_tabcard"),
                                style = "padding-left:1%",
                                cellProteinExpressionPlot(ns("cell_protein")),
                                tags$br()
                              )
                            )
                          ), 
                          #conditional for cellProteinExpressionTable
                          fluidRow(
                            shinyjs::hidden(
                              div(
                                id = ns("protein_exp_table_tabcard"),
                                style = "padding-left:1%",
                                cellProteinExpressionTable(ns("cell_protein_table")),
                                tags$br()
                              )
                            )
                          ),
                          #conditional for cellGeneProteinPlot
                          fluidRow(
                            shinyjs::hidden(
                              div(
                                id = ns("gene_protein_plot_tabcard"),
                                style = "padding-left:1%",
                                cellGeneProteinPlot(ns("cell_geneprotein")),
                                tags$br()
                              )
                            )
                          )
                 ), #end tab panel
                 tabPanel("Tissue", value = "expression_tissue", 
                          shinyjs::useShinyjs(),
                          fluidRow(
                            div(
                              id = ns("tissue_text_tabcard"),
                              style = "padding-left:1%",
                              tissuePlotText(ns("tissue_text"))
                            )
                          ),
                          fluidRow(
                            div(
                              id = ns("tissue_summary_plot_tabcard"),
                              style = "padding-left:1%",
                              column(6, maleAnatogramPlot(ns("male_anatogram"))),
                              column(6, femaleAnatogramPlot(ns("female_anatogram")))
                            )
                          ),
                          tags$hr(),
                          # cards in a fluid row
                          fluidRow(
                            cardLayout(
                              actionLink(inputId = ns("tissue_plot_click"), tissuePlotTab(ns("tissue_plot_tab"))), 
                              actionLink(inputId = ns("tissue_table_click"), tissueTableTab(ns("tissue_table_tab")))
                            )
                          ),
                          tags$br(),
                          #conditional for tissuePlotTab
                          fluidRow(
                            shinyjs::hidden(
                              div(
                                id = ns("tissue_plot_tabcard"),
                                style = "padding-left:1%",
                                tissuePlot(ns("tissue_plot")),
                                tags$br()
                              )
                            )
                          ), 
                          #conditional for tissueTableTab
                          fluidRow(
                            shinyjs::hidden(
                              div(
                                id = ns("tissue_table_tabcard"),
                                style = "padding-left:1%",
                                tissueTable(ns("tissue_table")),
                                tags$br()
                              )
                            )
                          )
                 ) #end tab panel
      ),
      # COMPOUNDS (thing)-----
      navbarMenu(title = "COMPOUNDS",
                 tabPanel("Drugs", value = "drugs",
                          fluidRow(
                            div(
                              id = ns("gene_drugs_tabcard"),
                              style = "padding-left:1%",
                              private(geneDrugsTable(ns("gene_drugs"))), 
                              private_msg(),
                              tags$br()
                              )
                            )
                 ),
                 tabPanel("Metabolites", value = "metabolites",
                          fluidRow(
                            div(
                              id = ns("gene_metabolites_tabcard"),
                              style = "padding-left:1%",
                              private(metabolitesTable(ns("gene_metabolites"))),
                              private_msg(),
                              tags$br()
                            )
                          )
                 ),
                 tabPanel("Graph", value = "gene_bipartite_graph",
                          fluidRow(
                            div(
                              id = ns("gene_bipartite_tabcard"),
                              style = "padding-left:1%",
                              private(geneBipartiteGraph(ns("gene_bipartite_graph"))), 
                              private_msg(),
                              tags$br()
                            )
                          )
                 )
      ),
      ## DEPENDENCIES (action)-----
      navbarMenu(title = "DEPENDENCIES",
                 tabPanel("Data", value = "dependencies_plots", 
                          shinyjs::useShinyjs(),
                          # summary plot
                          fluidRow(
                            div(
                              id = ns("cell_dependencies_tabcard"),
                              style = "padding-left:1%",
                              cellDependenciesPlot(ns("dep"))
                            )
                          ),
                          tags$hr(),
                          # cards in a fluid row
                          fluidRow(
                            cardLayout(
                              actionLink(inputId = ns("dep_bar_click"), cellDependenciesBarPlotTab(ns("bar_dep"))), #barplot
                              actionLink(inputId = ns("dep_density_click"), cellDependenciesDensityPlotTab(ns("dep_density_tab"))), #density
                              actionLink(inputId = ns("dep_lineage_click"), cellDepsLinPlotTab(ns("dep_lineage_tab"))), #lineage
                              actionLink(inputId = ns("dep_sublineage_click"), cellDepsSubLinPlotTab(ns("dep_sublineage_tab"))), #sublineage
                              actionLink(inputId = ns("dep_table_click"), cellDependenciesTableTab(ns("dep_table_tab"))), #table
                              actionLink(inputId = ns("expdep_click"), expdepPlotTab(ns("expdep_plot_tab"))) #expdep
                            )
                          ),
                          tags$br(),
                          # conditional for barplot
                          fluidRow(
                            shinyjs::hidden(
                              div(
                                id = ns("barplot_tabcard"),
                                style = "padding-left:1%",
                                cellDependenciesBarPlot(ns("dep_barplot")), 
                                tags$br()
                              )
                            )
                          ),
                          #conditional for density
                          fluidRow(
                            shinyjs::hidden(
                              div(
                                id = ns("density_tabcard"),
                                style = "padding-left:1%",
                                cellDependenciesDensityPlot(ns("dep_density")),
                                tags$br()
                              )
                            )
                          ), 
                          #conditional for lineage
                          fluidRow(
                            shinyjs::hidden(
                              div(
                                id = ns("lineage_tabcard"),
                                style = "padding-left:1%",
                                cellDepsLinPlot(ns("dep_lineage")),
                                tags$br()
                              )
                            )
                          ), 
                          #conditional for sublineage
                          fluidRow(
                            shinyjs::hidden(
                              div(
                                id = ns("sublineage_tabcard"),
                                style = "padding-left:1%",
                                cellDepsSubLinPlot(ns("dep_sublineage")),
                                tags$br()
                              )
                            )
                          ), 
                          #conditional for table
                          fluidRow(
                            shinyjs::hidden(
                              div(
                                id = ns("dep_table_tabcard"),
                                style = "padding-left:1%",
                                cellDependenciesTable(ns("dep_table")),
                                tags$br()
                              )
                            )
                          ),
                          #conditional for expdep
                          fluidRow(
                            shinyjs::hidden(
                              div(
                                id = ns("expdep_plot_tabcard"),
                                style = "padding-left:1%",
                                expdepPlot(ns("expdep_plot")),
                                tags$br()
                              )
                            )
                          )
                 ),
                 tabPanel("Co-essentiality", value = "dependencies_co-essentiality",
                          shinyjs::useShinyjs(),
                          # summary plot
                          fluidRow(
                            div(
                              id = ns("coessentiality_tabcard"),
                              style = "padding-left:1%",
                              cellDependenciesCorrPlot(ns("corrplot"))
                            )
                          ),
                          tags$hr(),
                          # cards in a fluid row
                          fluidRow(
                            cardLayout(
                              actionLink(inputId = ns("dep_pos_table_click"), cellDependenciesPosTableTab(ns("dep_pos_table_tab"))), #pos table
                              actionLink(inputId = ns("dep_neg_table_click"), cellDependenciesNegTableTab(ns("dep_neg_table_tab"))), #neg table
                              actionLink(inputId = ns("dep_gene_pathways_click"), genePathwayEnrichmentTableTab(ns("dep_gene_pathways_tab"))),
                              actionLink(inputId = ns("dep_graph_click"), cellDependenciesGraphTab(ns("depgraphtab"))),
                              actionLink(inputId = ns("dep_cca_click"), geneCCATableTab(ns("depccatab")))
                            )
                          ),
                          tags$br(),
                          #conditional for pos table
                          fluidRow(
                            shinyjs::hidden(
                              div(
                                id = ns("dep_pos_table_tabcard"),
                                style = "padding-left:1%",
                                similarGenesTable(ns("sim")),
                                tags$br()
                              )
                            )
                          ),
                          #conditional for neg table
                          fluidRow(
                            shinyjs::hidden(
                              div(
                                id = ns("dep_neg_table_tabcard"),
                                style = "padding-left:1%",
                                dissimilarGenesTable(ns("dsim")),
                                tags$br()
                              )
                            )
                          ),
                          #conditional for pathway enrichment
                          fluidRow(
                            shinyjs::hidden(
                              div(
                                id = ns("dep_gene_pathways_tabcard"),
                                style = "padding-left:1%",
                                GenePathwayEnrichmentTable(ns("gene_paths_comps")),
                                tags$br()
                              )
                            )
                          ),
                          # conditional for graph
                          fluidRow(
                            shinyjs::hidden(
                              div(
                                id = ns("dep_graph_tabcard"),
                                style = "padding-left:1%",
                                geneNetworkGraph(ns("graph")),
                                tags$br()
                              )
                            )
                          ),
                          # conditional for CCA
                          fluidRow(
                            shinyjs::hidden(
                              div(
                                id = ns("dep_cca_tabcard"),
                                style = "padding-left:1%",
                                geneCCATable(ns("dep_cca")),
                                tags$br()
                              )
                            )
                          )
                 ), # end of tabPanel
                 tabPanel("Drugs", value = "dependencies_drugs", 
                          fluidRow(
                            div(
                              id = ns("drugs_tabcard"),
                              style = "padding-left:1%",
                              private(geneDrugsCorTable(ns("gene_drugs_cor"))), 
                              private_msg(),
                              tags$br()
                            )
                          )
                 ),
                 tabPanel("Molecular Features", value = "molecular_features",
                          shinyjs::useShinyjs(),
                          # summary plot
                          fluidRow(
                            div(
                              id = ns("molecular_features_segments_tabcard"),
                              style = "padding-left:1%",
                              private(MolecularFeaturesSegmentPlot(ns("mol_feat_segments"))),
                              private_msg()
                            )
                          ),
                          tags$hr(),
                          # cards in a fluid row
                          fluidRow(
                            cardLayout(
                              private(actionLink(inputId = ns("mol_feat_click"), geneMolecularFeaturesTableDash(ns("mol_feat_table_tab")))),
                              private(actionLink(inputId = ns("mol_feat_pathways_click"), geneMolecularFeaturesPathwayTableTab(ns("mol_feat_pathways_table_tab"))))
                            )
                          ),
                          tags$br(),
                          #conditional for features
                          fluidRow(
                            shinyjs::hidden(
                              div(
                                id = ns("molecular_features_tabcard"),
                                style = "padding-left:1%",
                                MolecularFeaturesTable(ns("molecular_features_table_plot")),
                                tags$br()
                              )
                            )
                          ), 
                          #conditional for pathways
                          fluidRow(
                            shinyjs::hidden(
                              div(
                                id = ns("molecular_features_pathways_tabcard"),
                                style = "padding-left:1%",
                                MolecularFeaturesPathwaysTable(ns("molecular_features_pathways_table_plot")),
                                tags$br()
                              )
                            )
                          )
                          )
      ),
      ## DOWNLOADS-----
      downloadTab(ns("download")), #pull download tab from module
      
      ## SEARCH-----
      formContent=querySearchInput(ns("search"))
    )
  )
}

genePageServer <- function(id, subtype) {
  moduleServer(
    id,
    function(input, output, session) {
      type <- "gene"
      #this block is the logic to define the summary_var variable, to display the proper summary module
      if(subtype == "gene"){
        data <- reactive({
          gene_symbols <- getQueryString()$query
          validation_datasets <- make_validate(gene_symbols)
          list(
            type=type,
            subtype=subtype,
            query=gene_symbols,
            content=gene_symbols,
            validate=validation_datasets
          )
        })
        title_var <- geneTitleServer("title_var", data)
        gene_var <- customListTextServer("gene_var", data)
        protein_summary <- proteinTextServer("protein_summary", data)
        summary_table <- pathwayListServer("gene_pathways", data)
      } else if (subtype == "pathway"){
        data <- reactive({
          pathway_id <- getQueryString()$query
          pathway_genes <- ddh::get_gene_symbols_for_pathway(pathway_id)
          validation_datasets <- make_validate(pathway_genes)
          list(
            type=type,
            subtype=subtype,
            subtype_id=pathway_id,
            query=pathway_id,
            content=pathway_genes,
            validate=validation_datasets
            )
        })
        title_var <- pathwayTitleServer("title_var", data)
        gene_var <- pathwayTextServer("gene_var", data)
        protein_summary <- pathwayTextServer("protein_summary", data)
        summary_table <- pathwayListServer("gene_pathways", data)
      } else if (subtype == "gene_list") {
        data <- reactive({
          custom_gene_list <- getQueryString()$query
          gene_symbols <- c(str_split(custom_gene_list, "\\s*,\\s*", simplify = TRUE))
          validation_datasets <- make_validate(gene_symbols)
          list(
            type=type,
            subtype=subtype,
            query=custom_gene_list,
            content=gene_symbols,
            validate=validation_datasets
          )
        })
        title_var <- customListTitleServer("title_var", data)
        gene_var <- customListTextServer("gene_var", data)
        protein_summary <- proteinTextServer("protein_summary", data)
        summary_table <- pathwayListServer("gene_pathways", data)
      } else {
        stop("fix your summary argument")
      }
      
      ## SEARCH SERVER-----
      querySearchServer("search")
      
      ## DASHBOARD SERVER-----
      
      #ideogram plot
      observeEvent(input$link_to_ideogramPlotDash, {
        updateNavbarPage(session, inputId = "geneNavBar", selected = "about_gene")
      })
      ideogramPlotDashServer("ideogramdash", data)
      
      #sequence plot
      observeEvent(input$link_to_sequencePlotDash, {
        updateNavbarPage(session, inputId = "geneNavBar", selected = "about_protein")
      })
      sequenceDashServer("sequencedash", data)
      
      #pubmed gene plot
      observeEvent(input$link_to_pubmedGenePlotDash, {
        updateNavbarPage(session, inputId = "geneNavBar", selected = "about_literature")
      })
      pubmedPlotDashServer("pubmedgenedash", data)
      
      #cell anatogram plot
      observeEvent(input$link_to_cellAnatogramPlotDash, {
        updateNavbarPage(session, inputId = "geneNavBar", selected = "expression_sub")
      })
      cellAnatogramPlotDashServer("cellanatogramdash", data)
      
      #cell expression plot
      observeEvent(input$link_to_cellExpressionPlotDash, {
        updateNavbarPage(session, inputId = "geneNavBar", selected = "expression_cell")
      })
      cellExpressionPlotDashServer("cellexpressiondash", data)
      
      #tissue anatogram plot
      observeEvent(input$link_to_tissueAnatogramPlotDash, {
        updateNavbarPage(session, inputId = "geneNavBar", selected = "expression_tissue")
      })
      tissueAnatogramPlotDashServer("tissueanatogramdash", data)

      #dep plot
      observeEvent(input$link_to_cellDependenciesPlotDash, {
        updateNavbarPage(session, inputId = "geneNavBar", selected = "dependencies_plots")
      })
      cellDependenciesPlotDashServer("depdash", data)
      
      #dep table
      observeEvent(input$link_to_cellDependenciesTableDash, {
        updateNavbarPage(session, inputId = "geneNavBar", selected = "dependencies_co-essentiality")
      })
      cellDependenciesTableDashServer("deptabledash", data)
      
      #dep drugs
      observeEvent(input$link_to_geneDrugsCorTableDash, {
        updateNavbarPage(session, inputId = "geneNavBar", selected = "dependencies_drugs")
      })
      private(geneDrugsCorTableDashServer("genedrugscortabledash", data))
      
      #molecular features table
      observeEvent(input$link_to_geneMolecularFeaturesTableDash, {
        updateNavbarPage(session, inputId = "geneNavBar", selected = "molecular_features")
      })
      geneMolecularFeaturesTableDashServer("molfeattabledash", data)
      
      ## INFO SERVER-----
      #name pulls from if/else variables above
      
      # GENE
      gene_var
      ideogramPlotServer("chromo", data)
      
      #barcode plot
      #no observe event, because it's just a linkout
      barcodeTabServer("barcode_tabcard", data)
      
      # CONDITIONAL GO PATHWAY
      observeEvent(input$go_click, { #store click
        shinyjs::show("go_tabcard")
      })
      #serves the card for the image
      pathwaysTableTabServer("gotab", data)
      
      # PROTEIN
      protein_summary
      proteinStructurePlotServer("structure", data)
      
      # CONDITIONAL SIZE
      observeEvent(input$size_click, { #store click
        shinyjs::show("size_tabcard")
        shinyjs::hide("sequence_tabcard")
        shinyjs::hide("signature_tabcard")
        shinyjs::hide("structure_tabcard")
      })
      #serves the card for the image
      sizePlotTabServer("sizetab", data)
      #serves the data plots
      proteinSizePlotServer("protein_size", data)
      
      # CONDITIONAL SEQUENCE
      observeEvent(input$sequence_click, { #store click
        shinyjs::hide("size_tabcard")
        shinyjs::show("sequence_tabcard")
        shinyjs::hide("signature_tabcard")
        shinyjs::hide("structure_tabcard")
      })
      #serves the card for the image
      sequencePlotTabServer("sequencetab", data)
      #serves the data plots
      proteinSeqServer("protein_seq", data)
      proteinDomainPlotServer("protein_domain_plot", data)
      
      # CONDITIONAL SIGNATURE
      observeEvent(input$signature_click, { #store click
        shinyjs::hide("size_tabcard")
        shinyjs::hide("sequence_tabcard")
        shinyjs::show("signature_tabcard")
        shinyjs::hide("structure_tabcard")
      })
      #serves the card for the image
      signaturePlotTabServer("signaturetab", data)
      #serves the data plots
      private({radialPlotServer("radial_plot", data)})
      private({AABarPlotServer("aa_bar_plot", data)})
      private({UMAPPlotServer("umap_plot", data)})
      private({clusterRadialPlotServer("cluster_radial_plot", data)})
      private({proteinClusterTableServer("prot_clust_table", data)})
      private({clusterEnrichmentPlotServer("cluster_enrichment_plot", data)})
      
      # CONDITIONAL STRUCTURE
      observeEvent(input$structure_click, { #store click
        shinyjs::hide("size_tabcard")
        shinyjs::hide("sequence_tabcard")
        shinyjs::hide("signature_tabcard")
        shinyjs::show("structure_tabcard")
      })
      #serves the card for the image
      structurePlotTabServer("structuretab", data)
      #serves the data plots
      private({proteinStructurePlot3dServer("protein_structure_plot3d", data)})
      
      # Literature
      pubmedPlotServer("pubmed", data)
      pubmedTableServer("pubmed", data)
      
      ## EXPRESSION SERVER-----
      #Subcell
      cellAnatogramPlotServer("exp", data)
      observeEvent(input$anato_facet_click, { #event to store the anatogram facet 'click'
      })
      cellAnatogramFacetPlotServer("exp", data)
      cellAnatogramTableServer("exp", data)
      
      # CELL LINE
      cellGeneExpressionPlotServer("cell_gene", data)
      
      # CONDITIONAL cellGeneExpressionTable
      observeEvent(input$gene_exp_table_click, { #store click
        shinyjs::show("gene_exp_table_tabcard")
        shinyjs::hide("protein_exp_plot_tabcard")
        shinyjs::hide("protein_exp_table_tabcard")
        shinyjs::hide("gene_protein_plot_tabcard")
      })
      #serves the card for the image
      cellGeneExpressionTableTabServer("gene_exp_table_tab", data)
      #serves the data plots
      cellGeneExpressionTableServer("cell_gene_table", data)
      
      # CONDITIONAL cellProteinExpressionPlot
      observeEvent(input$protein_exp_plot_click, { #store click
        shinyjs::hide("gene_exp_table_tabcard")
        shinyjs::show("protein_exp_plot_tabcard")
        shinyjs::hide("protein_exp_table_tabcard")
        shinyjs::hide("gene_protein_plot_tabcard")
      })
      #serves the card for the image
      cellProteinExpressionPlotTabServer("protein_exp_plot_tab", data)
      #serves the data plots
      cellProteinExpressionPlotServer("cell_protein", data)
      
      # CONDITIONAL cellProteinExpressionTable
      observeEvent(input$protein_exp_table_click, { #store click
        shinyjs::hide("gene_exp_table_tabcard")
        shinyjs::hide("protein_exp_plot_tabcard")
        shinyjs::show("protein_exp_table_tabcard")
        shinyjs::hide("gene_protein_plot_tabcard")
      })
      #serves the card for the image
      cellProteinExpressionTableTabServer("protein_exp_table_tab", data)
      #serves the data plots
      cellProteinExpressionTableServer("cell_protein_table", data)
      
      # CONDITIONAL cellGeneProteinPlotTab
      observeEvent(input$gene_protein_plot_click, { #store click
        shinyjs::hide("gene_exp_table_tabcard")
        shinyjs::hide("protein_exp_plot_tabcard")
        shinyjs::hide("protein_exp_table_tabcard")
        shinyjs::show("gene_protein_plot_tabcard")
      })
      #serves the card for the image
      cellGeneProteinPlotTabServer("gene_protein_plot_tab", data)
      #serves the data plots
      cellGeneProteinPlotServer("cell_geneprotein", data)
      
      # HUMAN TISSUE
      tissuePlotTextServer("tissue_text", data)
      maleAnatogramPlotServer("male_anatogram", data)
      femaleAnatogramPlotServer("female_anatogram", data)
      
      # CONDITIONAL tissuePlot
      observeEvent(input$tissue_plot_click, { #store click
        shinyjs::show("tissue_plot_tabcard")
        shinyjs::hide("tissue_table_tabcard")
      })
      #serves the card for the image
      tissuePlotTabServer("tissue_plot_tab", data)
      #serves the data plots
      tissuePlotServer("tissue_plot", data)
      
      # CONDITIONAL tissueTable
      observeEvent(input$tissue_table_click, { #store click
        shinyjs::hide("tissue_plot_tabcard")
        shinyjs::show("tissue_table_tabcard")
      })
      #serves the card for the image
      tissueTableTabServer("tissue_table_tab", data)
      #serves the data plots
      tissueTableServer("tissue_table", data)
      
      # COMPOUNDS SERVER-----
      private({geneDrugsTableServer("gene_drugs", data)})
      private({metabolitesTableServer("gene_metabolites", data)})
      private({geneBipartiteGraphServer("gene_bipartite_graph", data)})
      
      # DEPENDENCIES ----
      ### Data ----
      cellDependenciesPlotServer("dep", data)
      
      # CONDITIONAL Barplot
      observeEvent(input$dep_bar_click, { #store click
        shinyjs::show("barplot_tabcard")
        shinyjs::hide("density_tabcard")
        shinyjs::hide("lineage_tabcard")
        shinyjs::hide("sublineage_tabcard")
        shinyjs::hide("dep_table_tabcard")
        shinyjs::hide("expdep_plot_tabcard")
      })
      #serves the card for the image
      cellDependenciesBarPlotTabServer("bar_dep", data)
      #serves the data plots
      cellDependenciesBarPlotServer("dep_barplot", data)
      
      # CONDITIONAL density
      observeEvent(input$dep_density_click, { #store click
        shinyjs::hide("barplot_tabcard")
        shinyjs::show("density_tabcard")
        shinyjs::hide("lineage_tabcard")
        shinyjs::hide("sublineage_tabcard")
        shinyjs::hide("dep_table_tabcard")
        shinyjs::hide("expdep_plot_tabcard")
      })
      #serves the card for the image
      cellDependenciesDensityPlotTabServer("dep_density_tab", data)
      #serves the data plots
      cellDependenciesDensityPlotServer("dep_density", data)
      
      # CONDITIONAL lineage
      observeEvent(input$dep_lineage_click, { #store click
        shinyjs::hide("barplot_tabcard")
        shinyjs::hide("density_tabcard")
        shinyjs::show("lineage_tabcard")
        shinyjs::hide("sublineage_tabcard")
        shinyjs::hide("dep_table_tabcard")
        shinyjs::hide("expdep_plot_tabcard")
      })
      #serves the card for the image
      cellDepsLinPlotTabServer("dep_lineage_tab", data)
      #serves the data plots
      cellDepsLinPlotServer("dep_lineage", data)
      
      # CONDITIONAL sublineage
      observeEvent(input$dep_sublineage_click, { #store click
        shinyjs::hide("barplot_tabcard")
        shinyjs::hide("density_tabcard")
        shinyjs::hide("lineage_tabcard")
        shinyjs::show("sublineage_tabcard")
        shinyjs::hide("dep_table_tabcard")
        shinyjs::hide("expdep_plot_tabcard")
      })
      #serves the card for the image
      cellDepsSubLinPlotTabServer("dep_sublineage_tab", data)
      #serves the data plots
      cellDepsSubLinPlotServer("dep_sublineage", data)
      
      # CONDITIONAL table
      observeEvent(input$dep_table_click, { #store click
        shinyjs::hide("barplot_tabcard")
        shinyjs::hide("density_tabcard")
        shinyjs::hide("lineage_tabcard")
        shinyjs::hide("sublineage_tabcard")
        shinyjs::show("dep_table_tabcard")
        shinyjs::hide("expdep_plot_tabcard")
      })
      #serves the card for the image
      cellDependenciesTableTabServer("dep_table_tab", data)
      #serves the data table
      cellDependenciesTableServer("dep_table", data)    
      
      # CONDITIONAL expdep
      observeEvent(input$expdep_click, { #store click
        shinyjs::hide("barplot_tabcard")
        shinyjs::hide("density_tabcard")
        shinyjs::hide("lineage_tabcard")
        shinyjs::hide("sublineage_tabcard")
        shinyjs::hide("dep_table_tabcard")
        shinyjs::show("expdep_plot_tabcard")
      })
      #serves the card for the image
      expdepPlotTabServer("expdep_plot_tab", data)
      #serves the data plots
      expdepPlotServer("expdep_plot", data)
      
      ### Co-essentiality ----
      cellDependenciesCorrPlotServer("corrplot", data)
      
      # CONDITIONAL pos table
      observeEvent(input$dep_pos_table_click, { #store click
        shinyjs::show("dep_pos_table_tabcard")
        shinyjs::hide("dep_neg_table_tabcard")
        shinyjs::hide("dep_gene_pathways_tabcard")
        shinyjs::hide("dep_graph_tabcard")
        shinyjs::hide("dep_cca_tabcard")
      })
      #serves the card
      cellDependenciesPosTableTabServer("dep_pos_table_tab", data)
      #serves the table
      similarGenesTableServer("sim", data)
      
      # CONDITIONAL neg table
      observeEvent(input$dep_neg_table_click, { #store click
        shinyjs::hide("dep_pos_table_tabcard")
        shinyjs::show("dep_neg_table_tabcard")
        shinyjs::hide("dep_gene_pathways_tabcard")
        shinyjs::hide("dep_graph_tabcard")
        shinyjs::hide("dep_cca_tabcard")
      })
      #serves the card
      cellDependenciesNegTableTabServer("dep_neg_table_tab", data)
      #serves the table
      dissimilarGenesTableServer("dsim", data)

      # CONDITIONAL gene pathways
      observeEvent(input$dep_gene_pathways_click, { #store click
        shinyjs::hide("dep_pos_table_tabcard")
        shinyjs::hide("dep_neg_table_tabcard")
        shinyjs::show("dep_gene_pathways_tabcard")
        shinyjs::hide("dep_graph_tabcard")
        shinyjs::hide("dep_cca_tabcard")
      })
      #serves the card
      genePathwayEnrichmentTableTabServer("dep_gene_pathways_tab", data)
      #serves the table
      GenePathwayEnrichmentTableServer("gene_paths_comps", data)
      
      # CONDITIONAL graph
      observeEvent(input$dep_graph_click, { #store click
        shinyjs::hide("dep_pos_table_tabcard")
        shinyjs::hide("dep_neg_table_tabcard")
        shinyjs::hide("dep_gene_pathways_tabcard")
        shinyjs::show("dep_graph_tabcard")
        shinyjs::hide("dep_cca_tabcard")
      })
      #serves the card
      cellDependenciesGraphTabServer("depgraphtab", data)
      #serves the graph
      geneNetworkGraphServer("graph", data)
      
      # CONDITIONAL CCA
      observeEvent(input$dep_cca_click, { #store click
        shinyjs::hide("dep_pos_table_tabcard")
        shinyjs::hide("dep_neg_table_tabcard")
        shinyjs::hide("dep_gene_pathways_tabcard")
        shinyjs::hide("dep_graph_tabcard")
        shinyjs::show("dep_cca_tabcard")
      })
      #serves the card
      geneCCATableTabServer("depccatab", data)
      #serves the table
      geneCCATableServer("dep_cca", data)

      ### Drugs ----
      private({geneDrugsCorTableServer("gene_drugs_cor", data)})
      
      ### Molecular Features ----
      #serves the main plot
      private({MolecularFeaturesSegmentPlotServer("mol_feat_segments", data)})
      
      #serves the cards
      private({geneMolecularFeaturesTableDashServer("mol_feat_table_tab", data)})
      private({geneMolecularFeaturesPathwayTableTabServer("mol_feat_pathways_table_tab", data)})
      
      # CONDITIONAL features
      observeEvent(input$mol_feat_click, { #store click
        shinyjs::show("molecular_features_tabcard")
        shinyjs::hide("molecular_features_pathways_tabcard")
      })
      
      MolecularFeaturesTableServer("molecular_features_table_plot", data)
      
      # CONDITIONAL pathways
      observeEvent(input$mol_feat_pathways_click, { #store click
        shinyjs::hide("molecular_features_tabcard")
        shinyjs::show("molecular_features_pathways_tabcard")
      })
      
      MolecularFeaturesPathwaysTableServer("molecular_features_pathways_table_plot", data)
      
      # DOWNLOAD SERVER-----
      downloadTabServer("download", data, privateMode) #pull download tab from module
    }
  )
}

