# SUMMARY CARDS -----
#shiny code to generate card modules to place on pages

card_contents_width = "200px"
card_contents_height = "300px"
card_title_height = "30px"
#card_text_height = "30px"
card_content_and_title_height = "330px"

card_cell_style <- paste0(
  "box-shadow: rgba(100, 100, 111, 0.2) 0px 7px 19px 0px; ", # card effect
  "text-align: center;", # horizontally center contents
  "padding: 4px;", # add spacing around content inside the card
  "margin-bottom: 4px;", # add spacing below the card
  "height: ", card_content_and_title_height
)

# Creates a flow layout where the children will have card styling
cardLayout <- function(...) {
  flowLayout(
    ...,
    cellArgs = list(style = card_cell_style)
  )
}

cardTitle <- function(...) {
  div(
    ...,
    style=paste0("height:", card_title_height, ";")
  )
}

divFlexAlignCenter <- function(title, ...) {
  div(
    cardTitle(title),
    ...,
    style="display: flex; align-items: center; flex-direction: column; height:100%;"
  )
}

# GENE -----
##ideogram plot-----
ideogramPlotDash <- function(id) {
  ns <- NS(id)
  divFlexAlignCenter(
    "Gene",
    # not centering vertically this because image already has a top left margin
    uiOutput(outputId = ns("ideogram_card"))
  )
}

ideogramPlotDashServer <- function (id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$ideogram_card <- renderUI({
        #check to see if data are there
        shiny::validate(
          shiny::need(c("gene_location") %in% data()$validate, "No location data for this gene"))
        #check to see if image exists
        img_path <- ddh::load_image(input = data(), fun_name = "make_ideogram", card = TRUE)
        if(!is.null(img_path)) {
          uiOutput(outputId = session$ns("ideogram_card_image"))
        } else {
          plotOutput(outputId = session$ns("ideogram_card_render"), 
                     height = card_contents_height,
                     width = card_contents_width) %>%
            withSpinnerColor(plot_type = data()$type)
        }
      })
      output$ideogram_card_image <- renderUI({
        tags$img(src = ddh::load_image(input = data(), fun_name = "make_ideogram", card = TRUE),
                 width = card_contents_width,
                 height = card_contents_height)
      })
      output$ideogram_card_render <- renderPlot({
        make_ideogram(input = data(), card = TRUE)
      })
    })
}

##Structure plots-----
structureDash <- function(id) {
  ns <- NS(id)
  divFlexAlignCenter(
    "Protein",
    uiOutput(outputId = ns("structure_card"))
  )
}

structureDashServer <- function (id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$structure_card <- renderUI({
        div(
          tags$img(src = make_structure(input = data(), card = TRUE), 
                   width = card_contents_width,
                   height = card_contents_width, #force to square
                   alt = glue::glue('Protein structure rendering in the style of a black and white line drawing'))
        )
      })
    })
}

## PUBMED plot-----
pubmedPlotDash <- function(id) {
  ns <- NS(id)
  divFlexAlignCenter(
    "Literature",
    uiOutput(outputId = ns("pubmed_gene_card"))
  )
}

pubmedPlotDashServer <- function (id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$pubmed_gene_card <- renderUI({
        #check to see if data are there
        shiny::validate(
          shiny::need(c("universal_pubmed") %in% data()$validate, "No literature data for this gene"))
        #check to see if image exists
        img_path <- ddh::load_image(input = data(), fun_name = "make_pubmed", card = TRUE)
        if(!is.null(img_path)) {
          uiOutput(outputId = session$ns("pubmed_gene_card_image"))
        } else {
          plotOutput(outputId = session$ns("pubmed_gene_card_render"), 
                     height = card_contents_height,
                     width = card_contents_width) %>%
            withSpinnerColor(plot_type = data()$type)
        }
      })
      output$pubmed_gene_card_image <- renderUI({
        tags$img(src = ddh::load_image(input = data(), fun_name = "make_pubmed", card = TRUE),
                 width = card_contents_width,
                 height = card_contents_height)
      })
      output$pubmed_gene_card_render <- renderPlot({
        make_pubmed(input = data(), card = TRUE)
      })
    })
}

#EXPRESSION plots-----
##expression plot-----
cellExpressionPlotDash <- function(id) {
  ns <- NS(id)
  divFlexAlignCenter(
    "Expression",
    uiOutput(outputId = ns("expression_gene_card"))
  )
}

cellExpressionPlotDashServer <- function (id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$expression_gene_card <- renderUI({
        #check to see if data are there
        shiny::validate(
          shiny::need(c("universal_expression_long") %in% data()$validate, "No expression data available"))
        #check to see if image exists
        img_path <- ddh::load_image(input = data(), fun_name = "make_cellexpression", card = TRUE)
        if(!is.null(img_path)) {
          uiOutput(outputId = session$ns("expression_gene_card_image"))
        } else {
          plotOutput(outputId = session$ns("expression_gene_card_render"), 
                     height = card_contents_height,
                     width = card_contents_width) %>%
            withSpinnerColor(plot_type = data()$type)
        }
      })
      output$expression_gene_card_image <- renderUI({
        tags$img(src = ddh::load_image(input = data(), fun_name = "make_cellexpression", card = TRUE),
                 width = card_contents_width,
                 height = card_contents_height)
      })
      output$expression_gene_card_render <- renderPlot({
        make_cellexpression(input = data(), 
                            card = TRUE)
      })
    }
  )
}

##cell anatogram plot-----
cellAnatogramPlotDash <- function(id) {
  ns <- NS(id)
  divFlexAlignCenter(
    "Subcellular Distribution",
    uiOutput(outputId = ns("cell_anatogram_gene_card"))
  )
}

cellAnatogramPlotDashServer <- function (id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$cell_anatogram_gene_card <- renderUI({
        #check to see if data are there
        shiny::validate(
          shiny::need(c("gene_subcell") %in% data()$validate, "No subcellular location data for this gene"))
        #check to see if image exists
        img_path <- ddh::load_image(input = data(), fun_name = "make_cellanatogram", card = TRUE)
        if(!is.null(img_path)) {
          uiOutput(outputId = session$ns("cell_anatogram_gene_card_image"))
        } else {
          plotOutput(outputId = session$ns("cell_anatogram_gene_card_render"), 
                     height = card_contents_height,
                     width = card_contents_width) %>%
            withSpinnerColor(plot_type = data()$type)
        }
      })
      output$cell_anatogram_gene_card_image <- renderUI({
        tags$img(src = ddh::load_image(input = data(), fun_name = "make_cellanatogram", card = TRUE),
                 width = card_contents_width,
                 height = card_contents_height)
      })
      output$cell_anatogram_gene_card_render <- renderPlot({
        make_cellanatogram(input = data(), 
                           card = TRUE)
      })
    }
  )
}

##tissue anatogram plot-----
tissueAnatogramPlotDash <- function(id) {
  ns <- NS(id)
  divFlexAlignCenter(
    "Tissue Distribution",
    uiOutput(outputId = ns("tissue_anatogram_gene_card"))
  )
}

tissueAnatogramPlotDashServer <- function (id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$tissue_anatogram_gene_card <- renderUI({
        #check to see if data are there
        shiny::validate(
          shiny::need(c("gene_tissue") %in% data()$validate, "No tissue data for this gene"))
        #check to see if image exists
        img_path <- ddh::load_image(input = data(), fun_name = "make_female_anatogram", card = TRUE)
        if(!is.null(img_path)) {
          uiOutput(outputId = session$ns("tissue_anatogram_gene_card_image"))
        } else {
          plotOutput(outputId = session$ns("tissue_anatogram_gene_card_render"), 
                     height = card_contents_height,
                     width = card_contents_width) %>%
            withSpinnerColor(plot_type = data()$type)
        }
      })
      output$tissue_anatogram_gene_card_image <- renderUI({
        tags$img(src = ddh::load_image(input = data(), fun_name = "make_female_anatogram", card = TRUE),
                 width = card_contents_width,
                 height = card_contents_height)
      })
      output$tissue_anatogram_gene_card_render <- renderPlot({
        make_female_anatogram(input = data(), 
                              card = TRUE)
      })
    }
  )
}

#DEPENDENCY plots-----
##cell dependencies plot-----
cellDependenciesPlotDash <- function(id) {
  ns <- NS(id)
  divFlexAlignCenter(
    "Cell Dependencies",
    uiOutput(outputId = ns("gene_dependencies_card"))
  )
}

cellDependenciesPlotDashServer <- function (id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$gene_dependencies_card <- renderUI({
        #check to see if data are there
        shiny::validate(
          shiny::need(c("universal_achilles_long", "cell_expression_meta") %in% data()$validate, "No dependency data for this gene"))
        #check to see if image exists
        img_path <- ddh::load_image(input = data(), fun_name = "make_celldeps", card = TRUE)
        if(!is.null(img_path)) {
          uiOutput(outputId = session$ns("gene_dependencies_card_image"))
        } else {
          plotOutput(outputId = session$ns("gene_dependencies_card_render"), 
                     height = card_contents_height,
                     width = card_contents_width) %>%
            withSpinnerColor(plot_type = data()$type)
        }
      })
      output$gene_dependencies_card_image <- renderUI({
        tags$img(src = ddh::load_image(input = data(), fun_name = "make_celldeps", card = TRUE),
                 width = card_contents_width,
                 height = card_contents_height)
      })
      output$gene_dependencies_card_render <- renderPlot({
        make_celldeps(input = data(), 
                      card = TRUE, 
                      scale = 0.3)
      })
    })
}

##cell dependencies table-----
cellDependenciesTableDash <- function(id) {
  ns <- NS(id)
  divFlexAlignCenter(
    "Co-essential genes",
    gt_output(outputId = ns("deptabledash"))
  )
}

cellDependenciesTableDashServer <- function (id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$deptabledash <- render_gt({
        shiny::validate(
          shiny::need(c("universal_achilles_long") %in% data()$validate, "No dependency data for this gene"))
        gt::gt(make_top_table(input = data()) %>% 
                 dplyr::mutate("Rank" = row_number()) %>% 
                 dplyr::select("Rank", "Gene" = "gene", "R^2" = "r2") %>% 
                 dplyr::slice(1:5))
      },
      height = card_contents_height,
      width = card_contents_width
      )
    }
  )
}

##cell dependencies table-----
geneMolecularFeaturesTableDash <- function(id) {
  ns <- NS(id)
  divFlexAlignCenter(
    "Molecular features",
    gt_output(outputId = ns("molfeattabledash"))
  )
}

geneMolecularFeaturesTableDashServer <- function (id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$molfeattabledash <- render_gt({
        shiny::validate(
          shiny::need(c("universal_achilles_long") %in% data()$validate, # Check this
                      "No molecular features for this gene"))
        gt::gt(make_gene_molecular_features(input = data()) %>% 
                 dplyr::mutate("Rank" = row_number()) %>% 
                 dplyr::select(Rank, Feature, logFC) %>% 
                 dplyr::slice(1:5))
      },
      height = card_contents_height,
      width = card_contents_width
      )
    }
  )
}

##cell dependencies graph-----
cellDependenciesGraphTab <- function(id) {
  ns <- NS(id)
  divFlexAlignCenter(
    "Dependencies Graph",
    visNetworkOutput(outputId = ns("depgraphtab"))
  )
}

cellDependenciesGraphTabServer <- function (id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$depgraphtab <- renderVisNetwork({
        #check to see if data are there
        shiny::validate(
          shiny::need(c("universal_achilles_long") %in% data()$validate, "No dependency data for this gene"))
        make_graph(input = data(), 
                   threshold = 10, 
                   deg = 2, 
                   corr_type = "positive", 
                   card = TRUE)
      })
    })
}

##cell functional plot-----
cellFunctionalPlotDash <- function(id) {
  ns <- NS(id)
  divFlexAlignCenter(
    "Pathway Expression",
    plotOutput(outputId = ns("functional_plot"))
  )
}

cellFunctionalPlotDashServer <- function (id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$functional_plot <- renderPlot({
        make_functional_cell(input = data(),
                             card = TRUE,
                             num_pathways = 5,
                             remove_equivalent_pathways = TRUE)
      })
    })
}

#DRUG table-----
geneDrugsCorTableDash <- function(id) {
  ns <- NS(id)
  divFlexAlignCenter(
    "Correlated Drugs",
    gt_output(outputId = ns("genedrugscortabledash"))
  )
}

geneDrugsCorTableDashServer <- function (id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$genedrugscortabledash <- render_gt({
        shiny::validate(
          shiny::need(c("gene_drugs_cor_table") %in% data()$validate, "No dependency data for this drug"))
        gt::gt(make_gene_drugs_cor_table(input = data()) %>%
                 dplyr::mutate("Rank" = row_number()) %>%
                 dplyr::select(Rank, Drug = drug) %>%
                 dplyr::slice(1:5))
      },
      height = card_contents_height,
      width = card_contents_width)
    }
  )
}

cellDrugsTableDash <- function(id) {
  ns <- NS(id)
  divFlexAlignCenter(
    "Associated Drugs",
    gt_output(outputId = ns("celldrugstabledash"))
  )
}

cellDrugsTableDashServer <- function (id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$celldrugstabledash <- render_gt({
        shiny::validate(
          shiny::need(c("universal_prism_long") %in% data()$validate, "No drug data for this cell line"))
        gt::gt(make_cell_drugs_table(input = data()) %>% 
                 dplyr::mutate("Rank" = row_number()) %>%
                 dplyr::select(Rank, Drug = name) %>%
                 dplyr::slice(1:5))
      },
      height = card_contents_height,
      width = card_contents_width)
    }
  )
}

cellMetabolitesTableDash <- function(id) {
  ns <- NS(id)
  divFlexAlignCenter(
    "Metabolite Levels",
    gt_output(outputId = ns("cellmetabolitestabledash"))
  )
}

cellMetabolitesTableDashServer <- function (id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$cellmetabolitestabledash <- render_gt({
        shiny::validate(
          shiny::need(c("cell_metabolites") %in% data()$validate, "No metabolite data for this cell line"))
        gt::gt(make_metabolite_table(input = data()) %>% 
                 dplyr::mutate("Rank" = row_number()) %>%
                 dplyr::select(Rank, Metabolite = metabolite) %>%
                 dplyr::slice(1:5))
      },
      height = card_contents_height,
      width = card_contents_width)
    }
  )
}

# CELL -----
##cell image card-----
cellImageDash <- function(id) {
  ns <- NS(id)
  divFlexAlignCenter(
    "Cell",
    imageOutput(outputId = ns("cellimage_card"),
                height = card_contents_height,
                width = card_contents_width, inline = TRUE) %>%
      withSpinnerColor(plot_type = "cell") 
  )
}

cellImageDashServer <- function (id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$structure_card <- renderUI({
        div(
          tags$img(src = make_cell_image(input = data(), card = TRUE), 
                   width = card_contents_width,
                   height = card_contents_width #force to square
          )
        )
      })
    })
}

##dependencies----
#dependencies are in gene dependency plots
##cell coexpression graph-----
cellExpressionGraphDash <- function(id) {
  ns <- NS(id)
  divFlexAlignCenter(
    "Co-Expression Graph",
    visNetworkOutput(outputId = ns("expgraphdash"))
  )
}

cellExpressionGraphDashServer <- function (id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$expgraphdash <- renderVisNetwork({
        #check to see if data are there
        shiny::validate(
          shiny::need(c("cell_expression_sim") %in% data()$validate, "No dependency data for this cell line"))
        make_graph(input = data(), 
                   threshold = 10, 
                   deg = 2, 
                   corr_type = "positive", 
                   cell_line_var = "expression",
                   card = TRUE)
      })
    })
}

# COMPOUND -----
#structure plot-----
compoundStructureDash <- function(id) {
  ns <- NS(id)
  divFlexAlignCenter(
    "Compound Structure",
    uiOutput(outputId = ns("conditional_compoundstructuredash"))
  )
}

compoundStructureDashServer <- function (id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$conditional_compoundstructuredash <- renderUI({
        #### CARD PARAMETERS
        fieldnameSingle <- "compounddash"
        fieldnameMultiple <- "compounddash_multiple"
        ImgFun <- "make_molecule_structure"
        query_type <- data()$type
        image_type <- "card"
        SpinnerColor <- query_type
        ImgWidth <- card_contents_width
        PlotWidth <- card_contents_width
        ImgHeight <- card_contents_height
        PlotHeight <- card_contents_height
        ####
        img_path <- load_image(input = list(type = query_type,
                                            content = data()$content),
                               fun_name = ImgFun,
                               image_type = image_type)
        
        if((length(data()$content) == 1) & (!is.null(img_path))) {
          imageOutput(outputId = session$ns(fieldnameSingle),
                      height = ImgHeight,
                      width = ImgWidth, inline = TRUE) %>%
            withSpinnerColor(plot_type = query_type)
        } else {
          plotOutput(outputId = session$ns(fieldnameMultiple),
                     height = PlotHeight,
                     width = PlotWidth) %>%
            withSpinnerColor(plot_type = query_type)
        }
      })
      output$compounddash <- renderImage({
        #shiny::validate(
        #  need(is.array(make_molecule_structure(compound = data()$content)), "No structure found for this compound."))
        # withProgress(message = 'Shiny molecule comin up...', value = 1, {
        # make_molecule_structure(compound = data()$content)
        list(src = load_image(input = data(), fun_name = "make_molecule_structure"), 
             width = card_contents_width,
             height = card_contents_height) #defined above
      }, deleteFile = FALSE)
      
      output$compounddash_multiple <- renderPlot({
        withProgress(message = 'Almost there...', value = 1, {
          make_molecule_structure(input = data(), 
                                  card = TRUE)
        })
      })
      
      # })
    })
}

#compound dependencies plot-----
compoundDependenciesPlotDash <- function(id) {
  ns <- NS(id)
  divFlexAlignCenter(
    "Compound Dependencies",
    uiOutput(outputId = ns("compound_dependencies_card"))
  )
}

compoundDependenciesPlotDashServer <- function (id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$compound_dependencies_card <- renderUI({
        #check to see if data are there
        shiny::validate(
          shiny::need(c("universal_prism_long") %in% data()$validate, "No viability data for this compound"))
        #check to see if image exists
        img_path <- ddh::load_image(input = data(), fun_name = "make_celldeps", card = TRUE)
        if(!is.null(img_path)) {
          uiOutput(outputId = session$ns("compound_dependencies_card_image"))
        } else {
          plotOutput(outputId = session$ns("compound_dependencies_card_render"), 
                     height = card_contents_height,
                     width = card_contents_width) %>%
            withSpinnerColor(plot_type = data()$type)
        }
      })
      output$compound_dependencies_card_image <- renderUI({
        tags$img(src = ddh::load_image(input = data(), fun_name = "make_celldeps", card = TRUE),
                 width = card_contents_width,
                 height = card_contents_height)
      })
      output$compound_dependencies_card_render <- renderPlot({
        make_celldeps(input = data(), 
                      card = TRUE)
      })
    })
}

#compound dependencies table-----
compoundDependenciesTableDash <- function(id) {
  ns <- NS(id)
  gt_output(outputId = ns("depcompoundtabledash"))
}

compoundDependenciesTableDashServer <- function (id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$depcompoundtabledash <- render_gt({
        shiny::validate(
          shiny::need(c("universal_prism_long") %in% data()$validate, "No viability data for this compound"))
        gt::gt(make_compound_table(input = data()) %>% 
                 dplyr::mutate("Rank" = row_number()) %>% 
                 dplyr::select("Rank", "Drug" = "name", "R^2" = "r2") %>% 
                 dplyr::slice(1:5))
      },
      height = card_contents_height,
      width = card_contents_width)
    }
  )
}

#compound dependencies graph-----
compoundDependenciesGraphDash <- function(id) {
  ns <- NS(id)
  divFlexAlignCenter(
    "Dependencies Graph",
    visNetworkOutput(outputId = ns("compoundgraphdash")) %>% 
      withSpinnerColor(plot_type = "compound") #see shiny_helper.R
  )
}

compoundDependenciesGraphDashServer <- function (id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$compoundgraphdash <- renderVisNetwork({
        shiny::validate(
          shiny::need(c("universal_prism_long") %in% data()$validate, "No viablity data for this compound"))
        make_graph(input = data(), 
                   threshold = 10, 
                   deg = 2, 
                   corr_type = "positive")
      })
    })
}

#pubmed compound plot-----
pubmedCompoundPlotDash <- function(id) {
  ns <- NS(id)
  divFlexAlignCenter(
    "Pubmed Compound",
    uiOutput(outputId = ns("pubmed_compound_card"))
  )
}

pubmedCompoundPlotDashServer <- function (id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$pubmed_compound_card <- renderUI({
        #check to see if data are there
        shiny::validate(
          shiny::need(c("universal_pubmed") %in% data()$validate, "No literature data for this compound"))
        #check to see if image exists
        img_path <- ddh::load_image(input = data(), fun_name = "make_pubmed", card = TRUE)
        if(!is.null(img_path)) {
          uiOutput(outputId = session$ns("pubmed_compound_card_image"))
        } else {
          plotOutput(outputId = session$ns("pubmed_compound_card_render"), 
                     height = card_contents_height,
                     width = card_contents_width) %>%
            withSpinnerColor(plot_type = data()$type)
        }
      })
      output$pubmed_compound_card_image <- renderUI({
        tags$img(src = ddh::load_image(input = data(), fun_name = "make_pubmed", card = TRUE),
                 width = card_contents_width,
                 height = card_contents_height)
      })
      output$pubmed_compound_card_render <- renderPlot({
        make_pubmed(input = data(), card = TRUE)
      })
    })
}

#DRUGS-----
drugGenesCorTableDash <- function(id) {
  ns <- NS(id)
  gt_output(outputId = ns("druggenescortabledash"))
}

drugGenesCorTableDashServer <- function (id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$druggenescortabledash <- render_gt({
        shiny::validate(
          shiny::need(c("compound_genes_cor_table") %in% data()$validate, "No gene correlates for this compound"))
        gt::gt(make_drug_genes_cor_table(data()$content) %>% 
                 dplyr::mutate("Rank" = row_number()) %>% 
                 dplyr::select(Rank, Gene = gene, 'R^2'=r2) %>% 
                 dplyr::slice(1:5))
      },
      height = card_contents_height,
      width = card_contents_width
      )
    }
  )
}

# TAB CARDS -----
## GENE -----
##gene barcode plot-----
barcodeTab <- function(id) {
  ns <- NS(id)
  divFlexAlignCenter(
    "Gene Barcode",
    uiOutput(outputId = ns("barcode_tab"))
  )
}

barcodeTabServer <- function (id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$barcode_tab <- renderUI({
        div(
          tags$a(
            tags$img(src = make_barcode(input = data(), card = TRUE), 
                     width = card_contents_width,
                     height = card_contents_width, #force to square
                     alt = glue::glue('Artistic image of a gene in the style of a barcode')), 
            href = make_barcode(input = data(), card = FALSE),
            target="_blank")
          #build a dynamic ahref here to point to???
        )
      })
    })
}

geneGoTableTab <- function(id) {
  ns <- NS(id)
  divFlexAlignCenter(
    "Pathway Table",
    gt_output(outputId = ns("genegotabletab"))
  )
}
geneGoTableTabServer <- function (id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$genegotabletab <- render_gt({
        shiny::validate(
          shiny::need(c("universal_gene_pathways") %in% data()$validate, "No pathway data for this gene"))
        gt::gt(make_pathway_list(input = data()) %>% 
                 dplyr::mutate(gs_name = purrr::map_chr(gs_name, clean_pathway_names)) %>% #from shiny_helper.R
                 dplyr::select(Pathway = gs_name, ID = gs_id) %>% 
                 dplyr::slice(1:3))
      },
      height = card_contents_height,
      width = card_contents_width
      )
    }
  )
}

###PROTEIN plots-----
sizePlotTab <- function(id) {
  ns <- NS(id)
  divFlexAlignCenter(
    "Size",
    uiOutput(outputId = ns("size_plot_tab"))
  )
}

sizePlotTabServer <- function (id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$size_plot_tab <- renderUI({
        #check to see if data are there
        shiny::validate(
          shiny::need(c("universal_proteins") %in% data()$validate, "No size data for this protein"))
        #check to see if image exists
        img_path <- ddh::load_image(input = data(), fun_name = "make_proteinsize", card = TRUE)
        if(!is.null(img_path)) {
          uiOutput(outputId = session$ns("size_plot_tab_image"))
        } else {
          plotOutput(outputId = session$ns("size_plot_tab_render"), 
                     height = card_contents_height,
                     width = card_contents_width) %>%
            withSpinnerColor(plot_type = "protein")
        }
      })
      output$size_plot_tab_image <- renderUI({
        tags$img(src = ddh::load_image(input = data(), fun_name = "make_proteinsize", card = TRUE),
                 width = card_contents_width,
                 height = card_contents_height)
      })
      output$size_plot_tab_render <- renderPlot({
        make_proteinsize(input = data(),
                         card = TRUE)
      })
    })
}

sequencePlotTab <- function(id) {
  ns <- NS(id)
  divFlexAlignCenter(
    "Sequence",
    uiOutput(outputId = ns("sequence_plot_tab"))
  )
}

sequencePlotTabServer <- function (id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$sequence_plot_tab <- renderUI({
        #check to see if data are there
        shiny::validate(
          shiny::need(c("universal_proteins") %in% data()$validate, "No sequence data for this protein"))
        #check to see if image exists
        img_path <- ddh::load_image(input = data(), fun_name = "make_sequence", card = TRUE)
        if(!is.null(img_path)) {
          uiOutput(outputId = session$ns("sequence_plot_tab_image"))
        } else {
          plotOutput(outputId = session$ns("sequence_plot_tab_render"), 
                     height = card_contents_height,
                     width = card_contents_width) %>%
            withSpinnerColor(plot_type = "protein")
        }
      })
      output$sequence_plot_tab_image <- renderUI({
        tags$img(src = ddh::load_image(input = data(), fun_name = "make_sequence", card = TRUE),
                 width = card_contents_width,
                 height = card_contents_height)
      })
      output$sequence_plot_tab_render <- renderPlot({
        make_sequence(input = data(),
                      card = TRUE)
      })
    })
}

signaturePlotTab <- function(id) {
  ns <- NS(id)
  divFlexAlignCenter(
    "Signature",
    uiOutput(outputId = ns("signature_plot_tab"))
  )
}

signaturePlotTabServer <- function (id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$signature_plot_tab <- renderUI({
        #check to see if data are there
        shiny::validate(
          shiny::need(c("gene_signatures") %in% data()$validate, "No signature data for this protein"))
        #check to see if image exists
        img_path <- ddh::load_image(input = data(), fun_name = "make_radial", card = TRUE)
        if(!is.null(img_path)) {
          uiOutput(outputId = session$ns("sequence_plot_tab_image"))
        } else {
          plotOutput(outputId = session$ns("sequence_plot_tab_render"), 
                     height = card_contents_height,
                     width = card_contents_width) %>%
            withSpinnerColor(plot_type = "protein")
        }
      })
      output$sequence_plot_tab_image <- renderUI({
        tags$img(src = ddh::load_image(input = data(), fun_name = "make_radial", card = TRUE),
                 width = card_contents_width,
                 height = card_contents_height)
      })
      output$sequence_plot_tab_render <- renderPlot({
        make_radial(input = data(),
                    card = TRUE)
      })
    })
}

structurePlotTab <- function(id) {
  ns <- NS(id)
  divFlexAlignCenter(
    "Structure",
    uiOutput(outputId = ns("structure_tab"))
  )
}

structurePlotTabServer <- function (id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$structure_tab <- renderUI({
        div(
          tags$img(src = make_structure(input = data(), card = TRUE), 
                   width = card_contents_width,
                   height = card_contents_width, #force to square
                   alt = glue::glue('Protein structure rendering in the style of a black and white line drawing'))
        )
      })
    })
}

###EXPRESSION tabs-----
cellGeneExpressionTableTab <- function(id) {
  ns <- NS(id)
  divFlexAlignCenter(
    "Gene Expression",
    gt_output(outputId = ns("geneexptabletab"))
  )
}

cellGeneExpressionTableTabServer <- function (id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$geneexptabletab <- render_gt({
        if(data()$type == "gene") {
          shiny::validate(
            shiny::need(c("universal_expression_long") %in% data()$validate, "No expression data available"))
          gt::gt(make_expression_table(input = data(), var = "gene") %>% 
                   dplyr::slice(1:5) %>% 
                   dplyr::select(-Lineage, -Subtype))
        } else if (data()$type == "cell") {
          shiny::validate(
            shiny::need(c("universal_expression_long") %in% data()$validate, "No expression data available"))
          gt::gt(make_expression_table(input = data(), var = "gene") %>% 
                   dplyr::slice(1:5) %>% 
                   dplyr::select(-`Gene Name`))
        }
      },
      height = card_contents_height,
      width = card_contents_width
      )
    }
  )
}

cellProteinExpressionPlotTab <- function(id) {
  ns <- NS(id)
  divFlexAlignCenter(
    "Protein Expression",
    uiOutput(outputId = ns("expression_protein_tab"))
  )
}

cellProteinExpressionPlotTabServer <- function (id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$expression_protein_tab <- renderUI({
        #check to see if data are there
        shiny::validate(
          shiny::need(c("universal_expression_long") %in% data()$validate, "No expression data available"))
        #check to see if image exists
        img_path <- NULL #override to NULL
        if(!is.null(img_path)) {
          uiOutput(outputId = session$ns("expression_protein_tab_image"))
        } else {
          plotOutput(outputId = session$ns("expression_protein_tab_render"), 
                     height = card_contents_height,
                     width = card_contents_width) %>%
            withSpinnerColor(plot_type = data()$type)
        }
      })
      output$expression_protein_tab_image <- renderUI({
        tags$img(src = ddh::load_image(input = data(), fun_name = "make_cellexpression", card = TRUE),
                 width = card_contents_width,
                 height = card_contents_height)
      })
      output$expression_protein_tab_render <- renderPlot({
        make_cellexpression(input = data(),
                            var = "protein",
                            card = TRUE)
      })
    }
  )
}

cellProteinExpressionTableTab <- function(id) {
  ns <- NS(id)
  divFlexAlignCenter(
    "Protein Expression",
    gt_output(outputId = ns("proteinexptabletab"))
  )
}

cellProteinExpressionTableTabServer <- function (id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$proteinexptabletab <- render_gt({
        if(data()$type == "gene") {
          shiny::validate(
            shiny::need(c("universal_expression_long") %in% data()$validate, "No expression data available"))
          gt::gt(make_expression_table(input = data(), var = "protein") %>% 
                   dplyr::slice(1:5) %>% 
                   dplyr::select(-Lineage, -Subtype))
          
        } else if(data()$type == "cell") {
          shiny::validate(
            shiny::need(c("universal_expression_long") %in% data()$validate, "No expression data available"))
          gt::gt(make_expression_table(input = data(), var = "protein") %>% 
                   dplyr::slice(1:5) %>% 
                   dplyr::select(-`Gene Name`))
        }
      },
      height = card_contents_height,
      width = card_contents_width
      )
    }
  )
}

cellGeneProteinPlotTab <- function(id) {
  ns <- NS(id)
  divFlexAlignCenter(
    "Gene v. Protein",
    uiOutput(outputId = ns("expression_genevprotein_tab"))
  )
}

cellGeneProteinPlotTabServer <- function (id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$expression_genevprotein_tab <- renderUI({
        #check to see if data are there
        shiny::validate(
          shiny::need(c("universal_expression_long") %in% data()$validate, "No expression data available"))
        #check to see if image exists
        img_path <- ddh::load_image(input = data(), fun_name = "make_cellgeneprotein", card = TRUE)
        if(!is.null(img_path)) {
          uiOutput(outputId = session$ns("expression_genevprotein_tab_image"))
        } else {
          plotOutput(outputId = session$ns("expression_genevprotein_tab_render"), 
                     height = card_contents_height,
                     width = card_contents_width) %>%
            withSpinnerColor(plot_type = data()$type)
        }
      })
      output$expression_genevprotein_tab_image <- renderUI({
        tags$img(src = ddh::load_image(input = data(), fun_name = "make_cellgeneprotein", card = TRUE),
                 width = card_contents_width,
                 height = card_contents_height)
      })
      output$expression_genevprotein_tab_render <- renderPlot({
        make_cellgeneprotein(input = data(),
                             card = TRUE)
      })
    }
  )
}

tissuePlotTab <- function(id) {
  ns <- NS(id)
  divFlexAlignCenter(
    "Tissue Plot",
    uiOutput(outputId = ns("expression_tissue_tab"))
  )
}

tissuePlotTabServer <- function (id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$expression_tissue_tab <- renderUI({
        #check to see if data are there
        shiny::validate(
          shiny::need(c("gene_tissue") %in% data()$validate, "No tissue expression data for this gene"))
        #check to see if image exists
        img_path <- ddh::load_image(input = data(), fun_name = "make_tissue", card = TRUE)
        if(!is.null(img_path)) {
          uiOutput(outputId = session$ns("expression_tissue_tab_image"))
        } else {
          plotOutput(outputId = session$ns("expression_tissue_tab_render"), 
                     height = card_contents_height,
                     width = card_contents_width) %>%
            withSpinnerColor(plot_type = data()$type)
        }
      })
      output$expression_tissue_tab_image <- renderUI({
        tags$img(src = ddh::load_image(input = data(), fun_name = "make_tissue", card = TRUE),
                 width = card_contents_width,
                 height = card_contents_height)
      })
      output$expression_tissue_tab_render <- renderPlot({
        make_tissue(input = data(),
                    card = TRUE)
      })
    }
  )
}

tissueTableTab <- function(id) {
  ns <- NS(id)
  divFlexAlignCenter(
    "Expression Values",
    gt_output(outputId = ns("tissuetabletab"))
  )
}
tissueTableTabServer <- function (id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$tissuetabletab <- render_gt({
        shiny::validate(
          shiny::need(c("gene_tissue") %in% data()$validate, "No tissue expression data for this gene"))
        gt::gt(make_humananatogram_table(input = data()) %>% 
                 dplyr::slice(1:5))
      },
      height = card_contents_height,
      width = card_contents_width
      )
    }
  )
}

cellLineExpressionPosTableTab <- function(id) {
  ns <- NS(id)
  divFlexAlignCenter(
    "Positive Associations",
    gt_output(outputId = ns("cell_exppostabletab"))
  )
}

cellLineExpressionPosTableTabServer <- function (id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$cell_exppostabletab <- render_gt({
        shiny::validate(
          shiny::need(c("cell_expression_sim") %in% data()$validate, "No expression data for this cell line"))
        gt::gt(
          make_cell_sim_table(input = data(),
                              similarity = "expression")$top_table %>%
            dplyr::rename("Cell" = "cell2_name", "Bonferroni" = "bonferroni") %>% 
            dplyr::select(Cell, Bonferroni) %>%
            dplyr::arrange(Bonferroni) %>% 
            dplyr::mutate_if(is.numeric, ~ signif(., digits = 3)) %>% 
            dplyr::slice(1:5))
      },
      height = card_contents_height,
      width = card_contents_width
      )
    }
  )
}

cellLineExpressionNegTableTab <- function(id) {
  ns <- NS(id)
  divFlexAlignCenter(
    "Negative Associations",
    gt_output(outputId = ns("cell_expnegtabletab"))
  )
}
cellLineExpressionNegTableTabServer <- function (id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$cell_expnegtabletab <- render_gt({
        shiny::validate(
          shiny::need(c("cell_expression_sim") %in% data()$validate, "No expression data for this cell line"))
        gt::gt(
          make_cell_sim_table(input = data(),
                              similarity = "expression")$bottom_table %>%
            dplyr::rename("Cell" = "cell2_name", "Bonferroni" = "bonferroni") %>% 
            dplyr::select(Cell, Bonferroni) %>%
            dplyr::arrange(Bonferroni) %>% 
            dplyr::mutate_if(is.numeric, ~ signif(., digits = 3)) %>% 
            dplyr::slice(1:5))
      },
      height = card_contents_height,
      width = card_contents_width
      )
    }
  )
}

###DEPENDENCY plots-----
cellDependenciesBarPlotTab <- function(id) {
  ns <- NS(id)
  divFlexAlignCenter(
    "Dependencies Barplot",
    uiOutput(outputId = ns("cell_dependencies_barplot_tab"))
  )
}

cellDependenciesBarPlotTabServer <- function (id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$cell_dependencies_barplot_tab <- renderUI({
        #check to see if data are there
        shiny::validate(
          shiny::need(c("universal_achilles_long", "cell_expression_meta") %in% data()$validate, "No dependency data for this gene"))
        #check to see if image exists
        img_path <- ddh::load_image(input = data(), fun_name = "make_cellbar", card = TRUE)
        if(!is.null(img_path)) {
          uiOutput(outputId = session$ns("cell_dependencies_barplot_tab_image"))
        } else {
          plotOutput(outputId = session$ns("cell_dependencies_barplot_tab_render"), 
                     height = card_contents_height,
                     width = card_contents_width) %>%
            withSpinnerColor(plot_type = data()$type)
        }
      })
      output$cell_dependencies_barplot_tab_image <- renderUI({
        tags$img(src = ddh::load_image(input = data(), fun_name = "make_cellbar", card = TRUE),
                 width = card_contents_width,
                 height = card_contents_height)
      })
      output$cell_dependencies_barplot_tab_render <- renderPlot({
        make_cellbar(input = data(),
                     card = TRUE,
                     scale = 0.2)
      })
    }
  )
}

cellDependenciesDensityPlotTab <- function(id) {
  ns <- NS(id)
  divFlexAlignCenter(
    "Dependencies Density",
    uiOutput(outputId = ns("cell_dependencies_densityplot_tab"))
  )
}

cellDependenciesDensityPlotTabServer <- function (id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$cell_dependencies_densityplot_tab <- renderUI({
        #check to see if data are there
        shiny::validate(
          shiny::need(c("universal_achilles_long", "cell_expression_meta") %in% data()$validate, "No dependency data for this gene"))
        #check to see if image exists
        img_path <- ddh::load_image(input = data(), fun_name = "make_cellbins", card = TRUE)
        if(!is.null(img_path)) {
          uiOutput(outputId = session$ns("cell_dependencies_densityplot_tab_image"))
        } else {
          plotOutput(outputId = session$ns("cell_dependencies_densityplot_tab_render"), 
                     height = card_contents_height,
                     width = card_contents_width) %>%
            withSpinnerColor(plot_type = data()$type)
        }
      })
      output$cell_dependencies_densityplot_tab_image <- renderUI({
        tags$img(src = ddh::load_image(input = data(), fun_name = "make_cellbins", card = TRUE),
                 width = card_contents_width,
                 height = card_contents_height)
      })
      output$cell_dependencies_densityplot_tab_render <- renderPlot({
        make_cellbins(input = data(),
                      card = TRUE)
      })
    }
  )
}

cellDepsLinPlotTab <- function(id) {
  ns <- NS(id)
  divFlexAlignCenter(
    "Lineage",
    uiOutput(outputId = ns("cell_dependencies_lineageplot_tab"))
  )
}

cellDepsLinPlotTabServer <- function (id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$cell_dependencies_lineageplot_tab <- renderUI({
        #check to see if data are there
        shiny::validate(
          shiny::need(c("universal_achilles_long") %in% data()$validate, "No dependency data for this gene"))
        #check to see if image exists
        img_path <- ddh::load_image(input = data(), fun_name = "make_lineage", card = TRUE)
        if(!is.null(img_path)) {
          uiOutput(outputId = session$ns("cell_dependencies_lineageplot_tab_image"))
        } else {
          plotOutput(outputId = session$ns("cell_dependencies_lineageplot_tab_render"), 
                     height = card_contents_height,
                     width = card_contents_width) %>%
            withSpinnerColor(plot_type = data()$type)
        }
      })
      output$cell_dependencies_lineageplot_tab_image <- renderUI({
        tags$img(src = ddh::load_image(input = data(), fun_name = "make_lineage", card = TRUE),
                 width = card_contents_width,
                 height = card_contents_height)
      })
      output$cell_dependencies_lineageplot_tab_render <- renderPlot({
        make_lineage(input = data(),
                     card = TRUE)
      })
    }
  )
}

cellDepsSubLinPlotTab <- function(id) {
  ns <- NS(id)
  divFlexAlignCenter(
    "Sublineage",
    uiOutput(outputId = ns("cell_dependencies_sublineageplot_tab"))
  )
}

cellDepsSubLinPlotTabServer <- function (id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$cell_dependencies_sublineageplot_tab <- renderUI({
        #check to see if data are there
        shiny::validate(
          shiny::need(c("universal_achilles_long") %in% data()$validate, "No dependency data for this gene"))
        #check to see if image exists
        img_path <- ddh::load_image(input = data(), fun_name = "make_sublineage", card = TRUE)
        if(!is.null(img_path)) {
          uiOutput(outputId = session$ns("cell_dependencies_sublineageplot_tab_image"))
        } else {
          plotOutput(outputId = session$ns("cell_dependencies_sublineageplot_tab_render"), 
                     height = card_contents_height,
                     width = card_contents_width) %>%
            withSpinnerColor(plot_type = data()$type)
        }
      })
      output$cell_dependencies_sublineageplot_tab_image <- renderUI({
        tags$img(src = ddh::load_image(input = data(), fun_name = "make_sublineage", card = TRUE),
                 width = card_contents_width,
                 height = card_contents_height)
      })
      output$cell_dependencies_sublineageplot_tab_render <- renderPlot({
        make_sublineage(input = data(),
                        card = TRUE)
      })
    }
  )
}

expdepPlotTab <- function(id) {
  ns <- NS(id)
  divFlexAlignCenter(
    "Expression v. Dependency",
    uiOutput(outputId = ns("cell_dependencies_expdep_tab"))
  )
}

expdepPlotTabServer <- function (id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$cell_dependencies_expdep_tab <- renderUI({
        #check to see if data are there
        shiny::validate(
          shiny::need(c("universal_achilles_long", "cell_expression_meta") %in% data()$validate, "No dependency data for this gene"))
        #check to see if image exists
        img_path <- ddh::load_image(input = data(), fun_name = "make_expdep", card = TRUE)
        if(!is.null(img_path)) {
          uiOutput(outputId = session$ns("cell_dependencies_expdep_tab_image"))
        } else {
          plotOutput(outputId = session$ns("cell_dependencies_expdep_tab_render"), 
                     height = card_contents_height,
                     width = card_contents_width) %>%
            withSpinnerColor(plot_type = data()$type)
        }
      })
      output$cell_dependencies_expdep_tab_image <- renderUI({
        tags$img(src = ddh::load_image(input = data(), fun_name = "make_expdep", card = TRUE),
                 width = card_contents_width,
                 height = card_contents_height)
      })
      output$cell_dependencies_expdep_tab_render <- renderPlot({
        make_expdep(input = data(),
                    card = TRUE)
      })
    }
  )
}

###DEPENDENCY tables-----
cellDependenciesTableTab <- function(id) {
  ns <- NS(id)
  divFlexAlignCenter(
    "Dependency Table",
    gt_output(outputId = ns("deptabletab"))
  )
}
cellDependenciesTableTabServer <- function (id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$deptabletab <- render_gt({
        shiny::validate(
          shiny::need(c("universal_achilles_long") %in% data()$validate, "No dependency data for this gene"))
        gt::gt(make_dep_table(input = data()) %>% 
                 dplyr::select(`Cell Line`, contains(data()$content)) %>%
                 dplyr::slice(1:5))
      },
      height = card_contents_height,
      width = card_contents_width
      )
    }
  )
}

cellDependenciesPosTableTab <- function(id) {
  ns <- NS(id)
  divFlexAlignCenter(
    "Positive Genes",
    gt_output(outputId = ns("deppostabletab"))
  )
}
cellDependenciesPosTableTabServer <- function (id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$deppostabletab <- render_gt({
        shiny::validate(
          shiny::need(c("gene_master_top_table") %in% data()$validate, "No dependency data for this gene"))
        gt::gt(make_top_table(input = data()) %>% 
                 # dplyr::mutate("Rank" = row_number()) %>% 
                 dplyr::select(c("Gene" = "gene", "Z-Score" = "z_score")) %>%
                 dplyr::slice(1:5))
      },
      height = card_contents_height,
      width = card_contents_width
      )
    }
  )
}

cellDependenciesPosPathwayTableTab <- function(id) {
  ns <- NS(id)
  divFlexAlignCenter(
    "Positive Enrichment",
    gt_output(outputId = ns("deppospathwaystab"))
  )
}
cellDependenciesPosPathwayTableTabServer <- function (id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$deppospathwaystab <- render_gt({
        shiny::validate(
          shiny::need(c("gene_master_positive") %in% data()$validate, "No dependency data for this gene"))
        gt::gt(make_enrichment_top(input = data()) %>% 
                 dplyr::select(`Gene List`) %>%
                 dplyr::slice(1:5))
      },
      height = card_contents_height,
      width = card_contents_width
      )
    }
  )
}

cellDependenciesNegTableTab <- function(id) {
  ns <- NS(id)
  divFlexAlignCenter(
    "Negative Genes",
    gt_output(outputId = ns("depnegtabletab"))
  )
}
cellDependenciesNegTableTabServer <- function (id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$depnegtabletab <- render_gt({
        shiny::validate(
          shiny::need(c("gene_master_bottom_table") %in% data()$validate, "No dependency data for this gene"))
        gt::gt(make_bottom_table(input = data()) %>% 
                 # dplyr::mutate("Rank" = row_number()) %>% 
                 dplyr::select(c("Gene" = "gene", "Z-Score" = "z_score")) %>%
                 dplyr::slice(1:5))
      },
      height = card_contents_height,
      width = card_contents_width
      )
    }
  )
}

cellDependenciesNegPathwayTableTab <- function(id) {
  ns <- NS(id)
  divFlexAlignCenter(
    "Negative Enrichment",
    gt_output(outputId = ns("depnegpathwaystab"))
  )
}
cellDependenciesNegPathwayTableTabServer <- function (id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$depnegpathwaystab <- render_gt({
        shiny::validate(
          shiny::need(c("gene_master_negative") %in% data()$validate, "No dependency data for this gene"))
        gt::gt(make_enrichment_bottom(input = data()) %>% 
                 dplyr::select(`Gene List`) %>%
                 dplyr::slice(1:5))
      },
      height = card_contents_height,
      width = card_contents_width
      )
    }
  )
}

cellDependenciesGenePathwayTableTab <- function(id) {
  ns <- NS(id)
  divFlexAlignCenter(
    "Pathways",
    gt_output(outputId = ns("depgenepathwaystab"))
  )
}

cellDependenciesGenePathwayTableTabServer <- function (id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$depgenepathwaystab <- render_gt({
        #UPDATEME
        # shiny::validate(
        #   shiny::need(c("gene_pathways_components") %in% data()$validate, "No dependency data for this gene"))
        gt::gt(make_gene_pathways_components(input = data()) %>%
                 dplyr::select(Pathway = feature2) %>%
                 dplyr::slice(1:5))
      },
      height = card_contents_height,
      width = card_contents_width
      )
    }
  )
}

geneMolecularFeaturesPathwayTableTab <- function(id) {
  ns <- NS(id)
  divFlexAlignCenter(
    "Pathways",
    gt_output(outputId = ns("molfeatpathwaystabletab"))
  )
}

geneMolecularFeaturesPathwayTableTabServer <- function (id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$molfeatpathwaystabletab <- render_gt({
        shiny::validate(
          shiny::need(c("universal_achilles_long") %in% data()$validate, # Check this
                      "No pathways found for this gene"))
        gt::gt(ddh::make_gene_molecular_features_pathways(input = data()) %>% 
                 dplyr::mutate("Rank" = row_number()) %>% 
                 dplyr::select(Rank, Pathway) %>% 
                 dplyr::slice(1:5))
      },
      height = card_contents_height,
      width = card_contents_width
      )
    }
  )
}

## CELL -----
# dash
cellSummaryTableTab <- function(id) {
  ns <- NS(id)
  divFlexAlignCenter(
    "Lineage Table",
    gt_output(outputId = ns("cell_summarydash"))
  )
}

cellSummaryTableTabServer <- function (id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$cell_summarydash <- render_gt({
        shiny::validate(
          shiny::need(c("cell_expression_meta") %in% data()$validate, "No summary data for this cell line"))
        gt::gt(
          make_cell_line_table(input = data()) %>%
            dplyr::select(1, 3) %>%
            dplyr::slice(1:5))
      },
      height = card_contents_height,
      width = card_contents_width
      )
    }
  )
}

cellLineDependenciesPosTableDash <- function(id) {
  ns <- NS(id)
  divFlexAlignCenter(
    "Co-essential Cell Lines",
    gt_output(outputId = ns("cell_deptabledash"))
  )
}

cellLineDependenciesPosTableDashServer <- function (id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$cell_deptabledash <- render_gt({
        shiny::validate(
          shiny::need(c("cell_dependency_sim") %in% data()$validate, "No dependency data for this cell line"))
        gt::gt(
          make_cell_sim_table(input = data())$top_table %>%
            dplyr::rename("Cell" = "cell2_name", "Bonferroni" = "bonferroni") %>% 
            dplyr::select(Cell, Bonferroni) %>%
            dplyr::arrange(Bonferroni) %>% 
            dplyr::mutate_if(is.numeric, ~ signif(., digits = 3)) %>% 
            dplyr::slice(1:5))
      },
      height = card_contents_height,
      width = card_contents_width
      )
    }
  )
}

cellLineExpressionPosTableDash <- function(id) {
  ns <- NS(id)
  divFlexAlignCenter(
    "Co-expressed Cell Lines",
    gt_output(outputId = ns("cell_exppostabletab"))
  )
}

cellLineExpressionPosTableDashServer <- function (id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$cell_exppostabletab <- render_gt({
        shiny::validate(
          shiny::need(c("cell_dependency_exp") %in% data()$validate, "No dependency data for this cell line"))
        gt::gt(
          make_cell_sim_table(input = data(),
                              similarity = "expression")$top_table %>%
            dplyr::rename("Cell" = "cell2_name", "Bonferroni" = "bonferroni") %>% 
            dplyr::select(Cell, Bonferroni) %>%
            dplyr::arrange(Bonferroni) %>% 
            dplyr::mutate_if(is.numeric, ~ signif(., digits = 3)) %>% 
            dplyr::slice(1:5))
      },
      height = card_contents_height,
      width = card_contents_width
      )
    }
  )
}

# tabs
cellLineDependenciesPosTableTab <- function(id) {
  ns <- NS(id)
  divFlexAlignCenter(
    "Positive Associations",
    gt_output(outputId = ns("cell_deppostabletab"))
  )
}

cellLineDependenciesPosTableTabServer <- function (id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$cell_deppostabletab <- render_gt({
        shiny::validate(
          shiny::need(c("cell_dependency_sim") %in% data()$validate, "No dependency data for this cell line"))
        gt::gt(
          make_cell_sim_table(input = data())$top_table %>%
            dplyr::rename("Cell" = "cell2_name", "Bonferroni" = "bonferroni") %>% 
            dplyr::select(Cell, Bonferroni) %>%
            dplyr::arrange(Bonferroni) %>% 
            dplyr::mutate_if(is.numeric, ~ signif(., digits = 3)) %>% 
            dplyr::slice(1:5))
      },
      height = card_contents_height,
      width = card_contents_width
      )
    }
  )
}

cellLineDependenciesNegTableTab <- function(id) {
  ns <- NS(id)
  divFlexAlignCenter(
    "Negative Associations",
    gt_output(outputId = ns("cell_depnegtabletab"))
  )
}
cellLineDependenciesNegTableTabServer <- function (id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$cell_depnegtabletab <- render_gt({
        shiny::validate(
          shiny::need(c("cell_dependency_sim") %in% data()$validate, "No dependency data for this cell line"))
        gt::gt(
          make_cell_sim_table(input = data())$bottom_table %>%
            dplyr::rename("Cell" = "cell2_name", "Bonferroni" = "bonferroni") %>% 
            dplyr::select(Cell, Bonferroni) %>%
            dplyr::arrange(Bonferroni) %>% 
            dplyr::mutate_if(is.numeric, ~ signif(., digits = 3)) %>% 
            dplyr::slice(1:5))
      },
      height = card_contents_height,
      width = card_contents_width
      )
    }
  )
}

cellLineDependenciesTableTab <- function(id) {
  ns <- NS(id)
  divFlexAlignCenter(
    "Dependency Table",
    gt_output(outputId = ns("deptabletab"))
  )
}
cellLineDependenciesTableTabServer <- function (id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$deptabletab <- render_gt({
        shiny::validate(
          shiny::need(c("universal_achilles_long") %in% data()$validate, "No dependency data for this cell line"))
        gt::gt(make_dep_table(input = data()) %>% 
                 dplyr::rename(Gene = gene) %>% 
                 dplyr::select(Gene, contains(data()$content)) %>%
                 dplyr::slice(1:5))
      },
      height = card_contents_height,
      width = card_contents_width
      )
    }
  )
}

cellLineMetadataPlotTab <- function(id) {
  ns <- NS(id)
  divFlexAlignCenter(
    "Lineage Similarity",
    plotOutput(outputId = ns("metadata_plot_tab"))
  )
}

cellLineMetadataPlotTabServer <- function (id, data, type) {
  moduleServer(
    id,
    function(input, output, session) {
      output$metadata_plot_tab <- renderPlot({
        shiny::validate(
          shiny::need(c("cell_dependency_sim") %in% data()$validate, "No association data for this cell line"))
        withProgress(message = 'Almost there...', value = 1, {
          make_metadata_cell(input = data(),
                             cell_line_similarity = type,
                             card = TRUE
          )
        })
      })
    })
}

## COMPOUND -----
