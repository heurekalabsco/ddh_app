# HELPER -----
#DUMMY
nameText <- function (id) {
  ns <- NS(id)
  "test"
}

nameTextServer <- function (id, data) {
  moduleServer(
    id,
    function(input, output, session) {
    }
  )
}

##TEMPLATE
# nameText <- function (id) {
#     ns <- NS(id)
# }
# 
# nameTextServer <- function (id, data) {
#   moduleServer(
#     id,
#     function(input, output, session) {
#     }
#   )
# }

#text for pages
# GLOBAL -----
## Titles -----
summaryTitle <- function (id) {
  ns <- NS(id)
  list(
    h3(textOutput(outputId = ns("summary_title")))
  )
}

summaryTitleServer <- function(id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$summary_title <- renderText({paste0(str_c(data()$content, collapse = ", "))})
    }
  )
}

summaryListTitle <- function (id) {
  ns <- NS(id)
  list(
    h3(textOutput(outputId = ns("custom_list_title")))
  )
}

summaryListTitleServer <- function(id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$custom_list_title <- renderText({paste0("Custom ", 
                                                     ifelse(data()$type == "gene", "Gene", "Cell Line"),
                                                     " List: ", 
                                                     str_c(data()$content, collapse = ", "))})
    }
  )
}

## Summary -----
summaryListText <- function (id) {
  ns <- NS(id)
  htmlOutput(outputId = ns("query_list_summary"))
}

summaryListTextServer <- function(id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$query_list_summary <- renderText({
        make_summary_list(input = data()) %>% 
          lit_linkr(summary_table = universal_gene_summary) # see fun_helper.R
      })
    }
  )
}

# GENE -----
## Titles -----
geneTitle <- function (id) {
  ns <- NS(id)
  tagList(
    h3(textOutput(outputId = ns("gene_summary_title")))
  )
}


geneTitleServer <- function (id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$gene_summary_title <- renderText({
        paste0(make_summary_gene(input = data(), var = "id"), ": ", make_summary_gene(input = data(), var = "approved_name"))
      })
    }
  )
}
pathwayTitle <- function (id) {
  ns <- NS(id)
  tagList(
    h3(textOutput(outputId = ns("pathway_title")))
  )
}

pathwayTitleServer <- function(id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$pathway_title <- renderText({paste0("Pathway: ", make_summary_pathway(input = data(), var = "gs_name"), " (Source:", make_summary_pathway(input = data(), var = "gs_exact_source"), ")")})
    }
  )
}

## Summary -----
geneText <- function (id) {
  ns <- NS(id)
  tagList(
    h3(textOutput(outputId = ns("gene_summary_title"))),
    tags$dl(
      tags$dt("Gene Summary"), tags$dd(htmlOutput(outputId = ns("gene_summary_entrez_summary"))),
      tags$dt("Entrez ID"), tags$dd(htmlOutput(outputId = ns("ncbi_link"))), 
      tags$dt("Coding Sequence Length"), tags$dd(textOutput(outputId = ns("gene_length"))), 
      tags$dt("aka"), tags$dd(textOutput(outputId = ns("gene_summary_aka")))
    )
  )
}

geneTextServer <- function (id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$gene_summary_title <- renderText({paste0(make_summary_gene(input = data(), var = "name"), ": ", make_summary_gene(input = data(), var = "approved_name"))})
      output$gene_summary_approved_symbol <- renderText(make_summary_gene(input = data(), var = "name"))
      output$gene_summary_approved_name <- renderText(make_summary_gene(input = data(), var = "approved_name"))
      output$gene_summary_aka <- renderText(make_summary_gene(input = data(), var = "aka"))
      output$gene_length <- renderText(paste0(make_summary_gene(data_gene_summary = gene_location, input = data(), var = "cds_length"), " bp"))
      output$gene_summary_entrez_summary <- renderText({
        shiny::validate(
          shiny::need(c("universal_gene_summary") %in% data()$validate, "No summary data for this gene"))
        make_summary_gene(input = data(), var = "entrez_summary") %>% 
          lit_linkr(summary_table = universal_gene_summary)})
      output$ncbi_link <- renderText(paste0('<a href="https://www.ncbi.nlm.nih.gov/gene/?term=', 
                                            make_summary_gene(input = data(), var = "ncbi_gene_id"),
                                            '" target="_blank">', 
                                            make_summary_gene(input = data(), var = "ncbi_gene_id"),
                                            '</a>'))
    }
  )
}

#pathways
pathwayText <- function (id) {
  ns <- NS(id)
  tagList(
    h3(textOutput(outputId = ns("pathway_title"))),
    tags$dl(
      tags$dt("Genes"), tags$dd(htmlOutput(outputId = ns("pathway_gene_symbols"))),
      tags$dt("Pathway Description"), tags$dd(textOutput(outputId = ns("pathway_def")))
    )
  )
}

pathwayTextServer <- function(id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$pathway_title <- renderText({paste0("Pathway: ", make_summary_pathway(input = data(), var = "pathway"), " (GO:", make_summary_pathway(input = data(), var = "go"), ")")})
      output$pathway_gene_symbols <- renderText({make_summary_pathway(input = data(), var = "data") %>% internal_link()})
      output$pathway_def <- renderText({make_summary_pathway(input = data(), var = "def")})
    }
  )
}

#protein page
#this is the protein tab for a gene search
proteinText <- function (id) {
  ns <- NS(id)
  tagList(
    h3(textOutput(outputId = ns("protein_summary_title"))),
    tags$dl(
      tags$dt("Protein Summary"), tags$dd(htmlOutput(outputId = ns("protein_summary_uniprot_summary"))), 
      tags$dt("Uniprot ID"), tags$dd(htmlOutput(outputId = ns("uniprot_link"))),
      tags$dt("Enzyme Commission", a(href = "https://en.wikipedia.org/wiki/Enzyme_Commission_number", img(src="link out_25.png", width="10", height="10"),  target="_blank")), tags$dd(htmlOutput(outputId = ns("ec_link"))),
      tags$dt("Protein Mass"), tags$dd(textOutput(outputId = ns("protein_summary_mass"))),
    ),
  )
}

proteinTextServer <- function (id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$protein_summary_title <- renderText({paste0(make_summary_protein(input = data(), var = "gene_name"), ": ", make_summary_protein(input = data(), var = "protein_name"))})
      output$ec_link <- renderText(
        if (is.na(make_summary_protein(input = data(), var = "ec"))) {paste0('<a href="https://enzyme.expasy.org" target="_blank">', make_summary_protein(input = data(), var = "ec"),'</a>')
        } else {paste0('<a href="https://enzyme.expasy.org/EC/', make_summary_protein(input = data(), var = "ec"), '">', make_summary_protein(input = data(), var = "ec"),'</a>')
        })
      output$protein_summary_uniprot_summary <- renderText({
        shiny::validate(
          shiny::need(c("universal_proteins") %in% data()$validate, "No summary data for this protein"))
        make_summary_protein(input = data(), var = "function_cc") %>% 
          lit_linkr(summary_table = universal_gene_summary)})
      output$uniprot_link <- renderText(paste0('<a href="https://www.uniprot.org/uniprot/', make_summary_protein(input = data(), var = "uniprot_id"), '" target="_blank">', make_summary_protein(input = data(), var = "uniprot_id"),'</a>'))
      output$protein_summary_mass <- renderText(paste0(make_summary_protein(input = data(), var = "mass"), " kDa"))
    }
  )
}

# CELL -----
## Titles -----
cellTitle <- function (id) {
  ns <- NS(id)
  tagList(
    h3(textOutput(outputId = ns("cell_summary_title")))
  )
}

cellTitleServer <- function (id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$cell_summary_title <- renderText({paste0(make_summary_cell(input = data(), var = "cell_line"), ": ", make_summary_cell(input = data(), var = "lineage_subtype"))})
    }
  )
}
lineageTitle <- function (id) {
  ns <- NS(id)
  tagList(
    h3(textOutput(outputId = ns("lineage_title")))
  )
}

lineageTitleServer <- function(id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$lineage_title <- renderText({paste0("Lineage: ", make_summary_cell(input = data(), var = "lineage"))})
    }
  )
}
lineageSubtypeTitle <- function (id) {
  ns <- NS(id)
  tagList(
    h3(textOutput(outputId = ns("lineage_subtype_title")))
  )
}

lineageSubtypeTitleServer <- function(id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$lineage_subtype_title <- renderText({paste0("Lineage: ", make_summary_cell(input = data(), var = "lineage_subtype"))})
    }
  )
}

tissueTitle <- function (id) {
  ns <- NS(id)
  list(
    h4(textOutput(outputId = ns("tissue_title")))
  )
}

tissueTitleServer <- function(id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$tissue_title <- renderText({paste0("Human anatogram of ", str_c(data()$content, collapse = ", "))})
    }
  )
}
###
## Summary -----

cellSummaryText <- function (id) {
  ns <- NS(id)
  tagList(
    h3(textOutput(outputId = ns("cell_summary_title"))),
    h4("Summary"),
    tags$dl(
      tags$dt("Lineage"), tags$dd(htmlOutput(outputId = ns("cell_summary_lineage"))),
      tags$dt("Lineage subtype"), tags$dd(htmlOutput(outputId = ns("cell_summary_lineage_subtype"))), 
      tags$dt("Description"), tags$dd(htmlOutput(outputId = ns("cell_description"))),
      tags$dt("Age"), tags$dd(textOutput(outputId = ns("cell_age"))),
      tags$dt("Sex"), tags$dd(textOutput(outputId = ns("cell_sex")))
    )
  )
}

cellSummaryTextServer <- function(id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$cell_summary_title <- renderText({
        data()$query
      })
      output$cell_summary_lineage <- renderText({
        make_summary_cell(input = data(), var = "lineage") %>% 
          map_chr(cell_linkr, type = "lineage")
      })
      output$cell_summary_lineage_subtype <- renderText({
        make_summary_cell(input = data(), var = "lineage_subtype") %>% 
          map_chr(cell_linkr, type = "lineage_subtype")
      })
      output$cell_description <- renderText({
        make_summary_cellosaurus(input = data(), var = "CC") %>% 
          lit_linkr(summary_table = universal_gene_summary)
      })
      output$cell_age <- renderText({
        make_summary_cellosaurus(input = data(), var = "AG")
      })
      output$cell_sex <- renderText({
        make_summary_cellosaurus(input = data(), var = "SX")
      })
    })
}


lineageSummaryText <- function (id) {
  ns <- NS(id)
  tagList(
    h3(textOutput(outputId = ns("lineage_summary_title"))),
    h4("Summary"),
    tags$dl(
      tags$dt("Cell lines"), tags$dd(textOutput(outputId = ns("lineage_summary_cell_lines"))),
    )
  )
}

lineageSummaryTextServer <- function(id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$lineage_summary_title <- renderText({
        data()$query
      })
      output$lineage_summary_cell_lines <- renderText({
        paste0(data()$content, collapse = ", ")
      })
    })
}

lineageSubtypeSummaryText <- function (id) {
  ns <- NS(id)
  tagList(
    h3(textOutput(outputId = ns("lineage_subtype_summary_title"))),
    h4("Summary"),
    tags$dl(
      tags$dt("Cells"), tags$dd(textOutput(outputId = ns("lineage_subtype_summary_cell_lines"))),
    )
  )
}

lineageSubtypeSummaryTextServer <- function(id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$lineage_subtype_summary_title <- renderText({
        data()$query
      })
      output$lineage_subtype_summary_cell_lines <- renderText({
        paste0(data()$content, collapse = ", ")
      })
    })
}

cellListSummaryText <- function (id) {
  ns <- NS(id)
  tagList(
    h3(textOutput(outputId = ns("title"))),
  )
}

cellListSummaryTextServer <- function(id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$title <- renderText({
        data()$query
      })
    })
}



# COMPOUND -----
## Titles -----
compoundTitle <- function (id) {
  ns <- NS(id)
  tagList(
    h3(textOutput(outputId = ns("compound_title")))
  )
}

compoundTitleServer <- function (id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$compound_title <- renderText({
        paste0(make_summary_compound(input = data(), var = "name") %>% stringr::str_to_title(), 
               ": ", 
               make_summary_compound(input = data(), var = "moa") %>% stringr::str_to_title())
      })
    }
  )
}

metaboliteTitle <- function (id) {
  ns <- NS(id)
  tagList(
    h3(textOutput(outputId = ns("metabolite_title")))
  )
}

metaboliteTitleServer <- function (id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$metabolite_title <- renderText({
        paste0(make_summary_metabolite(input = data(), var = "name") %>% stringr::str_to_title(),
               ": ",
               make_summary_metabolite(input = data(), var = "class") %>% stringr::str_to_title())
      })
    }
  )
}

moaTitle <- function (id) {
  ns <- NS(id)
  tagList(
    h3(textOutput(outputId = ns("moa_title")))
  )
}

moaTitleServer <- function(id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$moa_title <- renderText({
        paste0("MOA: ", 
               make_summary_compound(input = data(), var = "moa") %>% stringr::str_to_title())
      })
    }
  )
}
compoundListTitle <- function (id) {
  ns <- NS(id)
  list(
    h3(textOutput(outputId = ns("custom_compound_list")))
  )
}

compoundListTitleServer <- function(id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$custom_compound_list <- renderText({paste0("Custom Compound List: ", 
                                                        make_summary_compound_list(input = data())  %>% stringr::str_to_title())
      })
    }
  )
}
###
## Summary -----
compoundSummaryText <- function (id) {
  ns <- NS(id)
  tagList(
    h3(textOutput(outputId = ns("compound_summary_title"))),
    h4("Summary"),
    tags$dl(
      tags$dt("Description"), tags$dd(textOutput(outputId = ns("compound_summary_description"))),
      tags$dt("MOA"), tags$dd(htmlOutput(outputId = ns("compound_summary_moa"))),
      tags$dt("Phase"), tags$dd(textOutput(outputId = ns("compound_summary_phase"))),
      tags$dt("Disease Area"), tags$dd(textOutput(outputId = ns("compound_summary_diseasearea"))),
      tags$dt("Indication"), tags$dd(textOutput(outputId = ns("compound_summary_indication"))),
      tags$dt("Targets"), tags$dd(htmlOutput(outputId = ns("compound_summary_targets"))),
      tags$dt("CID"), tags$dd(htmlOutput(outputId = ns("cid_link")))
    )
  )
}

compoundSummaryTextServer <- function(id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$compound_summary_title <- renderText({
        data()$query %>% 
          stringr::str_to_title()
      })
      output$compound_summary_description <- renderText({
        make_summary_compound(input = data(), var = "description") %>% 
          stringr::str_to_sentence()
      })
      output$compound_summary_moa <- renderText({
        make_summary_compound(input = data(), var = "moa") %>% 
          map_chr(moa_linkr)
      })
      output$compound_summary_phase <- renderText({
        make_summary_compound(input = data(), var = "phase") %>% 
          stringr::str_to_title()
      })
      output$compound_summary_diseasearea <- renderText({
        make_summary_compound(input = data(), var = "disease_area") %>% 
          stringr::str_to_title()
      })
      output$compound_summary_indication <- renderText({
        make_summary_compound(input = data(), var = "indication") %>% 
          stringr::str_to_title()
      })
      output$compound_summary_targets <- renderText({
        make_summary_compound(input = data(), var = "target") %>% 
          stringr::str_to_upper() %>% 
          map_chr(internal_link)
      })
      output$compound_summary_cid <- renderText({
        make_summary_compound(input = data(), var = "cid")
      })
      output$cid_link <- renderText(paste0('<a href="https://pubchem.ncbi.nlm.nih.gov/compound/', 
                                           make_summary_compound(input = data(), var = "cid"),
                                           '" target="_blank">', 
                                           make_summary_compound(input = data(), var = "cid"),
                                           '</a>'))
    })
}

## MOA QUERY -----
moaSummaryText <- function (id) {
  ns <- NS(id)
  tagList(
    h3(textOutput(outputId = ns("moa_summary_title"))),
    h4("Summary"),
    tags$dl(
      tags$dt("compounds"), tags$dd(textOutput(outputId = ns("moa_summary_compounds"))),
    )
  )
}

moaSummaryTextServer <- function(id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$moa_summary_title <- renderText({
        data()$query
      })
      output$moa_summary_compounds <- renderText({
        paste0(data()$content, collapse = ", ")
      })
    })
}

## CUSTOM COMPOUND LIST QUERY -----
compoundListSummaryText <- function (id) {
  ns <- NS(id)
  tagList(
    h3(textOutput(outputId = ns("title"))),
  )
}

compoundListSummaryTextServer <- function(id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$title <- renderText({
        data()$query
      })
    })
}

## TISSUE
tissuePlotText <- function (id) {
  ns <- NS(id)
  fluidRow(h4(textOutput(ns("tissueplot_text"))))
}

tissuePlotTextServer <- function (id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$tissueplot_text <- renderText({paste0("Human tissue expression for ", 
                                                   str_c(data()$content, collapse = ", "))})
    }
  )
}

