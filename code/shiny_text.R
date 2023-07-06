
# GLOBAL ----
## TITLES -----
geneTitle <- function (id) {
  ns <- NS(id)
  fluidRow(h3(textOutput(outputId = ns("gene_summary_title"))))
}

geneTitleServer <- function (id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$gene_summary_title <- renderText({
        paste0(make_summary_gene(input = data(), var = "id"), ": ",
               make_summary_gene(input = data(), var = "name")
        )
      })
    }
  )
}

pathwayTitle <- function (id) {
  ns <- NS(id)
  fluidRow(h3(textOutput(outputId = ns("pathway_title"))))
}

pathwayTitleServer <- function(id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$pathway_title <- renderText({
        paste0("Pathway: ", 
               make_summary_pathway(input = data(), var = "gs_name"), 
               " (", 
               make_summary_pathway(input = data(), var = "gs_cat"), 
               "/",
               make_summary_pathway(input = data(), var = "gs_subcat"), 
               ")"
        )
      })
    }
  )
}

customListTitle <- function (id) {
  ns <- NS(id)
  fluidRow(h3(textOutput(outputId = ns("custom_list_title"))))
}

customListTitleServer <- function(id, data) {
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

cellTitle <- function (id) {
  ns <- NS(id)
  fluidRow(h3(textOutput(outputId = ns("cell_summary_title"))))
}

cellTitleServer <- function (id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$cell_summary_title <- renderText({
        paste0(make_summary_cell(input = data(), var = "cell_line"), ": ", 
               make_summary_cell(input = data(), var = "lineage_subtype"))})
    }
  )
}

lineageTitle <- function (id) {
  ns <- NS(id)
  fluidRow(h3(textOutput(outputId = ns("lineage_title"))))
}

lineageTitleServer <- function(id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$lineage_title <- renderText({
        paste0("Lineage: ", make_summary_cell(input = data(), var = "lineage"))})
    }
  )
}

lineageSubtypeTitle <- function (id) {
  ns <- NS(id)
  fluidRow(h3(textOutput(outputId = ns("lineage_subtype_title"))))
}

lineageSubtypeTitleServer <- function(id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$lineage_subtype_title <- renderText({
        paste0("Lineage: ", make_summary_cell(input = data(), var = "lineage_subtype"))})
    }
  )
}

## SUMMARIES -----
customListText <- function (id) {
  ns <- NS(id)
  fluidRow(htmlOutput(outputId = ns("query_list_summary")))
}

customListTextServer <- function(id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$query_list_summary <- renderText({
        make_summary_text(input = data()) %>%
          lit_linkr(summary_table = universal_gene_summary)
      })
    }
  )
}

pathwayText <- function (id) {
  ns <- NS(id)
  tagList(
    fluidRow(h3(textOutput(outputId = ns("pathway_title")))),
    fluidRow(
      tags$dl(
        tags$dt("Genes"), tags$dd(htmlOutput(outputId = ns("pathway_gene_symbols"))),
        tags$dt("Pathway Description"), tags$dd(textOutput(outputId = ns("pathway_def")))
        )
      )
  )
}

pathwayTextServer <- function(id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$pathway_title <- renderText({paste0("Pathway: ",
                                                 make_summary_pathway(input = data(), var = "pathway"), " (GO:", 
                                                 make_summary_pathway(input = data(), var = "go"), ")")})
      output$pathway_gene_symbols <- renderText({make_summary_pathway(input = data(), var = "data") %>% internal_link()})
      output$pathway_def <- renderText({make_summary_pathway(input = data(), var = "def")})
    }
  )
}

proteinText <- function (id) {
  ns <- NS(id)
  fluidRow(htmlOutput(outputId = ns("protein_summary")))
}

proteinTextServer <- function(id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$protein_summary <- renderText({
        make_summary_protein(input = data()) %>%
          lit_linkr(summary_table = universal_gene_summary)
      })
    }
  )
}

# GENE ----
## Tissue ----
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

# TO DO -----
lineageSummaryText <- function (id) {
  ns <- NS(id)
  tagList(
    fluidRow(h3(textOutput(outputId = ns("lineage_summary_title")))),
    fluidRow(h4("Summary")),
    fluidRow(
      tags$dl(
        tags$dt("Cell lines"), tags$dd(textOutput(outputId = ns("lineage_summary_cell_lines")))
        )
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

