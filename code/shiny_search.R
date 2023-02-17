gene_query_result_row <- function(row) {
  gene_summary <- universal_gene_summary %>%
    filter(approved_symbol==row["content_id"])
  title <- paste0(gene_summary$approved_symbol, ": ", gene_summary$approved_name)
  list(
    h4(
      tags$strong("Gene:"),
      tags$a(title, href=paste0("?show=gene&query=", gene_summary$approved_symbol))
    ),
    div(tags$strong("Aka:"), gene_summary$aka),
    div(tags$strong("Entrez ID:"), gene_summary$ncbi_gene_id),
    hr()
  )
}

pathway_query_result_row <- function(row) {
  pathways_row <- gene_pathways %>%
    filter(go==row["content_id"])
  gene_symbols <- lapply(pathways_row$data, function(x) { paste(x$gene, collapse=', ') })
  title <- paste0(pathways_row$pathway, " (GO:", pathways_row$go, ")")
  list(
    h4(
      tags$strong("Pathway:"),
      tags$a(title, href=paste0("?show=pathway&query=", pathways_row$go))
    ),
    tags$dl(
      tags$dt("Genes"),
      tags$dd(gene_symbols),
    ),
    hr()
  )
}

gene_list_query_result_row <- function(gene_summary_rows, multi_items) {
  known_gene_symbols <- gene_summary_rows$content_id
  unknown_gene_symbols <- setdiff(multi_items, known_gene_symbols)
  known_gene_symbols_tags <- NULL
  title <- paste0(multi_items, collapse=", ")

  gene_query_param <- paste0("query=", paste(known_gene_symbols, collapse=","))
  href <- paste0("?show=gene_list&", gene_query_param)
  known_gene_symbols_tags <- list(
    tags$h6("Known Gene Symbols"),
    tags$a(paste(known_gene_symbols, collapse=", "), href=href)
  )
  unknown_gene_symbols_tags <- NULL
  if (unknown_gene_symbols == "") {
    unknown_gene_symbols_tags <- list(
      tags$h6("Unknown Gene Symbols"),
      tags$div(paste(unknown_gene_symbols, collapse=", "))
    )
  }
  
  list(
    h4(
      tags$strong("Custom Gene List"),
      tags$span(title)
    ),
    known_gene_symbols_tags,
    unknown_gene_symbols_tags,
    hr()
  )
}

cell_query_result_row <- function(row) {
  cell_row <- cell_expression_names %>%
    filter(cell_line==row["content_id"])
  print(cell_row)
  list(
    h4(
      tags$strong("Cell:"),
      tags$a(cell_row$cell_line, href=paste0("?show=cell&query=", cell_row$cell_line))
    ),
    div(tags$strong("Lineage:"), cell_row$lineage),
    div(tags$strong("Sublineage:"), cell_row$lineage_subtype),
    hr()
  )
}

lineage_query_result_row <- function(row) {
  cell_lines <- cell_expression_names %>%
    filter(lineage==row["content_id"]) %>%
    pull(cell_line)
  cell_line_str <- paste(cell_lines, collapse=", ")
  list(
    h4(
      tags$strong("Lineage:"),
      tags$a(row["content_id"], href=paste0("?show=lineage&query=", row["content_id"]))
    ),
    div(tags$strong("Cells:"), cell_line_str),
    hr()
  )
}

lineage_subtype_query_result_row <- function(row) {
  cell_lines <- cell_expression_names %>%
    filter(lineage_subtype==row["content_id"]) %>%
    pull(cell_line)
  cell_line_str <- paste0(cell_lines, collapse=", ")
  list(
    h4(
      tags$strong("Sublineage:"),
      tags$a(row["content_id"], href=paste0("?show=lineage_subtype&query=", row["content_id"]))
    ),
    div(tags$strong("Cells:"), cell_line_str),
    hr()
  )
}

cell_list_query_result_row <- function(row) {
  expression_names_rows <- row$data
  title <- row$key
  
  known_expression_names <- expression_names_rows %>%
    filter(known == TRUE) %>%
    pull(cell_line)
  has_known_expression_names <- !is_empty(known_expression_names)
  
  unknown_expression_names <- expression_names_rows %>%
    filter(known == FALSE) %>%
    pull(cell_line)
  has_unknown_expression_names <- !is_empty(unknown_expression_names)
  
  known_cell_line_tags <- NULL
  if (has_known_expression_names) {
    cell_list_param <- paste0("query=", paste(known_expression_names, collapse=","))
    href <- paste0("?show=cell_list&", cell_list_param)
    known_cell_line_tags <- list(
      tags$h6("Known Cell Lines"),
      tags$a(paste(known_expression_names, collapse=", "), href=href)
    )
  }
  
  unknown_cell_line_tags <- NULL
  if (has_unknown_expression_names) {
    unknown_cell_line_tags <- list(
      tags$h6("Unknown Cell Lines"),
      tags$div(paste(unknown_expression_names, collapse=", "))
    )
  }
  
  list(
    h4(
      tags$strong("Custom Cell Line List"),
      tags$span(title)
    ),
    known_cell_line_tags,
    unknown_cell_line_tags,
    hr()
  )
}

compound_query_result_row <- function(row) {
  prism_name_row <- compound_prism_names %>%
    filter(name==row["content_id"])
  list(
    h4(
      tags$strong("Compound:"),
      tags$a(prism_name_row$name, href=paste0("?show=compound&query=", prism_name_row$name))
    ),
    div(tags$strong("Mechanism of Action:"), prism_name_row$moa),
    div(tags$strong("CID:"), prism_name_row$cid),
    hr()
  )
}

moa_query_result_row <- function(row) {
  prism_names <- compound_prism_names %>%
    filter(moa==row["content_id"]) %>%
    pull(name)
  compounds <- paste0(prism_names, collapse=", ")
  list(
    h4(
      tags$strong("Compound Mechanism of Action:"),
      tags$a(row["content_id"], href=paste0("?show=moa&query=", row["content_id"]))
    ),
    div(tags$strong("Compounds:"), compounds),
    hr()
  )
}

metabolite_query_result_row <- function(row) {
  hmdb_name_row <- compound_hmdb_names %>%
    filter(cid==row["content_id"])
  list(
    h4(
      tags$strong("Metabolite:"),
      tags$a(hmdb_name_row$name, href=paste0("?show=metabolite&query=", hmdb_name_row$name))
    ),
    div(tags$strong("Class:"), hmdb_name_row$class),
    div(tags$strong("CID:"), hmdb_name_row$cid),
    hr()
  )
}

compound_list_query_result_row <- function(row) {
  prism_names_rows <- row$data
  title <- row$key
  
  known_compound_names <- prism_names_rows %>%
    filter(known == TRUE) %>%
    pull(name)
  has_known_compound_names <- !is_empty(known_compound_names)
  
  unknown_compound_names <- prism_names_rows %>%
    filter(known == FALSE) %>%
    pull(name)
  has_unknown_compound_names <- !is_empty(unknown_compound_names)
  
  known_compound_tags <- NULL
  if (has_known_compound_names) {
    compound_list_param <- paste0("query=", paste(known_compound_names, collapse=","))
    href <- paste0("?show=compound_list&", compound_list_param)
    known_compound_tags <- list(
      tags$h6("Known Compounds"),
      tags$a(paste(known_compound_names, collapse=", "), href=href)
    )
  }
  
  unknown_compound_tags <- NULL
  if (has_unknown_compound_names) {
    unknown_compound_tags <- list(
      tags$h6("Unknown Compounds"),
      tags$div(paste(unknown_compound_names, collapse=", "))
    )
  }
  
  list(
    h4(
      tags$strong("Custom Compound List"),
      tags$span(title)
    ),
    known_compound_tags,
    unknown_compound_tags,
    hr()
  )
}

# specifies how to render the results for a specific subtype
# functions that generate rows in fun_tables.R eg. gene_list_query_results_table()
subtype_to_query_result_row = list(
  gene=gene_query_result_row,
  pathway=pathway_query_result_row,
  cell=cell_query_result_row,
  lineage=lineage_query_result_row,
  lineage_subtype=lineage_subtype_query_result_row,
  compound=compound_query_result_row,
  moa=moa_query_result_row,
  metabolite=metabolite_query_result_row
)

multi_query_result_row <- function(search_index_list, multi_query_items) {
  if (search_index_list$subtype == "gene") {
    gene_list_query_result_row(search_index_list$data, multi_query_items)
  } else if (search_index_list$subtype == "compound") {
    compound_list_query_result_row(search_index_list$data, multi_query_items)
  } else if (search_index_list$subtype == "cell") {
    compound_list_query_result_row(search_index_list$data, multi_query_items)
  } else {
    paste0("Unexpected subtype", search_index_list$subtype)
  }
}

query_result_row <- function(row) {
  subtype <- row["subtype"]
  func <- subtype_to_query_result_row[[subtype]]
  func(row)
}

search_query_html_result <- function(search_index, query_str) {
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
}

debug_print_html <-function(html_result) {
  print(tags$div(html_result))
}

debug_print_html(search_query_html_result(search_index, "ROCKa1,ROaCK2,POTATO"))