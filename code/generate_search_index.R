# Creates search_index feather file with the following columns:
# label - string - value searched (approved symbol, name, aka, etc)
# content_id - string - key filed (approved symbol, pathway id, etc)
# priority - number - used to prioritize some labels over others in search results ordering
# subtype - subtype of the data stored in content_id
#
# Columns with sample data:
# ____________________________________
# label    content_id priority subtype
# ____________________________________
# ROCK1    ROCK1      1        gene     <- approved symbol
# Rho...   ROCK1      2        gene     <- approved name
# p160ROCK ROCK1      3        gene     <- aka element

library(tidyverse)

## Generic Functions ----

word_starts_with_regex <- function(query_str) {
  regex(paste0('\\b', query_str), ignore_case=TRUE)
}

## Testing Search Functions ----

calc_rank <- function(query_str, value, priority) {
  # query_str must be within value (though case may be different)
  # if both items are the same length it is a perfect match so returns 1
  # otherwise returns a value comparing the lengths of the strings
  str_length(query_str) / str_length(value) / priority
}

search <- function(search_index, query_str) {
  word_starts_with_query_str <- word_starts_with_regex(query_str)
  search_index %>%
    filter(str_detect(label, word_starts_with_query_str)) %>%
    mutate(rank=calc_rank(query_str, label, priority)) %>%
    arrange(desc(rank))
}

search_custom_list <- function(search_index, query_str) {
  custom_content_ids <- str_split(query_str, "\\s*,\\s*", simplify = TRUE)
  search_index %>%
    filter(content_id %in% custom_content_ids, content_id==label) %>%
    filter(subtype %in% c("gene", "cell", "compound")) %>% # custom lists are only gene_symbols,cell_lines, ...
    group_by(subtype) %>%
    mutate(content_id = paste0(content_id, collapse = ",")) %>% 
    select(content_id, subtype) %>%
    distinct()
}

## Gene Search Functions ----

build_approved_symbol_search_index <- function(universal_gene_summary) {
  universal_gene_summary %>%
    select(label=approved_symbol, content_id=approved_symbol) %>%
    add_column(priority=1)
}

build_approved_name_search_index <- function(universal_gene_summary) {
  universal_gene_summary %>%
    select(label=approved_name, content_id=approved_symbol) %>%
    add_column(priority=1)
}

build_aka_search_index <- function(universal_gene_summary) {
  universal_gene_summary %>% 
    separate_rows(aka) %>%
    select(label=aka, content_id=approved_symbol) %>%
    add_column(priority=2)
}

build_subtype_gene_search_index <- function(universal_gene_summary) {
  bind_rows(
    build_approved_symbol_search_index(universal_gene_summary),
    build_approved_name_search_index(universal_gene_summary),
    build_aka_search_index(universal_gene_summary)
  ) %>%
    add_column(subtype="gene")
}

## Pathway Search Functions ----

build_pathway_name_search_index <- function(gene_pathways) {
  gene_pathways %>%
    select(label=pathway, content_id=go) %>%
    add_column(priority=1)
}

build_pathway_go_search_index <- function(gene_pathways) {
  gene_pathways %>%
    select(label=go, content_id=go) %>%
    add_column(priority=1)
}

build_subtype_pathway_search_index <-function(gene_pathways) {
  bind_rows(
    build_pathway_name_search_index(gene_pathways),
    build_pathway_go_search_index(gene_pathways)
  ) %>%
    add_column(subtype="pathway")
}

## Cell line Search Functions ----

build_subtype_cell_search_index <- function(cell_expression_names) {
  cell_expression_names %>%
    select(label=cell_line, content_id=cell_line) %>%
    add_column(priority=1) %>%
    add_column(subtype="cell")
}

build_subtype_lineage_search_index <- function(cell_expression_names) {
  cell_expression_names %>%
    select(label=lineage, content_id=lineage) %>%
    distinct() %>%
    add_column(priority=1) %>%
    add_column(subtype="lineage")
}

build_subtype_lineage_subtype_search_index <- function(cell_expression_names) {
  cell_expression_names %>%
    select(label=lineage_subtype, content_id=lineage_subtype) %>%
    distinct() %>%
    add_column(priority=1) %>%
    add_column(subtype="lineage_subtype")
}

## Drug Search Functions ----

build_subtype_compound_search_index <- function(compound_prism_names) {
  compound_prism_names %>%
    select(label=name, content_id=name) %>%
    add_column(priority=1) %>%
    add_column(subtype="compound")
}

build_subtype_moa_search_index <- function(compound_prism_names) {
  compound_prism_names %>%
    select(label=moa, content_id=moa) %>%
    distinct() %>%
    add_column(priority=1) %>%
    add_column(subtype="moa")
}

build_subtype_metabolite_search_index <- function(compound_hmdb_names) {
  bind_rows(
    compound_hmdb_names %>%
      select(label=name, content_id=cid) %>%
      add_column(priority=1),
    compound_hmdb_names %>%
      select(label=cid, content_id=cid) %>%
      add_column(priority=1),
    # Not really sure of a good way to split the synonym into parts
   # compound_hmdb_names %>%
    #  separate_rows(synonyms) %>%
    #  select(label=synonyms, content_id=cid) %>%
    #  add_column(priority=2),
  ) %>%
    add_column(subtype="metabolite")
}


# Generate search index

build_search_index <- function() {
  # Load data used in building search
  universal_gene_summary <- arrow::read_feather("data_s3/universal_gene_summary")
  gene_pathways <- arrow::read_feather("data_s3/gene_pathways")
  cell_expression_names <- arrow::read_feather("data_s3/cell_expression_names")
  compound_prism_names <- arrow::read_feather("data_s3/compound_prism_names")
  compound_hmdb_names <- arrow::read_feather("data_s3/compound_hmdb_names")

  # Load data used in building search
  bind_rows(
    build_subtype_gene_search_index(universal_gene_summary),
    build_subtype_pathway_search_index(gene_pathways),
    build_subtype_cell_search_index(cell_expression_names),
    build_subtype_lineage_search_index(cell_expression_names),
    build_subtype_lineage_subtype_search_index(cell_expression_names),
    build_subtype_compound_search_index(compound_prism_names),
    build_subtype_moa_search_index(compound_prism_names),
    build_subtype_metabolite_search_index(compound_hmdb_names),
  )
}

search_index <- build_search_index()


arrow::write_feather(search_index, "data/search_index")
