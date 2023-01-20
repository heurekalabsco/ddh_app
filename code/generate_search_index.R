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
  return (str_length(query_str) / str_length(value)) / priority
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
    add_column(priority=1)
}

build_gene_search_index <- function(universal_gene_summary) {
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


build_pathway_search_index <-function(gene_pathways) {
  bind_rows(
    build_pathway_name_search_index(gene_pathways),
    build_pathway_go_search_index(gene_pathways)
  ) %>%
    add_column(subtype="pathway")
}

# Generate search index

build_search_index <- function(universal_gene_summary, gene_pathways) {
  bind_rows(
    build_gene_search_index(universal_gene_summary),
    build_pathway_search_index(gene_pathways)
  )
}

universal_gene_summary <- arrow::read_feather("data_s3/universal_gene_summary")
gene_pathways <- arrow::read_feather("data_s3/gene_pathways")
search_index <- build_search_index(universal_gene_summary, gene_pathways)
print(search(search_index=search_index, query_str="A1BG"))
print(search(search_index=search_index, query_str="Alpha-1-b"))
print(search(search_index=search_index, query_str="cholesterol"))
print(search(search_index=search_index, query_str="0033344"))
print(search_custom_list(search_index=search_index, query_str="ROCK1,ROCK2,0033344,BOBX"))

arrow::write_feather(search_index, "data/search_index")

#search_index <- arrow::read_feather("data/search_index")