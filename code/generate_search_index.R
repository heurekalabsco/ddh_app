library(tidyverse)

# key, alias, alias2, content_id, 
universal_gene_summary <- arrow::read_feather("data_s3/universal_gene_summary")

build_approved_symbol_search_index <- function(universal_gene_summary) {
  universal_gene_summary %>%
    select(label=approved_symbol, content_id=approved_symbol) %>%
    add_column(priority=1)
}

build_approved_name_search_index <- function(universal_gene_summary) {
  universal_gene_summary %>%
    select(label=approved_name, content_id=approved_symbol) %>%
    add_column(priority=2)
}

build_aka_search_index <- function(universal_gene_summary) {
  universal_gene_summary %>% 
    separate_rows(aka) %>%
    select(label=aka, content_id=approved_symbol) %>%
    add_column(priority=3)
}

build_gene_search_index <- function(universal_gene_summary) {
  bind_rows(
    build_approved_symbol_search_index(universal_gene_summary),
    build_approved_name_search_index(universal_gene_summary),
    build_aka_search_index(universal_gene_summary)
  ) %>%
    add_column(subtype="gene")
}

word_starts_with_regex <- function(query_str) {
  regex(paste0('\\b', query_str), ignore_case=TRUE)
}

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
    mutate(rank=calc_rank(query_str, label, priority))
}

search_index <- build_gene_search_index(universal_gene_summary)
print(search(search_index=search_index, query_str="A1BG"))
print(search(search_index=search_index, query_str="Alpha-1-b"))

arrow::write_feather(search_index, "data/search_index")


arrow::write_feather(search_details, "data/search_details")