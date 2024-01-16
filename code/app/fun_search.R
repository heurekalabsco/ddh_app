# These functions are for searching and retrieving data that will fill the queryString and reactives()

# SEARCH -----
word_starts_with_regex <- function(query_str) {
  regex(paste0('\\b', query_str), ignore_case=TRUE)
}

split_query_str <- function(query_str) {
  c(str_split(query_str, "\\s*,\\s*", simplify = TRUE))
}

calc_rank <- function(query_str, value, priority) {
  # query_str must be within value (though case may be different)
  # if both items are the same length it is a perfect match so returns 1
  # otherwise returns a value comparing the lengths of the strings
  return (str_length(query_str) / str_length(value)) / priority
}

is_multi_query_search <- function(query_str) {
  grepl(",", query_str)
}

single_query_search <- function(search_index, query_str) {
  word_starts_with_query_str <- word_starts_with_regex(query_str)
  search_index %>%
    filter(str_detect(label, word_starts_with_query_str)) %>%
    mutate(rank=calc_rank(query_str, label, priority)) %>%
    distinct(content_id, .keep_all = TRUE)
}

multi_query_search <- function(search_index, multi_items) {
  search_subtypes <- c("gene", "compound", "cell")
  search_index %>%
    filter(toupper(content_id) %in% toupper(multi_items), label==content_id, subtype %in% search_subtypes) %>%
    group_by(subtype) %>%
    nest()
}

multi_query_get_unknown <- function(multi_items, known_items) {
  setdiff(toupper(multi_items), toupper(known_items))
}

search_query <- function(search_index, query_str, limit=100) {
  if (is_multi_query_search(query_str)) {
    multi_items <- split_query_str(query_str)
    rows <- multi_query_search(search_index, multi_items)
    list(
      rows=rows,
      multi_query=TRUE,
      multi_items=multi_items
    )
  } else {
    rows <- single_query_search(search_index, query_str) %>% arrange(desc(rank)) %>% head(limit)
    list(
      rows=rows,
      multi_query=FALSE,
      multi_items=NULL
    )
  }
}

#print(search_query(search_index=search_index, query_str="A1BG"))
#print(search_query(search_index=search_index, query_str="Alpha-1-b"))
#print(search_query(search_index=search_index, query_str="cholesterol"))
#print(search_query(search_index=search_index, query_str="0033344"))
#print(search_query(search_index=search_index, query_str="ROCK1,ROCK2,0033344,BOBX"))
#print(search_query(search_index=search_index, query_str="SALE"))
#print(search_query(search_index=search_index, query_str="SALE,NCO2"))
#print(search_query(search_index=search_index, query_str="Kidney"))
#print(search_query(search_index=search_index, query_str="Fibroblast Soft Tissue"))
#print(search_query(search_index=search_index, query_str="Fibroblast"))
#print(search_query(search_index=search_index, query_str="cloranolol"))
#print(search_query(search_index=search_index, query_str="immunostimulant"))
#print(search_query(search_index=search_index, query_str="BRCA2"))

