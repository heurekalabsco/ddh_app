# conditionalPanel that only shows field is not 0
# using array bracket notation to be compatible with namespaces
notZeroConditionalPanel <- function(fieldname, ...) {
  condition_str <- paste0("input['", fieldname, "'] != 0")
  conditionalPanel(condition = condition_str, ...)  
}

# dynamicSwitch <- function(fieldname, ...) {
#   prettySwitch(
#     inputId = fieldname,
#     value = FALSE,
#     label = "Dynamic",
#     fill = TRUE, 
#     status = "primary")
# }

# SPINNERS ----------
withSpinnerColor <- function(ui_element, plot_type, ...) { #plot type = "gene", "cell", "protein", "compound"
  withSpinner(ui_element,
              type = 3, 
              color = ddh_colors(plot_type), #use ddh_colors fun to give back hex code
              color.background = "#FFFFFF")
}

#' Function to make a validation dataset
#'
#' @param object_names Character vector, can be greater than 1, of file names to get
#'
#' @importFrom magrittr %>%
#'
#' @export
#' @examples
#' make_validate("ROCK1")
#' make_validate(c("ROCK1", "ROCK2"))
#' \dontrun{
#' make_validate("ROCK1")
#' }
make_validate <- function(object_names = NULL){
  if(is.null(object_names)){ 
    return(NULL)
  }
  make_dataset <- function(object_name){
    object <- ddh::get_content(object_name)
    single_dataset <-
      object %>%
      dplyr::distinct(data_set) %>%
      dplyr::filter(!is.na(data_set))
    return(single_dataset)
  }
  validate_datasets <-
    object_names %>%
    purrr::map(make_dataset) %>%
    purrr::list_rbind() %>% 
    dplyr::distinct(data_set) %>%
    dplyr::pull(data_set)
  return(validate_datasets)
}

# NAMES ----------
clean_pathway_names <- function(pathway_name = "C2_SIG_CHEMOTAXIS"){
  clean_name <- 
    pathway_name %>% 
    stringr::str_extract(pattern = "(?<=_).+") %>% 
    stringr::str_replace_all(pattern = "SIG_", replacement = "SIGNATURE_") %>% 
    stringr::str_replace_all(pattern = "\\_", replacement = " ") %>% 
    stringr::str_to_title()
  return(clean_name)
}

clean_pathway_descriptions <- function(pathway_description = "The process in which amyloid-beta is removed from extracellular brain regions by mechanisms involving cell surface receptors. [GOC:aruk, GOC:bc, GOC:BHF, PMID:18289866, PMID:19098903, PMID:26005850]"){
  clean_desc <- 
    pathway_description %>% 
    stringr::str_remove(pattern = "(\\s)?\\[[^\\]]*\\]")
  return(clean_desc)
}

#INTERNAL LINKS----
#' Internal link
#'
#' @importFrom magrittr %>%
#'
#' @export
internal_link <- function(query, linkout_img=FALSE) {
  hrefr <- function(x, linkout_img) {
    paste0('<a href="?show=gene&query=',
           x,
           '" target="_blank">',
           dplyr::if_else(linkout_img==TRUE, '<img src="link out_25.png", width="10", height="10">', x),
           '</a>')
  }
  if (stringr::str_detect(query, ",") == TRUE){ #code for list
    string_vec <- unlist(stringr::str_split(query, ", "))
    string_vec_link <- purrr::map_chr(string_vec, hrefr, linkout_img = FALSE)
    query_link <- stringr::str_c(string_vec_link, collapse = ", ")
    
  } else if (stringr::str_detect(query, "^[:digit:]{7}") == TRUE) { #regex for GO, used in shiny_tables browsePathwaysPanelServer
    query_link <- paste0('<a href="?show=pathway&go=',
                         query,
                         '">',
                         query,
                         '</a>')
  } else {
    query_link <- hrefr(query, linkout_img)
  }
  return(query_link)
}

#' Internal link cell
#'
#' @importFrom magrittr %>%
#'
#' @export
internal_link_cell <- function(query) {
  query_link <-
    paste0('<a href="?show=cell&cell_line=',
           query,
           '" target="_blank">',
           query,
           '</a>')
  
  return(query_link)
}

#' PubMed Linkr
#'
#' @importFrom magrittr %>%
#'
#' @export
pubmed_linkr <- function(query, number_only = FALSE) { #add for make_pubmed_table()
  if (stringr::str_detect(query, "PubMed:[:digit:]{1,9}|PubMed=[:digit:]{1,9}") == TRUE) {
    num <- stringr::str_extract(query, "[:digit:]{1,9}")
    link <- paste0("https://pubmed.ncbi.nlm.nih.gov/", num)
    href_link <- paste0('<a href="',
                        link,
                        '" target="_blank">', #open in new page
                        num,
                        '</a>')
    query_link <- stringr::str_replace(query, "PubMed:[:digit:]{1,9}", href_link)
    return(query_link)
  } else if (stringr::str_detect(query, "[:digit:]{1,9}") == TRUE && number_only == TRUE) {
    num <- query
    link <- paste0("https://pubmed.ncbi.nlm.nih.gov/", num)
    href_link <- paste0('<a href="',
                        link,
                        '" target="_blank">', #open in new page
                        num,
                        '</a>')
    query_link <- stringr::str_replace(query, "[:digit:]{1,9}", href_link)
    return(query_link)
  } else {
    return(query)
  }
}

#' PMC Linkr
#'
#' @importFrom magrittr %>%
#'
#' @export
pmc_linkr <- function(query) { #add for make_pubmed_table()
  link <- paste0("https://www.ncbi.nlm.nih.gov/pmc/articles/", query)
  href_link <- paste0('<a href="',
                      link,
                      '" target="_blank">', #open in new page
                      query,
                      '</a>')
  query_link <- stringr::str_replace(query, "PMC[:digit:]{1,9}", href_link)
  return(query_link)
}

#' Uniprot Linkr
#'
#' @importFrom magrittr %>%
#'
#' @export
uniprot_linkr <- function(query) {
  if (stringr::str_detect(query, "UniProtKB:[:alnum:]{1,6}") == TRUE) {
    num <- stringr::str_extract(query, "UniProtKB:[:alnum:]{1,6}") %>%
      stringr::str_split(pattern = ":")  %>%
      purrr::pluck(1, 2)
    link <- paste0("https://www.uniprot.org/uniprot/", num)
    href_link <- paste0('<a href="',
                        link,
                        '" target="_blank">', #open in new page
                        num,
                        '</a>')
    query_link <- stringr::str_replace(query, "UniProtKB:[:alnum:]{1,6}", href_link)
    return(query_link)
  } else {
    return(query)
  }
}

#' Uniprot Linkr 2
#'
#' @importFrom magrittr %>%
#'
#' @export
uniprot_linkr2 <- function(query) {
  link <- paste0("https://www.uniprot.org/uniprot/", query)
  query_link <- paste0('<a href="',
                       link,
                       '" target="_blank">',
                       query,
                       '</a>')
  return(query_link)
}

#' PDB Linkr
#'
#' @importFrom magrittr %>%
#'
#' @export
pdb_linkr <- function(query) {
  link <- paste0("https://www.rcsb.org/structure/", query)
  query_link <- paste0('<a href="',
                       link,
                       '" target="_blank">',
                       query,
                       '</a>')
  return(query_link)
}

#' Eco Zapr
#'
#' @importFrom magrittr %>%
#'
#' @export
eco_zapr <- function(query) {
  if (stringr::str_detect(query, "ECO:[:digit:]{1,7}") == TRUE) {
    query_zap <- stringr::str_remove_all(query, "ECO:[:digit:]{1,7}\\|")
    query_zap <- stringr::str_remove_all(query_zap, "ECO:[:digit:]{1,7}")
    return(query_zap)
  }
  else {
    return(query)
  }
}

#' Bracketr
#'
#' @importFrom magrittr %>%
#'
#' @export
bracketr <- function(query) {
  if (stringr::str_detect(query, "\\{") == TRUE) {
    query_plus <- stringr::str_replace(query, "\\{", "\\{Curated links: ")
    return(query_plus)
  }
  else {
    return(query)
  }
}

#' Gene Linkr
#'
#' @importFrom magrittr %>%
#'
#' @export
gene_linkr <- function(summary_table = universal_gene_summary, query) { #best to use with lit_linkr, which takes a char_vec
  query_clean <- stringr::str_extract(query, "[^[:punct:]]+") #anything but punct, one or more
  if ((query_clean %in% summary_table$approved_symbol) == TRUE) {
    query_link <- paste0('<a href="?show=gene&query=',
                         query_clean,
                         '" target="_blank">',
                         query,
                         '</a>')
    return(query_link)
    # NOT YET WORKING; AKA SEARCH EITHER RETURNS NO MATCHES B/C QUERY =! MULTIPLE GENES IN STR OR QUERY == TOO MANY GENES IN UNLISTED STR
    # } else if (str_detect(unlist(str_split(gene_summary$aka, ", ")), query_clean) == TRUE) { #search unlisted AKA
    #   aka_num <- str_which(summary_table$aka, query_clean)
    #   query_aka <- summary_table[aka_num, 1]
    #   query_link <- paste0('<a href="?show=gene&query=',
    #                        query_aka,
    #                        '" target="_blank">',
    #                        query,
    #                        '</a>')
    #   return(query_link)
  } else {
    return(query)
  }
}

#' Lit Linkr
#'
#' @importFrom magrittr %>%
#'
#' @export
lit_linkr <- function(summary_table = universal_gene_summary,
                      lit_string) { #split into char_vec, map function, glue back together, and then treat as htmlOutput in shiny_text
  lit_string <- stringr::str_replace_all(lit_string, "PubMed ", "PubMed:") #if space, then str_split breaks
  lit_string <- stringr::str_replace_all(lit_string, "PubMed=", "PubMed:") #for CC
  lit_vec <- unlist(stringr::str_split(lit_string, pattern = " "))
  lit_vec <- purrr::map_chr(lit_vec, eco_zapr) #remove ECO
  lit_links <-
    lit_vec %>%
    purrr::map_chr(pubmed_linkr) %>% #make pubmed links
    purrr::map_chr(uniprot_linkr) %>%  #make uniprot links
    purrr::map_chr(bracketr) %>% #annotate bracket
    purrr::map_chr(gene_linkr, summary_table = universal_gene_summary) #add some internal links
  lit_string_link <- stringr::str_c(lit_links, collapse = " ")
  return(lit_string_link)
}

#' Drug Linkr
#'
#'
#' @importFrom magrittr %>%
#'
#' @export
drug_linkr <- function(query) {
  if ((query %in% compound_prism_names$name) == TRUE) {
    query_link <- paste0('<a href="?show=compound&query=',
                         query,
                         '" target="_blank">',
                         query,
                         '</a>')
    return(query_link)
  } else {
    return(query)
  }
}

#' MOA Linkr
#'
#'
#' @importFrom magrittr %>%
#'
#' @export
moa_linkr <- function(query) {
  if ((query %in% compound_prism_names$moa) == TRUE) {
    query_link <- paste0('<a href="?show=moa&query=',
                         stringr::str_replace_all(query, " ", "%20"),
                         '" target="_blank">',
                         stringr::str_to_title(query),
                         '</a>')
    return(query_link)
  } else {
    return(query)
  }
}

#' Metabolite Linkr
#'
#'
#' @importFrom magrittr %>%
#'
#' @export
metabolite_linkr <- function(query) {
  if ((query %in% compound_hmdb_names$name) == TRUE) {
    query_link <- paste0('<a href="?show=compound&query=', #fix me
                         query,
                         '" target="_blank">',
                         query,
                         '</a>')
    return(query_link)
  } else {
    return(query)
  }
}

#' Cell Linkr
#'
#' Make cell, lineage, list linkr with type var
#'
#' @importFrom magrittr %>%
#'
#' @export
cell_linkr <- function(query, 
                       type,
                       data_search_index = search_index) {
  type_url <- switch (type,
                      cell = "?show=cell&cell_line=",
                      lineage = "?show=lineage&query=",
                      lineage_subtype = "?show=lineage_subtype&query="
  )
  if (query %in% data_search_index$label) {
    query_no_space <- stringr::str_replace_all(query, " ", "%20")
    query_title <- query #stringr::str_to_title(query)
    query_link <- glue::glue('<a href="{type_url}{query_no_space}" target="_blank">{query_title}</a>')
    return(query_link)
  } else {
    return(query)
  }
}

