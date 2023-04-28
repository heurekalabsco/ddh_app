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
