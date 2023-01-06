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
make_validate <- function(object_names){
  if(is.null(object_names)){ 
    return(NULL)
  }
  make_dataset <- function(object_name){
    object <- eval(parse(text = object_name))
    single_dataset <-
      object %>%
      dplyr::distinct(name) %>%
      dplyr::filter(!is.na(name))
    return(single_dataset)
  }
  validate_datasets <-
    object_names %>%
    purrr::map_dfr(make_dataset) %>%
    dplyr::distinct(name) %>%
    dplyr::pull(name)
  return(validate_datasets)
}
