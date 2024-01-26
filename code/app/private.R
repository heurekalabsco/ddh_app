private <- function(x, publicValue=NULL) {
  if (privateMode == TRUE) { x } else { publicValue }
}

private_message_html <- "This feature is only available to premium subscribers. If you want to subscribe to support this project and receive access, go to <a href='https://www.datadrivenhypothesis.com' target='_blank'>www.datadrivenhypothesis.com</a>."
private_message_md <- "This feature is only available to premium subscribers. If you want to subscribe to support this project and receive access, go to [datadrivenhypothesis.com](https://www.datadrivenhypothesis.com)."

private_searchbox <- "genes, cells, or compounds"

private_msg <- function() {
  if (privateMode == FALSE) {
    shiny::HTML(private_message_html) 
  }
}
