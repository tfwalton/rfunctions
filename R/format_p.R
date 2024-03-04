#' Format p-values
#'
#' This function will print p-values according to APA-7 guidelines
#' @param no arguments necessary
#' @keywords p-value
#' @export
#' @examples
#' values <- c(0.2, 0.035, 0.00021)
#' vapply(values, format_p, character(1))


# format p-values
format_p <- function(x){
  if (is.na(x)) {
    ("")
  } else if (x < 0.001) {
    ("p < .001")
  } else {
    stringr::str_remove((round(x, digits = 3)), "^0+")
  }
}


