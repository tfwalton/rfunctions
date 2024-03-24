#' Format p-values
#'
#' This function will print p-values according to APA-7 guidelines
#' @param suppress.p Select whether to suppress the "p" in the function output (FALSE by default)
#' @keywords p-value
#' @export
#' @examples
#' values <- c(0.2, 0.035, 0.00021)
#' format_p(values, suppress.p = TRUE)

format_p <- function(x, suppress.p = FALSE) {
  if ((ncol(as.data.frame(x)) > 1 & nrow(as.data.frame(x)) > 1) == TRUE) {
    apply(x, c(1,2), format1, suppress.p = suppress.p)
  } else {sapply(x, format1, suppress.p = suppress.p)}
}

# format p-values
format1 <- function(x, suppress.p = FALSE){
  if (is.na(x)) {
    ("")
  } else if (as.numeric(x) < 0.001) {
      if(suppress.p == FALSE) {
        ("p < .001")
      } else {
        ("< .001")
      }
    } else {
    stringr::str_remove((round(as.numeric(x), digits = 3)), "^0+")
  }
}


