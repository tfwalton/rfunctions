#' Format numbers
#'
#' This function will round numbers according to APA7 guidelines with no leading zero
#' @param No arguments necessary
#' @keywords round
#' @export
#' @examples
#' format_num(0.053634)


format_nozero <- function(x) {
  if (dim(as.data.frame(x))[1] > 1 & dim(as.data.frame(x))[2] > 1) {
    apply(x, c(1,2), format2)
  } else {sapply(x, format2)}
}

# express numbers in scientific notation or round where necessary


format2 <- function(x) {
  if (is.na(x) | x == "") {
    ""
  } else if(x == 0) {
    round(x, digits = 0)
  } else if (abs(as.numeric(x)) < 0.001) {
    formatC(x, format = "e", digits = 0)
  }  else if (abs(as.numeric(x)) < 1) {
    substr(round(as.numeric(unlist(x)), digits = 3),2,nchar(round(as.numeric(unlist(x)), digits = 3)))
  } else {
    stop("Error: input must be a number < 1")
  }
}
