#' Format numbers
#'
#' This function will round numbers according to APA7 guidelines and express as scientific notation where necessary
#' @param No arguments necessary
#' @keywords round
#' @export
#' @examples
#' mat <- matrix(data = c(0.0025, 37.812, 3002.79, 0.021, 54.0422222, 0.000000546), ncol = 2, nrow = 3)
#' format_num(mat)


format_num <- function(x) {
  if (dim(as.data.frame(x))[1] > 1 & dim(as.data.frame(x))[2] > 1) {
  apply(x, c(1,2), format1)
  } else {sapply(x, format1)}
}

# express numbers in scientific notation or round where necessary


format1 <- function(x, digit_no = 3) {
  if (is.na(x)) {
    ""
  } else if(x == 0) {
    round(x, digits = 0)
  } else if (abs(x) < 0.001) {
    formatC(x, format = "e", digits = 0)
  } else if (abs(x) < 1) {
    sub("\\.?0+$", "", format(round(x, digits = digit_no)))
  } else if (abs(x) < 10) {
    sub("\\.?0+$", "", format(round(x, digits = 2)))
  } else if (abs(x) < 100) {
    round(x, digits = 1)
  } else if (abs(x) < 1000) {
    round(x, digits = 0)
  } else {
    round(x, digits = 0)
  }
}
