#' Format numbers
#'
#' This function will round numbers according to APA7 guidelines with no leading zero
#' @param No arguments necessary
#' @keywords round
#' @export
#' @examples
#' format_num(0.053634)


format_nozero <- function(x, digits = 3) {
  if (dim(as.data.frame(x))[1] > 1 & dim(as.data.frame(x))[2] > 1) {
    apply(x, c(1,2), format2)
  } else {sapply(x, format2, digit_no = digits)}
}


format2 <- function(x, digit_no = 3) {
  if (is.na(x) | x == "") {
    ""
  } else if (x == 0) {
    round(x, digits = 0)
  } else if (as.numeric(x) < 1 & as.numeric(x) > 0) {
    substr(round(as.numeric(x), digits = digit_no),2,nchar(round(as.numeric(x), digits = digit_no)))
  } else if (as.numeric(x) < 0 & as.numeric(x) > -1) {
    paste0("-",substr(round(as.numeric(x), digits = digit_no),3,nchar(round(as.numeric(x), digits = digit_no))))
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
