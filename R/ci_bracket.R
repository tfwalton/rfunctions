#' Format confidence intervals
#'
#' This function will combine columns from output of the confint function into a single formatted column
#' @param no arguments necessary
#' @keywords confidence interval
#' @export
#' @examples
#' lm <- lm(disp~hp, data = mtcars)
#' ci <- confint(lm)
#' ci_formatted <- ci_bracket(format_num(ci))
#' ci_formatted

# Place confidence intervals into square brackets within the same column
ci_bracket <- function(x) {
  paste0("[", x[,1]," , ", x[,2], "]")
}
