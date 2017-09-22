
#' Detect if variable is a summary variable. Summary variable name has less
#' dots in the prefix than immediately following variable.
#' @param x Integer vector. Number of dots prefixed to variable name.
#' @return Logical vector. Summary variables are TRUE.
is_summary <- function(x) {

  vapply(seq_along(x), function(i) {
    pair <- x[c(i, i+1)]

    start <- all(pair[!is.na(pair)]==0)

    if(start) return(TRUE)

    is_summary <- diff(pair)>1

    if(is.na(is_summary)) return(FALSE)

    is_summary
  }, logical(1))
}

#' Identify summary variables using dots prefixes in variable names.
#' @param var Character or factor vector. Variable names from from Estonian
#' Health Statistics and Health Research database.
#' @examples
#' # "Kõik põhjused (A00-Y89)" includes all following variables and
#' # "Nakkus- ja parasiithaigused (A00-B99)" includes all following variables,
#' # prefixed with "..".
#' vars <- c("Kõik põhjused (A00-Y89)",  "Nakkus- ja parasiithaigused (A00-B99)",
#'          "..tuberkuloos (A15-A19, B90)", "..meningokokknakkus (A39)",
#'          "..sepsis (A40-A41)", "..viirushepatiit (B15-B19)")
#' sumvars <- summary_var(vars)
#' @import stringr
#' @export
summary_var <- function(var){
  dots <- stringr::str_extract(var, "^[[:punct:]]*")
  n_dots <- stringr::str_length(dots)
  is_summary(n_dots)
}
