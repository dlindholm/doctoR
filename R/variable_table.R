#' Create variable table
#'
#' Create a table in the form of a data.frame, which contains information about
#' variable name, variable description (as defined using the label function from
#' the Hmisc package), and class of variable. This could then of course be used together with
#' the latex function (in Hmisc) or kable (in knitr) to put the table into reports etc.
#'
#' @param df a data.frame
#' @author Daniel Lindholm
#' @export
#' @examples
#' library(Hmisc)
#' getHdata(plasma)
#' variable_table(plasma)

variable_table <- function(df){
  if(!is.data.frame(df)) stop("Not a data.frame!")
  var_names <- names(df)
  var_descriptions <- label(df)
  df <- clear_labels(df)
  var_classes <- lapply(df, function(i) {
                            if (class(i) == "factor")
                              return (paste0("factor (levels: ",
                                             paste(levels(i), collapse = ", "),")"))
                             else return(class(i))
  })
  x <- cbind(var_names, var_descriptions, var_classes)
  rownames(x) <- NULL
  colnames(x) <- c("Variable", "Description", "Class")
  return(as.data.frame(x))
}
