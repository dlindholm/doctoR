#' Define Hmisc labels from a list
#'
#' Apply labels to several variables simultaneously from a definition found in a
#' list object.
#'
#' @param x A data.frame.
#' @param labels A list containing label information, e.g. var = "Variable name"

#' @export
#' @examples
#' library(Hmisc)
#' getHdata(plasma)
#' #Look-up labels:
#' label(plasma)
#'
#' #Remove labels:
#' clean_plasma <- clear_labels(plasma)
#' label(clean_plasma)
#'
#' #Define variable labels from a list:
#' var_names <- list(age = "Age in years", smokstat = "Smoking status")
#' named_plasma <- define_labels(clean_plasma, var_names)
#' label(named_plasma)



define_labels <- function(x, labels) {
  if(!is.list(labels)) stop("Not a list")
  if(!is.data.frame(x)) stop("Not a dataframe")
  require(Hmisc)
  label(x[names(labels)]) <- labels
  return(x)
}
