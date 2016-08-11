#' Removes Hmisc labels from a dataframe
#'
#' Some functions do not work when variables in a data.frame are having the 
#' "Labelled" class appended to them. This removes labels from a dataframe.
#' 
#' @param x A data.frame.

#' @keywords remove labels
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



clear_labels <- function(x) {
  if(is.list(x)) {
    for(i in 1 : length(x)) class(x[[i]]) <- setdiff(class(x[[i]]), 'labelled') 
    for(i in 1 : length(x)) attr(x[[i]],"label") <- NULL
  }
  else {
    class(x) <- setdiff(class(x), "labelled")
    attr(x, "label") <- NULL
  }
  return(x)
}