#' Remove attributes from a data.frame
#' 
#' When loading e.g. SAS datasets using the haven package also loads irritating attributes like
#' SAS labels. This makes dplyr functions unhappy. The present function removes these attributes.
#'
#' @param df a dataframe to fix
#' @export
#' 
metafix <- function(df) { 
  df[] <- lapply(df, function(x) { 
    attributes(x) <- list() 
    x 
  }) 
  df 
}