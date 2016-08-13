#' Wrapper to write dataframe to disk as a csv and use chunk name
#'
#' This function is a wrapper for write.csv. It will write a dataframe to disk.
#' If no table_name is provided and the function is called from within a knitr
#' code chunk, the table_name variable will take the same value as the chunk label.
#' By default, the directory is a 'tables' subdirectory to the working directory,
#' which suits my directory structure nicely. Could of course be customized.
#'
#' @param df a dataframe
#' @param table_name the name of the file to write. By default, it will be same as
#' the label of the chunk from which it was called
#' @param directory the directory in which the file will be written
#' @param row_names include rownames in the .csv-file? FALSE by default.
#' @export
#' @author Daniel Lindholm

save_table <- function(df, table_name = opts_current$get("label"), directory = "tables", row_names = FALSE){
  library(knitr)
  library(readr)
   #Checks
  if(!is.data.frame(df)) stop("You must provide a dataframe!")
  if(!is.character(table_name)) stop("table_name: not a character!")
  if(!is.character(directory)) stop("directory: not a character!")
  write.csv(df, file.path(directory, paste0(table_name, ".csv")), row.names = row_names)
  }
