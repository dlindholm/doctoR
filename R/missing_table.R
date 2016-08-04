#' Create table showing missing values
#'
#' A tool to assess missingness in a dataset. This can be expanded to look into
#' specific groups of the data. Returns a data.frame.
#' 
#' @param data A data.frame.
#' @param group_variable A character string. If missing values should be assessed 
#' in relation to a specific group, this argument specifies the name of the column
#' which contains grouping information.
#' @param round_values Should the numbers be rounded?
#' @param show_total When looking at a table of missing values according to group,
#' should missingness in the whole population also be showed?
#' @param show_non_missing Should information about missingness be shown for all
#' variables in the data sets (even those with no missing values)?
#' @keywords missing_table
#' @export
#' @examples
#' missing_table(airquality)
#' 
#' missing_table(airquality, show_non_missing=FALSE)
#' 
#' missing_table(airquality, "Month", show_total = TRUE)


missing_table <- function (data, group_variable = NULL, 
                           round_values = TRUE,
                           show_total = FALSE,
                           show_non_missing = TRUE){
  
  if(!is.data.frame(data)) stop("Not a data frame")
  if(is.null(group_variable)){
    m_values <- sapply(data, function(x) sum(is.na(x)))
    m_values <- m_values[order(m_values, decreasing = TRUE)]
    if (!show_non_missing) m_values <- m_values[m_values>0]
    m_values_perc <- 100*m_values/nrow(data)
    m_table <- as.data.frame(cbind(m_values, m_values_perc))
    names(m_table) <- c("Number of missing values", "% missing values")
    if(round_values) return(round(m_table, 2)) else return(m_table)
  }
  else{
    if(!is.character(group_variable)) stop("group_variable not of class character")
    var_levels <- levels(as.factor(as.data.frame(data)[,group_variable]))  #as.data.frame in order to work with dplyr
    var_levels <- var_levels[order(var_levels)]
    m_values <- sapply(data, function(x) 100*sum(is.na(x))/nrow(data))
    m_table <- as.data.frame(m_values)
    
    for(i in 1:length(var_levels)){
      data_subset <- data[data[,group_variable] == var_levels[i],]
      m_values <- sapply(data_subset, function(x) sum(is.na(x)))
      m_values <- 100*m_values/nrow(data_subset)
      m_table <- cbind(m_table, m_values)
    }
    
    names(m_table) <- c("% missing - Total population", var_levels)
    m_table <- m_table[order(m_table[1], decreasing = T), ]
    if (!show_non_missing) m_table <- m_table[rowSums(m_table) != 0, ]
    if (round_values) m_table <- round(m_table, 2)
    if (show_total) return(m_table)
    else return(m_table[-1])
  }
}