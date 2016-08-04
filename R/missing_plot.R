#' Visualize missing values in a data.frame
#' 
#' This function looks at missing values in a data.frame in relation to a specific
#' group and creates a ggplot2 object. Quite helpful to identify missing not-at-random.
#' 
#' @param data a data.frame
#' @param group_variable a character string naming the column of the data.frame that 
#' contains information on grouping (e.g. treatment group, etc)
#' @param fill_color the plot will show a gradient from white to this color.
#' By default, steelblue will be used.
#' @keywords missing_plot, missing data
#' @export
#' @examples
#' 
#' missing_plot(airquality, "Month")
#' 
#' missing_plot(airquality, "Month", "Orange")

missing_plot <- function(data, group_variable, fill_color = "steelblue"){
  if(!is.data.frame(data)) stop("Not a data frame")
  if(!is.character(group_variable)) stop("group_variable not of class character")
  
  m_table <- missing_table(data, group_variable, round_values = F, show_total = F,
                           show_non_missing = T)
  ind <- rownames(m_table) == group_variable
  m_table <- m_table[!ind,]
  m_table$variable_name <- rownames(m_table)
  require(ggplot2)
  require(reshape2)
  
  m_table <- melt(m_table)
  m_table <- within(m_table,
                    variable_name <- ordered(variable_name,
                                    levels = rev(sort(unique(variable_name)))))

  p <- ggplot(m_table, aes(variable, variable_name)) + 
    geom_tile(aes(fill = value), color = "black") +
    scale_fill_gradient(low="white", high = fill_color) +
    labs(x = group_variable, y = "Variable", fill = "% missing")+
    theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
 return(p) 
}