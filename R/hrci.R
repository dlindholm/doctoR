#' Combine hazard ratio and 95 percent CI to one string
#' 
#' Useful for combining HR and 95CI information from a dataframe used for 
#' drawing a forestplot.
#' 
#' @param f_data a dataframe containing information for the forestplot
#' @param hr hazard ratio; hr by default
#' @param lci lower confidence interval; lci by default
#' @param uci upper confidence interval; uci by default
#' @keywords forestplot
#' @export
#' @examples 
#' Create some test data
#' hr <- seq(0.80, 1.1, by = 0.05)
#' lci <- hr - 0.1
#' uci <- hr + 0.1
#' left_text<- paste0("Left text no ", 1:7)
#' forest_data <- data.frame(left_text,lci,hr,uci)
#' 
#' forest_data$right_text <- hrci(forest_data)
#' forestplot(forest_data, hr, lci, uci, left_text, right_text) 
 
hrci <- function(f_data, hr = hr, lci = lci, uci = uci){
  arguments <- as.list(match.call())
  lci <- eval(arguments$lci, f_data)
  uci <- eval(arguments$uci, f_data)
  hr <- eval(arguments$hr, f_data)
  
  result <- paste0(
                   format(round(as.numeric(f_data$hr), 2), nsmall = 2),
                   " (",
                   format(round(as.numeric(f_data$lci), 2), nsmall = 2),
                   " - ",
                   format(round(as.numeric(f_data$uci), 2), nsmall = 2),
                   ")")
  return(result)
}