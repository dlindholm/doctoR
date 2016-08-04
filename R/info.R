#' Describe selection performed with dplyr functions
#'
#' When selecting an analysis population using the dplyr
#' functions (e.g. filter), it could be useful to describe
#' each step of the way. This function allows text annotation and
#' provides number of observations for each step.
#' 
#' Important! The descr_df should be non-existent in the beginning of the selection
#' procedures! 
#' @param data A data.frame.
#' @param descr_df A character string specifying where to store the descriptive information.
#' @param descr_text A character string describing the population.
#' @param event A character string indicating which column of the data.frame contains information on
#' whether an event occured. If this argument is specified, the number of events 
#' is saved to descriptive information.
#' @keywords selection, describe
#' @export
#' @examples
#' library(dplyr)
#' aq <- airquality
#' 
#' aq %>% info("descr", "Full dataset") %>% 
#' filter(Temp < 60) %>% info("descr", "Temperature below 60")
#' 
#' print(descr)

info <- function(data, descr_df=character(), descr_text=character(), event=NULL){
  if(!is.null(event)) tmp_df <- data.frame(descr_text = descr_text, 
                                           n = nrow(data), 
                                           events = sum(data[,event]))
  else tmp_df <- data.frame(descr_text = descr_text, n = nrow(data))
  
  if (!exists(descr_df)){
    define_df_call <- call("<<-", as.name(descr_df), quote(tmp_df))
  }
  
  if (exists(descr_df)){
    assign("old_df", eval(as.name(descr_df)))
    new_df <- rbind(old_df, tmp_df)
    define_df_call <- call("<<-", as.name(descr_df), quote(new_df))
  }
  
  eval(define_df_call)
  
  return(data)
}