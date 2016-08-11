#' Create a flowchart
#'
#' This function will draw a flowchart, useful for many purposes, e.g. visualizing
#' the population selection for a specific study, and many others. If n and events
#' are provided, event rates will automatically populate the boxes in the flowchart.
#'
#' @param description Typically a character vector with text for each of the boxes in the flowchart.
#' @param n Numeric vector of same length as 'description', describing number of subjects in each group.
#' @param events Numeric vector of same length, describing number of events.

#' @keywords flowchart
#' @author Daniel Lindholm
#' @export
#' @examples
#' flow_data <- data.frame(descr = c("Population", "Subset1", "Further subset"),
#'                         n = c(1000, 800, 600), events = c(80, 70 , 30))
#' with(flow_data, flowchart(descr, n, events))

flowchart <- function(description, n = NULL, events = NULL){
  library(diagram)
  #Checks
  if (is.null(n) | is.null(events)) calculate_eventrate <- FALSE
    else calculate_eventrate <- TRUE
  if (calculate_eventrate){
    if (length(description) != length(n) | length(description) != length(events))
        stop("Unequal lengths of provided variables!")
    if (!is.numeric(n)) stop("n should be numeric")
    if (!is.numeric(events)) stop("events should be numeric")
  }
  no_of_boxes <- length(description)
  M <- matrix(c(rep(c(rep(0, no_of_boxes),""), no_of_boxes - 1), 0), no_of_boxes, byrow = T)
  if (calculate_eventrate) NAME <- paste0(description, "\nN = ", n, ", Events = ", events,
                                          " (", sprintf("%3.1f", events/n*100), "%)")
  else NAME <- description
  plotmat(M, pos = rep(1, times = no_of_boxes),
                       name = NAME,
                       curve = 0, box.type = "round", shadow.size = 0,
                       box.size = 0.35, box.prop = 0.2, arr.pos = 0.5)
}
