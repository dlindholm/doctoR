#' Draw a flowchart of study population selection
#'
#' When using the dplyr package and the info function from the current package,
#' the resulting population selection table can be used to draw a flowchart with
#' the present function
#' @param description A description table created with the describe function.
#' @keywords selection, describe
#' @export
#' @examples
#' library(dplyr)
#' library(datasets)
#' # A simple example
#' aq_subset <- airquality %>% 
#'                info("descr", "Full dataset") %>% 
#'                filter(Temp < 60) %>% 
#'                info("descr", "Temperature below 60")
#' selection_flowchart(descr)
#' 
#' # An example with Event information as well
#' library(survival)
#' heart_subset <- heart %>% 
#'                    info("descr2", "Full study population", "event") %>% 
#'                    filter(transplant == 1) %>% 
#'                    info("descr2", "Transplanted patients", "event")
#' selection_flowchart(descr2)



selection_flowchart <- function(description=data.frame()){
  require(DiagrammeR)
  flowchart_labels <- character()
  
  if (ncol(description) == 2){
  for (i in 1:nrow(description)){
       flowchart_labels[i] <- paste0(as.character(description[i,1]), "\n", 
                                     "n = ", as.character(description[i,2]))
  }
  }
  
  if (ncol(description) == 3){
    for (i in 1:nrow(description)){
      flowchart_labels[i] <- paste0(as.character(description[i,1]), "\n", 
                                    "n = ", as.character(description[i,2]), ", ",
                                    "Events = ", as.character(description[i,3]),
                                    " (", round(100*as.numeric(description[i,3])/as.numeric(description[i,2]), 1),
                                    " %)")
    }
  }
  
  
  nodes <- 
    create_nodes(nodes = 1:nrow(description),
                 type = "number",
                 label = flowchart_labels,
                 shape = "box",
                 style = "rounded")
  edges <-
    create_edges(from = 1:(nrow(description)-1), 
                 to = 2:nrow(description),
                 rel = "related")
  
  graph <-
    create_graph(
      nodes_df = nodes,
      edges_df = edges,
      node_attrs = "fontname = Helvetica, penwidth=0.5",
    )
  
  render_graph(graph)
  
}