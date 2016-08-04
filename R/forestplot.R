#' Create a forestplot
#' 
#' Forest plots are commonly used in clinical research to illustrate e.g. odds
#' ratios or hazard ratios with 95 percent confidence intervals. The present function
#' will create a forestplot. It assumes the input to be in a dataframe, where 
#' there is one column each for hazard ratio, upper confidence interval, lower confidence
#' interval, text to be written to the left of the plot and (optional) text to the 
#' right of the plot.
#' 
#' @param f_data a dataframe containing information for the forestplot (arranged as
#' outlined above, and as demonstrated below)
#' @param hr which column contains hazard ratio, or odds ratio, etc. ?
#' @param lci which column contains lower confidence interval?
#' @param uci which column contains upper confidence interval?
#' @param ltext which column contains the text that should be put on the left side
#' of the plot?
#' @param rtext which column contains the text that should be put on the right side of the plot?
#' @param axis_text what should the axis label be? By default: Hazard ratio.
#' @param point_size size of the points in the plot
#' @param point_color color of the points in the plot
#' @param text_size size of the text in the plot
#' @param ref_line where should the reference line be drawn. By default: 1. Enter value NULL to
#' skip putting a reference line in the plot.
#' @param right_offset a number that controls the offset of the right text column. Its optimal
#' value depends of the size of the plot, etc. Might need some tweaking of this value to make the plot
#' look perfect!
#' @keywords forestplot
#' @export
#' @examples

#' # Create some test data
#' hr <- seq(0.80, 1.1, by = 0.05)
#' lci <- hr - 0.1
#' uci <- hr + 0.1
#' left_text<- paste0("Left text no ", 1:7)
#' right_text <- paste0("Right text no ", 1:7)
#' forest_data <- data.frame(left_text,lci,hr,uci,right_text)
#' 
#' # Basic forest plot
#' forestplot(forest_data, hr, lci, uci, left_text)
#' 
#' # With right text column
#' forestplot(forest_data, hr, lci, uci, left_text, right_text, point_color = "purple")
#' 
#' # Combined with hrci function
#' forestplot(forest_data, hr, lci, uci, left_text, hrci(forest_data), point_color = "darkgreen")
#' 

forestplot <- function(f_data,
                       hr,
                       lci,
                       uci,
                       ltext,
                       rtext = NULL,
                       axis_text = "Hazard ratio",
                       point_size = 4,
                       point_color = "black",
                       text_size = 5,
                       ref_line = 1,
                       right_offset = 0.4
                       ){

arguments <- as.list(match.call())
lci <- eval(arguments$lci, f_data)
uci <- eval(arguments$uci, f_data)
hr <- eval(arguments$hr, f_data)
ltext <- eval(arguments$ltext, f_data)
rtext <- eval(arguments$rtext, f_data)

require(ggplot2)
require(dplyr)
  
#-Define x scale----------------------------------------------------------------
xmin <- min(f_data$lci)
xmax <- max(f_data$uci)

#Since a log scale will be used, the following are reasonable 
#tick mark positions:
tics <- c(0.03125, 0.0625, 0.125, 0.25, 0.5, 0.625, 0.75, 0.875, 1.0, 1.25, 1.5, 2, 4, 8, 16, 32, 64, 128)

#Based on the upper and lower CI's provided, select appropriate x axis limits
t_sel <- between(tics, xmin, xmax)
for(i in 2:length(t_sel)){
  if(t_sel[i] & !t_sel[i-1]) t_sel[i-1] <- TRUE
}
for(i in length(t_sel):2){
  if(!t_sel[i] & t_sel[i-1]) t_sel[i] <- TRUE
}
tics_sel <- data.frame(tics, t_sel)
tics_sel <- filter(tics_sel, t_sel == TRUE)


#Define plot
p <- ggplot(f_data, aes(x=factor(ltext,levels = rev(levels(ltext)),ordered = TRUE),
                          y=hr,
                          ymin=lci,
                          ymax=uci))

p3 <- p + 
      geom_errorbar(aes(ymin=lci, ymax=uci), width=0)+
      {if(!is.null(ref_line)) geom_hline(yintercept=ref_line, size = 0.25, linetype=2)} +
      geom_point(shape=22, fill=point_color, size=point_size) +
      geom_segment(aes(x=0, y=min(tics_sel$tics), xend = 0, yend=max(tics_sel$tics)), size=1.5)+
      {if(!is.null(rtext)) geom_text(aes(label=rtext, y=max(tics_sel$tics) + right_offset), hjust=1, size=text_size)} +
      coord_flip()+
      {if(!is.null(rtext)) scale_y_log10(limits = c(min(tics_sel$tics), max(tics_sel$tics)+ right_offset),
                    breaks = tics_sel$tics, labels = tics_sel$tics)
        else scale_y_log10(limits = c(min(tics_sel$tics), max(tics_sel$tics)),
                          breaks = tics_sel$tics, labels = tics_sel$tics)}+
      xlab("")+
      ylab("Hazard ratio")+
      theme_bw()+
      theme(axis.text = element_text(size = (14/5)*text_size),
            axis.title = element_text(size= (14/5)*text_size),
            panel.border = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), 
            axis.line.x = element_blank(),
            axis.line.y = element_blank(),
            axis.ticks.y = element_blank())
 
return(p3)
}