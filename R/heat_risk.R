

heat_risk <- function(limits = c(0,100),
                     your_risk = NULL,
                     your_risk_text = "Your risk",
                     alt_risk = NULL,
                     alt_risk_text = NULL,
                     xlab = "%"){
  require(colorspace)
  require(ggplot2)
  # Create sequence of 1000 values between min and max risk, to provide
  # pretty gradient
  values <- data.frame(d = seq(limits[1], limits[2], by = limits[2]/1000))
  
  # Define plot
  p <- ggplot() +
  geom_tile(data=values, aes(x = 1, y = d, width = 1, fill = d)) +
  scale_x_continuous(limits=c(0,2),
                     breaks=NULL)+
  
  scale_fill_gradientn(colors=heat_hcl(1000,
                                       h = c(130,0),
                                       l = c(40,70))) +
  coord_flip()+  
    
  # Draw line and annotate predicted risk, if provided
  {if(!is.null(your_risk)) geom_hline(yintercept = your_risk, 
                                      size = 3)}+
  {if(!is.null(your_risk_text) & !is.null(your_risk))
  annotate("text", 
           x = 1.8, 
           y = your_risk + 0.01 * max(values$d), 
           label = your_risk_text, 
           size=8, 
           color="red",
           hjust = 0)}+

  # Draw line and annotate alternative risk, if provided  
  {if(!is.null(alt_risk)) geom_hline(yintercept = alt_risk, size = 1)}+
  {if(!is.null(alt_risk_text) & !is.null(alt_risk))
  annotate("text", 
              x = 1.8, 
              y = alt_risk + 0.01 * max(values$d), 
              label = alt_risk_text, 
              size=4, 
              color="black",
              hjust = 0)}+
  
  # Some theme stuff  
  ylab(xlab)+
  theme_bw()+
  theme(axis.title.y=element_blank(),
        legend.position="none")
  
  return(p)
}


#Example

# heat_risk(c(0,40), your_risk = 12, "Predicted risk")

  