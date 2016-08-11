#' Plot imputation results
#'
#' Typically used to compare imputed data with original data. I've used it
#' in conjunction with the missForest package. The function returns a ggplot
#' that shows the density of imputed data vs. original data.

#' @param original The original data.frame
#' @param imputed The imputed data.frame
#'
#' @keywords imputation, plot
#' @export
#' @examples
#' library(missForest)
#'
#' aq <- airquality
#' aq_imp <- missForest(aq)
#'
#' plot_imputation_results(aq[1:3], aq_imp$ximp[1:3])
#'
plot_imputation_results <- function(original, imputed){
  require(missForest)
  require(dplyr)
  require(reshape2)
  #Drop non-numeric variables
  ori <- original[varClass(original) == "numeric"]
  imp <- imputed[varClass(imputed) == "numeric"]
  #Create key variable
  ori$dataset <- "original"
  imp$dataset <- "imputed"
  #Combine datasets
  comb <- bind_rows(ori, imp)
  #Melt data to plotable format
  plot_data <- melt(comb, id.vars = "dataset")
  #Create plot
  cols = c("deepskyblue", "darkgrey")
  ggplot(plot_data, aes(value, fill = dataset, color = dataset)) +
    geom_density(alpha = 0.3) +
    scale_fill_manual(values = cols) +
    scale_color_manual(values = cols) +
    facet_wrap(~variable, scales = "free") +
    theme_bw()
}




