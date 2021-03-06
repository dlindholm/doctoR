% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_imputation_results.R
\name{plot_imputation_results}
\alias{plot_imputation_results}
\title{Plot imputation results}
\usage{
plot_imputation_results(original, imputed)
}
\arguments{
\item{original}{The original data.frame}

\item{imputed}{The imputed data.frame}
}
\description{
Typically used to compare imputed data with original data. I've used it
in conjunction with the missForest package. The function returns a ggplot
that shows the density of imputed data vs. original data.
}
\examples{
library(missForest)

aq <- airquality
aq_imp <- missForest(aq)

plot_imputation_results(aq[1:3], aq_imp$ximp[1:3])

}
\keyword{imputation,}
\keyword{plot}

