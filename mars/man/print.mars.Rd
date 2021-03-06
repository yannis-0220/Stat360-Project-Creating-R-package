% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/print.R
\name{print.mars}
\alias{print.mars}
\title{print.mars}
\usage{
S3 method for class 'print.mars'
}
\arguments{
\item{object}{object of class}
}
\value{
Print a summary of the mars objects.
}
\description{
Print a summary of the fitted model.
}
\details{
This method prints the summary method for mars objects and its coefficients
}
\examples{
mm<- print(marstestdata$coefficients)
print.mars(mm)
}
\seealso{
\link{mars.control} for constructing the control object.

\link{plot.mars} for plotting the results

\link{predict.mars} for predictions

\link{summary.mars} for summarizing
}
\concept{method}
