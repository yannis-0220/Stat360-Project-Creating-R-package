#' @title print.mars
#' @description Print a summary of the fitted model.
#' @usage S3 method for class 'print.mars'
#' @details This method prints the summary method for mars objects and its coefficients
#' @param object object of class
#' @family method
#' @return Print a summary of the mars objects.
#' @examples mm<- print(marstestdata$coefficients)
#' @examples print.mars(mm)
#' @seealso [mars.control] for constructing the control object.
#' @seealso [plot.mars] for plotting the results
#' @seealso [predict.mars] for predictions
#' @seealso[summary.mars] for summarizing
print.mars <- function(object) {
  cat("\nCoefficients: \n")
  print(object$coefficients)
  print(object$call)
}

