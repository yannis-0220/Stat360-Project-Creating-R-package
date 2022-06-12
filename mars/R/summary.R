#' @title: summary.mars
#' @description: This function summary method for mars objects.
#' @usage S3 method for class 'summary.mars'
#' @param object a mars object output by the mars()
#' @param ... additional arguments
#' @details Summary method for the mars objects and function in the final model and the coefficients of each basis function.
#' @return a summary of the mars object
#' @family methods
#' @seealso [mars.control] for constructing the control object.
#' @seealso [plot.mars] for plotting the results.
#' @seealso [predict.mars] for predictions
#' @seealso [print.mars] for summarizing results.
#' @examples summary.mars(marstestdata)
summary.mars <- function(object,...) {
  cat("\n\nCoefficients:\n")
  print(object$coefficients)
}

