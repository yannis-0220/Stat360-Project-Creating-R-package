#' @title plot.mars
#' @description Plots the fitted basis functions that depend on one explanatory variable (main effects) or two explanatory variables (two-way interactions). Use 'predict.lm()' to see the residual plots
#' @usage S3 method for class 'print.plot'
#' @param x a mars obeject
#' @param ... additional arguments to pass to plot()
#' @family methods
#' @details Plot a mars object. By default the plot shows residuals versus fitted values.
#' @return Plots of the fitted model of normal distribution and showing the linear relationships.
#' @import stats
#' @import tidyverse
#' @import ggplot2
#' @seealso [mars.control] for constructing the control object.
#' @seealso [predict.mars] for predictions
#' @seealso [summary.mars]/[print.mars] for summarizing
#' @examples mm <- mars(y~x1+x2, data = marstestdata, mars.control(Mmax=4))
#' @examples class(mm) <- 'lm'; plot(mm)
#' @examples mars::plot.mars(marstestdata)

plot.mars <- function(object) {
  y <- all.vars(object$formula)
  plot(object$fitted,object$y,
       xlab="fitted values",
       ylab="Residuals",
       main="Linear plot")
}

plot.mars <- function(object) {
  par(mfrow=c(1,2))
  qqnorm(object$residuals,
         ylab="Residuals Quantiles",
         xlab = "Normal Quantitles",
         main = "Residual",
         col="pink")
}


