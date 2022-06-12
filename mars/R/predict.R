#' @title predict.mars(): Predict values from a fitted mars object
#' @description Predicted values based on multivariate adaptive regression spline models(mars).
#' @usage S3 method for class 'print.predict'
#' @param object a mars object
#' @param newdata optional new data for which predictions are required
#' @param ... further arguments to be passed to or from methods.
#' @return a vector of predictions
#' @details It is based on `predict.lm()`It is based on `predict.lm()`,predictions from the fitted model in testmars on “new data” in marstestdata.This is the same data used to fit the model, but by passing it in as newdata we test that part of the code.Compare the output of this call to the expected output testpredict.
#' @family methods
#' @seealso [mars.control] for constructing the control object.
#' @seealso [plot.mars] for plotting the results
#' @seealso [summary.mars]/[print.mars] for summarizing
#' @examples mm <- mars(y~x1+x2, dat = mars::marstestdata)
#' @examples predict(mm, newdata=data.frame(x1=rnorm(100), x2= rnorm(100)))

predict.mars <- function(object,newdata,...) {
  if(missing(newdata) || is.null(newdata)) {
    B <- as.matrix(object$B)
  }
  else {
    tt <- terms(object$formula,data=newdata)
    tt <- delete.response(tt)
    mf <- model.frame(tt,newdata)
    mt <- attr(mf, "terms")
    X <- model.matrix(mt, mf)[,-1] # remove intercept
    B <- make_B(X,object$Bfuncs)
  }
  beta <- object$coefficients
  drop(B %*% beta)
}




