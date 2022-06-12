#' @title mars: Multivariate Adaptive Regression Splines Model
#' @description Build a regression model using the techniques in Friedman's papers "Multivariate Adaptive Regression Splines"
#' Mars can be used to model onlinear relationships between a set of predictor variables and a response variable.
#' @usage S3 method for class 'mars'
#' @param formula data frame for formula
#' @param data a data frame containing the data
#' @param control an object of class 'mars.control'
#' @details an object of class 'mars.control'that includes the final regression and a description of the basis function.There are plot,predit, summary and print methods for mars objects.
#' @return an object of class 'mars' that contains the value: fitted value and coefficients.
#' @family methods
#' @import  stats
#' @seealso [mars.control] for constructing the control object.
#' @seealso [plot.mars] for plotting the results, [predict.mars] for predictions, [summary.mars]/[print.mars] for summarizing
#' @examples mm <- mars(y~., dat=mars::marstestdata)
#' @author Yanrong Shi, Yongliang Ye
#' @references  Jerome H. Friedman The Annals of Statistics , Mar., 1991, Vol. 19, No. 1 (Mar., 1991), pp. 1-67. \url{https://doi.org/10.1214/aos/1176347963}.

mars = function(formula,data,control=NULL){
  cc=match.call()
  mf=model.frame(formula,data)
  y=model.response(mf)
  mt=attr(mf,"terms")
  x=model.matrix(mt,mf)[, -1, drop=FALSE]
  x_name <- colnames(x)
  if(is.null(control))
    control<- mars.control()
  control <- validate_mars.control(control)
  fwd = fwd_stepwise(y,x,control)
  bwd = bwd_stepwise(fwd,control)
  fit <- lm(y~.-1,data=data.frame(y=y,bwd$B))
  out = c(list(call=cc,formula=formula,y=y,B=bwd$B,Bfuncs=bwd$Bfuncs,x_name=x_name),fit)
  class(out) = c("mars",class(fit))
  out
  return(out)
}
#fwd_stepwise------------------------------------------
fwd_stepwise <- function(y,x,mc=mars.control()){
  #initial N n B Bfuncs
  N <- length(y)
  n <- ncol(x)
  B <- init_B(N, mc$Mmax)
  Bfuncs <- vector(mode = "list",length = mc$Mmax+1)
  #m loop
  for(i in 1:(mc$Mmax/2)) {
    M <- 2*i-1
    lof_best <- Inf
    #setdiff to find svars
    for(m in 1:M) {
      svar <- setdiff(1:n,Bfuncs[[m]][,'v']) #setdiff
      if(mc$trace)cat("M,m,svars",M,m,svars, "\n")
      for(v in svar){
        tt <- split_points(x[,v],B[,m])
        for(t in tt) {
          Bnew <- data.frame(B[,1:M],
                             Btem1=B[,m]*h(x[,v],+1,t),
                             Btem2=B[,m]*h(x[,v],-1,t))
          gdat <- data.frame(y=y,Bnew)
          lof <- LOF(y~.,gdat,mc)
          if(lof < lof_best) {
            lof_best <- lof
            split_best <- c(m=m,v=v,t=t)
          }#lof
        }
      }
    }#m loop
    m <- split_best['m']; v <- split_best['v']; t <- split_best['t']
    cat("best split on variable",v, "at", t, "\n")
    Bfuncs[[M+1]] <- rbind(Bfuncs[[m]],c(s=-1,v,t))
    Bfuncs[[M+2]] <- rbind(Bfuncs[[m]],c(s=+1,v,t))
    B[,M+1] <- B[,m]*h(x[,v],-1,t)
    B[,M+2] <- B[,m]*h(x[,v],+1,t)

  }
  colnames(B) <- paste0("B",(0:(ncol(B)-1)))
  return(list(y=y,B=B,Bfuncs=Bfuncs))
}#end loop over m

#' @title mars.control
#' @description Constructor for 'mars.control' object. This function constructs a 'mars.control' object that specifies parameters used in the model fitting procedure.
#' @usage S3 method for class 'mars.control'.
#' @param Mmax Maximum number of basis functions. Should be an even integer.
#' @param d The coefficient in the penalty term of the generalized cross validation measure
#' @param trace Should we print status information about the fitting? Default is 'False'
#' @import stats
#' @return a list which class is "mars.control", and contain three variables: Mmax, d and trace
#' @examples mc <- mars.control(Mmax = 10)
#' @seealso [plot.mars] for plotting the results
#' @seealso [predict.mars] for predictions.
#' @seealso [summary.mars]/[print.mars] for summarizing.

#Helper
mars.control <- function(Mmax=2, d=3, trace=FALSE){
  if (Mmax < 2) {
    warning(" Mmax < 2, we make Mmax=2 ")
    Mmax = 2
  }
  structure(list(Mmax=Mmax,d=d,trace=trace),class="mars.control")
}

init_B <- function(N,Mmax) {
  B <- data.frame(matrix(NA,nrow=N,ncol=(Mmax+1)))
  B[,1] <- 1
  names(B) <- c("B0",paste0("B",1:Mmax))
  return(B)
}

split_points <- function(xv,Bm) {
  out <- sort(unique(xv[Bm>0]))
  return(out[-length(out)])
}
h = function(x,s,t){
  return(pmax(0, s*(x-t)))
}

#validator
validate_mars.control <- function(control){
  if (control$Mmax < 2){
    control$Mmax = 2
  }
  control
}
#constructor
new_mars.control <- function(Mmax=2, d=3, trace=FALSE){
  structure(list(Mmax=Mmax,d=d,trace=trace),class="mars.control")
}

# LOF(lab8 Q4)
LOF <- function(form,data,mc) {
  ff <- lm(form,data)
  er <- sum(residuals(ff)^2)
  N <- nrow(data)
  M = length(coef(ff))-1
  CM <- sum(diag(hatvalues(ff)))+mc$d*M
  value <- er*N/((N-CM)^2)
  return(value)
}
LOF

##bwd_stepwise
bwd_stepwise <- function(fwd,mc){
  Mmax <- ncol(fwd$B)-1
  Jstar <- 2:(Mmax+1)
  Kstar <- Jstar
  dat <- data.frame(y=fwd$y,fwd$B)
  lofstar <- LOF(y~.-1,dat,mc)
  for (M in (ncol(fwd$B)):2 ){
    b <- Inf
    L <- Kstar
    for (m in L){
      K <- setdiff(L,m)
      dat <- data.frame(y=fwd$y,fwd$B[,K])
      lof <- LOF(y~.,dat,mc)
      if(lof < b) {
        b <- lof
        Kstar <- K
      }
      if(lof < lofstar) {
        lofstar <- lof
        Jstar <- K
      }
    }
  }
  Jstar <- c(1,Jstar)
  B <- fwd$B[,Jstar]
  Bfuncs <- fwd$Bfuncs[Jstar]
  return(list(y = fwd$y, B= B,Bfuncs = Bfuncs))
}





