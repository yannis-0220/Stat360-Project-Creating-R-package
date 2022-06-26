# Stat360Project-Mars
## Team:
	Yanrong Shi
	Yongliang Ye
# Project Overview:
The project is to create an R package that implements the Multivariate Adaptive Regression Splines (MARS) algorithm described in Friedman (1991).
	Jerome H. Friedman
	The Annals of Statistics , Mar., 1991, Vol. 19, No. 1 (Mar., 1991), pp. 1-67
# Abstract
It is a non-parametric regression technique and can be seen as an extension of linear models that automatically models nonlinearities and interactions between variables. The term "MARS" is trademarked and licensed to Salford Systems. In order to avoid trademark infringements, many open-source implementations of MARS are called "Earth".

The main function, mars(), takes a formula, data frame, and control object as input, fits the model by the forward and backward stepwise algorithms, and returns a mars object that contains the final least squares fit, along with a description of the basis functions from the final fit. 
There are also predict(), plot(), summary(), and print() methods for mars objects. And package also includes fwd_stepwise() and bwd_stepwise() functions that implement the forward and backward algorithms, and the four methods. 
You will also see a package vignette that shows users how to use mars() and the four methods. 
# Mars Algorithm
Multivariate Adaptive Regression Splines(MARS), is an algorithm for complex non-linear regression problems. The algorithm involves finding a set of simple linear functions that in aggregate result in the best predictive performance.
# Data Test
On this R-data statistics page, you will find information about the Wage data set which pertains to Mid-Atlantic Wage Data. The Wage data set is found in the ISLR R package. You can load the Wage data set in R by issuing the following command at the console data("Wage"). This will load the data into a variable called Wage. If R says the Wage data set is not found, you can try installing the package by issuing this command install.packages("ISLR") and then attempt to reload the data. If you need to download R, you can go to the R project website. 
# wage Format
	A data frame with 3000 observations on the  12 variables.
	year        Year that wage information was recorded
	age         Age of worker
	sex         Gender
	education   A factor with levels 1. < HS Grad 2. HS Grad 3. Some College4.
							College Grad and 5. Advanced Degree indicating education level
	...            ...
	
	library(ISLR)
	data(Wage)
	
The optional argument is 'mars.control' object. Users should use the constructor 'mars.control()' to specify the three model fitting parameters.

		- Parameter  Mmax : default value is 2
	
		- Parameter  d    : default value is 3
		
		- Parameter trace : default value is FALSE
		
	
		mc <- mars.control(Mmax=10)
# calling mars
	m <- mars(wage ~ age + education, data=Wage, control=mc)

## calling mars function
	plot.mars(m)
	print.mars(m)
	summary.mars(m)
	predict.mars(m,newdata=data.frame(age=Wage$age,education=Wage$education))

# Reference
Jerome H. Friedman The Annals of Statistics , Mar., 1991, Vol. 19, No. 1 (Mar., 1991), pp. 1-67. \url{https://doi.org/10.1214/aos/1176347963}.

Multivariate adaptive regression spline https://en.wikipedia.org/wiki/Multivariate_adaptive_regression_spline

Wage R dataset:
https://r-data.pmagunia.com/dataset/r-dataset-package-islr-wage
