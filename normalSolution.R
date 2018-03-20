#1 -- Normal Solution for Simple and Multivariate linear regression

rm(list=ls())
#set according to your working directory
old.dir <- getwd()
setwd("C:/Users/Neha Rawat/Desktop/IU-Data Science/AML/Assignment 3")

require("ISLR")
Auto
summary(Auto)
names(Auto)
class(Auto)
dim(Auto)
mod_Auto <- data.frame(x_not=rep(1,nrow(Auto)),mpg=Auto$mpg,cylinders=Auto$cylinders,displacement=Auto$displacement,
                       horsepower=Auto$horsepower,weight=Auto$weight,acceleration=Auto$acceleration,year=Auto$year,
                       origin=Auto$origin, name=Auto$name)
str(mod_Auto)

#************************************************************************************************#
#Simple Linear Regression
y <- mod_Auto$mpg
x <- cbind(mod_Auto$x_not,mod_Auto$horsepower)
#ginv in MASS package -- takes care of singularity too
#Using normal equations
inv_normal <- ginv(t(x)%*%x)
theta_normal <- inv_normal%*%t(x)%*%y
theta_normal

#******************************************************************************************#
#Multivariate Linear Regression

rm(list=ls())
#set according to your working directory
old.dir <- getwd()
setwd("C:/Users/Neha Rawat/Desktop/IU-Data Science/AML/Assignment 3")

noname_Auto <- data.frame(cylinders=Auto$cylinders,displacement=Auto$displacement,
                          horsepower=Auto$horsepower,weight=Auto$weight,acceleration=Auto$acceleration,year=Auto$year,
                          origin=Auto$origin)
scaled_Auto <- data.frame(scale(noname_Auto))
mod_Auto <- data.frame(x_not=rep(1,nrow(Auto)),mpg=Auto$mpg,cylinders=scaled_Auto$cylinders,displacement=scaled_Auto$displacement,
                       horsepower=scaled_Auto$horsepower,weight=scaled_Auto$weight,acceleration=scaled_Auto$acceleration,year=scaled_Auto$year,
                       origin=scaled_Auto$origin)

#initializations
y <- mod_Auto$mpg
x <- cbind(mod_Auto$x_not,mod_Auto$cylinders,mod_Auto$displacement,mod_Auto$horsepower,mod_Auto$weight,mod_Auto$acceleration,mod_Auto$year,mod_Auto$origin)
#ginv in MASS package -- takes care of singularity too
#Using normal equations
inv_normal <- ginv(t(x)%*%x)
theta_normal <- inv_normal%*%t(x)%*%y
theta_normal