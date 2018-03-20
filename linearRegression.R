#1 -- Implementation of linear regression on Auto dataset

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

#initializations
y <- mod_Auto$mpg
alpha=0.0001
max_iter=600000
x <- cbind(mod_Auto$x_not,mod_Auto$horsepower)
theta <- matrix(rep(0,2),nrow=2,ncol=1)
cost_mat <- numeric(length=max_iter)
theta_list <- list()
conv_chk <- 0.00000001

#Functions
CostFuncMain <- function(x,y,tht){
  res_err <- (x%*%tht)-y
  cost <- (sum(res_err**2))/(2*nrow(x))
  return(cost)
}

ParamUpdateMain <- function(x,y,tht){
  res_err <- (x%*%tht)-y
  mid_step <- alpha*((t(x)%*%res_err)/nrow(x))
  tht <- tht - mid_step
  return(tht)
}

#Main code
start.time <- Sys.time()
converged = FALSE
for(i in 1:max_iter){
  cost <- CostFuncMain(x,y,theta)
  cost_mat[i] <- cost
  theta_list[[i]] <- t(theta)
  if (i>1 && (abs(cost_mat[i-1]-cost_mat[i]) <= conv_chk)){
    converged = TRUE
    break} else{
      theta <- ParamUpdateMain(x,y,theta)}
}
end.time <- Sys.time()
print(end.time - start.time)

#i=517062
#converged
#cost = 11.97

#display line and plot
print(theta)
plot(x[,2],y,main="Simple Linear Regression - mpg vs horsepower",xlab="horsepower",ylab="mpg")
abline(coef = theta,col = "red")

#cost function display
plot(cost_mat[1:i], type='l', col='blue', lwd=2, main='Cost Function', ylab='Cost', xlab='Iterations')
#Cost function Contour
theta_mat=do.call(rbind,theta_list)
x_grid <- seq(min(theta_mat[,1])-30, max(theta_mat[,1])+40, length.out = 100)
y_grid <- seq(min(theta_mat[,2])-1, max(theta_mat[,2])+1, length.out = 100)
z_value <- sapply(x_grid, function(i){
  sapply(y_grid, function(j){
    CostFuncMain(x,y,c(i,j))
  })
})
contour(x_grid, y_grid, t(z_value), levels = z_value[50,][seq(1, 100, 7)],xlab = "theta0", ylab = "theta1", main = "Contour Plot - Cost Function")

#Predict for horsepower=220
x_chk <- matrix(c(1,220),nrow=1,ncol=2)
mpg_pred <- x_chk%*%theta
mpg_pred
#5.281695

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
alpha=0.003
max_iter=50000
x <- cbind(mod_Auto$x_not,mod_Auto$cylinders,mod_Auto$displacement,mod_Auto$horsepower,mod_Auto$weight,mod_Auto$acceleration,mod_Auto$year,mod_Auto$origin)
theta <- matrix(rep(0,ncol(x)),nrow=ncol(x),ncol=1)
cost_mat <- numeric(length=max_iter)
theta_list <- list()
conv_chk <- 0.00000001

#Functions
CostFuncMain <- function(x,y,tht){
  res_err <- (x%*%tht)-y
  cost <- (sum(res_err**2))/(2*nrow(x))
  return(cost)
}

ParamUpdateMain <- function(x,y,tht){
  res_err <- (x%*%tht)-y
  mid_step <- alpha*((t(x)%*%res_err)/nrow(x))
  tht <- tht - mid_step
  return(tht)
}

#Main code
start.time <- Sys.time()
converged=FALSE
for(i in 1:max_iter){
  cost <- CostFuncMain(x,y,theta)
  cost_mat[i] <- cost
  theta_list[[i]] <- t(theta)
  if (i>1 && (abs(cost_mat[i-1]-cost_mat[i]) <= conv_chk)){
    converged = TRUE
    break} else{
      theta <- ParamUpdateMain(x,y,theta)}
}
end.time <- Sys.time()
print(end.time - start.time)
#i=41150
#converged
#cost = 5.42

#display thetas
print(theta)

#cost function display
plot(cost_mat[1:i], type='l', col='blue', lwd=2, main='Cost Function', ylab='Cost', xlab='Iterations')

#Predict for values given
x_chk1 <- data.frame(rbind(noname_Auto,c(4,300,200,3500,11,70,2)))
x_scale <- scale(x_chk1)
x_chk <- matrix(c(1,x_scale[nrow(x_scale),]),nrow=1,ncol=8)
mpg_pred <- x_chk%*%theta
mpg_pred
#17.01599

#Cross-checking with package lm
a.fit <- lm(y~x)
coeffs <- as.matrix(as.matrix(coefficients(a.fit))[-2,1])
pred_value <- x_chk%*%coeffs
pred_value
#17.05505

#Test different learning rates (3,0.3,0.03,0.00003)
#Learning Rate = 3 and iterations = 100
alpha=3
max_iter=100
cost_mat <- numeric(length=max_iter)
theta_ck <- matrix(rep(0,ncol(x)),nrow=ncol(x),ncol=1)

start.time <- Sys.time()
for(i in 1:max_iter){
  cost <- CostFuncMain(x,y,theta_ck)
  cost_mat[i] <- cost
  theta_ck <- ParamUpdateMain(x,y,theta_ck)
}
end.time <- Sys.time()
print(end.time - start.time)

plot(cost_mat, type='l', col='blue', lwd=2, main='Cost Function', ylab='Cost', xlab='Iterations')

#Learning Rate = 0.3 and iterations = 100
alpha=0.3
max_iter=100
cost_mat <- numeric(length=max_iter)
theta_ck <- matrix(rep(0,ncol(x)),nrow=ncol(x),ncol=1)

start.time <- Sys.time()
for(i in 1:max_iter){
  cost <- CostFuncMain(x,y,theta_ck)
  cost_mat[i] <- cost
  theta_ck <- ParamUpdateMain(x,y,theta_ck)
}
end.time <- Sys.time()
print(end.time - start.time)

plot(cost_mat, type='l', col='blue', lwd=2, main='Cost Function', ylab='Cost', xlab='Iterations')

#Learning Rate = 0.03 and iterations = 100
alpha=0.03
max_iter=100
cost_mat <- numeric(length=max_iter)
theta_ck <- matrix(rep(0,ncol(x)),nrow=ncol(x),ncol=1)

start.time <- Sys.time()
for(i in 1:max_iter){
  cost <- CostFuncMain(x,y,theta_ck)
  cost_mat[i] <- cost
  theta_ck <- ParamUpdateMain(x,y,theta_ck)
}
end.time <- Sys.time()
print(end.time - start.time)

plot(cost_mat, type='l', col='blue', lwd=2, main='Cost Function', ylab='Cost', xlab='Iterations')

#Learning Rate = 0.00003 and iterations = 100
alpha=0.00003
max_iter=100
cost_mat <- numeric(length=max_iter)
theta_ck <- matrix(rep(0,ncol(x)),nrow=ncol(x),ncol=1)

start.time <- Sys.time()
for(i in 1:max_iter){
  cost <- CostFuncMain(x,y,theta_ck)
  cost_mat[i] <- cost
  theta_ck <- ParamUpdateMain(x,y,theta_ck)
}
end.time <- Sys.time()
print(end.time - start.time)

plot(cost_mat, type='l', col='blue', lwd=2, main='Cost Function', ylab='Cost', xlab='Iterations')

#0.3 looks the best, 3 does not converge -- skips optima, 0.03 is fine (takes more iterations). 0.00003 is sharply decreasing -- does not converge in 100 iterations

#ginv in MASS package -- takes care of singularity too
#Using normal equations
inv_normal <- ginv(t(x)%*%x)
theta_normal <- inv_normal%*%t(x)%*%y
theta_normal

#**************************************************************************************#