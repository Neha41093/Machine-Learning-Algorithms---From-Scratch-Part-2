#3 -- Implementation of Logistic Regression on Auto dataset

rm(list=ls())
#set according to your working directory
old.dir <- getwd()
setwd("C:/Users/Neha Rawat/Desktop/IU-Data Science/AML/Assignment 3")

#testing sigmoid function
z <- c(-5:5)
sigmoid_func <- function(z){
  g_sigm <- 1/(1+exp(-z))
  return(g_sigm)
}

g_sigm <- sigmoid_func(z)
plot(z,g_sigm,type='l',lwd=2,main="Sigmoid Function")


#new.Auto dataset
mpg01 <- numeric()

med_auto <- median(Auto$mpg)
for (i in 1:nrow(Auto)){
  if (Auto$mpg[i] >= med_auto){
    mpg01[i] <- 1}else{
      mpg01[i] <- 0
    }
}

new.Auto <- data.frame(mpg01 = mpg01, cylinders=Auto$cylinders,displacement=Auto$displacement,
                       horsepower=Auto$horsepower,weight=Auto$weight)
View(new.Auto)

#Scaling the dataset
noname_Auto <- data.frame(cylinders=Auto$cylinders,displacement=Auto$displacement,
                          horsepower=Auto$horsepower,weight=Auto$weight)
scaled_Auto <- data.frame(scale(noname_Auto))
new.Auto <- data.frame(mpg01=new.Auto$mpg01,cylinders=scaled_Auto$cylinders,displacement=scaled_Auto$displacement,
                       horsepower=scaled_Auto$horsepower,weight=scaled_Auto$weight)
mod_Auto <- data.frame(x_not=rep(1,nrow(Auto)),mpg01=new.Auto$mpg01,cylinders=scaled_Auto$cylinders,displacement=scaled_Auto$displacement,
                       horsepower=scaled_Auto$horsepower,weight=scaled_Auto$weight)

#write the new.Auto data to a csv file
write.csv(as.matrix(new.Auto),file="new_Auto.csv",row.names=FALSE)

#initializations
y <- mod_Auto$mpg01
alpha=0.003
max_iter=400000
x <- cbind(mod_Auto$x_not,mod_Auto$cylinders,mod_Auto$displacement,mod_Auto$horsepower,mod_Auto$weight)
theta <- matrix(rep(0,ncol(x)),nrow=ncol(x),ncol=1)
cost_mat <- numeric(length=max_iter)
theta_list <- list()
conv_chk <- 0.0000000001

#Functions
CostFuncMain <- function(x,y,tht){
  pred_func <- sigmoid_func(x%*%tht)
  res_err <- sum((-y*log(pred_func)) - ((1-y)*log(1-pred_func)))
  cost <- res_err/nrow(x)
  return(cost)
}

ParamUpdateMain <- function(x,y,tht){
  res_err <- (sigmoid_func(x%*%tht))-y
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
#i=317435
#converged
#cost = 0.26437
print(theta)
#-0.98799248,-0.05773217,-1.29564284,-1.61081927,-1.67798579

#check result -- with optim function
CostFuncMainCheck <- function(tht){
  pred_func <- sigmoid_func(x%*%tht)
  res_err <- sum((-y*log(pred_func)) - ((1-y)*log(1-pred_func)))
  cost <- res_err/nrow(x)
  return(cost)
}
theta1 <- matrix(rep(0,ncol(x)),nrow=ncol(x),ncol=1)
theta_optim <- optim(theta1,CostFuncMainCheck)
theta1 <- theta_optim$par
theta_optim$value
theta1
#-0.99315693,-0.02255445,-1.35773394,-1.62144955,-1.65301926
#cost = 0.2643726
#close enough

#check with 'glm' package
k.fit <- glm(mpg01~cylinders+displacement+horsepower+weight,data = new.Auto,family = binomial)
coefficients(k.fit)
# (Intercept)    cylinders displacement   horsepower       weight 
#-0.99318196  -0.02205626  -1.35848553  -1.62171231  -1.65273396 
#close enough

#Cost Function plot
plot(cost_mat[1:i], type='l', col='blue', lwd=2, main='Cost Function', ylab='Cost', xlab='Iterations')

#Error of model
#accuracy
prediction <- sigmoid_func(as.matrix(mod_Auto[,c(1,3:6)])%*%theta)
predict_bin <- matrix(NA,nrow=nrow(prediction),ncol=ncol(prediction))
for (p in 1:nrow(prediction)){
  if(prediction[p,] >= 0.5){
    predict_bin[p] <- 1
  } else {
    predict_bin[p] <- 0
  }
}

orig_labels <- as.matrix(new.Auto$mpg01)
pred_diff <- abs(orig_labels - predict_bin)
accuracy <- 1-(sum(pred_diff)/nrow(pred_diff))
accuracy
#0.8979592
#error in accuracy : 0.1020408

#precision and recall
true_positive <- matrix(NA,nrow=nrow(prediction),ncol=ncol(prediction))

#recall
for (p in 1:nrow(predict_bin)){
  if(predict_bin[p] == 1 && pred_diff[p] == 0){
    true_positive[p] <- 1
  } else{
    true_positive[p] <- 0
  }
}

recall <- sum(true_positive)/length(which(orig_labels == 1))
recall
#0.9183673
#error in recall : 0.08163265

#precision
precision <- sum(true_positive)/length(which(predict_bin == 1))
precision
#0.8823529
#error in precision : 0.1176471


#Predict for values given
x_chk1 <- data.frame(rbind(noname_Auto,c(8,340,200,3500)))
x_scale <- scale(x_chk1)
x_chk <- matrix(c(1,x_scale[nrow(x_scale),]),nrow=1,ncol=5)
mpg01_pred <- sigmoid_func(x_chk%*%theta)
mpg01_pred
#0.0003853077 -- means 0 label

#check with glm
predict(k.fit,data.frame(cylinders=x_chk[,1],displacement=x_chk[,2],horsepower=x_chk[,3],weight=x_chk[,4]),type="response")
#8.843772e-05 -- 0 label

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

#3 is best. 0.3 also fine. 0.03 slower. 0.00003 does not converge.
