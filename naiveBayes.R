library(e1071)
library(mvtnorm)

#********************************************Naive Bayes Implementation*********************************************
#*****************************************Ionosphere Dataset********************************************************
#Trying it out on the Ionosphere dataset
#Have the cross-validation datasets available (ionosphere_data_train1 ... ionosphere_data_train5) -- can read them through the .csv files as well
#ion_test1, ion_test2, ion_test3, ion_test4 and ion_test5 datasets already have the target variable removed in them
#ion_train1, ion_train2 ... ion_train5 -- train datasets with target removed (used in R function)
#ion_target1, ion_target2, ion_target3 ... ion_target5 -- targets of train (used in R function)
#ion_target_check1, ion_target_check2 ... ion_target_check5 -- targets of test

#***************************Test dataset 1****************************
ion_train_b <- ionosphere_data_train1[ionosphere_data_train1[,35] == "b",]
ion_train_g <- ionosphere_data_train1[ionosphere_data_train1[,35] == "g",]
ion_train_b <- as.data.frame(ion_train_b[,-c(1,2)])
ion_train_g <- as.data.frame(ion_train_g[,-c(1,2)])
#Finding Parameters
mean_b <- colSums(as.matrix(ion_train_b[,-33]))/nrow(ion_train_b)
mean_g <- colSums(as.matrix(ion_train_g[,-33]))/nrow(ion_train_g)
mean_train1 <- t(cbind(mean_b,mean_g))
prior_train1 <- matrix(c(nrow(ion_train_b)/nrow(ionosphere_data_train1),nrow(ion_train_g)/nrow(ionosphere_data_train1)),ncol=2,nrow=1)
ident_mat_b <- matrix(1,ncol=1,nrow=nrow(ion_train_b))
ident_mat_g <- matrix(1,ncol=1,nrow=nrow(ion_train_g))
centered_dat_b <- as.matrix(ion_train_b[,-33]) - (ident_mat_b %*% t(mean_b))
centered_dat_g <- as.matrix(ion_train_g[,-33]) - (ident_mat_g %*% t(mean_g))
cov_b <- (t(centered_dat_b) %*% centered_dat_b)/nrow(centered_dat_b)
cov_g <- (t(centered_dat_g) %*% centered_dat_g)/nrow(centered_dat_g)
cov_train1 <- list(cov_b,cov_g)

#Checking on test data
ion_test1_p <- ion_test1[,-c(1,2)]
prob_mat = matrix(NA, nrow=2, ncol=nrow(ion_test1_p))
for (i in c(1:2)){
  prob_mat[i,] <-  dmvnorm(ion_test1_p,mean_train1[i,],cov_train1[[i]]) * prior_train1[1,i]
}
assign_mat <- apply(prob_mat, 2, which.max)
label_pred_train1 <- ifelse(assign_mat == 2,"g","b")
error_ion_train1 <-mean(label_pred_train1 != ion_target_check1)
#0.1571429

#check with R package
nb_ion_1 <- naiveBayes(ion_target1~. , data=ion_train1)
pred_ion_1 <- predict(nb_ion_1, ion_test1, type="class")
error_ionr_t1 <-mean(pred_ion_1 != ion_target_check1)
#0.1142857

#***************************Test dataset 2****************************
ion_train_b <- ionosphere_data_train2[ionosphere_data_train2[,35] == "b",]
ion_train_g <- ionosphere_data_train2[ionosphere_data_train2[,35] == "g",]
ion_train_b <- as.data.frame(ion_train_b[,-c(1,2)])
ion_train_g <- as.data.frame(ion_train_g[,-c(1,2)])
#Finding Parameters
mean_b <- colSums(as.matrix(ion_train_b[,-33]))/nrow(ion_train_b)
mean_g <- colSums(as.matrix(ion_train_g[,-33]))/nrow(ion_train_g)
mean_train2 <- t(cbind(mean_b,mean_g))
prior_train2 <- matrix(c(nrow(ion_train_b)/nrow(ionosphere_data_train2),nrow(ion_train_g)/nrow(ionosphere_data_train2)),ncol=2,nrow=1)
ident_mat_b <- matrix(1,ncol=1,nrow=nrow(ion_train_b))
ident_mat_g <- matrix(1,ncol=1,nrow=nrow(ion_train_g))
centered_dat_b <- as.matrix(ion_train_b[,-33]) - (ident_mat_b %*% t(mean_b))
centered_dat_g <- as.matrix(ion_train_g[,-33]) - (ident_mat_g %*% t(mean_g))
cov_b <- (t(centered_dat_b) %*% centered_dat_b)/nrow(centered_dat_b)
cov_g <- (t(centered_dat_g) %*% centered_dat_g)/nrow(centered_dat_g)
cov_train2 <- list(cov_b,cov_g)

#Checking on test data
ion_test2_p <- ion_test2[,-c(1,2)]
prob_mat = matrix(NA, nrow=2, ncol=nrow(ion_test2_p))
for (i in c(1:2)){
  prob_mat[i,] <-  dmvnorm(ion_test2_p,mean_train2[i,],cov_train2[[i]]) * prior_train2[1,i]
}
assign_mat <- apply(prob_mat, 2, which.max)
label_pred_train2 <- ifelse(assign_mat == 2,"g","b")
error_ion_train2 <-mean(label_pred_train2 != ion_target_check2)
#0.1

#check with R package
nb_ion_2 <- naiveBayes(ion_target2~. , data=ion_train2)
pred_ion_2 <- predict(nb_ion_2, ion_test2, type="class")
error_ionr_t2 <-mean(pred_ion_2 != ion_target_check2)
#0.1

#***************************Test dataset 3****************************
ion_train_b <- ionosphere_data_train3[ionosphere_data_train3[,35] == "b",]
ion_train_g <- ionosphere_data_train3[ionosphere_data_train3[,35] == "g",]
ion_train_b <- as.data.frame(ion_train_b[,-c(1,2)])
ion_train_g <- as.data.frame(ion_train_g[,-c(1,2)])
#Finding Parameters
mean_b <- colSums(as.matrix(ion_train_b[,-33]))/nrow(ion_train_b)
mean_g <- colSums(as.matrix(ion_train_g[,-33]))/nrow(ion_train_g)
mean_train3 <- t(cbind(mean_b,mean_g))
prior_train3 <- matrix(c(nrow(ion_train_b)/nrow(ionosphere_data_train3),nrow(ion_train_g)/nrow(ionosphere_data_train3)),ncol=2,nrow=1)
ident_mat_b <- matrix(1,ncol=1,nrow=nrow(ion_train_b))
ident_mat_g <- matrix(1,ncol=1,nrow=nrow(ion_train_g))
centered_dat_b <- as.matrix(ion_train_b[,-33]) - (ident_mat_b %*% t(mean_b))
centered_dat_g <- as.matrix(ion_train_g[,-33]) - (ident_mat_g %*% t(mean_g))
cov_b <- (t(centered_dat_b) %*% centered_dat_b)/nrow(centered_dat_b)
cov_g <- (t(centered_dat_g) %*% centered_dat_g)/nrow(centered_dat_g)
cov_train3 <- list(cov_b,cov_g)

#Checking on test data
ion_test3_p <- ion_test3[,-c(1,2)]
prob_mat = matrix(NA, nrow=2, ncol=nrow(ion_test3_p))
for (i in c(1:2)){
  prob_mat[i,] <-  dmvnorm(ion_test3_p,mean_train3[i,],cov_train3[[i]]) * prior_train3[1,i]
}
assign_mat <- apply(prob_mat, 2, which.max)
label_pred_train3 <- ifelse(assign_mat == 2,"g","b")
error_ion_train3 <-mean(label_pred_train3 != ion_target_check3)
#0.08571429

#check with R package
nb_ion_3 <- naiveBayes(ion_target3~. , data=ion_train3)
pred_ion_3 <- predict(nb_ion_3, ion_test3, type="class")
error_ionr_t3 <-mean(pred_ion_3 != ion_target_check3)
#0.1285714

#***************************Test dataset 4****************************
ion_train_b <- ionosphere_data_train4[ionosphere_data_train4[,35] == "b",]
ion_train_g <- ionosphere_data_train4[ionosphere_data_train4[,35] == "g",]
ion_train_b <- as.data.frame(ion_train_b[,-c(1,2)])
ion_train_g <- as.data.frame(ion_train_g[,-c(1,2)])
#Finding Parameters
mean_b <- colSums(as.matrix(ion_train_b[,-33]))/nrow(ion_train_b)
mean_g <- colSums(as.matrix(ion_train_g[,-33]))/nrow(ion_train_g)
mean_train4 <- t(cbind(mean_b,mean_g))
prior_train4 <- matrix(c(nrow(ion_train_b)/nrow(ionosphere_data_train4),nrow(ion_train_g)/nrow(ionosphere_data_train4)),ncol=2,nrow=1)
ident_mat_b <- matrix(1,ncol=1,nrow=nrow(ion_train_b))
ident_mat_g <- matrix(1,ncol=1,nrow=nrow(ion_train_g))
centered_dat_b <- as.matrix(ion_train_b[,-33]) - (ident_mat_b %*% t(mean_b))
centered_dat_g <- as.matrix(ion_train_g[,-33]) - (ident_mat_g %*% t(mean_g))
cov_b <- (t(centered_dat_b) %*% centered_dat_b)/nrow(centered_dat_b)
cov_g <- (t(centered_dat_g) %*% centered_dat_g)/nrow(centered_dat_g)
cov_train4 <- list(cov_b,cov_g)

#Checking on test data
ion_test4_p <- ion_test4[,-c(1,2)]
prob_mat = matrix(NA, nrow=2, ncol=nrow(ion_test4_p))
for (i in c(1:2)){
  prob_mat[i,] <-  dmvnorm(ion_test4_p,mean_train4[i,],cov_train4[[i]]) * prior_train4[1,i]
}
assign_mat <- apply(prob_mat, 2, which.max)
label_pred_train4 <- ifelse(assign_mat == 2,"g","b")
error_ion_train4 <-mean(label_pred_train4 != ion_target_check4)
#0.1857143

#check with R package
nb_ion_4 <- naiveBayes(ion_target4~. , data=ion_train4)
pred_ion_4 <- predict(nb_ion_4, ion_test4, type="class")
error_ionr_t4 <-mean(pred_ion_4 != ion_target_check4)
#0.1285714

#***************************Test dataset 5****************************
ion_train_b <- ionosphere_data_train5[ionosphere_data_train5[,35] == "b",]
ion_train_g <- ionosphere_data_train5[ionosphere_data_train5[,35] == "g",]
ion_train_b <- as.data.frame(ion_train_b[,-c(1,2)])
ion_train_g <- as.data.frame(ion_train_g[,-c(1,2)])
#Finding Parameters
mean_b <- colSums(as.matrix(ion_train_b[,-33]))/nrow(ion_train_b)
mean_g <- colSums(as.matrix(ion_train_g[,-33]))/nrow(ion_train_g)
mean_train5 <- t(cbind(mean_b,mean_g))
prior_train5 <- matrix(c(nrow(ion_train_b)/nrow(ionosphere_data_train5),nrow(ion_train_g)/nrow(ionosphere_data_train5)),ncol=2,nrow=1)
ident_mat_b <- matrix(1,ncol=1,nrow=nrow(ion_train_b))
ident_mat_g <- matrix(1,ncol=1,nrow=nrow(ion_train_g))
centered_dat_b <- as.matrix(ion_train_b[,-33]) - (ident_mat_b %*% t(mean_b))
centered_dat_g <- as.matrix(ion_train_g[,-33]) - (ident_mat_g %*% t(mean_g))
cov_b <- (t(centered_dat_b) %*% centered_dat_b)/nrow(centered_dat_b)
cov_g <- (t(centered_dat_g) %*% centered_dat_g)/nrow(centered_dat_g)
cov_train5 <- list(cov_b,cov_g)

#Checking on test data
ion_test5_p <- ion_test5[,-c(1,2)]
prob_mat = matrix(NA, nrow=2, ncol=nrow(ion_test5_p))
for (i in c(1:2)){
  prob_mat[i,] <-  dmvnorm(ion_test5_p,mean_train5[i,],cov_train5[[i]]) * prior_train5[1,i]
}
assign_mat <- apply(prob_mat, 2, which.max)
label_pred_train5 <- ifelse(assign_mat == 2,"g","b")
error_ion_train5 <-mean(label_pred_train5 != ion_target_check5)
#0.1408451

#check with R package
nb_ion_5 <- naiveBayes(ion_target5~. , data=ion_train5)
pred_ion_5 <- predict(nb_ion_5, ion_test5, type="class")
error_ionr_t5 <-mean(pred_ion_5 != ion_target_check5)
#0.08450704

#*****************************Compiling Results********************************
error_ion_nb <-c(error_ion_train1,error_ion_train2,error_ion_train3,error_ion_train4,error_ion_train5)
error_ion_rnb <- c(error_ionr_t1,error_ionr_t2,error_ionr_t3,error_ionr_t4,error_ionr_t5)

mean(error_ion_nb)
#0.1338833
mean(error_ion_rnb)
#0.1111871

#Plot for error values over all test datasets
plot(c(1,2,3,4,5), error_ion_nb,
     type="b", pch = 19, col="blue",frame = FALSE, 
     main = "Naive Bayes on Ionosphere Data",
     xlab="Dataset Number",
     ylab="Test errors on CV datasets")
minor.tick(nx=4)
plot(c(1,2,3,4,5), error_ion_rnb,
     type="b", pch = 19, col="green",frame = FALSE, 
     main = "Naive Bayes on Ionosphere Data - R Function",
     xlab="Dataset Number",
     ylab="Test errors on CV datasets")
minor.tick(nx=4)

#*************************************************************************************************************

#**********************************Car Evaluation Dataset*****************************************************
#Using m-estimate for categorical variables (handling for zero probability cases) -- using m=4, p=1/4 , alpha=1 (laplace case)
#input data files already present here -- can use read.csv to load them too
# test and train files containing 'data' in their names in folder -- "Car - Test and Train"
#test and train files without 'data' in their names in folder -- "Car Modified - Test and Train" (contains numerical ranks for categorical data)
summary(car_data)
#V1          V2          V3         V4          V5         V6          V7      
#high :432   high :432   2    :432   2   :576   big  :576   high:576   acc  : 384  
#low  :432   low  :432   3    :432   4   :576   med  :576   low :576   good :  69  
#med  :432   med  :432   4    :432   more:576   small:576   med :576   unacc:1210  
#vhigh:432   vhigh:432   5more:432                                     vgood:  65  

#***************************Test dataset 1****************************
#Calculating probability matrices for different classes
car_t1_unacc <- car_data_train1[car_data_train1[,7] == "unacc",]
car_t1_acc <- car_data_train1[car_data_train1[,7] == "acc",]
car_t1_good <- car_data_train1[car_data_train1[,7] == "good",]
car_t1_vgood <- car_data_train1[car_data_train1[,7] == "vgood",]

#unacceptable
car_t1_unaccp_d1 <- data.frame(v1.vhigh = length(which(car_t1_unacc[,1] == "vhigh")),v1.high = length(which(car_t1_unacc[,1] == "high")),
                               v1.med = length(which(car_t1_unacc[,1] == "med")), v1.low = length(which(car_t1_unacc[,1] == "low")),
                               v2.vhigh = length(which(car_t1_unacc[,2] == "vhigh")),v2.high = length(which(car_t1_unacc[,2] == "high")),
                               v2.med = length(which(car_t1_unacc[,2] == "med")), v2.low = length(which(car_t1_unacc[,2] == "low")),
                               v3.5more = length(which(car_t1_unacc[,3] == "5more")),v3.4 = length(which(car_t1_unacc[,3] == "4")),
                               v3.3 = length(which(car_t1_unacc[,3] == "3")), v3.2 = length(which(car_t1_unacc[,3] == "2")),
                               v4.more = length(which(car_t1_unacc[,4] == "more")),v4.4 = length(which(car_t1_unacc[,4] == "4")),
                               v4.2 = length(which(car_t1_unacc[,4] == "2")),
                               v5.big = length(which(car_t1_unacc[,5] == "big")),v5.med = length(which(car_t1_unacc[,5] == "med")),
                               v5.small = length(which(car_t1_unacc[,5] == "small")),
                               v6.high = length(which(car_t1_unacc[,6] == "high")),v6.med = length(which(car_t1_unacc[,6] == "med")),
                               v6.low = length(which(car_t1_unacc[,6] == "low")))
car_t1_unaccp_d1[2,] <- data.frame(t(rep(nrow(car_t1_unacc),21)))
car_t1_unaccp_d1[3,] <- (car_t1_unaccp_d1[1,] + 1)/(car_t1_unaccp_d1[2,] + 4)
#acceptable
car_t1_accp_d1 <- data.frame(v1.vhigh = length(which(car_t1_acc[,1] == "vhigh")),v1.high = length(which(car_t1_acc[,1] == "high")),
                             v1.med = length(which(car_t1_acc[,1] == "med")), v1.low = length(which(car_t1_acc[,1] == "low")),
                             v2.vhigh = length(which(car_t1_acc[,2] == "vhigh")),v2.high = length(which(car_t1_acc[,2] == "high")),
                             v2.med = length(which(car_t1_acc[,2] == "med")), v2.low = length(which(car_t1_acc[,2] == "low")),
                             v3.5more = length(which(car_t1_acc[,3] == "5more")),v3.4 = length(which(car_t1_acc[,3] == "4")),
                             v3.3 = length(which(car_t1_acc[,3] == "3")), v3.2 = length(which(car_t1_acc[,3] == "2")),
                             v4.more = length(which(car_t1_acc[,4] == "more")),v4.4 = length(which(car_t1_acc[,4] == "4")),
                             v4.2 = length(which(car_t1_acc[,4] == "2")),
                             v5.big = length(which(car_t1_acc[,5] == "big")),v5.med = length(which(car_t1_acc[,5] == "med")),
                             v5.small = length(which(car_t1_acc[,5] == "small")),
                             v6.high = length(which(car_t1_acc[,6] == "high")),v6.med = length(which(car_t1_acc[,6] == "med")),
                             v6.low = length(which(car_t1_acc[,6] == "low")))
car_t1_accp_d1[2,] <- data.frame(t(rep(nrow(car_t1_acc),21)))
car_t1_accp_d1[3,] <- (car_t1_accp_d1[1,] + 1)/(car_t1_accp_d1[2,] + 4)
#good
car_t1_goodp_d1 <- data.frame(v1.vhigh = length(which(car_t1_good[,1] == "vhigh")),v1.high = length(which(car_t1_good[,1] == "high")),
                              v1.med = length(which(car_t1_good[,1] == "med")), v1.low = length(which(car_t1_good[,1] == "low")),
                              v2.vhigh = length(which(car_t1_good[,2] == "vhigh")),v2.high = length(which(car_t1_good[,2] == "high")),
                              v2.med = length(which(car_t1_good[,2] == "med")), v2.low = length(which(car_t1_good[,2] == "low")),
                              v3.5more = length(which(car_t1_good[,3] == "5more")),v3.4 = length(which(car_t1_good[,3] == "4")),
                              v3.3 = length(which(car_t1_good[,3] == "3")), v3.2 = length(which(car_t1_good[,3] == "2")),
                              v4.more = length(which(car_t1_good[,4] == "more")),v4.4 = length(which(car_t1_good[,4] == "4")),
                              v4.2 = length(which(car_t1_good[,4] == "2")),
                              v5.big = length(which(car_t1_good[,5] == "big")),v5.med = length(which(car_t1_good[,5] == "med")),
                              v5.small = length(which(car_t1_good[,5] == "small")),
                              v6.high = length(which(car_t1_good[,6] == "high")),v6.med = length(which(car_t1_good[,6] == "med")),
                              v6.low = length(which(car_t1_good[,6] == "low")))
car_t1_goodp_d1[2,] <- data.frame(t(rep(nrow(car_t1_good),21)))
car_t1_goodp_d1[3,] <- (car_t1_goodp_d1[1,] + 1)/(car_t1_goodp_d1[2,] + 4)
#vgood
car_t1_vgoodp_d1 <- data.frame(v1.vhigh = length(which(car_t1_vgood[,1] == "vhigh")),v1.high = length(which(car_t1_vgood[,1] == "high")),
                               v1.med = length(which(car_t1_vgood[,1] == "med")), v1.low = length(which(car_t1_vgood[,1] == "low")),
                               v2.vhigh = length(which(car_t1_vgood[,2] == "vhigh")),v2.high = length(which(car_t1_vgood[,2] == "high")),
                               v2.med = length(which(car_t1_vgood[,2] == "med")), v2.low = length(which(car_t1_vgood[,2] == "low")),
                               v3.5more = length(which(car_t1_vgood[,3] == "5more")),v3.4 = length(which(car_t1_vgood[,3] == "4")),
                               v3.3 = length(which(car_t1_vgood[,3] == "3")), v3.2 = length(which(car_t1_vgood[,3] == "2")),
                               v4.more = length(which(car_t1_vgood[,4] == "more")),v4.4 = length(which(car_t1_vgood[,4] == "4")),
                               v4.2 = length(which(car_t1_vgood[,4] == "2")),
                               v5.big = length(which(car_t1_vgood[,5] == "big")),v5.med = length(which(car_t1_vgood[,5] == "med")),
                               v5.small = length(which(car_t1_vgood[,5] == "small")),
                               v6.high = length(which(car_t1_vgood[,6] == "high")),v6.med = length(which(car_t1_vgood[,6] == "med")),
                               v6.low = length(which(car_t1_vgood[,6] == "low")))
car_t1_vgoodp_d1[2,] <- data.frame(t(rep(nrow(car_t1_vgood),21)))
car_t1_vgoodp_d1[3,] <- (car_t1_vgoodp_d1[1,] + 1)/(car_t1_vgoodp_d1[2,] + 4)

prob_mat_car1 <- matrix(NA,nrow=nrow(car_data_test1),ncol=4)
#Checking on test dataset
for (i in 1:nrow(car_data_test1)){
  flags <- character()
  flags[1] <-paste("v1.",names(table(car_data_test1[i,1]))[which.max(table(car_data_test1[i,1]))],sep = "")
  flags[2] <-paste("v2.",names(table(car_data_test1[i,2]))[which.max(table(car_data_test1[i,2]))],sep = "")
  flags[3] <-paste("v3.",names(table(car_data_test1[i,3]))[which.max(table(car_data_test1[i,3]))],sep = "")
  flags[4] <-paste("v4.",names(table(car_data_test1[i,4]))[which.max(table(car_data_test1[i,4]))],sep = "")
  flags[5] <-paste("v5.",names(table(car_data_test1[i,5]))[which.max(table(car_data_test1[i,5]))],sep = "")
  flags[6] <-paste("v6.",names(table(car_data_test1[i,6]))[which.max(table(car_data_test1[i,6]))],sep = "")
  prob_mat_car1[i,1] <- (nrow(car_t1_unaccp_d1)/nrow(car_data_train1))*
    prod(car_t1_unaccp_d1[3,grep(flags[1],colnames(car_t1_unaccp_d1))],car_t1_unaccp_d1[3,grep(flags[2],colnames(car_t1_unaccp_d1))],
         car_t1_unaccp_d1[3,grep(flags[3],colnames(car_t1_unaccp_d1))],car_t1_unaccp_d1[3,grep(flags[4],colnames(car_t1_unaccp_d1))],
         car_t1_unaccp_d1[3,grep(flags[5],colnames(car_t1_unaccp_d1))],car_t1_unaccp_d1[3,grep(flags[6],colnames(car_t1_unaccp_d1))])
  prob_mat_car1[i,2] <- (nrow(car_t1_accp_d1)/nrow(car_data_train1))*
    prod(car_t1_accp_d1[3,grep(flags[1],colnames(car_t1_accp_d1))],car_t1_accp_d1[3,grep(flags[2],colnames(car_t1_accp_d1))],
         car_t1_accp_d1[3,grep(flags[3],colnames(car_t1_accp_d1))],car_t1_accp_d1[3,grep(flags[4],colnames(car_t1_accp_d1))],
         car_t1_accp_d1[3,grep(flags[5],colnames(car_t1_accp_d1))],car_t1_accp_d1[3,grep(flags[6],colnames(car_t1_accp_d1))])
  prob_mat_car1[i,3] <- (nrow(car_t1_goodp_d1)/nrow(car_data_train1))*
    prod(car_t1_goodp_d1[3,grep(flags[1],colnames(car_t1_goodp_d1))],car_t1_goodp_d1[3,grep(flags[2],colnames(car_t1_goodp_d1))],
         car_t1_goodp_d1[3,grep(flags[3],colnames(car_t1_goodp_d1))],car_t1_goodp_d1[3,grep(flags[4],colnames(car_t1_goodp_d1))],
         car_t1_goodp_d1[3,grep(flags[5],colnames(car_t1_goodp_d1))],car_t1_goodp_d1[3,grep(flags[6],colnames(car_t1_goodp_d1))])
  prob_mat_car1[i,4] <- (nrow(car_t1_vgoodp_d1)/nrow(car_data_train1))*
    prod(car_t1_vgoodp_d1[3,grep(flags[1],colnames(car_t1_vgoodp_d1))],car_t1_vgoodp_d1[3,grep(flags[2],colnames(car_t1_vgoodp_d1))],
         car_t1_vgoodp_d1[3,grep(flags[3],colnames(car_t1_vgoodp_d1))],car_t1_vgoodp_d1[3,grep(flags[4],colnames(car_t1_vgoodp_d1))],
         car_t1_vgoodp_d1[3,grep(flags[5],colnames(car_t1_vgoodp_d1))],car_t1_vgoodp_d1[3,grep(flags[6],colnames(car_t1_vgoodp_d1))])
  
}

assign_mat_car1 <- apply(prob_mat_car1, 1, which.max)
assign_mat_car1 <-replace(assign_mat_car1,assign_mat_car1 == 1,"unacc")
assign_mat_car1 <-replace(assign_mat_car1,assign_mat_car1 == "2","acc")
assign_mat_car1 <-replace(assign_mat_car1,assign_mat_car1 == "3","good")
assign_mat_car1 <-replace(assign_mat_car1,assign_mat_car1 == "4","vgood")
error_car_train1 <- mean(assign_mat_car1 != as.character(car_data_test1[,7]))
#0.2144928

#Using R package
nb_car_1 <- naiveBayes(car_data_train1[,7]~. , data=car_data_train1[,-7], laplace=1)
pred_car_1 <- predict(nb_car_1, car_data_test1[,-7], type="class")
error_carr_t1 <-mean(pred_car_1 != as.character(car_data_test1[,7]))
#0.1884058

#***************************Test dataset 2****************************
#Calculating probability matrices for different classes
car_t2_unacc <- car_data_train2[car_data_train2[,7] == "unacc",]
car_t2_acc <- car_data_train2[car_data_train2[,7] == "acc",]
car_t2_good <- car_data_train2[car_data_train2[,7] == "good",]
car_t2_vgood <- car_data_train2[car_data_train2[,7] == "vgood",]

#unacceptable
car_t2_unaccp_d1 <- data.frame(v1.vhigh = length(which(car_t2_unacc[,1] == "vhigh")),v1.high = length(which(car_t2_unacc[,1] == "high")),
                               v1.med = length(which(car_t2_unacc[,1] == "med")), v1.low = length(which(car_t2_unacc[,1] == "low")),
                               v2.vhigh = length(which(car_t2_unacc[,2] == "vhigh")),v2.high = length(which(car_t2_unacc[,2] == "high")),
                               v2.med = length(which(car_t2_unacc[,2] == "med")), v2.low = length(which(car_t2_unacc[,2] == "low")),
                               v3.5more = length(which(car_t2_unacc[,3] == "5more")),v3.4 = length(which(car_t2_unacc[,3] == "4")),
                               v3.3 = length(which(car_t2_unacc[,3] == "3")), v3.2 = length(which(car_t2_unacc[,3] == "2")),
                               v4.more = length(which(car_t2_unacc[,4] == "more")),v4.4 = length(which(car_t2_unacc[,4] == "4")),
                               v4.2 = length(which(car_t2_unacc[,4] == "2")),
                               v5.big = length(which(car_t2_unacc[,5] == "big")),v5.med = length(which(car_t2_unacc[,5] == "med")),
                               v5.small = length(which(car_t2_unacc[,5] == "small")),
                               v6.high = length(which(car_t2_unacc[,6] == "high")),v6.med = length(which(car_t2_unacc[,6] == "med")),
                               v6.low = length(which(car_t2_unacc[,6] == "low")))
car_t2_unaccp_d1[2,] <- data.frame(t(rep(nrow(car_t2_unacc),21)))
car_t2_unaccp_d1[3,] <- (car_t2_unaccp_d1[1,] + 1)/(car_t2_unaccp_d1[2,] + 4)
#acceptable
car_t2_accp_d1 <- data.frame(v1.vhigh = length(which(car_t2_acc[,1] == "vhigh")),v1.high = length(which(car_t2_acc[,1] == "high")),
                             v1.med = length(which(car_t2_acc[,1] == "med")), v1.low = length(which(car_t2_acc[,1] == "low")),
                             v2.vhigh = length(which(car_t2_acc[,2] == "vhigh")),v2.high = length(which(car_t2_acc[,2] == "high")),
                             v2.med = length(which(car_t2_acc[,2] == "med")), v2.low = length(which(car_t2_acc[,2] == "low")),
                             v3.5more = length(which(car_t2_acc[,3] == "5more")),v3.4 = length(which(car_t2_acc[,3] == "4")),
                             v3.3 = length(which(car_t2_acc[,3] == "3")), v3.2 = length(which(car_t2_acc[,3] == "2")),
                             v4.more = length(which(car_t2_acc[,4] == "more")),v4.4 = length(which(car_t2_acc[,4] == "4")),
                             v4.2 = length(which(car_t2_acc[,4] == "2")),
                             v5.big = length(which(car_t2_acc[,5] == "big")),v5.med = length(which(car_t2_acc[,5] == "med")),
                             v5.small = length(which(car_t2_acc[,5] == "small")),
                             v6.high = length(which(car_t2_acc[,6] == "high")),v6.med = length(which(car_t2_acc[,6] == "med")),
                             v6.low = length(which(car_t2_acc[,6] == "low")))
car_t2_accp_d1[2,] <- data.frame(t(rep(nrow(car_t2_acc),21)))
car_t2_accp_d1[3,] <- (car_t2_accp_d1[1,] + 1)/(car_t2_accp_d1[2,] + 4)
#good
car_t2_goodp_d1 <- data.frame(v1.vhigh = length(which(car_t2_good[,1] == "vhigh")),v1.high = length(which(car_t2_good[,1] == "high")),
                              v1.med = length(which(car_t2_good[,1] == "med")), v1.low = length(which(car_t2_good[,1] == "low")),
                              v2.vhigh = length(which(car_t2_good[,2] == "vhigh")),v2.high = length(which(car_t2_good[,2] == "high")),
                              v2.med = length(which(car_t2_good[,2] == "med")), v2.low = length(which(car_t2_good[,2] == "low")),
                              v3.5more = length(which(car_t2_good[,3] == "5more")),v3.4 = length(which(car_t2_good[,3] == "4")),
                              v3.3 = length(which(car_t2_good[,3] == "3")), v3.2 = length(which(car_t2_good[,3] == "2")),
                              v4.more = length(which(car_t2_good[,4] == "more")),v4.4 = length(which(car_t2_good[,4] == "4")),
                              v4.2 = length(which(car_t2_good[,4] == "2")),
                              v5.big = length(which(car_t2_good[,5] == "big")),v5.med = length(which(car_t2_good[,5] == "med")),
                              v5.small = length(which(car_t2_good[,5] == "small")),
                              v6.high = length(which(car_t2_good[,6] == "high")),v6.med = length(which(car_t2_good[,6] == "med")),
                              v6.low = length(which(car_t2_good[,6] == "low")))
car_t2_goodp_d1[2,] <- data.frame(t(rep(nrow(car_t2_good),21)))
car_t2_goodp_d1[3,] <- (car_t2_goodp_d1[1,] + 1)/(car_t2_goodp_d1[2,] + 4)
#vgood
car_t2_vgoodp_d1 <- data.frame(v1.vhigh = length(which(car_t2_vgood[,1] == "vhigh")),v1.high = length(which(car_t2_vgood[,1] == "high")),
                               v1.med = length(which(car_t2_vgood[,1] == "med")), v1.low = length(which(car_t2_vgood[,1] == "low")),
                               v2.vhigh = length(which(car_t2_vgood[,2] == "vhigh")),v2.high = length(which(car_t2_vgood[,2] == "high")),
                               v2.med = length(which(car_t2_vgood[,2] == "med")), v2.low = length(which(car_t2_vgood[,2] == "low")),
                               v3.5more = length(which(car_t2_vgood[,3] == "5more")),v3.4 = length(which(car_t2_vgood[,3] == "4")),
                               v3.3 = length(which(car_t2_vgood[,3] == "3")), v3.2 = length(which(car_t2_vgood[,3] == "2")),
                               v4.more = length(which(car_t2_vgood[,4] == "more")),v4.4 = length(which(car_t2_vgood[,4] == "4")),
                               v4.2 = length(which(car_t2_vgood[,4] == "2")),
                               v5.big = length(which(car_t2_vgood[,5] == "big")),v5.med = length(which(car_t2_vgood[,5] == "med")),
                               v5.small = length(which(car_t2_vgood[,5] == "small")),
                               v6.high = length(which(car_t2_vgood[,6] == "high")),v6.med = length(which(car_t2_vgood[,6] == "med")),
                               v6.low = length(which(car_t2_vgood[,6] == "low")))
car_t2_vgoodp_d1[2,] <- data.frame(t(rep(nrow(car_t2_vgood),21)))
car_t2_vgoodp_d1[3,] <- (car_t2_vgoodp_d1[1,] + 1)/(car_t2_vgoodp_d1[2,] + 4)

prob_mat_car2 <- matrix(NA,nrow=nrow(car_data_test2),ncol=4)
#Checking on test dataset
for (i in 1:nrow(car_data_test2)){
  flags <- character()
  flags[1] <-paste("v1.",names(table(car_data_test2[i,1]))[which.max(table(car_data_test2[i,1]))],sep = "")
  flags[2] <-paste("v2.",names(table(car_data_test2[i,2]))[which.max(table(car_data_test2[i,2]))],sep = "")
  flags[3] <-paste("v3.",names(table(car_data_test2[i,3]))[which.max(table(car_data_test2[i,3]))],sep = "")
  flags[4] <-paste("v4.",names(table(car_data_test2[i,4]))[which.max(table(car_data_test2[i,4]))],sep = "")
  flags[5] <-paste("v5.",names(table(car_data_test2[i,5]))[which.max(table(car_data_test2[i,5]))],sep = "")
  flags[6] <-paste("v6.",names(table(car_data_test2[i,6]))[which.max(table(car_data_test2[i,6]))],sep = "")
  prob_mat_car2[i,1] <- (nrow(car_t2_unaccp_d1)/nrow(car_data_train2))*
    prod(car_t2_unaccp_d1[3,grep(flags[1],colnames(car_t2_unaccp_d1))],car_t2_unaccp_d1[3,grep(flags[2],colnames(car_t2_unaccp_d1))],
         car_t2_unaccp_d1[3,grep(flags[3],colnames(car_t2_unaccp_d1))],car_t2_unaccp_d1[3,grep(flags[4],colnames(car_t2_unaccp_d1))],
         car_t2_unaccp_d1[3,grep(flags[5],colnames(car_t2_unaccp_d1))],car_t2_unaccp_d1[3,grep(flags[6],colnames(car_t2_unaccp_d1))])
  prob_mat_car2[i,2] <- (nrow(car_t2_accp_d1)/nrow(car_data_train2))*
    prod(car_t2_accp_d1[3,grep(flags[1],colnames(car_t2_accp_d1))],car_t2_accp_d1[3,grep(flags[2],colnames(car_t2_accp_d1))],
         car_t2_accp_d1[3,grep(flags[3],colnames(car_t2_accp_d1))],car_t2_accp_d1[3,grep(flags[4],colnames(car_t2_accp_d1))],
         car_t2_accp_d1[3,grep(flags[5],colnames(car_t2_accp_d1))],car_t2_accp_d1[3,grep(flags[6],colnames(car_t2_accp_d1))])
  prob_mat_car2[i,3] <- (nrow(car_t2_goodp_d1)/nrow(car_data_train2))*
    prod(car_t2_goodp_d1[3,grep(flags[1],colnames(car_t2_goodp_d1))],car_t2_goodp_d1[3,grep(flags[2],colnames(car_t2_goodp_d1))],
         car_t2_goodp_d1[3,grep(flags[3],colnames(car_t2_goodp_d1))],car_t2_goodp_d1[3,grep(flags[4],colnames(car_t2_goodp_d1))],
         car_t2_goodp_d1[3,grep(flags[5],colnames(car_t2_goodp_d1))],car_t2_goodp_d1[3,grep(flags[6],colnames(car_t2_goodp_d1))])
  prob_mat_car2[i,4] <- (nrow(car_t2_vgoodp_d1)/nrow(car_data_train2))*
    prod(car_t2_vgoodp_d1[3,grep(flags[1],colnames(car_t2_vgoodp_d1))],car_t2_vgoodp_d1[3,grep(flags[2],colnames(car_t2_vgoodp_d1))],
         car_t2_vgoodp_d1[3,grep(flags[3],colnames(car_t2_vgoodp_d1))],car_t2_vgoodp_d1[3,grep(flags[4],colnames(car_t2_vgoodp_d1))],
         car_t2_vgoodp_d1[3,grep(flags[5],colnames(car_t2_vgoodp_d1))],car_t2_vgoodp_d1[3,grep(flags[6],colnames(car_t2_vgoodp_d1))])
  
}

assign_mat_car2 <- apply(prob_mat_car2, 1, which.max)
assign_mat_car2 <-replace(assign_mat_car2,assign_mat_car2 == 1,"unacc")
assign_mat_car2 <-replace(assign_mat_car2,assign_mat_car2 == "2","acc")
assign_mat_car2 <-replace(assign_mat_car2,assign_mat_car2 == "3","good")
assign_mat_car2 <-replace(assign_mat_car2,assign_mat_car2 == "4","vgood")
error_car_train2 <- mean(assign_mat_car2 != as.character(car_data_test2[,7]))
#0.1797101

#Using R package
nb_car_2 <- naiveBayes(car_data_train2[,7]~. , data=car_data_train2[,-7], laplace=1)
pred_car_2 <- predict(nb_car_2, car_data_test2[,-7], type="class")
error_carr_t2 <-mean(pred_car_2 != as.character(car_data_test2[,7]))
#0.1304348

#***************************Test dataset 3****************************
#Calculating probability matrices for different classes
car_t3_unacc <- car_data_train3[car_data_train3[,7] == "unacc",]
car_t3_acc <- car_data_train3[car_data_train3[,7] == "acc",]
car_t3_good <- car_data_train3[car_data_train3[,7] == "good",]
car_t3_vgood <- car_data_train3[car_data_train3[,7] == "vgood",]

#unacceptable
car_t3_unaccp_d1 <- data.frame(v1.vhigh = length(which(car_t3_unacc[,1] == "vhigh")),v1.high = length(which(car_t3_unacc[,1] == "high")),
                               v1.med = length(which(car_t3_unacc[,1] == "med")), v1.low = length(which(car_t3_unacc[,1] == "low")),
                               v2.vhigh = length(which(car_t3_unacc[,2] == "vhigh")),v2.high = length(which(car_t3_unacc[,2] == "high")),
                               v2.med = length(which(car_t3_unacc[,2] == "med")), v2.low = length(which(car_t3_unacc[,2] == "low")),
                               v3.5more = length(which(car_t3_unacc[,3] == "5more")),v3.4 = length(which(car_t3_unacc[,3] == "4")),
                               v3.3 = length(which(car_t3_unacc[,3] == "3")), v3.2 = length(which(car_t3_unacc[,3] == "2")),
                               v4.more = length(which(car_t3_unacc[,4] == "more")),v4.4 = length(which(car_t3_unacc[,4] == "4")),
                               v4.2 = length(which(car_t3_unacc[,4] == "2")),
                               v5.big = length(which(car_t3_unacc[,5] == "big")),v5.med = length(which(car_t3_unacc[,5] == "med")),
                               v5.small = length(which(car_t3_unacc[,5] == "small")),
                               v6.high = length(which(car_t3_unacc[,6] == "high")),v6.med = length(which(car_t3_unacc[,6] == "med")),
                               v6.low = length(which(car_t3_unacc[,6] == "low")))
car_t3_unaccp_d1[2,] <- data.frame(t(rep(nrow(car_t3_unacc),21)))
car_t3_unaccp_d1[3,] <- (car_t3_unaccp_d1[1,] + 1)/(car_t3_unaccp_d1[2,] + 4)
#acceptable
car_t3_accp_d1 <- data.frame(v1.vhigh = length(which(car_t3_acc[,1] == "vhigh")),v1.high = length(which(car_t3_acc[,1] == "high")),
                             v1.med = length(which(car_t3_acc[,1] == "med")), v1.low = length(which(car_t3_acc[,1] == "low")),
                             v2.vhigh = length(which(car_t3_acc[,2] == "vhigh")),v2.high = length(which(car_t3_acc[,2] == "high")),
                             v2.med = length(which(car_t3_acc[,2] == "med")), v2.low = length(which(car_t3_acc[,2] == "low")),
                             v3.5more = length(which(car_t3_acc[,3] == "5more")),v3.4 = length(which(car_t3_acc[,3] == "4")),
                             v3.3 = length(which(car_t3_acc[,3] == "3")), v3.2 = length(which(car_t3_acc[,3] == "2")),
                             v4.more = length(which(car_t3_acc[,4] == "more")),v4.4 = length(which(car_t3_acc[,4] == "4")),
                             v4.2 = length(which(car_t3_acc[,4] == "2")),
                             v5.big = length(which(car_t3_acc[,5] == "big")),v5.med = length(which(car_t3_acc[,5] == "med")),
                             v5.small = length(which(car_t3_acc[,5] == "small")),
                             v6.high = length(which(car_t3_acc[,6] == "high")),v6.med = length(which(car_t3_acc[,6] == "med")),
                             v6.low = length(which(car_t3_acc[,6] == "low")))
car_t3_accp_d1[2,] <- data.frame(t(rep(nrow(car_t3_acc),21)))
car_t3_accp_d1[3,] <- (car_t3_accp_d1[1,] + 1)/(car_t3_accp_d1[2,] + 4)
#good
car_t3_goodp_d1 <- data.frame(v1.vhigh = length(which(car_t3_good[,1] == "vhigh")),v1.high = length(which(car_t3_good[,1] == "high")),
                              v1.med = length(which(car_t3_good[,1] == "med")), v1.low = length(which(car_t3_good[,1] == "low")),
                              v2.vhigh = length(which(car_t3_good[,2] == "vhigh")),v2.high = length(which(car_t3_good[,2] == "high")),
                              v2.med = length(which(car_t3_good[,2] == "med")), v2.low = length(which(car_t3_good[,2] == "low")),
                              v3.5more = length(which(car_t3_good[,3] == "5more")),v3.4 = length(which(car_t3_good[,3] == "4")),
                              v3.3 = length(which(car_t3_good[,3] == "3")), v3.2 = length(which(car_t3_good[,3] == "2")),
                              v4.more = length(which(car_t3_good[,4] == "more")),v4.4 = length(which(car_t3_good[,4] == "4")),
                              v4.2 = length(which(car_t3_good[,4] == "2")),
                              v5.big = length(which(car_t3_good[,5] == "big")),v5.med = length(which(car_t3_good[,5] == "med")),
                              v5.small = length(which(car_t3_good[,5] == "small")),
                              v6.high = length(which(car_t3_good[,6] == "high")),v6.med = length(which(car_t3_good[,6] == "med")),
                              v6.low = length(which(car_t3_good[,6] == "low")))
car_t3_goodp_d1[2,] <- data.frame(t(rep(nrow(car_t3_good),21)))
car_t3_goodp_d1[3,] <- (car_t3_goodp_d1[1,] + 1)/(car_t3_goodp_d1[2,] + 4)
#vgood
car_t3_vgoodp_d1 <- data.frame(v1.vhigh = length(which(car_t3_vgood[,1] == "vhigh")),v1.high = length(which(car_t3_vgood[,1] == "high")),
                               v1.med = length(which(car_t3_vgood[,1] == "med")), v1.low = length(which(car_t3_vgood[,1] == "low")),
                               v2.vhigh = length(which(car_t3_vgood[,2] == "vhigh")),v2.high = length(which(car_t3_vgood[,2] == "high")),
                               v2.med = length(which(car_t3_vgood[,2] == "med")), v2.low = length(which(car_t3_vgood[,2] == "low")),
                               v3.5more = length(which(car_t3_vgood[,3] == "5more")),v3.4 = length(which(car_t3_vgood[,3] == "4")),
                               v3.3 = length(which(car_t3_vgood[,3] == "3")), v3.2 = length(which(car_t3_vgood[,3] == "2")),
                               v4.more = length(which(car_t3_vgood[,4] == "more")),v4.4 = length(which(car_t3_vgood[,4] == "4")),
                               v4.2 = length(which(car_t3_vgood[,4] == "2")),
                               v5.big = length(which(car_t3_vgood[,5] == "big")),v5.med = length(which(car_t3_vgood[,5] == "med")),
                               v5.small = length(which(car_t3_vgood[,5] == "small")),
                               v6.high = length(which(car_t3_vgood[,6] == "high")),v6.med = length(which(car_t3_vgood[,6] == "med")),
                               v6.low = length(which(car_t3_vgood[,6] == "low")))
car_t3_vgoodp_d1[2,] <- data.frame(t(rep(nrow(car_t3_vgood),21)))
car_t3_vgoodp_d1[3,] <- (car_t3_vgoodp_d1[1,] + 1)/(car_t3_vgoodp_d1[2,] + 4)

prob_mat_car3 <- matrix(NA,nrow=nrow(car_data_test3),ncol=4)
#Checking on test dataset
for (i in 1:nrow(car_data_test3)){
  flags <- character()
  flags[1] <-paste("v1.",names(table(car_data_test3[i,1]))[which.max(table(car_data_test3[i,1]))],sep = "")
  flags[2] <-paste("v2.",names(table(car_data_test3[i,2]))[which.max(table(car_data_test3[i,2]))],sep = "")
  flags[3] <-paste("v3.",names(table(car_data_test3[i,3]))[which.max(table(car_data_test3[i,3]))],sep = "")
  flags[4] <-paste("v4.",names(table(car_data_test3[i,4]))[which.max(table(car_data_test3[i,4]))],sep = "")
  flags[5] <-paste("v5.",names(table(car_data_test3[i,5]))[which.max(table(car_data_test3[i,5]))],sep = "")
  flags[6] <-paste("v6.",names(table(car_data_test3[i,6]))[which.max(table(car_data_test3[i,6]))],sep = "")
  prob_mat_car3[i,1] <- (nrow(car_t3_unaccp_d1)/nrow(car_data_train3))*
    prod(car_t3_unaccp_d1[3,grep(flags[1],colnames(car_t3_unaccp_d1))],car_t3_unaccp_d1[3,grep(flags[2],colnames(car_t3_unaccp_d1))],
         car_t3_unaccp_d1[3,grep(flags[3],colnames(car_t3_unaccp_d1))],car_t3_unaccp_d1[3,grep(flags[4],colnames(car_t3_unaccp_d1))],
         car_t3_unaccp_d1[3,grep(flags[5],colnames(car_t3_unaccp_d1))],car_t3_unaccp_d1[3,grep(flags[6],colnames(car_t3_unaccp_d1))])
  prob_mat_car3[i,2] <- (nrow(car_t3_accp_d1)/nrow(car_data_train3))*
    prod(car_t3_accp_d1[3,grep(flags[1],colnames(car_t3_accp_d1))],car_t3_accp_d1[3,grep(flags[2],colnames(car_t3_accp_d1))],
         car_t3_accp_d1[3,grep(flags[3],colnames(car_t3_accp_d1))],car_t3_accp_d1[3,grep(flags[4],colnames(car_t3_accp_d1))],
         car_t3_accp_d1[3,grep(flags[5],colnames(car_t3_accp_d1))],car_t3_accp_d1[3,grep(flags[6],colnames(car_t3_accp_d1))])
  prob_mat_car3[i,3] <- (nrow(car_t3_goodp_d1)/nrow(car_data_train3))*
    prod(car_t3_goodp_d1[3,grep(flags[1],colnames(car_t3_goodp_d1))],car_t3_goodp_d1[3,grep(flags[2],colnames(car_t3_goodp_d1))],
         car_t3_goodp_d1[3,grep(flags[3],colnames(car_t3_goodp_d1))],car_t3_goodp_d1[3,grep(flags[4],colnames(car_t3_goodp_d1))],
         car_t3_goodp_d1[3,grep(flags[5],colnames(car_t3_goodp_d1))],car_t3_goodp_d1[3,grep(flags[6],colnames(car_t3_goodp_d1))])
  prob_mat_car3[i,4] <- (nrow(car_t3_vgoodp_d1)/nrow(car_data_train3))*
    prod(car_t3_vgoodp_d1[3,grep(flags[1],colnames(car_t3_vgoodp_d1))],car_t3_vgoodp_d1[3,grep(flags[2],colnames(car_t3_vgoodp_d1))],
         car_t3_vgoodp_d1[3,grep(flags[3],colnames(car_t3_vgoodp_d1))],car_t3_vgoodp_d1[3,grep(flags[4],colnames(car_t3_vgoodp_d1))],
         car_t3_vgoodp_d1[3,grep(flags[5],colnames(car_t3_vgoodp_d1))],car_t3_vgoodp_d1[3,grep(flags[6],colnames(car_t3_vgoodp_d1))])
  
}

assign_mat_car3 <- apply(prob_mat_car3, 1, which.max)
assign_mat_car3 <-replace(assign_mat_car3,assign_mat_car3 == 1,"unacc")
assign_mat_car3 <-replace(assign_mat_car3,assign_mat_car3 == "2","acc")
assign_mat_car3 <-replace(assign_mat_car3,assign_mat_car3 == "3","good")
assign_mat_car3 <-replace(assign_mat_car3,assign_mat_car3 == "4","vgood")
error_car_train3 <- mean(assign_mat_car3 != as.character(car_data_test3[,7]))
#0.1942029

#Using R package
nb_car_3 <- naiveBayes(car_data_train3[,7]~. , data=car_data_train3[,-7], laplace = 1)
pred_car_3 <- predict(nb_car_3, car_data_test3[,-7], type="class")
error_carr_t3 <-mean(pred_car_3 != as.character(car_data_test3[,7]))
#0.1275362

#***************************Test dataset 4****************************
#Calculating probability matrices for different classes
car_t4_unacc <- car_data_train4[car_data_train4[,7] == "unacc",]
car_t4_acc <- car_data_train4[car_data_train4[,7] == "acc",]
car_t4_good <- car_data_train4[car_data_train4[,7] == "good",]
car_t4_vgood <- car_data_train4[car_data_train4[,7] == "vgood",]

#unacceptable
car_t4_unaccp_d1 <- data.frame(v1.vhigh = length(which(car_t4_unacc[,1] == "vhigh")),v1.high = length(which(car_t4_unacc[,1] == "high")),
                               v1.med = length(which(car_t4_unacc[,1] == "med")), v1.low = length(which(car_t4_unacc[,1] == "low")),
                               v2.vhigh = length(which(car_t4_unacc[,2] == "vhigh")),v2.high = length(which(car_t4_unacc[,2] == "high")),
                               v2.med = length(which(car_t4_unacc[,2] == "med")), v2.low = length(which(car_t4_unacc[,2] == "low")),
                               v3.5more = length(which(car_t4_unacc[,3] == "5more")),v3.4 = length(which(car_t4_unacc[,3] == "4")),
                               v3.3 = length(which(car_t4_unacc[,3] == "3")), v3.2 = length(which(car_t4_unacc[,3] == "2")),
                               v4.more = length(which(car_t4_unacc[,4] == "more")),v4.4 = length(which(car_t4_unacc[,4] == "4")),
                               v4.2 = length(which(car_t4_unacc[,4] == "2")),
                               v5.big = length(which(car_t4_unacc[,5] == "big")),v5.med = length(which(car_t4_unacc[,5] == "med")),
                               v5.small = length(which(car_t4_unacc[,5] == "small")),
                               v6.high = length(which(car_t4_unacc[,6] == "high")),v6.med = length(which(car_t4_unacc[,6] == "med")),
                               v6.low = length(which(car_t4_unacc[,6] == "low")))
car_t4_unaccp_d1[2,] <- data.frame(t(rep(nrow(car_t4_unacc),21)))
car_t4_unaccp_d1[3,] <- (car_t4_unaccp_d1[1,] + 1)/(car_t4_unaccp_d1[2,] + 4)
#acceptable
car_t4_accp_d1 <- data.frame(v1.vhigh = length(which(car_t4_acc[,1] == "vhigh")),v1.high = length(which(car_t4_acc[,1] == "high")),
                             v1.med = length(which(car_t4_acc[,1] == "med")), v1.low = length(which(car_t4_acc[,1] == "low")),
                             v2.vhigh = length(which(car_t4_acc[,2] == "vhigh")),v2.high = length(which(car_t4_acc[,2] == "high")),
                             v2.med = length(which(car_t4_acc[,2] == "med")), v2.low = length(which(car_t4_acc[,2] == "low")),
                             v3.5more = length(which(car_t4_acc[,3] == "5more")),v3.4 = length(which(car_t4_acc[,3] == "4")),
                             v3.3 = length(which(car_t4_acc[,3] == "3")), v3.2 = length(which(car_t4_acc[,3] == "2")),
                             v4.more = length(which(car_t4_acc[,4] == "more")),v4.4 = length(which(car_t4_acc[,4] == "4")),
                             v4.2 = length(which(car_t4_acc[,4] == "2")),
                             v5.big = length(which(car_t4_acc[,5] == "big")),v5.med = length(which(car_t4_acc[,5] == "med")),
                             v5.small = length(which(car_t4_acc[,5] == "small")),
                             v6.high = length(which(car_t4_acc[,6] == "high")),v6.med = length(which(car_t4_acc[,6] == "med")),
                             v6.low = length(which(car_t4_acc[,6] == "low")))
car_t4_accp_d1[2,] <- data.frame(t(rep(nrow(car_t4_acc),21)))
car_t4_accp_d1[3,] <- (car_t4_accp_d1[1,] + 1)/(car_t4_accp_d1[2,] + 4)
#good
car_t4_goodp_d1 <- data.frame(v1.vhigh = length(which(car_t4_good[,1] == "vhigh")),v1.high = length(which(car_t4_good[,1] == "high")),
                              v1.med = length(which(car_t4_good[,1] == "med")), v1.low = length(which(car_t4_good[,1] == "low")),
                              v2.vhigh = length(which(car_t4_good[,2] == "vhigh")),v2.high = length(which(car_t4_good[,2] == "high")),
                              v2.med = length(which(car_t4_good[,2] == "med")), v2.low = length(which(car_t4_good[,2] == "low")),
                              v3.5more = length(which(car_t4_good[,3] == "5more")),v3.4 = length(which(car_t4_good[,3] == "4")),
                              v3.3 = length(which(car_t4_good[,3] == "3")), v3.2 = length(which(car_t4_good[,3] == "2")),
                              v4.more = length(which(car_t4_good[,4] == "more")),v4.4 = length(which(car_t4_good[,4] == "4")),
                              v4.2 = length(which(car_t4_good[,4] == "2")),
                              v5.big = length(which(car_t4_good[,5] == "big")),v5.med = length(which(car_t4_good[,5] == "med")),
                              v5.small = length(which(car_t4_good[,5] == "small")),
                              v6.high = length(which(car_t4_good[,6] == "high")),v6.med = length(which(car_t4_good[,6] == "med")),
                              v6.low = length(which(car_t4_good[,6] == "low")))
car_t4_goodp_d1[2,] <- data.frame(t(rep(nrow(car_t4_good),21)))
car_t4_goodp_d1[3,] <- (car_t4_goodp_d1[1,] + 1)/(car_t4_goodp_d1[2,] + 4)
#vgood
car_t4_vgoodp_d1 <- data.frame(v1.vhigh = length(which(car_t4_vgood[,1] == "vhigh")),v1.high = length(which(car_t4_vgood[,1] == "high")),
                               v1.med = length(which(car_t4_vgood[,1] == "med")), v1.low = length(which(car_t4_vgood[,1] == "low")),
                               v2.vhigh = length(which(car_t4_vgood[,2] == "vhigh")),v2.high = length(which(car_t4_vgood[,2] == "high")),
                               v2.med = length(which(car_t4_vgood[,2] == "med")), v2.low = length(which(car_t4_vgood[,2] == "low")),
                               v3.5more = length(which(car_t4_vgood[,3] == "5more")),v3.4 = length(which(car_t4_vgood[,3] == "4")),
                               v3.3 = length(which(car_t4_vgood[,3] == "3")), v3.2 = length(which(car_t4_vgood[,3] == "2")),
                               v4.more = length(which(car_t4_vgood[,4] == "more")),v4.4 = length(which(car_t4_vgood[,4] == "4")),
                               v4.2 = length(which(car_t4_vgood[,4] == "2")),
                               v5.big = length(which(car_t4_vgood[,5] == "big")),v5.med = length(which(car_t4_vgood[,5] == "med")),
                               v5.small = length(which(car_t4_vgood[,5] == "small")),
                               v6.high = length(which(car_t4_vgood[,6] == "high")),v6.med = length(which(car_t4_vgood[,6] == "med")),
                               v6.low = length(which(car_t4_vgood[,6] == "low")))
car_t4_vgoodp_d1[2,] <- data.frame(t(rep(nrow(car_t4_vgood),21)))
car_t4_vgoodp_d1[3,] <- (car_t4_vgoodp_d1[1,] + 1)/(car_t4_vgoodp_d1[2,] + 4)

prob_mat_car4 <- matrix(NA,nrow=nrow(car_data_test4),ncol=4)
#Checking on test dataset
for (i in 1:nrow(car_data_test4)){
  flags <- character()
  flags[1] <-paste("v1.",names(table(car_data_test4[i,1]))[which.max(table(car_data_test4[i,1]))],sep = "")
  flags[2] <-paste("v2.",names(table(car_data_test4[i,2]))[which.max(table(car_data_test4[i,2]))],sep = "")
  flags[3] <-paste("v3.",names(table(car_data_test4[i,3]))[which.max(table(car_data_test4[i,3]))],sep = "")
  flags[4] <-paste("v4.",names(table(car_data_test4[i,4]))[which.max(table(car_data_test4[i,4]))],sep = "")
  flags[5] <-paste("v5.",names(table(car_data_test4[i,5]))[which.max(table(car_data_test4[i,5]))],sep = "")
  flags[6] <-paste("v6.",names(table(car_data_test4[i,6]))[which.max(table(car_data_test4[i,6]))],sep = "")
  prob_mat_car4[i,1] <- (nrow(car_t4_unaccp_d1)/nrow(car_data_train4))*
    prod(car_t4_unaccp_d1[3,grep(flags[1],colnames(car_t4_unaccp_d1))],car_t4_unaccp_d1[3,grep(flags[2],colnames(car_t4_unaccp_d1))],
         car_t4_unaccp_d1[3,grep(flags[3],colnames(car_t4_unaccp_d1))],car_t4_unaccp_d1[3,grep(flags[4],colnames(car_t4_unaccp_d1))],
         car_t4_unaccp_d1[3,grep(flags[5],colnames(car_t4_unaccp_d1))],car_t4_unaccp_d1[3,grep(flags[6],colnames(car_t4_unaccp_d1))])
  prob_mat_car4[i,2] <- (nrow(car_t4_accp_d1)/nrow(car_data_train4))*
    prod(car_t4_accp_d1[3,grep(flags[1],colnames(car_t4_accp_d1))],car_t4_accp_d1[3,grep(flags[2],colnames(car_t4_accp_d1))],
         car_t4_accp_d1[3,grep(flags[3],colnames(car_t4_accp_d1))],car_t4_accp_d1[3,grep(flags[4],colnames(car_t4_accp_d1))],
         car_t4_accp_d1[3,grep(flags[5],colnames(car_t4_accp_d1))],car_t4_accp_d1[3,grep(flags[6],colnames(car_t4_accp_d1))])
  prob_mat_car4[i,3] <- (nrow(car_t4_goodp_d1)/nrow(car_data_train4))*
    prod(car_t4_goodp_d1[3,grep(flags[1],colnames(car_t4_goodp_d1))],car_t4_goodp_d1[3,grep(flags[2],colnames(car_t4_goodp_d1))],
         car_t4_goodp_d1[3,grep(flags[3],colnames(car_t4_goodp_d1))],car_t4_goodp_d1[3,grep(flags[4],colnames(car_t4_goodp_d1))],
         car_t4_goodp_d1[3,grep(flags[5],colnames(car_t4_goodp_d1))],car_t4_goodp_d1[3,grep(flags[6],colnames(car_t4_goodp_d1))])
  prob_mat_car4[i,4] <- (nrow(car_t4_vgoodp_d1)/nrow(car_data_train4))*
    prod(car_t4_vgoodp_d1[3,grep(flags[1],colnames(car_t4_vgoodp_d1))],car_t4_vgoodp_d1[3,grep(flags[2],colnames(car_t4_vgoodp_d1))],
         car_t4_vgoodp_d1[3,grep(flags[3],colnames(car_t4_vgoodp_d1))],car_t4_vgoodp_d1[3,grep(flags[4],colnames(car_t4_vgoodp_d1))],
         car_t4_vgoodp_d1[3,grep(flags[5],colnames(car_t4_vgoodp_d1))],car_t4_vgoodp_d1[3,grep(flags[6],colnames(car_t4_vgoodp_d1))])
  
}

assign_mat_car4 <- apply(prob_mat_car4, 1, which.max)
assign_mat_car4 <-replace(assign_mat_car4,assign_mat_car4 == 1,"unacc")
assign_mat_car4 <-replace(assign_mat_car4,assign_mat_car4 == "2","acc")
assign_mat_car4 <-replace(assign_mat_car4,assign_mat_car4 == "3","good")
assign_mat_car4 <-replace(assign_mat_car4,assign_mat_car4 == "4","vgood")
error_car_train4 <- mean(assign_mat_car4 != as.character(car_data_test4[,7]))
#0.2086957

#Using R package
nb_car_4 <- naiveBayes(car_data_train4[,7]~. , data=car_data_train4[,-7], laplace = 1)
pred_car_4 <- predict(nb_car_4, car_data_test4[,-7], type="class")
error_carr_t4 <-mean(pred_car_4 != as.character(car_data_test4[,7]))
#0.1913043

#***************************Test dataset 5****************************
#Calculating probability matrices for different classes
car_t5_unacc <- car_data_train5[car_data_train5[,7] == "unacc",]
car_t5_acc <- car_data_train5[car_data_train5[,7] == "acc",]
car_t5_good <- car_data_train5[car_data_train5[,7] == "good",]
car_t5_vgood <- car_data_train5[car_data_train5[,7] == "vgood",]

#unacceptable
car_t5_unaccp_d1 <- data.frame(v1.vhigh = length(which(car_t5_unacc[,1] == "vhigh")),v1.high = length(which(car_t5_unacc[,1] == "high")),
                               v1.med = length(which(car_t5_unacc[,1] == "med")), v1.low = length(which(car_t5_unacc[,1] == "low")),
                               v2.vhigh = length(which(car_t5_unacc[,2] == "vhigh")),v2.high = length(which(car_t5_unacc[,2] == "high")),
                               v2.med = length(which(car_t5_unacc[,2] == "med")), v2.low = length(which(car_t5_unacc[,2] == "low")),
                               v3.5more = length(which(car_t5_unacc[,3] == "5more")),v3.4 = length(which(car_t5_unacc[,3] == "4")),
                               v3.3 = length(which(car_t5_unacc[,3] == "3")), v3.2 = length(which(car_t5_unacc[,3] == "2")),
                               v4.more = length(which(car_t5_unacc[,4] == "more")),v4.4 = length(which(car_t5_unacc[,4] == "4")),
                               v4.2 = length(which(car_t5_unacc[,4] == "2")),
                               v5.big = length(which(car_t5_unacc[,5] == "big")),v5.med = length(which(car_t5_unacc[,5] == "med")),
                               v5.small = length(which(car_t5_unacc[,5] == "small")),
                               v6.high = length(which(car_t5_unacc[,6] == "high")),v6.med = length(which(car_t5_unacc[,6] == "med")),
                               v6.low = length(which(car_t5_unacc[,6] == "low")))
car_t5_unaccp_d1[2,] <- data.frame(t(rep(nrow(car_t5_unacc),21)))
car_t5_unaccp_d1[3,] <- (car_t5_unaccp_d1[1,] + 1)/(car_t5_unaccp_d1[2,] + 4)
#acceptable
car_t5_accp_d1 <- data.frame(v1.vhigh = length(which(car_t5_acc[,1] == "vhigh")),v1.high = length(which(car_t5_acc[,1] == "high")),
                             v1.med = length(which(car_t5_acc[,1] == "med")), v1.low = length(which(car_t5_acc[,1] == "low")),
                             v2.vhigh = length(which(car_t5_acc[,2] == "vhigh")),v2.high = length(which(car_t5_acc[,2] == "high")),
                             v2.med = length(which(car_t5_acc[,2] == "med")), v2.low = length(which(car_t5_acc[,2] == "low")),
                             v3.5more = length(which(car_t5_acc[,3] == "5more")),v3.4 = length(which(car_t5_acc[,3] == "4")),
                             v3.3 = length(which(car_t5_acc[,3] == "3")), v3.2 = length(which(car_t5_acc[,3] == "2")),
                             v4.more = length(which(car_t5_acc[,4] == "more")),v4.4 = length(which(car_t5_acc[,4] == "4")),
                             v4.2 = length(which(car_t5_acc[,4] == "2")),
                             v5.big = length(which(car_t5_acc[,5] == "big")),v5.med = length(which(car_t5_acc[,5] == "med")),
                             v5.small = length(which(car_t5_acc[,5] == "small")),
                             v6.high = length(which(car_t5_acc[,6] == "high")),v6.med = length(which(car_t5_acc[,6] == "med")),
                             v6.low = length(which(car_t5_acc[,6] == "low")))
car_t5_accp_d1[2,] <- data.frame(t(rep(nrow(car_t5_acc),21)))
car_t5_accp_d1[3,] <- (car_t5_accp_d1[1,] + 1)/(car_t5_accp_d1[2,] + 4)
#good
car_t5_goodp_d1 <- data.frame(v1.vhigh = length(which(car_t5_good[,1] == "vhigh")),v1.high = length(which(car_t5_good[,1] == "high")),
                              v1.med = length(which(car_t5_good[,1] == "med")), v1.low = length(which(car_t5_good[,1] == "low")),
                              v2.vhigh = length(which(car_t5_good[,2] == "vhigh")),v2.high = length(which(car_t5_good[,2] == "high")),
                              v2.med = length(which(car_t5_good[,2] == "med")), v2.low = length(which(car_t5_good[,2] == "low")),
                              v3.5more = length(which(car_t5_good[,3] == "5more")),v3.4 = length(which(car_t5_good[,3] == "4")),
                              v3.3 = length(which(car_t5_good[,3] == "3")), v3.2 = length(which(car_t5_good[,3] == "2")),
                              v4.more = length(which(car_t5_good[,4] == "more")),v4.4 = length(which(car_t5_good[,4] == "4")),
                              v4.2 = length(which(car_t5_good[,4] == "2")),
                              v5.big = length(which(car_t5_good[,5] == "big")),v5.med = length(which(car_t5_good[,5] == "med")),
                              v5.small = length(which(car_t5_good[,5] == "small")),
                              v6.high = length(which(car_t5_good[,6] == "high")),v6.med = length(which(car_t5_good[,6] == "med")),
                              v6.low = length(which(car_t5_good[,6] == "low")))
car_t5_goodp_d1[2,] <- data.frame(t(rep(nrow(car_t5_good),21)))
car_t5_goodp_d1[3,] <- (car_t5_goodp_d1[1,] + 1)/(car_t5_goodp_d1[2,] + 4)
#vgood
car_t5_vgoodp_d1 <- data.frame(v1.vhigh = length(which(car_t5_vgood[,1] == "vhigh")),v1.high = length(which(car_t5_vgood[,1] == "high")),
                               v1.med = length(which(car_t5_vgood[,1] == "med")), v1.low = length(which(car_t5_vgood[,1] == "low")),
                               v2.vhigh = length(which(car_t5_vgood[,2] == "vhigh")),v2.high = length(which(car_t5_vgood[,2] == "high")),
                               v2.med = length(which(car_t5_vgood[,2] == "med")), v2.low = length(which(car_t5_vgood[,2] == "low")),
                               v3.5more = length(which(car_t5_vgood[,3] == "5more")),v3.4 = length(which(car_t5_vgood[,3] == "4")),
                               v3.3 = length(which(car_t5_vgood[,3] == "3")), v3.2 = length(which(car_t5_vgood[,3] == "2")),
                               v4.more = length(which(car_t5_vgood[,4] == "more")),v4.4 = length(which(car_t5_vgood[,4] == "4")),
                               v4.2 = length(which(car_t5_vgood[,4] == "2")),
                               v5.big = length(which(car_t5_vgood[,5] == "big")),v5.med = length(which(car_t5_vgood[,5] == "med")),
                               v5.small = length(which(car_t5_vgood[,5] == "small")),
                               v6.high = length(which(car_t5_vgood[,6] == "high")),v6.med = length(which(car_t5_vgood[,6] == "med")),
                               v6.low = length(which(car_t5_vgood[,6] == "low")))
car_t5_vgoodp_d1[2,] <- data.frame(t(rep(nrow(car_t5_vgood),21)))
car_t5_vgoodp_d1[3,] <- (car_t5_vgoodp_d1[1,] + 1)/(car_t5_vgoodp_d1[2,] + 4)

prob_mat_car5 <- matrix(NA,nrow=nrow(car_data_test5),ncol=4)
#Checking on test dataset
for (i in 1:nrow(car_data_test5)){
  flags <- character()
  flags[1] <-paste("v1.",names(table(car_data_test5[i,1]))[which.max(table(car_data_test5[i,1]))],sep = "")
  flags[2] <-paste("v2.",names(table(car_data_test5[i,2]))[which.max(table(car_data_test5[i,2]))],sep = "")
  flags[3] <-paste("v3.",names(table(car_data_test5[i,3]))[which.max(table(car_data_test5[i,3]))],sep = "")
  flags[4] <-paste("v4.",names(table(car_data_test5[i,4]))[which.max(table(car_data_test5[i,4]))],sep = "")
  flags[5] <-paste("v5.",names(table(car_data_test5[i,5]))[which.max(table(car_data_test5[i,5]))],sep = "")
  flags[6] <-paste("v6.",names(table(car_data_test5[i,6]))[which.max(table(car_data_test5[i,6]))],sep = "")
  prob_mat_car5[i,1] <- (nrow(car_t5_unaccp_d1)/nrow(car_data_train5))*
    prod(car_t5_unaccp_d1[3,grep(flags[1],colnames(car_t5_unaccp_d1))],car_t5_unaccp_d1[3,grep(flags[2],colnames(car_t5_unaccp_d1))],
         car_t5_unaccp_d1[3,grep(flags[3],colnames(car_t5_unaccp_d1))],car_t5_unaccp_d1[3,grep(flags[4],colnames(car_t5_unaccp_d1))],
         car_t5_unaccp_d1[3,grep(flags[5],colnames(car_t5_unaccp_d1))],car_t5_unaccp_d1[3,grep(flags[6],colnames(car_t5_unaccp_d1))])
  prob_mat_car5[i,2] <- (nrow(car_t5_accp_d1)/nrow(car_data_train5))*
    prod(car_t5_accp_d1[3,grep(flags[1],colnames(car_t5_accp_d1))],car_t5_accp_d1[3,grep(flags[2],colnames(car_t5_accp_d1))],
         car_t5_accp_d1[3,grep(flags[3],colnames(car_t5_accp_d1))],car_t5_accp_d1[3,grep(flags[4],colnames(car_t5_accp_d1))],
         car_t5_accp_d1[3,grep(flags[5],colnames(car_t5_accp_d1))],car_t5_accp_d1[3,grep(flags[6],colnames(car_t5_accp_d1))])
  prob_mat_car5[i,3] <- (nrow(car_t5_goodp_d1)/nrow(car_data_train5))*
    prod(car_t5_goodp_d1[3,grep(flags[1],colnames(car_t5_goodp_d1))],car_t5_goodp_d1[3,grep(flags[2],colnames(car_t5_goodp_d1))],
         car_t5_goodp_d1[3,grep(flags[3],colnames(car_t5_goodp_d1))],car_t5_goodp_d1[3,grep(flags[4],colnames(car_t5_goodp_d1))],
         car_t5_goodp_d1[3,grep(flags[5],colnames(car_t5_goodp_d1))],car_t5_goodp_d1[3,grep(flags[6],colnames(car_t5_goodp_d1))])
  prob_mat_car5[i,4] <- (nrow(car_t5_vgoodp_d1)/nrow(car_data_train5))*
    prod(car_t5_vgoodp_d1[3,grep(flags[1],colnames(car_t5_vgoodp_d1))],car_t5_vgoodp_d1[3,grep(flags[2],colnames(car_t5_vgoodp_d1))],
         car_t5_vgoodp_d1[3,grep(flags[3],colnames(car_t5_vgoodp_d1))],car_t5_vgoodp_d1[3,grep(flags[4],colnames(car_t5_vgoodp_d1))],
         car_t5_vgoodp_d1[3,grep(flags[5],colnames(car_t5_vgoodp_d1))],car_t5_vgoodp_d1[3,grep(flags[6],colnames(car_t5_vgoodp_d1))])
  
}

assign_mat_car5 <- apply(prob_mat_car5, 1, which.max)
assign_mat_car5 <-replace(assign_mat_car5,assign_mat_car5 == 1,"unacc")
assign_mat_car5 <-replace(assign_mat_car5,assign_mat_car5 == "2","acc")
assign_mat_car5 <-replace(assign_mat_car5,assign_mat_car5 == "3","good")
assign_mat_car5 <-replace(assign_mat_car5,assign_mat_car5 == "4","vgood")
error_car_train5 <- mean(assign_mat_car5 != as.character(car_data_test5[,7]))
#0.1982759

#Using R package
nb_car_5 <- naiveBayes(car_data_train5[,7]~. , data=car_data_train5[,-7], laplace = 1)
pred_car_5 <- predict(nb_car_5, car_data_test5[,-7], type="class")
error_carr_t5 <-mean(pred_car_5 != as.character(car_data_test5[,7]))
#0.1206897

#********************************Compiling Results************************************************
error_car_nb <-c(error_car_train1,error_car_train2,error_car_train3,error_car_train4,error_car_train5)
error_car_rnb <- c(error_carr_t1,error_carr_t2,error_carr_t3,error_carr_t4,error_carr_t5)

mean(error_car_nb)
#0.1990755
mean(error_car_rnb)
#0.1516742

#Plot for error values over all test datasets
plot(c(1,2,3,4,5), error_car_nb,
     type="b", pch = 19, col="blue",frame = FALSE, 
     main = "Naive Bayes on Car Data",
     xlab="Dataset Number",
     ylab="Test errors on CV datasets")
minor.tick(nx=4)
plot(c(1,2,3,4,5), error_car_rnb,
     type="b", pch = 19, col="green",frame = FALSE, 
     main = "Naive Bayes on Car Data - R Function",
     xlab="Dataset Number",
     ylab="Test errors on CV datasets")
minor.tick(nx=4)

#***************************************************************************************************************
library(Rlab)
#************************************Credit Approval Dataset****************************************************
#Contains mixed data
#datasets already available here -- but can be obtained using read.csv from folders "Credit - Test and Train" (original data) and
#"Credit Modified - Test and Train" (using dummy data for categorical variables)
#Training using datasets with dummy data (the one without "data" in it) for categorical variables (in binary form) 
#-- Bernoulli Distribution for these, Gaussian for the continuous
#Testing on R with original dataset (no dummy variables)

ParametersCredit <- function(train){
  partition_plus <- train[train[,47] == "+",]
  partition_minus <- train[train[,47] == "-",]
  mean_train <- t(cbind(colSums(as.matrix(partition_plus[,c(1:6)]))/nrow(partition_plus),
                        colSums(as.matrix(partition_minus[,c(1:6)]))/nrow(partition_minus)))
  prior_train <- matrix(c(nrow(partition_plus)/nrow(train),nrow(partition_minus)/nrow(train)),ncol=2,nrow=1)
  indent_plus <- matrix(1,ncol=1,nrow=nrow(partition_plus))
  indent_minus <- matrix(1,ncol=1,nrow=nrow(partition_minus))
  center_plus <- as.matrix(partition_plus[,c(1:6)]) - (indent_plus %*% t(mean_train[1,]))
  center_minus <- as.matrix(partition_minus[,c(1:6)]) - (indent_minus %*% t(mean_train[2,]))
  cov_train <- list((t(center_plus) %*% center_plus)/nrow(center_plus),
                    (t(center_minus) %*% center_minus)/nrow(center_minus))
  params <- list(mean = mean_train, prior = prior_train, cov = cov_train)
  return(params)
}

BinaryPartProbs <- function(train,test){
  partition_plus <- train[train[,47] == "+",]
  partition_minus <- train[train[,47] == "-",]
  prior_train <- matrix(c(nrow(partition_plus)/nrow(train),nrow(partition_minus)/nrow(train)),ncol=2,nrow=1)
  bin_prob <- matrix(1, nrow=2, ncol=nrow(test))
  test_t <- test[,-c(1,2,3,4,5,6,47)]
  for (i in 1:nrow(test_t)){
    for (j in 1:ncol(test_t)){
      bin_prob[1,i] <- bin_prob[1,i] * dbern(test_t[i,j],prior_train[1,1])
      bin_prob[2,i] <- bin_prob[2,i] * dbern(test_t[i,j],prior_train[1,2])
    }
  }
  return(bin_prob)
}

#**********************************Test Dataset 1*******************************
prob_credit1 = matrix(NA, nrow=2, ncol=nrow(credit_test1))
parameters_t1 <- ParametersCredit(credit_train1)
bin_credit1 <- BinaryPartProbs(credit_train1,credit_test1)

for (i in c(1:2)){
  prob_credit1[i,] <-  dmvnorm(as.matrix(credit_test1[,c(1:6)]),parameters_t1$mean[i,],parameters_t1$cov[[i]]) * parameters_t1$prior[1,i]
  prob_credit1[i,] <- prob_credit1[i,] * bin_credit1[i,]
}

assign_mat <- apply(prob_credit1, 2, which.max)
label_credit_train1 <- ifelse(assign_mat == 2,"-","+")
error_credit_train1 <-mean(label_credit_train1 != as.character(credit_test1[,47]))
#0.2307692

#check with R package
nb_credit_1 <- naiveBayes(credit_data_train1[,16]~. , data=credit_data_train1[,-16])
pred_credit_1 <- predict(nb_credit_1, credit_data_test1[,-16], type="class")
error_creditr_t1 <-mean(pred_credit_1 != as.character(credit_data_test1[,16]))
#0.1923077

#**********************************Test Dataset 2*******************************
prob_credit2 = matrix(NA, nrow=2, ncol=nrow(credit_test2))
parameters_t2 <- ParametersCredit(credit_train2)
bin_credit2 <- BinaryPartProbs(credit_train2,credit_test2)

for (i in c(1:2)){
  prob_credit2[i,] <-  dmvnorm(as.matrix(credit_test2[,c(1:6)]),parameters_t2$mean[i,],parameters_t2$cov[[i]]) * parameters_t2$prior[1,i]
  prob_credit2[i,] <- prob_credit2[i,] * bin_credit2[i,]
}

assign_mat <- apply(prob_credit2, 2, which.max)
label_credit_train2 <- ifelse(assign_mat == 2,"-","+")
error_credit_train2 <-mean(label_credit_train2 != as.character(credit_test2[,47]))
#0.2846154

#check with R package
nb_credit_2 <- naiveBayes(credit_data_train2[,16]~. , data=credit_data_train2[,-16])
pred_credit_2 <- predict(nb_credit_2, credit_data_test2[,-16], type="class")
error_creditr_t2 <-mean(pred_credit_2 != as.character(credit_data_test2[,16]))
#0.2769231

#**********************************Test Dataset 3*******************************
prob_credit3 = matrix(NA, nrow=2, ncol=nrow(credit_test3))
parameters_t3 <- ParametersCredit(credit_train3)
bin_credit3 <- BinaryPartProbs(credit_train3,credit_test3)

for (i in c(1:2)){
  prob_credit3[i,] <-  dmvnorm(as.matrix(credit_test3[,c(1:6)]),parameters_t3$mean[i,],parameters_t3$cov[[i]]) * parameters_t3$prior[1,i]
  prob_credit3[i,] <- prob_credit3[i,] * bin_credit3[i,]
}

assign_mat <- apply(prob_credit3, 2, which.max)
label_credit_train3 <- ifelse(assign_mat == 2,"-","+")
error_credit_train3 <-mean(label_credit_train3 != as.character(credit_test3[,47]))
#0.2461538

#check with R package
nb_credit_3 <- naiveBayes(credit_data_train3[,16]~. , data=credit_data_train3[,-16])
pred_credit_3 <- predict(nb_credit_3, credit_data_test3[,-16], type="class")
error_creditr_t3 <-mean(pred_credit_3 != as.character(credit_data_test3[,16]))
#0.1846154

#**********************************Test Dataset 4*******************************
prob_credit4 = matrix(NA, nrow=2, ncol=nrow(credit_test4))
parameters_t4 <- ParametersCredit(credit_train4)
bin_credit4 <- BinaryPartProbs(credit_train4,credit_test4)

for (i in c(1:2)){
  prob_credit4[i,] <-  dmvnorm(as.matrix(credit_test4[,c(1:6)]),parameters_t4$mean[i,],parameters_t4$cov[[i]]) * parameters_t4$prior[1,i]
  prob_credit4[i,] <- prob_credit4[i,] * bin_credit4[i,]
}

assign_mat <- apply(prob_credit4, 2, which.max)
label_credit_train4 <- ifelse(assign_mat == 2,"-","+")
error_credit_train4 <-mean(label_credit_train4 != as.character(credit_test4[,47]))
#0.2923077

#check with R package
nb_credit_4 <- naiveBayes(credit_data_train4[,16]~. , data=credit_data_train4[,-16])
pred_credit_4 <- predict(nb_credit_4, credit_data_test4[,-16], type="class")
error_creditr_t4 <-mean(pred_credit_4 != as.character(credit_data_test4[,16]))
#0.3

#**********************************Test Dataset 5*******************************
prob_credit5 = matrix(NA, nrow=2, ncol=nrow(credit_test5))
parameters_t5 <- ParametersCredit(credit_train5)
bin_credit5 <- BinaryPartProbs(credit_train5,credit_test5)

for (i in c(1:2)){
  prob_credit5[i,] <-  dmvnorm(as.matrix(credit_test5[,c(1:6)]),parameters_t5$mean[i,],parameters_t5$cov[[i]]) * parameters_t5$prior[1,i]
  prob_credit5[i,] <- prob_credit5[i,] * bin_credit5[i,]
}

assign_mat <- apply(prob_credit5, 2, which.max)
label_credit_train5 <- ifelse(assign_mat == 2,"-","+")
error_credit_train5 <-mean(label_credit_train5 != as.character(credit_test5[,47]))
#0.2406015

#check with R package
nb_credit_5 <- naiveBayes(credit_data_train5[,16]~. , data=credit_data_train5[,-16])
pred_credit_5 <- predict(nb_credit_5, credit_data_test5[,-16], type="class")
error_creditr_t5 <-mean(pred_credit_5 != as.character(credit_data_test5[,16]))
#0.1954887

#********************************Compiling Results************************************************
error_credit_nb <-c(error_credit_train1,error_credit_train2,error_credit_train3,error_credit_train4,error_credit_train5)
error_credit_rnb <- c(error_creditr_t1,error_creditr_t2,error_creditr_t3,error_creditr_t4,error_creditr_t5)

mean(error_credit_nb)
#0.2588895
mean(error_credit_rnb)
#0.229867

#Plot for error values over all test datasets
plot(c(1,2,3,4,5), error_credit_nb,
     type="b", pch = 19, col="blue",frame = FALSE, 
     main = "Naive Bayes on Credit Data",
     xlab="Dataset Number",
     ylab="Test errors on CV datasets")
minor.tick(nx=4)
plot(c(1,2,3,4,5), error_credit_rnb,
     type="b", pch = 19, col="green",frame = FALSE, 
     main = "Naive Bayes on Credit Data - R Function",
     xlab="Dataset Number",
     ylab="Test errors on CV datasets")
minor.tick(nx=4)

#***************************************************************************************************

#***************************************************************************************************
#*********************************Comparing KNN and Naive Bayes*************************************

#************Ionosphere Dataset**********************
#We observe best KNN for this dataset is Manhattan with k=1
#Comparison Plot
plot(c(1,2,3,4,5), error_ion_nb,
     type="b", pch = 19, col="red",frame = FALSE, 
     ylim=range(0.03,0.20),
     main = "Ionosphere Data - KNN vs Naive Bayes",
     xlab="Dataset Number",
     ylab="Test errors on CV datasets")
lines(c(1,2,3,4,5), error_ion_man[1,], col="blue",type="b", pch = 19)
minor.tick(nx=4)
legend(1,0.20, legend=c("KNN", "Naive Bayes"),
       col=c("blue", "red"), lty=1:2, cex=0.8)

#************Car Dataset**********************
#We observe best KNN for this dataset is Mahalanobis with k=5
#Comparison Plot
plot(c(1,2,3,4,5), error_car_nb,
     type="b", pch = 19, col="red",frame = FALSE, 
     ylim=range(0.02,0.30),
     main = "Car Evaluation Data - KNN vs Naive Bayes",
     xlab="Dataset Number",
     ylab="Test errors on CV datasets")
lines(c(1,2,3,4,5), error_car_maha[2,], col="blue",type="b", pch = 19)
minor.tick(nx=4)
legend(1,0.30, legend=c("KNN", "Naive Bayes"),
       col=c("blue", "red"), lty=1:2, cex=0.8)

#************Credit Dataset**********************
#We observe best KNN for this dataset is Gower's with k=100
#Comparison Plot
plot(c(1,2,3,4,5), error_credit_nb,
     type="b", pch = 19, col="red",frame = FALSE, 
     ylim=range(0.06,0.35),
     main = "Car Evaluation Data - KNN vs Naive Bayes",
     xlab="Dataset Number",
     ylab="Test errors on CV datasets")
lines(c(1,2,3,4,5), error_credit_gow[5,], col="blue",type="b", pch = 19)
minor.tick(nx=4)
legend(1,0.35, legend=c("KNN", "Naive Bayes"),
       col=c("blue", "red"), lty=1:2, cex=0.8)