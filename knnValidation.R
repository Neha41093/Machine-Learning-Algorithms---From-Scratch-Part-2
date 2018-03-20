
#***********************************KNN Validation**********************************************

#***********************************Ionosphere Dataset***************************************
#*************Compiling results for Ionosphere and validating with R package 'knn'*********
error_ion_euclidean <- cbind(error_ion_test1[,1],error_ion_test2[,1],error_ion_test3[,1],error_ion_test4[,1],error_ion_test5[,1])
error_ion_man <- cbind(error_ion_test1[,2],error_ion_test2[,2],error_ion_test3[,2],error_ion_test4[,2],error_ion_test5[,2])

library(Hmisc)
#Error plots
plot(c(1,5,10,50,100), apply(error_ion_euclidean,1,mean),
     type="b", pch = 19, col="blue",frame = FALSE, 
     main = "KNN on Ionosphere Data - Euclidean Distance",
     xlab="K-value",
     ylab="Average test errors on CV datasets")
minor.tick(nx=4)
#0.1453521 0.1452716 0.1480885 0.2849095 0.3590342

plot(c(1,5,10,50,100), apply(error_ion_man,1,mean),
     type="b", pch = 19, col="red", frame = FALSE, 
     main = "KNN on Ionosphere Data - Manhattan Distance",
     xlab="K-value",
     ylab="Average test errors on CV datasets")
minor.tick(nx=4)
#0.09975855 0.11682093 0.11955734 0.22229376 0.33911469

#Manhattan with k=1 best for this dataset among Euclidean and Manhattan

#With R package "knn"
library(class)

r_error_ion <- matrix(NA,nrow=5,ncol=5)
count2 <- 0
for(k1 in c(1,5,10,50,100)){
  knn_pred_temp <- knn(ion_train1,ion_test1,ion_target1,k=k1)
  count2 <- count2 + 1
  r_error_ion[count2,1] <- mean(knn_pred_temp != ion_target_check1)
}
count2 <- 0
for(k1 in c(1,5,10,50,100)){
  knn_pred_temp <- knn(ion_train2,ion_test2,ion_target2,k=k1)
  count2 <- count2 + 1
  r_error_ion[count2,2] <- mean(knn_pred_temp != ion_target_check2)
}
count2 <- 0
for(k1 in c(1,5,10,50,100)){
  knn_pred_temp <- knn(ion_train3,ion_test3,ion_target3,k=k1)
  count2 <- count2 + 1
  r_error_ion[count2,3] <- mean(knn_pred_temp != ion_target_check3)
}
count2 <- 0
for(k1 in c(1,5,10,50,100)){
  knn_pred_temp <- knn(ion_train4,ion_test4,ion_target4,k=k1)
  count2 <- count2 + 1
  r_error_ion[count2,4] <- mean(knn_pred_temp != ion_target_check4)
}
count2 <- 0
for(k1 in c(1,5,10,50,100)){
  knn_pred_temp <- knn(ion_train5,ion_test5,ion_target5,k=k1)
  count2 <- count2 + 1
  r_error_ion[count2,5] <- mean(knn_pred_temp != ion_target_check5)
}

r_error_ion
#[,1]      [,2]      [,3]      [,4]      [,5]
#[1,] 0.1428571 0.1428571 0.1285714 0.1857143 0.1267606
#[2,] 0.1571429 0.1571429 0.1000000 0.1571429 0.1549296
#[3,] 0.1571429 0.1714286 0.1428571 0.1571429 0.1830986
#[4,] 0.2428571 0.3142857 0.2857143 0.3285714 0.2957746
#[5,] 0.3714286 0.3428571 0.3714286 0.3714286 0.3380282

#Plot for R mean error values over all test datasets
plot(c(1,5,10,50,100), apply(r_error_ion,1,mean),
     type="b", pch = 19, col="green",frame = FALSE, 
     main = "KNN on Ionosphere Data - R Function",
     xlab="K-value",
     ylab="Average test errors on CV datasets")
minor.tick(nx=4)
#0.1453521 0.1452716 0.1623340 0.2934406 0.3590342

#Least error at k=5 (similar to that obtained using Euclidean distance above)

#**********************************************************************************************************

#******************************Car Evaluation Dataset*************************************************
#*************Compiling results for Car dataset and validating with R package 'knncat'*********
error_car_euclidean <- cbind(error_car_test1[,1],error_car_test2[,1],error_car_test3[,1],error_car_test4[,1],error_car_test5[,1])
error_car_maha <- cbind(error_car_test1[,2],error_car_test2[,2],error_car_test3[,2],error_car_test4[,2],error_car_test5[,2])

library(Hmisc)
#Error plots
plot(c(1,5,10,50,100), apply(error_car_euclidean,1,mean),
     type="b", pch = 19, col="blue",frame = FALSE, 
     main = "KNN on Car Data - Euclidean Distance",
     xlab="K-value",
     ylab="Average test errors on CV datasets")
minor.tick(nx=4)
#0.12156422 0.09551724 0.10245377 0.14412794 0.18060470

plot(c(1,5,10,50,100), apply(error_car_maha,1,mean),
     type="b", pch = 19, col="red", frame = FALSE, 
     main = "KNN on Car Data - Mahalanobis Distance",
     xlab="K-value",
     ylab="Average test errors on CV datasets")
minor.tick(nx=4)
#0.11168416 0.04747626 0.05093453 0.11055472 0.16091454

#Mahalanobis with k=5 best for this dataset among Euclidean and Mahalanobis

#With R package "knncat"
library(knncat)

r_error_car <- matrix(NA,nrow=5,ncol=5)
count2 <- 0
for(k1 in c(1,5,10,50,100)){
  knn_pred_tempc <- knncat(car_data_train1,k=k1,classcol=7)
  count2 <- count2 + 1
  prediction_temp <- predict(knn_pred_tempc,car_data_train1,car_data_test1,train.classcol=7,newdata.classcol=7)
  r_error_car[count2,1]<- mean(as.character(prediction_temp) != as.character(car_data_test1[,7]))
}
count2 <- 0
for(k1 in c(1,5,10,50,100)){
  knn_pred_tempc <- knncat(car_data_train2,k=k1,classcol=7)
  count2 <- count2 + 1
  prediction_temp <- predict(knn_pred_tempc,car_data_train2,car_data_test2,train.classcol=7,newdata.classcol=7)
  r_error_car[count2,2]<- mean(as.character(prediction_temp) != as.character(car_data_test2[,7]))
}
count2 <- 0
for(k1 in c(1,5,10,50,100)){
  knn_pred_tempc <- knncat(car_data_train3,k=k1,classcol=7)
  count2 <- count2 + 1
  prediction_temp <- predict(knn_pred_tempc,car_data_train3,car_data_test3,train.classcol=7,newdata.classcol=7)
  r_error_car[count2,3]<- mean(as.character(prediction_temp) != as.character(car_data_test3[,7]))
}
count2 <- 0
for(k1 in c(1,5,10,50,100)){
  knn_pred_tempc <- knncat(car_data_train4,k=k1,classcol=7)
  count2 <- count2 + 1
  prediction_temp <- predict(knn_pred_tempc,car_data_train4,car_data_test4,train.classcol=7,newdata.classcol=7)
  r_error_car[count2,4]<- mean(as.character(prediction_temp) != as.character(car_data_test4[,7]))
}
count2 <- 0
for(k1 in c(1,5,10,50,100)){
  knn_pred_tempc <- knncat(car_data_train5,k=k1,classcol=7)
  count2 <- count2 + 1
  prediction_temp <- predict(knn_pred_tempc,car_data_train5,car_data_test5,train.classcol=7,newdata.classcol=7)
  r_error_car[count2,5]<- mean(as.character(prediction_temp) != as.character(car_data_test5[,7]))
}

r_error_car
#          [,1]       [,2]       [,3]       [,4]       [,5]
#[1,] 0.04057971 0.04347826 0.03188406 0.05507246 0.04310345
#[2,] 0.06376812 0.05217391 0.04057971 0.05797101 0.03448276
#[3,] 0.05507246 0.05217391 0.06376812 0.06086957 0.05747126
#[4,] 0.08695652 0.08985507 0.08405797 0.07826087 0.10919540
#[5,] 0.13333333 0.10434783 0.10724638 0.15942029 0.10632184

#Plot for R mean error values over all test datasets
plot(c(1,5,10,50,100), apply(r_error_car,1,mean),
     type="b", pch = 19, col="green",frame = FALSE, 
     main = "KNN on Car Data - R Function",
     xlab="K-value",
     ylab="Average test errors on CV datasets")
minor.tick(nx=4)
#0.04282359 0.04979510 0.05787106 0.08966517 0.12213393

#Least error at k=1

#************************************************************************************************************

#*******************************Credit Approval Dataset*******************************************************
#*************Compiling results for Credit dataset and validating with R package 'knn'*********
error_credit_mink <- cbind(error_credit_test1[,1],error_credit_test2[,1],error_credit_test3[,1],error_credit_test4[,1],error_credit_test5[,1])
error_credit_gow <- cbind(error_credit_test1[,2],error_credit_test2[,2],error_credit_test3[,2],error_credit_test4[,2],error_credit_test5[,2])

library(Hmisc)
#Error plots
plot(c(1,5,10,50,100), apply(error_credit_mink,1,mean),
     type="b", pch = 19, col="blue",frame = FALSE, 
     main = "KNN on Credit Data - Minkowski Distance",
     xlab="K-value",
     ylab="Average test errors on CV datasets")
minor.tick(nx=4)
#0.3459919 0.3155350 0.3278080 0.3277039 0.3322499

plot(c(1,5,10,50,100), apply(error_credit_gow,1,mean),
     type="b", pch = 19, col="red", frame = FALSE, 
     main = "KNN on Credit Data - Gowers Distance",
     xlab="K-value",
     ylab="Average test errors on CV datasets")
minor.tick(nx=4)
#0.1869983 0.1392713 0.1348294 0.1348988 0.1331868

#Gowers with k=100 best for this dataset among Minkowski and Gowers

#With R package "knn"
library(class)

r_error_credit <- matrix(NA,nrow=5,ncol=5)
count2 <- 0
for(k1 in c(1,5,10,50,100)){
  knn_pred_tempr <- knn(m_credit_train1,m_credit_test1,credit_target1,k=k1)
  count2 <- count2 + 1
  r_error_credit[count2,1] <- mean(knn_pred_tempr != credit_target_check1)
}
count2 <- 0
for(k1 in c(1,5,10,50,100)){
  knn_pred_tempr <- knn(m_credit_train2,m_credit_test2,credit_target2,k=k1)
  count2 <- count2 + 1
  r_error_credit[count2,2] <- mean(knn_pred_tempr != credit_target_check2)
}
count2 <- 0
for(k1 in c(1,5,10,50,100)){
  knn_pred_tempr <- knn(m_credit_train3,m_credit_test3,credit_target3,k=k1)
  count2 <- count2 + 1
  r_error_credit[count2,3] <- mean(knn_pred_tempr != credit_target_check3)
}
count2 <- 0
for(k1 in c(1,5,10,50,100)){
  knn_pred_tempr <- knn(m_credit_train4,m_credit_test4,credit_target4,k=k1)
  count2 <- count2 + 1
  r_error_credit[count2,4] <- mean(knn_pred_tempr != credit_target_check4)
}
count2 <- 0
for(k1 in c(1,5,10,50,100)){
  knn_pred_tempr <- knn(m_credit_train5,m_credit_test5,credit_target5,k=k1)
  count2 <- count2 + 1
  r_error_credit[count2,5] <- mean(knn_pred_tempr != credit_target_check5)
}

r_error_credit
#          [,1]      [,2]      [,3]      [,4]      [,5]
#[1,]  0.2769231 0.3923077 0.2923077 0.3307692 0.3759398
#[2,] 0.3230769 0.3461538 0.2615385 0.3538462 0.2781955
#[3,] 0.3230769 0.3461538 0.3307692 0.3153846 0.3233083
#[4,] 0.2846154 0.3615385 0.3230769 0.3307692 0.3308271
#[5,] 0.2923077 0.3461538 0.3384615 0.3384615 0.3308271

#Plot for R mean error values over all test datasets
plot(c(1,5,10,50,100), apply(r_error_credit,1,mean),
     type="b", pch = 19, col="green",frame = FALSE, 
     main = "KNN on Credit Data - R Function",
     xlab="K-value",
     ylab="Average test errors on CV datasets")
minor.tick(nx=4)
#0.3336495 0.3125622 0.3277386 0.3261654 0.3292423

#Least error at k=5

#************************************************************************************************************
