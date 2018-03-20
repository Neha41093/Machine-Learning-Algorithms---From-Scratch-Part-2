
#*********************************************************************************************#
#**************************Creating cross validation datasets***************************
old.dir <- getwd()
setwd("C:/Users/Neha Rawat/Desktop/IU-Data Science/AML/Assignment 4")

#*********Reading Datasets**********************
ionosphere_data <- read.csv("ionosphere_data.csv",header = FALSE)
car_data <- read.csv("car_data.csv",header = FALSE)
credit_data <- read.csv("crx_data.csv",header=FALSE)

#***********checking datasets for missing values**********
length(which(car_data == "NA"))
#0
length(which(car_data == "?"))
#0
length(which(credit_data == "?"))
#67

#**********removing missing values from credit_data********
na_value_length <- length(which(credit_data[,c(1,2,4,5,6,7,14)] == "?"))
na_value_rows <- integer()
a <- 0
#finding rows containing NA values to eliminate them
for (i in 1:nrow(credit_data)){
  if (length(which(credit_data[i,c(1,2,4,5,6,7,14)] == "?")) >= 1)
  {
    a <- a+1
    na_value_rows[a] <- i
  }
}
#Remove the NA containing rows from data
credit_data_no_na <- credit_data[-na_value_rows,]
#check if any NAs in this data
length(which(credit_data_no_na == "?"))
#0

#change the structure of the dataset
write.csv(credit_data_no_na,"credit_data_no_na.csv",row.names = FALSE, col.names = FALSE)
credit_data_mod <- read.csv("credit_data_no_na.csv", header = TRUE)
str(credit_data_mod)

#**********Creating the cross validation datasets for k=5**********

#***********************ionosphere dataset*************************
set.seed(520)
all_indices <- c(1:nrow(ionosphere_data))
test1_indices <- sample(nrow(ionosphere_data),70)
remain_1 <- all_indices[! all_indices %in% test1_indices]
test2_indices <- sample(remain_1,70)
remain_2 <- remain_1[! remain_1 %in% test2_indices]
test3_indices <- sample(remain_2,70)
remain_3 <- remain_2[! remain_2 %in% test3_indices]
test4_indices <- sample(remain_3,70)
remain_4 <- remain_3[! remain_3 %in% test4_indices]
test5_indices <- sample(remain_4,71)

#check if all the test indices make up the total dataset
chk1 <- rbind(as.matrix(test1_indices), as.matrix(test2_indices))
chk2 <- rbind(as.matrix(test3_indices), as.matrix(test4_indices))
chk3 <- rbind(chk2, as.matrix(test5_indices))
chk4 <- sort(rbind(chk1,chk3))

sum(1:nrow(ionosphere_data))
#61776
sum(chk4)
#61776

#Creating datasets
ionosphere_data_test1 <- ionosphere_data[test1_indices,]
ionosphere_data_train1 <- ionosphere_data[-test1_indices,]
ionosphere_data_test2 <- ionosphere_data[test2_indices,]
ionosphere_data_train2 <- ionosphere_data[-test2_indices,]
ionosphere_data_test3 <- ionosphere_data[test3_indices,]
ionosphere_data_train3 <- ionosphere_data[-test3_indices,]
ionosphere_data_test4 <- ionosphere_data[test4_indices,]
ionosphere_data_train4 <- ionosphere_data[-test4_indices,]
ionosphere_data_test5 <- ionosphere_data[test5_indices,]
ionosphere_data_train5 <- ionosphere_data[-test5_indices,]

#Test Datasets
write.csv(ionosphere_data_test1,"ionosphere_data_test1.csv", row.names = FALSE, col.names = FALSE)
write.csv(ionosphere_data_test2,"ionosphere_data_test2.csv", row.names = FALSE, col.names = FALSE)
write.csv(ionosphere_data_test3,"ionosphere_data_test3.csv", row.names = FALSE, col.names = FALSE)
write.csv(ionosphere_data_test4,"ionosphere_data_test4.csv", row.names = FALSE, col.names = FALSE)
write.csv(ionosphere_data_test5,"ionosphere_data_test5.csv", row.names = FALSE, col.names = FALSE)

#Train Datasets
write.csv(ionosphere_data_train1,"ionosphere_data_train1.csv", row.names = FALSE, col.names = FALSE)
write.csv(ionosphere_data_train2,"ionosphere_data_train2.csv", row.names = FALSE, col.names = FALSE)
write.csv(ionosphere_data_train3,"ionosphere_data_train3.csv", row.names = FALSE, col.names = FALSE)
write.csv(ionosphere_data_train4,"ionosphere_data_train4.csv", row.names = FALSE, col.names = FALSE)
write.csv(ionosphere_data_train5,"ionosphere_data_train5.csv", row.names = FALSE, col.names = FALSE)

#***********************Car dataset************************************************
set.seed(520)
all_indices1 <- c(1:nrow(car_data))
test1_indices1 <- sample(nrow(car_data),345)
remain_11 <- all_indices1[! all_indices1 %in% test1_indices1]
test2_indices1 <- sample(remain_11,345)
remain_21 <- remain_11[! remain_11 %in% test2_indices1]
test3_indices1 <- sample(remain_21,345)
remain_31 <- remain_21[! remain_21 %in% test3_indices1]
test4_indices1 <- sample(remain_31,345)
remain_41 <- remain_31[! remain_31 %in% test4_indices1]
test5_indices1 <- sample(remain_41,348)

#check if all the test indices make up the total dataset
chk11 <- rbind(as.matrix(test1_indices1), as.matrix(test2_indices1))
chk21 <- rbind(as.matrix(test3_indices1), as.matrix(test4_indices1))
chk31 <- rbind(chk21, as.matrix(test5_indices1))
chk41 <- sort(rbind(chk11,chk31))

sum(1:nrow(car_data))
#1493856
sum(chk41)
#1493856

#Creating datasets
car_data_test1 <- car_data[test1_indices1,]
car_data_train1 <- car_data[-test1_indices1,]
car_data_test2 <- car_data[test2_indices1,]
car_data_train2 <- car_data[-test2_indices1,]
car_data_test3 <- car_data[test3_indices1,]
car_data_train3 <- car_data[-test3_indices1,]
car_data_test4 <- car_data[test4_indices1,]
car_data_train4 <- car_data[-test4_indices1,]
car_data_test5 <- car_data[test5_indices1,]
car_data_train5 <- car_data[-test5_indices1,]

#Test Datasets
write.csv(car_data_test1,"car_data_test1.csv", row.names = FALSE, col.names = FALSE)
write.csv(car_data_test2,"car_data_test2.csv", row.names = FALSE, col.names = FALSE)
write.csv(car_data_test3,"car_data_test3.csv", row.names = FALSE, col.names = FALSE)
write.csv(car_data_test4,"car_data_test4.csv", row.names = FALSE, col.names = FALSE)
write.csv(car_data_test5,"car_data_test5.csv", row.names = FALSE, col.names = FALSE)

#Train Datasets
write.csv(car_data_train1,"car_data_train1.csv", row.names = FALSE, col.names = FALSE)
write.csv(car_data_train2,"car_data_train2.csv", row.names = FALSE, col.names = FALSE)
write.csv(car_data_train3,"car_data_train3.csv", row.names = FALSE, col.names = FALSE)
write.csv(car_data_train4,"car_data_train4.csv", row.names = FALSE, col.names = FALSE)
write.csv(car_data_train5,"car_data_train5.csv", row.names = FALSE, col.names = FALSE)

#***********************Credit dataset************************************************
set.seed(520)
all_indices2 <- c(1:nrow(credit_data_mod))
test1_indices2 <- sample(nrow(credit_data_mod),130)
remain_12 <- all_indices2[! all_indices2 %in% test1_indices2]
test2_indices2 <- sample(remain_12,130)
remain_22 <- remain_12[! remain_12 %in% test2_indices2]
test3_indices2 <- sample(remain_22,130)
remain_32 <- remain_22[! remain_22 %in% test3_indices2]
test4_indices2 <- sample(remain_32,130)
remain_42 <- remain_32[! remain_32 %in% test4_indices2]
test5_indices2 <- sample(remain_42,133)

#check if all the test indices make up the total dataset
chk12 <- rbind(as.matrix(test1_indices2), as.matrix(test2_indices2))
chk22 <- rbind(as.matrix(test3_indices2), as.matrix(test4_indices2))
chk32 <- rbind(chk22, as.matrix(test5_indices2))
chk42 <- sort(rbind(chk12,chk32))

sum(1:nrow(credit_data_mod))
#213531
sum(chk42)
#213531

#Creating datasets
credit_data_test1 <- credit_data_mod[test1_indices2,]
credit_data_train1 <- credit_data_mod[-test1_indices2,]
credit_data_test2 <- credit_data_mod[test2_indices2,]
credit_data_train2 <- credit_data_mod[-test2_indices2,]
credit_data_test3 <- credit_data_mod[test3_indices2,]
credit_data_train3 <- credit_data_mod[-test3_indices2,]
credit_data_test4 <- credit_data_mod[test4_indices2,]
credit_data_train4 <- credit_data_mod[-test4_indices2,]
credit_data_test5 <- credit_data_mod[test5_indices2,]
credit_data_train5 <- credit_data_mod[-test5_indices2,]

#Test Datasets
write.csv(credit_data_test1,"credit_data_test1.csv", row.names = FALSE, col.names = FALSE)
write.csv(credit_data_test2,"credit_data_test2.csv", row.names = FALSE, col.names = FALSE)
write.csv(credit_data_test3,"credit_data_test3.csv", row.names = FALSE, col.names = FALSE)
write.csv(credit_data_test4,"credit_data_test4.csv", row.names = FALSE, col.names = FALSE)
write.csv(credit_data_test5,"credit_data_test5.csv", row.names = FALSE, col.names = FALSE)

#Train Datasets
write.csv(credit_data_train1,"credit_data_train1.csv", row.names = FALSE, col.names = FALSE)
write.csv(credit_data_train2,"credit_data_train2.csv", row.names = FALSE, col.names = FALSE)
write.csv(credit_data_train3,"credit_data_train3.csv", row.names = FALSE, col.names = FALSE)
write.csv(credit_data_train4,"credit_data_train4.csv", row.names = FALSE, col.names = FALSE)
write.csv(credit_data_train5,"credit_data_train5.csv", row.names = FALSE, col.names = FALSE)

#*********************************************************************************************#

#*********************************************************************************************#
#********************************KNN Algorithm Implementation*********************************
library(cluster)

#*********Distance Functions*****************
#Euclidean Distance
EuclideanDistance <- function(test, train){
  n <- nrow(test)
  data <- rbind(test,train)
  distance.1 <- as.matrix(dist(data,method="euclidean"))
  distance.m <- matrix(distance.1[1:n,(n+1):ncol(distance.1)],nrow=n,ncol=nrow(train))
  return(distance.m)
}

#Manhattan Distance
ManhattanDistance <- function(test, train){
  n <- nrow(test)
  data <- rbind(test,train)
  distance.1 <- as.matrix(dist(data,method="manhattan"))
  distance.m <- matrix(distance.1[1:n,(n+1):ncol(distance.1)],nrow=n,ncol=nrow(train))
  return(distance.m)
}

#Minkowski Distance -- with power=3
MinkowskiDistance <- function(test, train){
  n <- nrow(test)
  data <- rbind(test,train)
  distance.1 <- as.matrix(dist(data,method="minkowski",p=3))
  distance.m <- matrix(distance.1[1:n,(n+1):ncol(distance.1)],nrow=n,ncol=nrow(train))
  return(distance.m)
}

#Mahalanobis Distance
MahalanobisDistance <- function(test, train){
  distance.1 <- matrix(NA,nrow=nrow(test),ncol=nrow(train))
  for (i in 1:nrow(test)){
    distance.1[i,] <- mahalanobis(train,as.matrix(test[i,]),cov=ginv(cov(train)),inverted = TRUE)
  }
  return(distance.1)
}

#Gower's Distance
GowerDistance <- function(test, train){
  n <- nrow(test)
  data <- rbind(test,train)
  distance.1 <- as.matrix(daisy(data,metric="gower"))
  distance.m <- matrix(distance.1[1:n,(n+1):ncol(distance.1)],nrow=n,ncol=nrow(train))
  return(distance.m)
}

#****************KNN Function - Majority Voting*****************

KNNFunction <- function(train,test,target,method,k){
  test_labels <- list()
  if(method == "euclidean"){
    distance = EuclideanDistance(test, train)
  } else if(method == "minkowski"){
    distance = MinkowskiDistance(test, train)
  } else if(method == "manhattan"){
    distance = ManhattanDistance(test, train)
  }else if(method == "mahalanobis"){
    distance = MahalanobisDistance(test, train)
  } else if(method == "gower"){
    distance = GowerDistance(test, train)
  } else{
    print("wrong method")
  }
  if (k==1){
    for (j in 1:nrow(test)){
      x <- distance[j,]
      v <- sort(x,index.return=TRUE)[[2]]
      t_i <- v[1]
      t_f <- target[t_i]
      c <- table(t_f)   
      test_labels[[j]] <- names(c)[which.max(c)]
    } 
  }
  if (k > 1){
    for (j in 1:nrow(test)){
      x <- distance[j,]
      v <- sort(x,index.return=TRUE)[[2]]
      t_i <- v[1:k]
      t_f <- target[t_i]
      c <- table(t_f)   
      test_labels[[j]] <- names(c)[which.max(c)]
    }
  }
  test_labels_fin <- do.call("rbind",test_labels)
  return(test_labels_fin)
}

#**********************************************Ionosphere dataset*******************************************

#***************Test dataset 1***********************************
#**********Euclidean Distance (k=1,5,10,50,100)************
method1 <- "euclidean"
ion_target1 <- ionosphere_data_train1[,35]
ion_target_check1 <- ionosphere_data_test1[,35]
ion_train1 <- ionosphere_data_train1[,-35]
ion_test1 <- ionosphere_data_test1[,-35]
error_ion_test1 <- matrix(NA,nrow=5,ncol=2)
knn_test1 <- matrix(NA,nrow=5,ncol=nrow(ion_test1))
k_list <- c(1,5,10,50,100)
counter <- 0

for (k in k_list){
  counter <- counter+1
  knn_test1[counter,] <- KNNFunction(ion_train1,ion_test1,ion_target1,method1,k)
  error_ion_test1[counter,1] <- mean(knn_test1[counter,] != ion_target_check1)
}

error_ion_test1
#        [,1] [,2]
#[1,] 0.1428571   NA
#[2,] 0.1571429   NA
#[3,] 0.1571429   NA
#[4,] 0.2285714   NA
#[5,] 0.3714286   NA

#**********Manhattan Distance (k=1,5,10,50,100)************
method2 <- "manhattan"
counter <- 0

for (k in k_list){
  counter <- counter+1
  knn_test1[counter,] <- KNNFunction(ion_train1,ion_test1,ion_target1,method2,k)
  error_ion_test1[counter,2] <- mean(knn_test1[counter,] != ion_target_check1)
}

error_ion_test1
#           [,1]      [,2]
#[1,] 0.1428571 0.1428571
#[2,] 0.1571429 0.1285714
#[3,] 0.1571429 0.1142857
#[4,] 0.2285714 0.2000000
#[5,] 0.3714286 0.3571429

#***************Test dataset 2***********************************
#**********Euclidean Distance (k=1,5,10,50,100)************
method1 <- "euclidean"
ion_target2 <- ionosphere_data_train2[,35]
ion_target_check2 <- ionosphere_data_test2[,35]
ion_train2 <- ionosphere_data_train2[,-35]
ion_test2 <- ionosphere_data_test2[,-35]
error_ion_test2 <- matrix(NA,nrow=5,ncol=2)
knn_test2 <- matrix(NA,nrow=5,ncol=nrow(ion_test2))
k_list <- c(1,5,10,50,100)
counter <- 0

for (k in k_list){
  counter <- counter+1
  knn_test2[counter,] <- KNNFunction(ion_train2,ion_test2,ion_target2,method1,k)
  error_ion_test2[counter,1] <- mean(knn_test2[counter,] != ion_target_check2)
}

error_ion_test2
#         [,1] [,2]
#[1,] 0.1428571   NA
#[2,] 0.1571429   NA
#[3,] 0.1714286   NA
#[4,] 0.3142857   NA
#[5,] 0.3428571   NA

#**********Manhattan Distance (k=1,5,10,50,100)************
method2 <- "manhattan"
counter <- 0

for (k in k_list){
  counter <- counter+1
  knn_test2[counter,] <- KNNFunction(ion_train2,ion_test2,ion_target2,method2,k)
  error_ion_test2[counter,2] <- mean(knn_test2[counter,] != ion_target_check2)
}

error_ion_test2
#         [,1]      [,2]
#[1,] 0.1428571 0.1000000
#[2,] 0.1571429 0.1285714
#[3,] 0.1714286 0.1285714
#[4,] 0.3142857 0.2285714
#[5,] 0.3428571 0.3142857

#***************Test dataset 3***********************************
#**********Euclidean Distance (k=1,5,10,50,100)************
method1 <- "euclidean"
ion_target3 <- ionosphere_data_train3[,35]
ion_target_check3 <- ionosphere_data_test3[,35]
ion_train3 <- ionosphere_data_train3[,-35]
ion_test3 <- ionosphere_data_test3[,-35]
error_ion_test3 <- matrix(NA,nrow=5,ncol=2)
knn_test3 <- matrix(NA,nrow=5,ncol=nrow(ion_test3))
k_list <- c(1,5,10,50,100)
counter <- 0

for (k in k_list){
  counter <- counter+1
  knn_test3[counter,] <- KNNFunction(ion_train3,ion_test3,ion_target3,method1,k)
  error_ion_test3[counter,1] <- mean(knn_test3[counter,] != ion_target_check3)
}

error_ion_test3
#          [,1] [,2]
#[1,] 0.1285714   NA
#[2,] 0.1000000   NA
#[3,] 0.1142857   NA
#[4,] 0.2714286   NA
#[5,] 0.3714286   NA

#**********Manhattan Distance (k=1,5,10,50,100)************
method2 <- "manhattan"
counter <- 0

for (k in k_list){
  counter <- counter+1
  knn_test3[counter,] <- KNNFunction(ion_train3,ion_test3,ion_target3,method2,k)
  error_ion_test3[counter,2] <- mean(knn_test3[counter,] != ion_target_check3)
}

error_ion_test3
#         [,1]       [,2]
#[1,] 0.1285714 0.04285714
#[2,] 0.1000000 0.07142857
#[3,] 0.1142857 0.07142857
#[4,] 0.2714286 0.15714286
#[5,] 0.3714286 0.34285714

#***************Test dataset 4***********************************
#**********Euclidean Distance (k=1,5,10,50,100)************
method1 <- "euclidean"
ion_target4 <- ionosphere_data_train4[,35]
ion_target_check4 <- ionosphere_data_test4[,35]
ion_train4 <- ionosphere_data_train4[,-35]
ion_test4 <- ionosphere_data_test4[,-35]
error_ion_test4 <- matrix(NA,nrow=5,ncol=2)
knn_test4 <- matrix(NA,nrow=5,ncol=nrow(ion_test4))
k_list <- c(1,5,10,50,100)
counter <- 0

for (k in k_list){
  counter <- counter+1
  knn_test4[counter,] <- KNNFunction(ion_train4,ion_test4,ion_target4,method1,k)
  error_ion_test4[counter,1] <- mean(knn_test4[counter,] != ion_target_check4)
}

error_ion_test4
#          [,1] [,2]
#[1,] 0.1857143   NA
#[2,] 0.1571429   NA
#[3,] 0.1285714   NA
#[4,] 0.3285714   NA
#[5,] 0.3714286   NA

#**********Manhattan Distance (k=1,5,10,50,100)************
method2 <- "manhattan"
counter <- 0

for (k in k_list){
  counter <- counter+1
  knn_test4[counter,] <- KNNFunction(ion_train4,ion_test4,ion_target4,method2,k)
  error_ion_test4[counter,2] <- mean(knn_test4[counter,] != ion_target_check4)
}

error_ion_test4
#         [,1]      [,2]
#[1,] 0.1857143 0.1285714
#[2,] 0.1571429 0.1428571
#[3,] 0.1285714 0.1285714
#[4,] 0.3285714 0.3285714
#[5,] 0.3714286 0.3714286

#***************Test dataset 5***********************************
#**********Euclidean Distance (k=1,5,10,50,100)************
method1 <- "euclidean"
ion_target5 <- ionosphere_data_train5[,35]
ion_target_check5 <- ionosphere_data_test5[,35]
ion_train5 <- ionosphere_data_train5[,-35]
ion_test5 <- ionosphere_data_test5[,-35]
error_ion_test5 <- matrix(NA,nrow=5,ncol=2)
knn_test5 <- matrix(NA,nrow=5,ncol=nrow(ion_test5))
k_list <- c(1,5,10,50,100)
counter <- 0

for (k in k_list){
  counter <- counter+1
  knn_test5[counter,] <- KNNFunction(ion_train5,ion_test5,ion_target5,method1,k)
  error_ion_test5[counter,1] <- mean(knn_test5[counter,] != ion_target_check5)
}

error_ion_test5
#          [,1] [,2]
#[1,] 0.1267606   NA
#[2,] 0.1549296   NA
#[3,] 0.1690141   NA
#[4,] 0.2816901   NA
#[5,] 0.3380282   NA

#**********Manhattan Distance (k=1,5,10,50,100)************
method2 <- "manhattan"
counter <- 0

for (k in k_list){
  counter <- counter+1
  knn_test5[counter,] <- KNNFunction(ion_train5,ion_test5,ion_target5,method2,k)
  error_ion_test5[counter,2] <- mean(knn_test5[counter,] != ion_target_check5)
}

error_ion_test5
#         [,1]       [,2]
#[1,] 0.1267606 0.08450704
#[2,] 0.1549296 0.11267606
#[3,] 0.1690141 0.15492958
#[4,] 0.2816901 0.19718310
#[5,] 0.3380282 0.30985915

#************************************************************************
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

#************************************************************************************************************

#*******************************Car Evaluation Dataset*******************************************************
summary(car_data)
#V1          V2          V3         V4          V5         V6          V7      
#high :432   high :432   2    :432   2   :576   big  :576   high:576   acc  : 384  
#low  :432   low  :432   3    :432   4   :576   med  :576   low :576   good :  69  
#med  :432   med  :432   4    :432   more:576   small:576   med :576   unacc:1210  
#vhigh:432   vhigh:432   5more:432                                     vgood:  65  

#Creating modified car datasets using the same indexes as used in cross-validation above
car_data_d <- data.frame(V1=as.character(car_data$V1),V2=as.character(car_data$V2),V3=as.character(car_data$V3),V4=as.character(car_data$V4),
                         V5=as.character(car_data$V5),V6=as.character(car_data$V6),V7=as.character(car_data$V7),stringsAsFactors = FALSE)
car_data_d$V1[car_data_d$V1 == "vhigh"] = 4
car_data_d$V1[car_data_d$V1 == "high"] = 3
car_data_d$V1[car_data_d$V1 == "med"] = 2
car_data_d$V1[car_data_d$V1 == "low"] = 1

car_data_d$V2[car_data_d$V2 == "vhigh"] = 4
car_data_d$V2[car_data_d$V2 == "high"] = 3
car_data_d$V2[car_data_d$V2 == "med"] = 2
car_data_d$V2[car_data_d$V2 == "low"] = 1

car_data_d$V3[car_data_d$V3 == "2"] = 1
car_data_d$V3[car_data_d$V3 == "3"] = 2
car_data_d$V3[car_data_d$V3 == "4"] = 3
car_data_d$V3[car_data_d$V3 == "5more"] = 4

car_data_d$V4[car_data_d$V4 == "2"] = 1
car_data_d$V4[car_data_d$V4 == "4"] = 2
car_data_d$V4[car_data_d$V4 == "more"] = 3

car_data_d$V5[car_data_d$V5 == "big"] = 3
car_data_d$V5[car_data_d$V5 == "med"] = 2
car_data_d$V5[car_data_d$V5 == "small"] = 1

car_data_d$V6[car_data_d$V6 == "high"] = 3
car_data_d$V6[car_data_d$V6 == "med"] = 2
car_data_d$V6[car_data_d$V6 == "low"] = 1

car_data_d$V7[car_data_d$V7 == "vgood"] = 4
car_data_d$V7[car_data_d$V7 == "good"] = 3
car_data_d$V7[car_data_d$V7 == "acc"] = 2
car_data_d$V7[car_data_d$V7 == "unacc"] = 1

car_data_d <- data.frame(V1=as.numeric(car_data_d$V1),V2=as.numeric(car_data_d$V2),V3=as.numeric(car_data_d$V3),V4=as.numeric(car_data_d$V4),
                         V5=as.numeric(car_data_d$V5),V6=as.numeric(car_data_d$V6),V7=as.numeric(car_data_d$V7))

#Creating datasets
car_test1 <- car_data_d[test1_indices1,]
car_train1 <- car_data_d[-test1_indices1,]
car_test2 <- car_data_d[test2_indices1,]
car_train2 <- car_data_d[-test2_indices1,]
car_test3 <- car_data_d[test3_indices1,]
car_train3 <- car_data_d[-test3_indices1,]
car_test4 <- car_data_d[test4_indices1,]
car_train4 <- car_data_d[-test4_indices1,]
car_test5 <- car_data_d[test5_indices1,]
car_train5 <- car_data_d[-test5_indices1,]

#Write these datasets too -- for future use
#Test Datasets
write.csv(car_test1,"car_test1.csv", row.names = FALSE, col.names = FALSE)
write.csv(car_test2,"car_test2.csv", row.names = FALSE, col.names = FALSE)
write.csv(car_test3,"car_test3.csv", row.names = FALSE, col.names = FALSE)
write.csv(car_test4,"car_test4.csv", row.names = FALSE, col.names = FALSE)
write.csv(car_test5,"car_test5.csv", row.names = FALSE, col.names = FALSE)

#Train Datasets
write.csv(car_train1,"car_train1.csv", row.names = FALSE, col.names = FALSE)
write.csv(car_train2,"car_train2.csv", row.names = FALSE, col.names = FALSE)
write.csv(car_train3,"car_train3.csv", row.names = FALSE, col.names = FALSE)
write.csv(car_train4,"car_train4.csv", row.names = FALSE, col.names = FALSE)
write.csv(car_train5,"car_train5.csv", row.names = FALSE, col.names = FALSE)


#******************Test dataset 1*************************************
#**********Euclidean Distance (k=1,5,10,50,100)************
method1 <- "euclidean"
car_target1 <- car_train1[,7]
car_target_check1 <- car_test1[,7]
m_car_train1 <- car_train1[,-7]
m_car_test1 <- car_test1[,-7]
error_car_test1 <- matrix(NA,nrow=5,ncol=2)
knn_car_test1 <- matrix(NA,nrow=5,ncol=nrow(m_car_test1))
k_list <- c(1,5,10,50,100)
counter <- 0

for (k in k_list){
  counter <- counter+1
  knn_car_test1[counter,] <- KNNFunction(m_car_train1,m_car_test1,car_target1,method1,k)
  error_car_test1[counter,1] <- mean(as.numeric(knn_car_test1[counter,]) != car_target_check1)
}

error_car_test1
#        [,1] [,2]
#[1,] 0.14202899   NA
#[2,] 0.09275362   NA
#[3,] 0.11884058   NA
#[4,] 0.16811594   NA
#[5,] 0.21449275   NA

#**********Mahalanobis Distance (k=1,5,10,50,100)************
method3 <- "mahalanobis"
counter <- 0

for (k in k_list){
  counter <- counter+1
  knn_car_test1[counter,] <- KNNFunction(m_car_train1,m_car_test1,car_target1,method3,k)
  error_car_test1[counter,2] <- mean(as.numeric(knn_car_test1[counter,]) != car_target_check1)
}

error_car_test1
#           [,1]       [,2]
#[1,] 0.14202899 0.16811594
#[2,] 0.09275362 0.06666667
#[3,] 0.11884058 0.06086957
#[4,] 0.16811594 0.12173913
#[5,] 0.21449275 0.17971014

#******************Test dataset 2*************************************
#**********Euclidean Distance (k=1,5,10,50,100)************
method1 <- "euclidean"
car_target2 <- car_train2[,7]
car_target_check2 <- car_test2[,7]
m_car_train2 <- car_train2[,-7]
m_car_test2 <- car_test2[,-7]
error_car_test2 <- matrix(NA,nrow=5,ncol=2)
knn_car_test2 <- matrix(NA,nrow=5,ncol=nrow(m_car_test2))
k_list <- c(1,5,10,50,100)
counter <- 0

for (k in k_list){
  counter <- counter+1
  knn_car_test2[counter,] <- KNNFunction(m_car_train2,m_car_test2,car_target2,method1,k)
  error_car_test2[counter,1] <- mean(as.numeric(knn_car_test2[counter,]) != car_target_check2)
}

error_car_test2
#        [,1] [,2]
#[1,] 0.10434783   NA
#[2,] 0.08405797   NA
#[3,] 0.08405797   NA
#[4,] 0.12753623   NA
#[5,] 0.16811594   NA

#**********Mahalanobis Distance (k=1,5,10,50,100)************
method3 <- "mahalanobis"
counter <- 0

for (k in k_list){
  counter <- counter+1
  knn_car_test2[counter,] <- KNNFunction(m_car_train2,m_car_test2,car_target2,method3,k)
  error_car_test2[counter,2] <- mean(as.numeric(knn_car_test2[counter,]) != car_target_check2)
}

error_car_test2
#           [,1]       [,2]
#[1,] 0.10434783 0.10144928
#[2,] 0.08405797 0.02898551
#[3,] 0.08405797 0.03188406
#[4,] 0.12753623 0.09565217
#[5,] 0.16811594 0.15362319

#******************Test dataset 3*************************************
#**********Euclidean Distance (k=1,5,10,50,100)************
method1 <- "euclidean"
car_target3 <- car_train3[,7]
car_target_check3 <- car_test3[,7]
m_car_train3 <- car_train3[,-7]
m_car_test3 <- car_test3[,-7]
error_car_test3 <- matrix(NA,nrow=5,ncol=2)
knn_car_test3 <- matrix(NA,nrow=5,ncol=nrow(m_car_test3))
k_list <- c(1,5,10,50,100)
counter <- 0

for (k in k_list){
  counter <- counter+1
  knn_car_test3[counter,] <- KNNFunction(m_car_train3,m_car_test3,car_target3,method1,k)
  error_car_test3[counter,1] <- mean(as.numeric(knn_car_test3[counter,]) != car_target_check3)
}

error_car_test3
#        [,1] [,2]
#[1,] 0.08985507   NA
#[2,] 0.06086957   NA
#[3,] 0.06086957   NA
#[4,] 0.08985507   NA
#[5,] 0.12463768   NA

#**********Mahalanobis Distance (k=1,5,10,50,100)************
method3 <- "mahalanobis"
counter <- 0

for (k in k_list){
  counter <- counter+1
  knn_car_test3[counter,] <- KNNFunction(m_car_train3,m_car_test3,car_target3,method3,k)
  error_car_test3[counter,2] <- mean(as.numeric(knn_car_test3[counter,]) != car_target_check3)
}

error_car_test3
#           [,1]       [,2]
#[1,] 0.08985507 0.09565217
#[2,] 0.06086957 0.01739130
#[3,] 0.06086957 0.02608696
#[4,] 0.08985507 0.06086957
#[5,] 0.12463768 0.11014493

#******************Test dataset 4*************************************
#**********Euclidean Distance (k=1,5,10,50,100)************
method1 <- "euclidean"
car_target4 <- car_train4[,7]
car_target_check4 <- car_test4[,7]
m_car_train4 <- car_train4[,-7]
m_car_test4 <- car_test4[,-7]
error_car_test4 <- matrix(NA,nrow=5,ncol=2)
knn_car_test4 <- matrix(NA,nrow=5,ncol=nrow(m_car_test4))
k_list <- c(1,5,10,50,100)
counter <- 0

for (k in k_list){
  counter <- counter+1
  knn_car_test4[counter,] <- KNNFunction(m_car_train4,m_car_test4,car_target4,method1,k)
  error_car_test4[counter,1] <- mean(as.numeric(knn_car_test4[counter,]) != car_target_check4)
}

error_car_test4
#        [,1] [,2]
#[1,] 0.1710145   NA
#[2,] 0.1623188   NA
#[3,] 0.1594203   NA
#[4,] 0.2086957   NA
#[5,] 0.2434783   NA

#**********Mahalanobis Distance (k=1,5,10,50,100)************
method3 <- "mahalanobis"
counter <- 0

for (k in k_list){
  counter <- counter+1
  knn_car_test4[counter,] <- KNNFunction(m_car_train4,m_car_test4,car_target4,method3,k)
  error_car_test4[counter,2] <- mean(as.numeric(knn_car_test4[counter,]) != car_target_check4)
}

error_car_test4
#           [,1]       [,2]
#[1,] 0.1710145 0.07826087
#[2,] 0.1623188 0.08985507
#[3,] 0.1594203 0.08985507
#[4,] 0.2086957 0.17681159
#[5,] 0.2434783 0.22028986

#******************Test dataset 5*************************************
#**********Euclidean Distance (k=1,5,10,50,100)************
method1 <- "euclidean"
car_target5 <- car_train5[,7]
car_target_check5 <- car_test5[,7]
m_car_train5 <- car_train5[,-7]
m_car_test5 <- car_test5[,-7]
error_car_test5 <- matrix(NA,nrow=5,ncol=2)
knn_car_test5 <- matrix(NA,nrow=5,ncol=nrow(m_car_test5))
k_list <- c(1,5,10,50,100)
counter <- 0

for (k in k_list){
  counter <- counter+1
  knn_car_test5[counter,] <- KNNFunction(m_car_train5,m_car_test5,car_target5,method1,k)
  error_car_test5[counter,1] <- mean(as.numeric(knn_car_test5[counter,]) != car_target_check5)
}

error_car_test5
#        [,1] [,2]
#[1,] 0.10057471   NA
#[2,] 0.07758621   NA
#[3,] 0.08908046   NA
#[4,] 0.12643678   NA
#[5,] 0.15229885   NA

#**********Mahalanobis Distance (k=1,5,10,50,100)************
method3 <- "mahalanobis"
counter <- 0

for (k in k_list){
  counter <- counter+1
  knn_car_test5[counter,] <- KNNFunction(m_car_train5,m_car_test5,car_target5,method3,k)
  error_car_test5[counter,2] <- mean(as.numeric(knn_car_test5[counter,]) != car_target_check5)
}

error_car_test5
#           [,1]       [,2]
#[1,] 0.10057471 0.11494253
#[2,] 0.07758621 0.03448276
#[3,] 0.08908046 0.04597701
#[4,] 0.12643678 0.09770115
#[5,] 0.15229885 0.14080460

#************************************************************************
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
#Creating dummy variables and modifying the dataset accordingly
str(credit_data_mod)
dum<- credit_data_mod

credit_data_v1 <- model.matrix(~.-1,data = data.frame(dum$V1))
credit_data_v4 <- model.matrix(~.-1,data = data.frame(dum$V4))
credit_data_v5 <- model.matrix(~.-1,data = data.frame(dum$V5))
credit_data_v6 <- model.matrix(~.-1,data = data.frame(dum$V6))
credit_data_v7 <- model.matrix(~.-1,data = data.frame(dum$V7))
credit_data_v9 <- model.matrix(~.-1,data = data.frame(dum$V9))
credit_data_v10 <- model.matrix(~.-1,data = data.frame(dum$V10))
credit_data_v12 <- model.matrix(~.-1,data = data.frame(dum$V12))
credit_data_v13 <- model.matrix(~.-1,data = data.frame(dum$V13))
credit_data_cdummy <- data.frame(credit_data_v1,credit_data_v4,credit_data_v5,credit_data_v6,credit_data_v7,credit_data_v9,
                                 credit_data_v10,credit_data_v12,credit_data_v13)

credit_data_fin <- data.frame(credit_data_mod[,c(2,3,8,11,14,15)],credit_data_cdummy,credit_data_mod[,16])

#Creating datasets
credit_test1 <- credit_data_fin[test1_indices2,]
credit_train1 <- credit_data_fin[-test1_indices2,]
credit_test2 <- credit_data_fin[test2_indices2,]
credit_train2 <- credit_data_fin[-test2_indices2,]
credit_test3 <- credit_data_fin[test3_indices2,]
credit_train3 <- credit_data_fin[-test3_indices2,]
credit_test4 <- credit_data_fin[test4_indices2,]
credit_train4 <- credit_data_fin[-test4_indices2,]
credit_test5 <- credit_data_fin[test5_indices2,]
credit_train5 <- credit_data_fin[-test5_indices2,]

#Writing the datasets too -- for future use
#Test Datasets
write.csv(credit_test1,"credit_test1.csv", row.names = FALSE, col.names = FALSE)
write.csv(credit_test2,"credit_test2.csv", row.names = FALSE, col.names = FALSE)
write.csv(credit_test3,"credit_test3.csv", row.names = FALSE, col.names = FALSE)
write.csv(credit_test4,"credit_test4.csv", row.names = FALSE, col.names = FALSE)
write.csv(credit_test5,"credit_test5.csv", row.names = FALSE, col.names = FALSE)

#Train Datasets
write.csv(credit_train1,"credit_train1.csv", row.names = FALSE, col.names = FALSE)
write.csv(credit_train2,"credit_train2.csv", row.names = FALSE, col.names = FALSE)
write.csv(credit_train3,"credit_train3.csv", row.names = FALSE, col.names = FALSE)
write.csv(credit_train4,"credit_train4.csv", row.names = FALSE, col.names = FALSE)
write.csv(credit_train5,"credit_train5.csv", row.names = FALSE, col.names = FALSE)

#******************Test dataset 1*************************************
#**********Minkowski Distance with power=3 (k=1,5,10,50,100)************
method4 <- "minkowski"
credit_target1 <- credit_train1[,47]
credit_target_check1 <- credit_test1[,47]
m_credit_train1 <- credit_train1[,-47]
m_credit_test1 <- credit_test1[,-47]
error_credit_test1 <- matrix(NA,nrow=5,ncol=2)
knn_credit_test1 <- matrix(NA,nrow=5,ncol=nrow(m_credit_test1))
k_list <- c(1,5,10,50,100)
counter <- 0

for (k in k_list){
  counter <- counter+1
  knn_credit_test1[counter,] <- KNNFunction(m_credit_train1,m_credit_test1,credit_target1,method4,k)
  error_credit_test1[counter,1] <- mean(knn_credit_test1[counter,] != credit_target_check1)
}

error_credit_test1
#        [,1] [,2]
#[1,] 0.2923077   NA
#[2,] 0.3230769   NA
#[3,] 0.3307692   NA
#[4,] 0.2846154   NA
#[5,] 0.3000000   NA

#**********Gower's Distance (k=1,5,10,50,100)************
method5 <- "gower"
counter <- 0
m_credit_train1x <- m_credit_train1
m_credit_train1x[,-c(1,2,3,4,5,6)] <- sapply(m_credit_train1x[,-c(1,2,3,4,5,6)], FUN=function(x) ifelse(x==1, TRUE, FALSE))
m_credit_test1x <- m_credit_test1
m_credit_test1x[,-c(1,2,3,4,5,6)] <- sapply(m_credit_test1x[,-c(1,2,3,4,5,6)], FUN=function(x) ifelse(x==1, TRUE, FALSE))

for (k in k_list){
  counter <- counter+1
  knn_credit_test1[counter,] <- KNNFunction(m_credit_train1x,m_credit_test1x,credit_target1,method5,k)
  error_credit_test1[counter,2] <- mean(knn_credit_test1[counter,] != credit_target_check1)
}

error_credit_test1
#           [,1]       [,2]
#[1,] 0.2923077 0.14615385
#[2,] 0.3230769 0.08461538
#[3,] 0.3307692 0.08461538
#[4,] 0.2846154 0.08461538
#[5,] 0.3000000 0.07692308

#******************Test dataset 2*************************************
#**********Minkowski Distance with power=3 (k=1,5,10,50,100)************
method4 <- "minkowski"
credit_target2 <- credit_train2[,47]
credit_target_check2 <- credit_test2[,47]
m_credit_train2 <- credit_train2[,-47]
m_credit_test2 <- credit_test2[,-47]
error_credit_test2 <- matrix(NA,nrow=5,ncol=2)
knn_credit_test2 <- matrix(NA,nrow=5,ncol=nrow(m_credit_test2))
k_list <- c(1,5,10,50,100)
counter <- 0

for (k in k_list){
  counter <- counter+1
  knn_credit_test2[counter,] <- KNNFunction(m_credit_train2,m_credit_test2,credit_target2,method4,k)
  error_credit_test2[counter,1] <- mean(knn_credit_test2[counter,] != credit_target_check2)
}

error_credit_test2
#        [,1] [,2]
#[1,] 0.4153846   NA
#[2,] 0.3538462   NA
#[3,] 0.3461538   NA
#[4,] 0.3615385   NA
#[5,] 0.3461538   NA

#**********Gower's Distance (k=1,5,10,50,100)************
method5 <- "gower"
counter <- 0
m_credit_train2x <- m_credit_train2
m_credit_train2x[,-c(1,2,3,4,5,6)] <- sapply(m_credit_train2x[,-c(1,2,3,4,5,6)], FUN=function(x) ifelse(x==1, TRUE, FALSE))
m_credit_test2x <- m_credit_test2
m_credit_test2x[,-c(1,2,3,4,5,6)] <- sapply(m_credit_test2x[,-c(1,2,3,4,5,6)], FUN=function(x) ifelse(x==1, TRUE, FALSE))

for (k in k_list){
  counter <- counter+1
  knn_credit_test2[counter,] <- KNNFunction(m_credit_train2x,m_credit_test2x,credit_target2,method5,k)
  error_credit_test2[counter,2] <- mean(knn_credit_test2[counter,] != credit_target_check2)
}

error_credit_test2
#           [,1]      [,2]
#[1,] 0.4153846 0.2461538
#[2,] 0.3538462 0.1846154
#[3,] 0.3461538 0.2076923
#[4,] 0.3615385 0.1846154
#[5,] 0.3461538 0.1923077

#******************Test dataset 3*************************************
#**********Minkowski Distance with power=3 (k=1,5,10,50,100)************
method4 <- "minkowski"
credit_target3 <- credit_train3[,47]
credit_target_check3 <- credit_test3[,47]
m_credit_train3 <- credit_train3[,-47]
m_credit_test3 <- credit_test3[,-47]
error_credit_test3 <- matrix(NA,nrow=5,ncol=2)
knn_credit_test3 <- matrix(NA,nrow=5,ncol=nrow(m_credit_test3))
k_list <- c(1,5,10,50,100)
counter <- 0

for (k in k_list){
  counter <- counter+1
  knn_credit_test3[counter,] <- KNNFunction(m_credit_train3,m_credit_test3,credit_target3,method4,k)
  error_credit_test3[counter,1] <- mean(knn_credit_test3[counter,] != credit_target_check3)
}

error_credit_test3
#        [,1] [,2]
#[1,] 0.3076923   NA
#[2,] 0.2692308   NA
#[3,] 0.3230769   NA
#[4,] 0.3230769   NA
#[5,] 0.3307692   NA

#**********Gower's Distance (k=1,5,10,50,100)************
method5 <- "gower"
counter <- 0
m_credit_train3x <- m_credit_train3
m_credit_train3x[,-c(1,2,3,4,5,6)] <- sapply(m_credit_train3x[,-c(1,2,3,4,5,6)], FUN=function(x) ifelse(x==1, TRUE, FALSE))
m_credit_test3x <- m_credit_test3
m_credit_test3x[,-c(1,2,3,4,5,6)] <- sapply(m_credit_test3x[,-c(1,2,3,4,5,6)], FUN=function(x) ifelse(x==1, TRUE, FALSE))

for (k in k_list){
  counter <- counter+1
  knn_credit_test3[counter,] <- KNNFunction(m_credit_train3x,m_credit_test3x,credit_target3,method5,k)
  error_credit_test3[counter,2] <- mean(knn_credit_test3[counter,] != credit_target_check3)
}

error_credit_test3
#           [,1]       [,2]
#[1,] 0.3076923 0.16923077
#[2,] 0.2692308 0.09230769
#[3,] 0.3230769 0.09230769
#[4,] 0.3230769 0.10000000
#[5,] 0.3307692 0.09230769

#******************Test dataset 4*************************************
#**********Minkowski Distance with power=3 (k=1,5,10,50,100)************
method4 <- "minkowski"
credit_target4 <- credit_train4[,47]
credit_target_check4 <- credit_test4[,47]
m_credit_train4 <- credit_train4[,-47]
m_credit_test4 <- credit_test4[,-47]
error_credit_test4 <- matrix(NA,nrow=5,ncol=2)
knn_credit_test4 <- matrix(NA,nrow=5,ncol=nrow(m_credit_test4))
k_list <- c(1,5,10,50,100)
counter <- 0

for (k in k_list){
  counter <- counter+1
  knn_credit_test4[counter,] <- KNNFunction(m_credit_train4,m_credit_test4,credit_target4,method4,k)
  error_credit_test4[counter,1] <- mean(knn_credit_test4[counter,] != credit_target_check4)
}

error_credit_test4
#        [,1] [,2]
#[1,] 0.3461538   NA
#[2,] 0.3307692   NA
#[3,] 0.3307692   NA
#[4,] 0.3384615   NA
#[5,] 0.3384615   NA

#**********Gower's Distance (k=1,5,10,50,100)************
method5 <- "gower"
counter <- 0
m_credit_train4x <- m_credit_train4
m_credit_train4x[,-c(1,2,3,4,5,6)] <- sapply(m_credit_train4x[,-c(1,2,3,4,5,6)], FUN=function(x) ifelse(x==1, TRUE, FALSE))
m_credit_test4x <- m_credit_test4
m_credit_test4x[,-c(1,2,3,4,5,6)] <- sapply(m_credit_test4x[,-c(1,2,3,4,5,6)], FUN=function(x) ifelse(x==1, TRUE, FALSE))

for (k in k_list){
  counter <- counter+1
  knn_credit_test4[counter,] <- KNNFunction(m_credit_train4x,m_credit_test4x,credit_target4,method5,k)
  error_credit_test4[counter,2] <- mean(knn_credit_test4[counter,] != credit_target_check4)
}

error_credit_test4
#           [,1]      [,2]
#[1,] 0.3461538 0.2230769
#[2,] 0.3307692 0.1769231
#[3,] 0.3307692 0.1692308
#[4,] 0.3384615 0.2000000
#[5,] 0.3384615 0.1615385

#******************Test dataset 5*************************************
#**********Minkowski Distance with power=3 (k=1,5,10,50,100)************
method4 <- "minkowski"
credit_target5 <- credit_train5[,47]
credit_target_check5 <- credit_test5[,47]
m_credit_train5 <- credit_train5[,-47]
m_credit_test5 <- credit_test5[,-47]
error_credit_test5 <- matrix(NA,nrow=5,ncol=2)
knn_credit_test5 <- matrix(NA,nrow=5,ncol=nrow(m_credit_test5))
k_list <- c(1,5,10,50,100)
counter <- 0

for (k in k_list){
  counter <- counter+1
  knn_credit_test5[counter,] <- KNNFunction(m_credit_train5,m_credit_test5,credit_target5,method4,k)
  error_credit_test5[counter,1] <- mean(knn_credit_test5[counter,] != credit_target_check5)
}

error_credit_test5
#        [,1] [,2]
#[1,] 0.3684211   NA
#[2,] 0.3007519   NA
#[3,] 0.3082707   NA
#[4,] 0.3308271   NA
#[5,] 0.3458647   NA

#**********Gower's Distance (k=1,5,10,50,100)************
method5 <- "gower"
counter <- 0
m_credit_train5x <- m_credit_train5
m_credit_train5x[,-c(1,2,3,4,5,6)] <- sapply(m_credit_train5x[,-c(1,2,3,4,5,6)], FUN=function(x) ifelse(x==1, TRUE, FALSE))
m_credit_test5x <- m_credit_test5
m_credit_test5x[,-c(1,2,3,4,5,6)] <- sapply(m_credit_test5x[,-c(1,2,3,4,5,6)], FUN=function(x) ifelse(x==1, TRUE, FALSE))

for (k in k_list){
  counter <- counter+1
  knn_credit_test5[counter,] <- KNNFunction(m_credit_train5x,m_credit_test5x,credit_target5,method5,k)
  error_credit_test5[counter,2] <- mean(knn_credit_test5[counter,] != credit_target_check5)
}

error_credit_test5
#           [,1]      [,2]
#[1,] 0.3684211 0.1503759
#[2,] 0.3007519 0.1578947
#[3,] 0.3082707 0.1203008
#[4,] 0.3308271 0.1052632
#[5,] 0.3458647 0.1428571

#************************************************************************
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
