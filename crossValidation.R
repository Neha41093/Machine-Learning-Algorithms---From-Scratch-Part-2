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