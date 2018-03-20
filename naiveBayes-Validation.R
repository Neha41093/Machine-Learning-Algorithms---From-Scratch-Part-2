
#*********************************Naive Bayes Validation*****************************************

#**************************Ionosphere Dataset****************************************************
nb_ion_1 <- naiveBayes(ion_target1~. , data=ion_train1)
pred_ion_1 <- predict(nb_ion_1, ion_test1, type="class")
error_ionr_t1 <-mean(pred_ion_1 != ion_target_check1)
#0.1142857

nb_ion_2 <- naiveBayes(ion_target2~. , data=ion_train2)
pred_ion_2 <- predict(nb_ion_2, ion_test2, type="class")
error_ionr_t2 <-mean(pred_ion_2 != ion_target_check2)
#0.1

nb_ion_3 <- naiveBayes(ion_target3~. , data=ion_train3)
pred_ion_3 <- predict(nb_ion_3, ion_test3, type="class")
error_ionr_t3 <-mean(pred_ion_3 != ion_target_check3)
#0.1285714

nb_ion_4 <- naiveBayes(ion_target4~. , data=ion_train4)
pred_ion_4 <- predict(nb_ion_4, ion_test4, type="class")
error_ionr_t4 <-mean(pred_ion_4 != ion_target_check4)
#0.1285714

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
nb_car_1 <- naiveBayes(car_data_train1[,7]~. , data=car_data_train1[,-7], laplace=1)
pred_car_1 <- predict(nb_car_1, car_data_test1[,-7], type="class")
error_carr_t1 <-mean(pred_car_1 != as.character(car_data_test1[,7]))
#0.1884058

nb_car_2 <- naiveBayes(car_data_train2[,7]~. , data=car_data_train2[,-7], laplace=1)
pred_car_2 <- predict(nb_car_2, car_data_test2[,-7], type="class")
error_carr_t2 <-mean(pred_car_2 != as.character(car_data_test2[,7]))
#0.1304348

nb_car_3 <- naiveBayes(car_data_train3[,7]~. , data=car_data_train3[,-7], laplace = 1)
pred_car_3 <- predict(nb_car_3, car_data_test3[,-7], type="class")
error_carr_t3 <-mean(pred_car_3 != as.character(car_data_test3[,7]))
#0.1275362

nb_car_4 <- naiveBayes(car_data_train4[,7]~. , data=car_data_train4[,-7], laplace = 1)
pred_car_4 <- predict(nb_car_4, car_data_test4[,-7], type="class")
error_carr_t4 <-mean(pred_car_4 != as.character(car_data_test4[,7]))
#0.1913043

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

#************************************Credit Approval Dataset****************************************************
nb_credit_1 <- naiveBayes(credit_data_train1[,16]~. , data=credit_data_train1[,-16])
pred_credit_1 <- predict(nb_credit_1, credit_data_test1[,-16], type="class")
error_creditr_t1 <-mean(pred_credit_1 != as.character(credit_data_test1[,16]))
#0.1923077

nb_credit_2 <- naiveBayes(credit_data_train2[,16]~. , data=credit_data_train2[,-16])
pred_credit_2 <- predict(nb_credit_2, credit_data_test2[,-16], type="class")
error_creditr_t2 <-mean(pred_credit_2 != as.character(credit_data_test2[,16]))
#0.2769231

nb_credit_3 <- naiveBayes(credit_data_train3[,16]~. , data=credit_data_train3[,-16])
pred_credit_3 <- predict(nb_credit_3, credit_data_test3[,-16], type="class")
error_creditr_t3 <-mean(pred_credit_3 != as.character(credit_data_test3[,16]))
#0.1846154

nb_credit_4 <- naiveBayes(credit_data_train4[,16]~. , data=credit_data_train4[,-16])
pred_credit_4 <- predict(nb_credit_4, credit_data_test4[,-16], type="class")
error_creditr_t4 <-mean(pred_credit_4 != as.character(credit_data_test4[,16]))
#0.3

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