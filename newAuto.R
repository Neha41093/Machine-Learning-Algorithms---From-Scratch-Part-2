#3 -- Creation of new.Auto dataset for Logistic Regression
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

#write the new.Auto data to a csv file
write.csv(as.matrix(new.Auto),file="new_Auto.csv",row.names=FALSE)