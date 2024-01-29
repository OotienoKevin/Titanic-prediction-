#I go this data "TITANIC" from kaggle ,special thanks to kaggle family
#the competition was to build a predictive model that predict if a passenger survived or not
#we were  provided with 2 data set ,train data and test data,train for building the model
train<-read.csv(file.choose())
test<-read.csv(file.choose())
dim(train)
dim(test)
#lets see the sample data of the train
print(head(train))
View(train)
#we have some missing data in the Age variable,so lets clean the data
mean(train$Age)#since we have some missing values we get NA
summary(train)
train$Age<-ifelse(is.na(train$Age),
                  median(train$Age, na.rm = TRUE),
                  train$Age)
summary(train)
mean(train$Age)
dim(train)
##we have succeffully get rid of the missing values
#lets confirm if we have outliers 
png(file="diabet.png")
boxplot(train$Age,train$Fare)#we have outliers ,,,remove the outliers
dev.off()
outliers_age<-boxplot.stats(train$Age)$out
outliers_fare<-boxplot.stats(train$Fare)$out
rows_with_outliers_age<-which(train$Age %in% outliers_age)
rows_with_outliers_fare<-which(train$Fare %in% outliers_fare)
data_no_outliers<-train[-c(rows_with_outliers_age,rows_with_outliers_fare),]
boxplot(data_no_outliers$Age,data_no_outliers$Fare)
dim(data_no_outliers)
###successfully reduced the nmber of outliers
summary(data_no_outliers)
View(data_no_outliers)
data_no_outliers$Pclass<-as.factor(data_no_outliers$Pclass)
data_no_outliers$Survived<-as.factor(data_no_outliers$Survived)
##now lets build our model with random forest 
library(randomForest)
model<-randomForest(data_no_outliers$Survived~.,data = data_no_outliers)
test$Pclass<-as.factor(test$Pclass)
#remove missing values from our data
test$Age<-ifelse(is.na(test$Age),
                  median(test$Age, na.rm = TRUE),
                  test$Age)
mean(test$Age)
summary(test)
test$Fare<-ifelse(is.na(test$Fare),
                  median(test$Fare,na.rm = TRUE),
                  test$Fare)
summary(test)
test$Pclass<-as.factor(test$Pclass)
predict_test<-predict(model,newdata = test)
result<-data.frame(test$PassengerId, predict_test)

##export data to excel as a csv file
write.csv(result, "mresult.csv")

