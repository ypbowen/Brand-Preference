install.packages('caret',dependencies=c("Depends", "Suggests"))
library(caret)
install.packages('lattice')
library(lattice)
install.packages("ggplot2")
library(ggplot2)
install.packages("randomForest")
library(randomForest)
install.packages('mlbench',dependencies=c("Depends", "Suggests"))
library(mlbench)
data=read.csv('C:/Users/ypbow/Downloads/SurveyData/CompleteResponses.csv')
test=read.csv('C:/Users/ypbow/Downloads/SurveyData/surveyIncomplete.csv')
test$brand<-NULL
attributes(data)
summary(data)
summary(data$brand)
str(data)
names(data)
hist(data$salary)
hist(data$brand,main="Brand Prefered", xlab="Brand: 0-Acer    1-Sony", ylab="Count")
hist(data$age)
hist(data$car)
hist(data$zipcode)
hist(data$brand)
plot(data$salary,data$brand)
plot(data$age, data$elevel)
qqnorm(data$age)     
qqnorm(data$salary)
qqnorm(data$age)
qqnorm(data$brand)
sum(is.na(data))
set.seed(123)

1.  #FIRST EXAMPLE

#Following the tutorial in resources
# define an 75%/25% train/test split of the dataset
data$brand<-as.factor(data$brand)
names(data)
str(data)


inTrain <- createDataPartition(data$brand, p=.75, list = FALSE)
training <- data[ inTrain,]
testing  <- data[-inTrain,]
#10 fold cross validation
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)

#train Random Forest linear model with a tuneLenght = 1 (trains with 1 mtry value for RandomForest)

rfFit1 <- train(brand~., data = training, method = "rf", trControl=fitControl, tuneLength = 1)
#training results

rfFit1
#plot(rfFit1)

importance1<-varImp(rfFit1, scale=FALSE)
plot(importance1)

predrf1=predict(rfFit1,test)

test$modelPred<-predrf1
head(test)
plot(test$modelPred, main="Brand  Prediction DT",xlab="Brand: 0-Acer    1-Sony",ylab="count")

test1=read.csv('C:/Users/ypbow/Downloads/SurveyData/surveyIncomplete.csv')
test1$brand=as.factor(test1$brand)


postResample(test$modelPred,test1$brand)

summary(test$modelPred)





2.  #SECOND EXAMPLE
#train Linear Regression model with a tuneLenght = 2 (trains with 2 mtry values for RandomForest)
data$brand<-as.factor(data$brand)
inTrain <- createDataPartition(data$brand, p=.75, list = FALSE)
training <- data[ inTrain,]
testing  <- data[-inTrain,]
fitControl <- trainControl(method = "repeatedcv", number =10, repeats = 1)
rfFit2 <- train(brand~., data = training, method = "rf", trControl=fitControl, tuneLength = 2)

#training results

rfFit2
plot(rfFit2)

importance2<-varImp(rfFit2, scale=FALSE)
plot(importance2)

predrf2<-predict(rfFit2,test)
test$modelPred<-predrf2
plot(test$modelPred, main="Model Prediction DT 2",xlab="Brand: 0-Acer    1-Sony",ylab="count")


test2<-read.csv('C:/Users/ypbow/Downloads/SurveyData/surveyIncomplete.csv')
test2$brand<-as.factor(test2$brand)

postResample(test$modelPred,test2$brand)

summary(test$modelPred)


#2TL 5.  # EXAMPLE 2.a TL5 
#train Linear Regression model with a tuneLenght = 2 (trains with 2 mtry values for RandomForest)
data$brand<-as.factor(data$brand)
inTrain <- createDataPartition(data$brand, p=.75, list = FALSE)
training <- data[ inTrain,]
testing  <- data[-inTrain,]
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)
rfFit5 <- train(brand~., data = training, method = "rf", trControl=fitControl, tuneLength = 5)

#training results

rfFit5
plot(rfFit5)

importance5<-varImp(rfFit5, scale=FALSE)
plot(importance5)




3. # THIRD EXAMPLE
#caret model - Random Forest Manual Tuning Grid

data$brand<-as.factor(data$brand)
names(data)
str(data)
inTrain <- createDataPartition(data$brand, p=.75, list = FALSE)
training <- data[ inTrain,]
testing  <- data[-inTrain,]

#10 fold cross validation
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)
#dataframe for manual tuning of mtry

rfGrid <- expand.grid(mtry=c(1,2,3))

#train Random Forest Regression model
#note the system time wrapper. system.time()
#this is used to measure process execution time 
system.time(rfFitm1 <- train(brand~., data = training, method = "rf", trControl=fitControl, tuneGrid=rfGrid))
#training results

rfFitm1
plot(rfFitm1)


importance3<-varImp(rfFitm1, scale=FALSE)
plot(importance3)

predrf3<-predict(rfFitm1,test)
test$modelPred<-predrf3
plot(test$modelPred, main="Model Prediction Manual Tuning",xlab="Brand: 0-Acer    1-Sony",ylab="count")

test3<-read.csv('C:/Users/ypbow/Downloads/SurveyData/surveyIncomplete.csv')
test3$brand<-as.factor(test3$brand)

postResample(test$modelPred,test3$brand)

summary(test$modelPred)





4. # FOURTH EXAMPLE
data$brand<-as.factor(data$brand)
names(data)
str(data)
inTrain <- createDataPartition(data$brand, p=.75, list = FALSE)
training <- data[ inTrain,]
testing  <- data[-inTrain,]

#10 fold cross validation
rfitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1, search = 'random')

#train Random Forest Regression model

rfFitr2 <- train(brand~., data = training, method = "rf", trControl=rfitControl)


#training results

rfFitr2
plot(rfFitr2)

importance4=varImp(rfFitr2, scale=FALSE)
plot(importance4)


predrf4=predict(rfFitr2,test)
test$modelPred=predrf4
plot(test$modelPred,main=search = 'random')

test4<-read.csv('C:/Users/ypbow/Downloads/SurveyData/surveyIncomplete.csv')
test4$brand<-as.factor(test4$brand)

postResample(test$modelPred,test4$brand)

summary(test$modelPred)


  # RF 4.a

control<-trainControl(method='repeatedcv',number=10,repeats=3,search='random')
set.seed(123)
rf_random<-train(brand~.,data=training,method='rf',metric='Accuracy',tuneLength=5,trControl=control)

print(rf_random)
plot(rf_random)

importance4a<-varImp(rf_random, scale=FALSE)
plot(importance4a)

predrf4a<-predict(rf_random,test)
test$modelPred<-predrf4a
plot(test$modelPred)

test4a<-read.csv('C:/Users/ypbow/Downloads/SurveyData/surveyIncomplete.csv')
test4a$brand<-as.factor(test4a$brand)

postResample(test$modelPred,test4a$brand)

summary(test$modelPred)


#RF 4.b

control<-trainControl(method='repeatedcv',number=10,repeats=3,search='grid')

set.seed(123)
tunegrid<-expand.grid(.mtry=(1:5))

rf_gs<-train(brand~.,data=training,method='rf',metric='Accuracy',tuneGrid=tunegrid,trControl=control)

print(rf_gs)
plot(rf_gs)

importance<-varImp(rf_gs, scale=FALSE)
plot(importance)


predrf4b<-predict(rf_gs,test)
test$modelPred<-predrf4b

plot(test$modelPred, main="Tune Grid")


test4b<-read.csv('C:/Users/ypbow/Downloads/SurveyData/surveyIncomplete.csv')
test4b$brand<-as.factor(test4b$brand)

postResample(test$modelPred,test4b$brand)

summary(test$modelPred)






6. #EXAMPLE WITH GBM

data$brand=as.factor(data$brand)
names(data)
str(data)

inTrain <- createDataPartition(data$brand, p=.75, list = FALSE)
training <- data[ inTrain,]
testing  <- data[-inTrain,]
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)
gbmFit1 <- train(brand~., data = training, method = "gbm", trControl=fitControl, verbose=FALSE)



gbmFit1
plot(gbmFit1)

summary(data$brand)

importancegbm=varImp(gbmFit1, scale=FALSE)
plot(importancegbm)


predgbm=predict(gbmFit1,test)
test$modelPred=predgbm
plot(test$modelPred)



testgbm<-read.csv('C:/Users/ypbow/Downloads/SurveyData/surveyIncomplete.csv')
testgbm$brand<-as.factor(testgbm$brand)

postResample(test$modelPred,testgbm$brand)

summary(test$modelPred)






9. #EXAMPLE WITH GBM 3 repeats

data$brand<-as.factor(data$brand)
names(data)
str(data)

inTrain <- createDataPartition(data$brand, p=.75, list = FALSE)
training <- data[ inTrain,]
testing  <- data[-inTrain,]
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
gbmFit3 <- train(brand~., data = training, method = "gbm", trControl=fitControl, tuneLength = 1,verbose=FALSE)
gbmFit3

10. #EXAMPLE WITH GBM 4 repeats

data$brand<-as.factor(data$brand)
names(data)
str(data)

inTrain <- createDataPartition(data$brand, p=.75, list = FALSE)
training <- data[ inTrain,]
testing  <- data[-inTrain,]
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 4)
gbmFit4 <- train(brand~., data = training, method = "gbm", trControl=fitControl, tuneLength = 1,verbose=FALSE)
gbmFit4


11. #EXAMPLE WITH GBM 5 repeats

data$brand<-as.factor(data$brand)
names(data)
str(data)

inTrain <- createDataPartition(data$brand, p=.75, list = FALSE)
training <- data[ inTrain,]
testing  <- data[-inTrain,]
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 5)
gbmFit5 <- train(brand~., data = training, method = "gbm", trControl=fitControl, tuneLength = 1,verbose=FALSE)
gbmFit5


12.  #EXAMPLE WITH c5.0

data$brand<-as.factor(data$brand)
names(data)

str(data)

inTrain <- createDataPartition(data$brand, p=.75, list = FALSE)
training <- data[ inTrain,]
testing  <- data[-inTrain,]
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)
C5.0Fit1 <- train(brand~., data = training, method = "C5.0", trControl=fitControl, tuneLength = 1,verbose=FALSE)
C5.0Fit1
plot(C5.0Fit1)


importancec5<-varImp(C5.0Fit1, scale=FALSE)
plot(importancec5)


predrfc5<-predict(C5.0Fit1,test)
test$modelPred<-predrfc5

plot(test$modelPred)


testc5<-read.csv('C:/Users/ypbow/Downloads/SurveyData/surveyIncomplete.csv')
testc5$brand<-as.factor(testc5$brand)

postResample(test$modelPred,testc5$brand)

summary(test$modelPred)



#C5.0 v2
inTrain <- createDataPartition(data$brand, p=.75, list = FALSE)
training <- data[ inTrain,]
testing  <- data[-inTrain,]
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
C5.0Fit2 <- train(brand~., data = training, method = "C5.0", metric='Accuracy', trControl=fitControl, tuneLength = 5,verbose=FALSE)
C5.0Fit2

plot(C5.0Fit2)


importancec5v2<-varImp(C5.0Fit2, scale=FALSE)
plot(importancec5)


predrfc5v2<-predict(C5.0Fit2,test)
test$modelPred<-predrfc5v2

plot(test$modelPred)



testc5v2=<-read.csv('C:/Users/ypbow/Downloads/SurveyData/surveyIncomplete.csv')
test$brand<-NULL
str(test)


postResample(test$modelPred,testc5v2$brand)

summary(test$modelPred)


#Using varImp()

# REMOVE REDUNDANT FEATURES
data()
data$brand<-as.factor(data$brand)
names(data)
str(data)
correlationMatrix <- cor(data[,1:6])
print(correlationMatrix)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5)
print(highlyCorrelated)

#RANK FEATURES BY IMPORTANCE
set.seed(7)
data()
control <- trainControl(method="repeatedcv", number=10, repeats=3)
model <- train(brand~., data=data, method="rf", preProcess="scale", trControl=control)
importance <- varImp(model, scale=FALSE)
print(importance)
plot(importance)

#FEATURE SELECTION
data()
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
results <- rfe(data[,1:6], data[,7], sizes=c(1:6), rfeControl=control)
print(results)
predictors(results)
plot(results, type=c("g", "o"))




