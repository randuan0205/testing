install.packages("doParallel")
library(caret)
library(parallel)
library(doParallel)
library(rpart)
# read csv files
rawdata<-read.csv("pml-training.csv",header=T)
finaltestdata<-read.csv("pml-testing.csv",header=T)
dim(rawdata)
dim(finaltestdata)

# pick up predictors
   # exclude variables with missing values and keep major predictors (belt,arm,dumbbell,forearm)
Missingdata <- sapply(rawdata, function (x) any(is.na(x) | x == ""))  
potentialpredictor<-!Missingdata & grepl("belt|[^(fore)]arm|dumbbell|forearm", names(Missingdata))
predictorlist <- names(Missingdata)[potentialpredictor]
filterdata<-rawdata[,c("classe", predictorlist)]
dim(filterdata)
finaltest<-finaltestdata[,predictorlist]
dim(finaltest)

# data preprocess 1 - split training and testing set
   #split the training data into 60% training and 40% testing, which is purposed for out-of-sample error estimate
set.seed (123)
intrain<-createDataPartition(filterdata$classe,p=0.6,list=FALSE)
datatraining<-filterdata[intrain,]
datatesting<-filterdata[-intrain,]

# data preprocess 2 - center&scaling
preobj<-preProcess(datatraining[,-1],method=c("center","scale"))
datatrainingS<-predict(preobj,datatraining[,-1])
datatrainingS<-data.frame(datatraining$classe,datatrainingS)
datatestingS<-predict(preobj,datatesting[,-1])
datatestingS<-data.frame(datatesting$classe,datatestingS)
finaltestS<-predict(preobj,finaltest)

# model training - decison tree step1 - model fit (results show 32.7% training error rate)
dtfit <- tree(datatraining.classe ~ ., data=datatrainingS)
summary(dtfit)
plot(dtfit)
text(dtfit,pretty=0)
# model training - decision tree step2 - apply to test set to estimate out-of-sample error rate
  # results show 66% testing accuracy rate
dtpred<-predict(dtfit,datatestingS,type="class")
confusionMatrix(dtpred, datatestingS[, "datatesting.classe"])

# model training - random forrest step1 - model fit (allowing parallel to accelerate model building)
  #model used 35 reps bootstrapp and tried 27 variables for each split decision
c1 <- makeCluster(detectCores() - 1)
registerDoParallel(c1)
ctrl <- trainControl(classProbs=TRUE,
                     savePredictions=TRUE,
                     allowParallel=TRUE)
system.time(rffit <- train(datatraining.classe ~ ., data=datatrainingS, method="rf"))
stopCluster(c1)
rffit
trainpred <- predict(rffit, datatrainingS)
confusionMatrix(trainpred, datatrainingS[, "datatraining.classe"])

# model training - random forrest step2 - apply to test set to estimate out-of-sample error rate
   #random forests achieved 99% accuracy rate on testing set, beating decision tree. This model will be used for final test
testpred <- predict(rffit, datatestingS)
confusionMatrix(testpred, datatestingS[, "datatesting.classe"])

# model training - random forrest step3 - save model
rffit$finalModel

#Apply to assigned test data
finaltestpred <- predict(rffit, finaltestS)
answers=finaltestpred

#results submission
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}
pml_write_files(answers)


