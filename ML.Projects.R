install.packages("doParallel")
library(caret)
library(parallel)
library(doParallel)

# read csv files
rawdata<-read.csv("pml-training.csv",header=T)
finaltestdata<-read.csv("pml-testing.csv",header=T)
dim(rawdata)
dim(finaltestdata)

# pick up predictors
Missingdata <- sapply(rawdata, function (x) any(is.na(x) | x == ""))  
potentialpredictor<-!Missingdata & grepl("belt|[^(fore)]arm|dumbbell|forearm", names(Missingdata))
predictorlist <- names(Missingdata)[potentialpredictor]
filterdata<-rawdata[,c("classe", predictorlist)]
dim(filterdata)

finaltest<-finaltestdata[,predictorlist]
dim(finaltest)

# data preprocess 1 - split training and testing set
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

#? data preprocess 3 - check predictor correlation
datatrainingS$classe<-factor(datatrainingS$classe,levels=c("A","B","C","D","E"))
datatrainingS <- datatrainingS[, classe = factor(datatrainingS[, classe])]
glmfit<-glm(classe~.,data=filgerdata,family=binomial)
glmfit<-train(classe~.,method="glm",data=datatraining,preProcess=c("center","scale"))
vif(datatraining[,-1])

# model training - random forrest 1 - model fit
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

# model training - random forrest 2 - apply to test set
testpred <- predict(rffit, datatestingS)
confusionMatrix(testpred, datatestingS[, "datatesting.classe"])

# model training - random forrest 3 - save model
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

# Model Training - Logistic regression

# Model Training - Naive Bayes

