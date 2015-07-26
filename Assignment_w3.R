
## function that determines if a columns has a number of NA and NULL fields above a certain threshold (in percentaje)
nullcol <- function (x,thresh) {

nullmatrix<-is.na(x)+is.null(x)
numnulls<-apply(nullmatrix,2,sum)
dim(numnulls)<-c(1,dim(x)[2])
nullcol<-(numnulls/dim(x)[1]>thresh)

}

##loading libraries

library("caret")

## reading the raw data. It is important to convert to NA certain weird strings (#DIV/0!) and empty values
data<-read.csv("pml-training.csv",sep = ",",dec=".",na.string=c("NA","NaN","","#DIV/0!"))

## using the 'nullcol' function it is easy to take out the columns where more than 80% of the records are null
data_light<-data[,nullcol(data,0.8)==FALSE]

## the number of rows is high, so we use 75% for the training data set
inTrain<-createDataPartition(y=data_light$classe,p=0.75,list=FALSE)
training<-data_light[inTrain,]
testing<-data_light[-inTrain,]

## preprocessing with PCA seems usefull to reduce the number of rows. Previously we checked that 
## there were a lot of pairs of columns with a correlation greater than 80%
## we take out columns that are of class 'factor' or with unusefull information

cols_out<-c(-1,-2,-3,-4,-5,-6,-7,-60)
preProc<-preProcess(training[,cols_out],method="pca",pcaComp=4)
trainPC <- predict(preProc,training[,cols_out])

## launching train for a Random Forest will need some time. Time to go take the nap ... :)
modelFit<-train(training$classe ~ .,method="rf",data=trainPC)

## predicting the results of the test set and calculating the accuracy with confusionMatrix
## PCA has to be used with the testing data set previously to do the prediction

testPC <- predict(preProc,testing[,cols_out])
testpredict<-predict(modelFit,testPC)
confusionMatrix(testing$classe,testpredict)

## finally we apply the same procedures with the testing csv file (including PCA), and predict the
## answer with our model

valdata<-read.csv("pml-testing.csv",sep = ",",dec=".",na.string=c("NA","NaN","","#DIV/0!"))
valdata_not_null<-valdata[,nullcol(data,0.8)==FALSE]
valdataPC<-predict(preProc,valdata_not_null[,cols_out])
testpredict_valdata<-predict(modelFit,valdataPC)
answers<-testpredict_valdata

