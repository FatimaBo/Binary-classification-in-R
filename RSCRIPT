


## Analyzing the datasets

dim(data)
dimnames(data)[2]
head(data)
View(data)

## sum of NA  values

sum(is.na(data))

na_count <-sapply(data, function(y) sum(length(which(is.na(y)))))
na_count<- as.data.frame(na_count)

na_count$percentage<- round((na_count$na_count/nrow(data))*100,2)

#### ploting NAs for frequency visualization ########

library(ggplot2)

p<- ggplot(na_count,aes(x=percentage)) +geom_histogram(fill="#9999cc")
p

### # Removing columns where missing values are greater than or equal to 45% #####

secom<- as.data.frame(data[, -which(colMeans(is.na(data)) > 0.45)])

sum(is.na(secom))

na_count1 <-sapply(secom, function(y) sum(length(which(is.na(y)))))
na_count1<- as.data.frame(na_count1)

na_count1$percentage<- round((na_count1$na_count1/nrow(data))*100,2)

p1<- ggplot(na_count1,aes(x=percentage)) +geom_histogram(fill="#9999cc")
p1

library(gridExtra)

grid.a
grid.arrange(p,P1)

grid.arrange(p,p1)

#### save the plot ###

ggsave("na.png")

# remove all features with 0 variance and almost 

library(caret)
library(lattice)
variance0<- nearZeroVar(secom,saveMetrics = TRUE)

cols_remove<- nearZeroVar(secom)

newdata <-secom[,-cols_remove]

newdata_no_time=newdata[,-2]

# Impute missing data using KNN ###

dataframe=data.frame(newdata_no_time)
library(DMwR)
cdata <- knnImputation(dataframe)

### scaling 
sel_f=data.frame(sel_f)
sel_f$label=as.factor(sel_f$label)

#### Feature selection using BORUTA ####

library(Boruta)
label=cdata$Passed
cnolabel=cdata[,-1]

set.seed(100)
boruta.train <- Boruta(label~.,cnolabel,doTrace=2)

plot(boruta.train, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(boruta.train$ImpHistory),function(i)
  boruta.train$ImpHistory[is.finite(boruta.train$ImpHistory[,i]),i])
names(lz) <- colnames(boruta.train$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
     at = 1:ncol(boruta.train$ImpHistory), cex.axis = 0.7)

final.boruta <- TentativeRoughFix(boruta.train)
print(final.boruta)
features=getSelectedAttributes(final.boruta, withTentative = F)
boruta.df <- attStats(final.boruta)
print(boruta.df)
b=cdata[,features]
cleandata_f=cbind(label,b)


### Spliting the data 70/30#####

library(caTools)
set.seed(123)
split = sample.split(sel_f$label, SplitRatio = 0.7)
training_set = subset(sel_f, split == TRUE)
test_set = subset(sel_f, split == FALSE)


## Applying SMOTE function to Oversample Minority class(Fail) and Undersample majority class(Pass) 

library(ROSE)
data.smote <- ROSE(label ~ ., data = training_set, p=0.3,seed = 1)$data
table(data.smote$label)

## Random Forest
#install.packages("randomForest")

library(randomForest)

ctrl <- trainControl(method="repeatedcv",number = 10, repeats = 2,savePredictions = T)

set.seed(100)
datarf<-data.smote


#Data Preparation Random Forest
View(datarf)
str(datarf)
datarf$label<-as.factor(datarf$label)
table(datarf$label)
#data parttion
set.seed(123)

ind<-sample(2,nrow(datarf),replace = TRUE,prob = c(0.7,0.3))
Trainrf<-datarf[ind==1,]
Testrf<-datarf[ind==2,]

#randonForest Model

library(randomForest)
set.seed(222)

rf<- randomForest(label~.,data = Trainrf)
print(rf)  

attributes(rf) 

rf$confusion

rf$importance
hist(rf$importance)
rf$predicted

#Prediction and confusion Matrix

library(caret)

predict2<-predict(rf, Trainrf)
predict2

View(p2)
head(Trainrf$label)
head(predict2)
confusionMatrix(predict2,Trainrf$label)

#Test Data
Predict3<-predict(rf,Testrf)
cm2<-confusionMatrix(Predict3,Testrf$label, mode="everything")


#error rate Random Forest

plot(rf)
varImpPlot(rf, sort = T,
           n.var = 10,
           main = "Top - 10 variable Imprtance")

#No of nodes of the tree
hist(treesize(rf),main = "No. of Nodes for the Trees",col = "light blue")

#ROC for Random Forest achieving accuracy of 90% ###########

library(ROCR)
require(randomForest)

rf.pred11=predict(rf,Testrf,type="class")
table(rf.pred11,Testrf$label)
mean(rf.pred11==Testrf$label)
mean(rf.pred11!=Testrf$label)
pROC::r(rf.pred11)

library(ROCR)
library(pROC)

roc.test(Testrf$label,rf) 

######### confusing Matrix ############

# create the matrix 
rect(150, 430, 240, 370, col='#3F97D0')
text(195, 435, 'Pass', cex=1.2)
rect(250, 430, 340, 370, col='#F7AD50')
text(295, 435, 'Faill', cex=1.2)
text(125, 370, 'Predicted', cex=1.3, srt=90, font=2)
text(245, 450, 'Actual', cex=1.3, font=2)
rect(150, 305, 240, 365, col='#F7AD50')
rect(250, 305, 340, 365, col='#3F97D0')
text(140, 400, 'Pass', cex=1.2, srt=90)
text(140, 335, 'Fail', cex=1.2, srt=90)

# add in the cm results 
res <- as.numeric(cm$table)
text(195, 400, res[1], cex=1.6, font=2, col='white')
text(195, 335, res[2], cex=1.6, font=2, col='white')
text(295, 400, res[3], cex=1.6, font=2, col='white')
text(295, 335, res[4], cex=1.6, font=2, col='white')

# add in the specifics 
plot(c(100, 0), c(100, 0), type = "n", xlab="", ylab="", main = "DETAILS", xaxt='n', yaxt='n')
text(10, 85, names(cm$byClass[1]), cex=1.2, font=2)
text(10, 70, round(as.numeric(cm$byClass[1]), 3), cex=1.2)
text(30, 85, names(cm$byClass[2]), cex=1.2, font=2)
text(30, 70, round(as.numeric(cm$byClass[2]), 3), cex=1.2)
text(50, 85, names(cm$byClass[5]), cex=1.2, font=2)
text(50, 70, round(as.numeric(cm$byClass[5]), 3), cex=1.2)
text(70, 85, names(cm$byClass[6]), cex=1.2, font=2)
text(70, 70, round(as.numeric(cm$byClass[6]), 3), cex=1.2)
text(90, 85, names(cm$byClass[7]), cex=1.2, font=2)
text(90, 70, round(as.numeric(cm$byClass[7]), 3), cex=1.2)

# add in the accuracy information 
text(30, 35, names(cm$overall[1]), cex=1.5, font=2)
text(30, 20, round(as.numeric(cm$overall[1]), 3), cex=1.4)
text(70, 35, names(cm$overall[2]), cex=1.5, font=2)
text(70, 20, round(as.numeric(cm$overall[2]), 3), cex=1.4)


###### Type 2 error ######
rf.pred.prob<- predict(rf, Testrf, type = "prob")

################################################ END #############################################################
