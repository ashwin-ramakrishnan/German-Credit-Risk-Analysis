#German Credit Risk Analysis

#Install necessary packages
install.packages("rpart")
library('rpart')

library(data.table)
library(dplyr)
library(ggplot2)
library(knitr)
library(stringr)
library(DT)

mdData=read.csv('GermanCredit_assgt_S18.csv', header=TRUE, sep=",")  #load the data from the csv file

summary(mdData)  
attributes(mdData)

library(pastecs)
options(scipen=999)
options(digits=4)


#Perform descriptive statistics

statistics<-stat.desc(mdData)
install.packages("xlsx")
library(xlsx)
write.xlsx(statistics,file = "statistics.xlsx") #Write results of descriptive statistics to a excel file

#Impute missing values
#Replace missing values with 0 for the below variables since they are the majority class:

mdData$NEW_CAR[is.na(mdData$NEW_CAR)] <- 0
mdData$USED_CAR[is.na(mdData$USED_CAR)] <- 0
mdData$FURNITURE[is.na(mdData$FURNITURE)] <- 0
mdData$RADIO.TV[is.na(mdData$RADIO.TV)] <- 0
mdData$EDUCATION[is.na(mdData$EDUCATION)] <- 0
mdData$RETRAINING[is.na(mdData$RETRAINING)] <- 0


dataset<-mdData[c(1:32)]

#Impute missing values of age with the mean of the column values.
mdData$AGE[is.na(mdData$AGE)] <- round(mean(dataset$AGE, na.rm = TRUE))

#Converting Factor Variables
cols<-dataset[-c(1,3,11,23)]
colnames<-names(cols)
dataset[colnames]<-lapply(dataset[colnames],factor)
sapply(dataset,class)

#Lets look at the distribution of the response variable
plot(dataset$RESPONSE)


#Univariate analysis of the Foreign column
tmp<-dataset%>% 
  group_by(FOREIGN) %>%
  summarise(count = n()) %>%
  mutate(proportion = count/sum(count))

tmp %>% 
    ggplot(aes(x=FOREIGN,y = count, fill=FOREIGN))+
  geom_bar(stat="identity")


#Let us understand any interesting relationships between dependent variable and the independent variables,

#Bivariate analysis
dat <- data.frame(table(dataset$FOREIGN, dataset$RESPONSE))
names(dat) <- c("FOREIGN", "RESPONSE", "Count")
ggplot(data=dat, aes(x=FOREIGN, y=Count, fill=RESPONSE)) + geom_bar(stat="identity")

#Does buying a new car affect the credit risk response variable?
dat <- data.frame(table(dataset$NEW_CAR, dataset$RESPONSE))
names(dat) <- c("NEW_CAR", "RESPONSE", "Count")
ggplot(data=dat, aes(x=NEW_CAR, y=Count, fill=RESPONSE)) + geom_bar(stat="identity")


#Correlation Plots
library('corrplot')
correlation_Spearman<-cor(subset(dataset, select =  c(3,11,23)), use="complete.obs", method="spearman")
corrplot(correlation_Spearman,method= "pie",tl.cex = 0.8,tl.offset = 1.5,number.cex = 0.7,type = 'lower')


dataset_2<- dataset[,sapply(dataset,nlevels)>1]
dataset_numeric<-subset(dataset, select =  c(3,11,23))

List_Significant_flags<-list()

#Chisquare test results 
for (i in names(dataset_2))
{
  
  if(!is.numeric(dataset_2[i]))
    
  {
    chisq<-chisq.test(dataset_2[i],dataset_2$RESPONSE)  #Chisquare test w.r.t the response variable
    
    if(chisq$p.value <0.05)  #Checking for significance levels of the chi-square test results. 
    {
      print(i)
      List_Significant_flags<-c(List_Significant_flags,i)
      out <- capture.output(print(chisq))
      i<-capture.output(print(i))
      #
    cat(out, file="chisq_results.doc", sep="\n", append=TRUE)
      cat(i, file="chisq_results.doc", sep="\n", append=TRUE)
      
    }
  }# print(chisq$residuals)
}

myvars<- names(dataset) %in% List_Significant_flags | names(dataset) %in% names(dataset_numeric)

dataset_final<- dataset[,myvars]  #The data set is now cleaned and ready for analysis and models. 


# MODELS:

rpModel1=rpart(RESPONSE ~ ., data=dataset, method="class")  

print(rpModel1)

summary(rpModel1)

options(scipen=999)
options(digits=4)

plot(rpModel1, uniform=TRUE,  main="Decision Tree for Credit defaulters")
text(rpModel1, use.n=TRUE, all=TRUE, cex=.7)

install.packages("rpart.plot")
library(rpart.plot)

rpart.plot::prp(rpModel1, type=2, extra=1)



#Decision Tree Model

nr=nrow(dataset)

trnIndex = sample(1:nr, size = round(0.7*nr), replace=FALSE) 
mdTrn=dataset_final[trnIndex,] #Train Set
mdTst = dataset_final[-trnIndex,] #Test Set

rpModel2=rpart(RESPONSE ~ ., data=mdTrn, method="class")

predTrn=predict(rpModel2, mdTrn, type='class')
predTrn=predict(rpModel2, mdTrn, type='class')

table(pred = predTrn, true=mdTrn$RESPONSE)

mean(predTrn==mdTrn$RESPONSE)


#table(pred=predict(rpModel2,mdTst, type="class"), true=mdTst$RESPONSE)

#predTrnProb=predict(rpModel2, mdTrn, type='prob')
#head(predTrnProb)

#library("caret")

#trnSc <- subset(mdTrn, select=c("RESPONSE"))  # selects the OUTCOME column into trnSc
#trnSc["score"]<-predTrnProb[, 1]  

#trnSc<-trnSc[order(trnSc$score, decreasing=TRUE),]
#trnSc$RESPONSE<-as.numeric(trnSc$RESPONSE)
#trnSc$cumDefault<-cumsum(trnSc$RESPONSE)
#head(trnSc)
#plot(seq(nrow(trnSc)), trnSc$cumDefault,type = "l", xlab='#cases', ylab='#default')

library('ROCR')

cm <- table(pred=predict(rpModel2,mdTst, type="class"), true=mdTst$RESPONSE)
n = sum(cm) # number of instances
d # number of correctly classified instances per class 
rowsums = apply(cm, 2, sum) # number of instances per class
colsums = apply(cm, 1, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted classes
accuracy = sum(diag) / n 
accuracy
precision = diag / colsums 
recall = diag / rowsums 
f1 = 2 * precision * recall / (precision + recall) 

CTHRESH=0.5

predProbTrn=predict(rpModel2, mdTrn, type='prob')
#Confusion table
predTrn = ifelse(predProbTrn[,'1'] >= CTHRESH, '1', '0')
ct = table( pred = predTrn, true=mdTrn$RESPONSE)
#Accuracy
mean(predTrn==mdTrn$RESPONSE)

library(ROCR)
#score test data set
mdTst$score<-predict(rpModel2,type='prob',mdTst)
pred<-prediction(mdTst$score[,2],mdTst$RESPONSE)
perf <- performance(pred,"tpr","fpr")
plot(perf)


costMatrix <- matrix(c(0,1,5, 0), byrow=TRUE, nrow=2)
colnames(costMatrix) <- c('Predict Good','Predict Bad')
rownames(costMatrix) <- c('Actual Good','Actual Bad')
costMatrix



rpTree = rpart(RESPONSE ~ ., data=mdTrn, method="class", parms = list( prior = c(.70,.30), loss = costMatrix, split = "information"))


th = costMatrix[2,1]/(costMatrix[2,1] + costMatrix[1,2])
th

#Random Forest Model 
library('randomForest')

set.seed(123)
mdTrn.imputed <- rfImpute(RESPONSE ~ ., mdTrn)


rfModel = randomForest(factor(RESPONSE) ~ ., data=mdTrn.imputed, ntree=200, importance=TRUE )

importance(rfModel)
varImpPlot(rfModel)

perf_rf=performance(prediction(predict(rfModel,mdTst, type="prob")[,2], mdTst$RESPONSE), "tpr", "fpr")
plot(perf_rf)

row.has.na <- apply(mdTrn, 1, function(x){any(is.na(x))})
predictors_no_NA <- mdTrn[!row.has.na,]


