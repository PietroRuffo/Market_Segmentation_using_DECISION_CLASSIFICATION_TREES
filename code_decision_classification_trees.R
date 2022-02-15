
library(openxlsx)
my_data<-read.xlsx("tvprices.xlsx",sheet="tvprices")

#1
library(partykit)
library(CHAID)

input_data1<-my_data[,2:7] 
head(input_data1)
str(input_data1)
summary(input_data1)

input_data1$Screen<-as.factor(ifelse(input_data1$Screen>mean(input_data1$Screen),"big","small"))
input_data1$DCR<-as.factor(ifelse(input_data1$DCR>mean(input_data1$DCR),"high","low"))
input_data1$LCD.Hz<-as.factor(ifelse(input_data1$LCD.Hz>mean(input_data1$LCD.Hz),"fast","slow"))
input_data1$NatRes<-as.factor(input_data1$NatRes)
input_data1$Price<-as.factor(ifelse(input_data1$Price>mean(input_data1$Price),"expensive","cheap"))
names(input_data1)[1]<-"Category"
input_data1$Category<-as.factor(input_data1$Category)

head(input_data1)
str(input_data1)

#searching for the best variable to split the root
t1<-chisq.test(input_data1$Category,input_data1$Screen)
t2<-chisq.test(input_data1$Category,input_data1$DCR)
t3<-chisq.test(input_data1$Category,input_data1$LCD.Hz)
t4<-chisq.test(input_data1$Category,input_data1$NatRes)
t5<-chisq.test(input_data1$Category,input_data1$Price)

ris.test<-cbind(rbind(t1$statistic,t2$statistic,t3$statistic,t4$statistic,t5$statistic),
                rbind(t1$p.value,t2$p.value,t3$p.value,t4$p.value,t5$p.value))
row.names(ris.test)<-colnames(input_data1[,2:6])
colnames(ris.test)[2]<-"p value"
ris.test
which.min(ris.test[,2])
which.max(ris.test[,1])

ctrl<-chaid_control(minsplit=5,alpha4=0.0002)
#0.0002 it splits only if there is a strong dependence between explanatory variable and response variable
tree<-chaid(Category~NatRes+Price+Screen+DCR+LCD.Hz,data=input_data1,control=ctrl)

tree
plot(tree,main="Classification Tree for TV Models",margin=1.5,
     gp=gpar(col=c("darkblue"),lty=1,lwd=2,fontsize=13))

#Misclassification rate estimation (resubstitution rate)
table(predict(tree)==tree$fitted[,2])["FALSE"]/nrow(input_data1)

#Misclassification rate estimation (cross validation)
library(plyr)
set.seed(12345)
folds<-split(input_data1,cut(sample(1:nrow(input_data1)),5))
errs<-rep(NA,length(folds))

for(i in 1:length(folds)){
  test<-ldply(folds[i],data.frame)
  train<-ldply(folds[-i],data.frame)
  tmp.model<-chaid(Category~NatRes+Price+Screen+DCR+LCD.Hz,train,control=ctrl)
  tmp.predict<-predict(tmp.model,test)
  conf.mat<-table(test$Category,tmp.predict)
  errs[i]<-1-sum(diag(conf.mat))/sum(conf.mat)
}
print(sprintf("average error using 5-fold cross-validation: %.3f percent",100*mean(errs)))

pred<-predict(tree)
#confusion matrix
library(caret)
confusionMatrix(data=pred,reference=input_data1$Category)$table

#2
library(rpart)
library(rpart.plot)

input_data2<-my_data[,2:7] 
head(input_data2)

summary(input_data2)

names(input_data2)[1]<-"Category"
input_data2$Category<-as.factor(input_data2$Category)
input_data2$LCD.Hz<-as.factor(input_data2$LCD.Hz)
input_data2$NatRes<-as.factor(input_data2$NatRes)

str(input_data2)

set.seed(12345)

ctrl2<-rpart.control(minsplit=5,cp=0,xval=5)
classification_tree<-rpart(Category~NatRes+Price+DCR+Screen+LCD.Hz,
              data=input_data2,method="class",control=ctrl2)

classification_tree
summary(classification_tree)
rpart.plot(classification_tree,cex=0.8,type=4,extra=1,shadow.col="gray",uniform=T,branch=1,box.palette=list("green","pink","yellow","lightblue"))

#optimal sequence of trees (T1=root,T2,T3,T4=Tmax) vs relative error graph 
library(ggplot2)
ggp<-ggplot(data=data.frame(classification_tree$cptable,Tree.number=1:nrow(classification_tree$cptable)),
            mapping=aes(x=Tree.number,y=rel.error))
ggp<-ggp+geom_line()
ggp<-ggp+geom_point()
ggp

#Misclassification rate estimation (resubstitution rate)
table(predict(classification_tree,type="vector")==input_data2$Category)["FALSE"]/nrow(input_data2)

#Misclassification rate estimation (cross validation)
set.seed(12345)
folds<-split(input_data2,cut(sample(1:nrow(input_data2)),5))
errs<-rep(NA,length(folds))

for(i in 1:length(folds)){
  test<-ldply(folds[i],data.frame)
  train<-ldply(folds[-i],data.frame)
  tmp.model<-rpart(Category~Screen+DCR+LCD.Hz+Price+NatRes,train,control=ctrl2)
  tmp.predict<-predict(tmp.model,test,type="class")
  conf.mat<-table(test$Category,tmp.predict)
  errs[i]<-1-sum(diag(conf.mat))/sum(conf.mat)
}
print(sprintf("average error using 5-fold cross-validation: %.3f percent",100*mean(errs)))

opt<-classification_tree$cptable[which.min(classification_tree$cptable[,"xerror"]),"CP"]
opt
#no pruning needed
pred2<-predict(classification_tree,type="class")
#confusion matrix
confusionMatrix(data=pred2,reference=input_data2$Category)$table