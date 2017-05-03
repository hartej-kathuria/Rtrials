#import data
library('readxl')
data<-data<-read_excel('/home/hartej/Desktop/datasets/thoracic/d.xlsx')


#converting char factors to numeric factors
library('car')
data$Risk1Y<-recode(data$Risk1Y,"'T'=1;'F'=0")
data$Smoking<-recode(data$Smoking,"'T'=1;'F'=0")
data$Diabetes<-recode(data$Diabetes,"'T'=1;'F'=0")
data$Dysnopea<-recode(data$Dysnopea,"'T'=1;'F'=0")
data$Diagnosis<-recode(data$Diagnosis,"'DGN1'=1;'DGN2'=2;'DGN3'=3;'DGN4'=4;'DGN5'=5;'DGN6'=6;'DGN8'=7")
data$Tumorsize<-recode(data$Tumorsize,"'OC11'=1;'OC12'=2;'OC13'=3;'OC14'=4")
data$Cough<-recode(data$Cough,"'T'=1;'F'=0")

data<-subset(data,select = c(1,7,8,10,11,14,17))
colnames(data)<-c("Diagnosis","Dysnopea","Cough","Tumorsize","Diabetes","Smoking","Risk1Y")

#dividing data into training set and testing set
i<-sample(1:483,size = round(0.9*483))
train<-data[i,]
test<-data[-i,]

#building model
mylogit2 <- glm(Risk1Y ~ Diagnosis+Dysnopea+Tumorsize+Diabetes+Smoking, data = train, family = "binomial")

#validating against test set
pred2<-predict(mylogit2,train,type = "response")
predvalue2<-ifelse(pred2<0.5,0,1)
library('caret')
confusionMatrix(predvalue2,train[,7])
library(pROC)
plot(roc(test[,7],pred))

perf=function(cut){
  y<-test[,7]
  ypred<-ifelse(pred<cut,0,1)
  w<-which(y==1)
  sens<-mean(ypred[w]==1)
  spec<-mean( ypred[-w] == 0 )
  out<-cbind(sens,spec)
  return(out)
}
s = seq(.01,.99,length=1000)
OUT = matrix(0,1000,2)
for(i in 1:1000) OUT[i,]=perf(s[i])
plot(s,OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
lines(s,OUT[,2],col="darkgreen",lwd=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
box()
legend(0.75,.75,col=c(2,"darkgreen"),lwd=c(2,2),c("Sensitivity","Specificity"))

plot(1-OUT[,2],OUT[,1],xlab="1-Specificity",ylab="Sensitivity",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(1,0,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
box()
legend(0.5,.75,lwd=2,"Area under the curve=0.8391")