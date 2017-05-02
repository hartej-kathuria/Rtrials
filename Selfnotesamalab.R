#manova test

Y<-cbind(Dataset$WDIM,Dataset$CIRCUM,Dataset$FBEYE,Dataset$EYEHD,Dataset$EARHD,Dataset$JAW)
g<-Dataset[,1]
summary(manova(Y~g),test = "Pillai")

###Discriminant analysis###
#Using cv=TRUE gives a leavE one out classification model of dataset
#using table function to obtain confuion matrix
r <- lda(formula = Group ~ WDIM+CIRCUM+FBEYE +EYEHD+EARHD+JAW,data=data,CV = TRUE)
table(r$class,data[,1])

#to obtain proportion of variance explained by different functions
#dont give value for cv
r1 <-lda(formula = Group ~ WDIM+CIRCUM+FBEYE +EYEHD+EARHD+JAW,data=data)
lamda<-r1$svd^2
#variance explanied by each eigen value
prop<-r1$svd^2/sum((r1$svd)^2)

#for selecting number of clusters in dendogram
#select the number of clusters which give equal observations in each cluster
rect.hclust(HClust.1,k=4)


