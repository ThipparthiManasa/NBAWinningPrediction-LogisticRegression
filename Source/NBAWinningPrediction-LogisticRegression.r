install.packages("leaps")
library(leaps)
getwd()
setwd("D:\UMKC\ISL\Data") 
X = read.csv("data_NBA_CSV.csv")
install.packages("ISLR")

glm.fit1=glm(WinPer~ .,family="binomial", data=data_NBA)
fix(X)
summary(glm.fit)
coef(glm.fit)

contrasts(WinPer)
attach(X)
confint(glm.fit)
plot(WinPer~year,data = X)
scale(X)
train=data_NBA[year<2015,]
glm.fit=glm(WinPer~ .,family="binomial", data=train)
dim(train)
test=X[!train,]
dim(test)
prob1=predict(glm.fit,newdata=test,type="response")
predict1=ifelse(prob1>0.5,"Win Pred","Lose Pred")
WinPrediction=data_NBA.WinPer[test]
table(predict1,test$WinPer)
table(predict1,train$WinPer)
plot(test$WinPer,predict1)
summary(predict1)
plot(data_NBA$WinPer,)


pairs(data_NBA)

--------------------
  data_NBA=read.csv("data_NBA_CSV.csv")

  normalize <- function(x)
  {
    num<-x-min(x)
    denom<-max(x)-min(x)
    return(num/denom)
  }

data_NBA<-as.data.frame(lapply(data_NBA[,-1],normalize))


summary(data_NBA)
data_NBA = data_NBA[,-c(3,4,9,10,11,12,15,27)]
glm.fit=glm(WinPer~.,data=data_NBA,family="binomial")
summary(data_NBA)
length(glm.fit)


+MIN+FTA+FTM+PTS+year+GP


NBA_data = read.csv(file.choose())

}

--------------------------
  plot(WinPer~.,data=data_NBA)
lines(data_NBA$~,glm.fit$fitted,col="red")
lines(data_NBA$FGA,glm.fit$fitted,col="red",type="1")
xlab="predictor",ylab="WinPer"
lines(WinPer/year~FGA,data=data_NBA)
dim(glm.fit)
length(glm.fit)
length(data_NBA)


  

  
   length(glm.fit$FGA)
  
  


