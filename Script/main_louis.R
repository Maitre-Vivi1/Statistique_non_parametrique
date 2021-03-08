rm(list=ls())

library(dplyr)
library(readr)
library(CRAN)
library(bde)

data<-read.table("donnees_source/devA.txt")

summary(data)
barplot(data$V1)


hist(data$V1)

data<-data %>% 
  filter(-10<V1 & V1<20)

test<- data %>% 
  filter(-5<V1 & V1<7)

a<-hist(test$V1)
b<-curve(dnorm(x),-10,10)
points(a,b)

plot(density(data$V1,bw=.7,adjust=1,kernel="rectangular"),xlim=c(0,5),
     col="red",xlab="t",ylab="f(t)",main="Fig. 1. Estimations de densité par noyaux",sub="Fenêtre h=0,7");
lines(density(data$V1,bw=.7,adjust=1,kernel="triangular"),col="green4");
lines(density(data$V1,bw=.7,adjust=1,kernel="gaussian"),col="blue");
legend(x="topright",y=NULL,legend=c("Unif.","Trian.","gauss.",""),text.col=c("red","green4","blue"));


plot(density(data$V1,bw=.7,adjust=1,kernel="cosine"),col="green4",xlim=c(0,5));
lines(density(data$V1,bw=.7,adjust=1,kernel="biweight"),col="blue");
lines(density(data$V1,bw=.7,adjust=1,kernel="epanechnikov"),col="red",xlim=c(0,5));



mise(data,kernel,discreteApproximation=TRUE)
