rm(list=ls())

library(dplyr)
library(readr)
library(CRAN)
library(bde)
library(zoo)

# PArtie B ----------------------------------------------------------------

dataB<-read.table("donnees_source/devB.txt")

summary(dataB)


# traitement de NA par 0 --------------------------------------------------

dataB$V1[is.na(dataB$V1)]<-0
summary(dataB)

plot(ksmooth(dataB$V2,dataB$V1, kernel = "normal", bandwidth =0.2,
             range.x = range(dataB$V2)),ylim=c(min(dataB$V1),max(dataB$V1)),col="red")
lines(ksmooth(dataB$V2,dataB$V1, kernel = "normal", bandwidth = 1,
              range.x = range(dataB$V2)),col="blue")
lines(ksmooth(dataB$V2,dataB$V1, kernel = "normal", bandwidth = 0.5,
              range.x = range(dataB$V2)),col="green")
lines(ksmooth(dataB$V2,dataB$V1, kernel = "normal", bandwidth = 2,
              range.x = range(dataB$V2)),col="skyblue")
lines(ksmooth(dataB$V2,dataB$V1, kernel = "normal", bandwidth = 5,
              range.x = range(dataB$V2)),col="black")


plot(ksmooth(dataB$V2,dataB$V1, kernel = "box", bandwidth =0.2,
             range.x = range(dataB$V2)),ylim=c(min(dataB$V1),max(dataB$V1)),col="red")
lines(ksmooth(dataB$V2,dataB$V1, kernel = "box", bandwidth = 1,
              range.x = range(dataB$V2)),col="blue")
lines(ksmooth(dataB$V2,dataB$V1, kernel = "box", bandwidth = 0.5,
              range.x = range(dataB$V2)),col="green")
lines(ksmooth(dataB$V2,dataB$V1, kernel = "box", bandwidth = 2,
              range.x = range(dataB$V2)),col="skyblue")
lines(ksmooth(dataB$V2,dataB$V1, kernel = "box", bandwidth = 5,
              range.x = range(dataB$V2)),col="black")

# traitement de NA avec median --------------------------------------------

dataB<-read.table("donnees_source/devB.txt")

dataB$V1<-na.aggregate(dataB$V1,fUN=median)

# wekdjzid<-mean(ksmooth(dataB$V2,dataB$V1, kernel = "normal", bandwidth = 1,
#                 range.x = range(dataB$V2)))

# plot(ksmooth(dataB$V2,dataB$V1, kernel = "box", bandwidth ="Rule of thumb",
#              range.x = range(dataB$V2)),ylim=c(min(dataB$V1),max(dataB$V1)),col="red")
# lines(ksmooth(dataB$V2,dataB$V1, kernel = "normal", bandwidth = 1,
#               range.x = range(dataB$V2)),col="blue")

plot(ksmooth(dataB$V2,dataB$V1, kernel = "normal", bandwidth =0.2,
             range.x = range(dataB$V2)),ylim=c(min(dataB$V1),max(dataB$V1)),col="red")
lines(ksmooth(dataB$V2,dataB$V1, kernel = "normal", bandwidth = 1,
              range.x = range(dataB$V2)),col="blue")
lines(ksmooth(dataB$V2,dataB$V1, kernel = "normal", bandwidth = 0.5,
              range.x = range(dataB$V2)),col="green")
lines(ksmooth(dataB$V2,dataB$V1, kernel = "normal", bandwidth = 2,
              range.x = range(dataB$V2)),col="skyblue")
lines(ksmooth(dataB$V2,dataB$V1, kernel = "normal", bandwidth = 5,
              range.x = range(dataB$V2)),col="pink")
abline(h=0.5,col="black")



sum(dataB$V1)/200




NW<-function(x,h=1){
  a=list()
  kernel=list()
  
  for (i in 200){
    kernel<-exp((((x-dataB$V2[i])/h)**2)/2)/2*3.69
    a<-dataB$V1[i]*kernel[i]
    
  }
  
  numerateur= sum(a)
  denominateur= sum(kernel)
  NW=numerateur/denominateur
  
  return(NW)
}


Nadi<-sapply(dataB$V2, NW)



# data<-read.table("donnees_source/devA.txt")
# 
# summary(data)
# barplot(data$V1)
# 
# 
# hist(data$V1)
# 
# data<-data %>% 
#   filter(-10<V1 & V1<20)
# 
# test<- data %>% 
#   filter(-5<V1 & V1<7)
# 
# a<-hist(test$V1)
# b<-curve(dnorm(x),-10,10)
# points(a,b)
# 
# plot(density(data$V1,bw=.7,adjust=1,kernel="rectangular"),xlim=c(0,5),
#      col="red",xlab="t",ylab="f(t)",main="Fig. 1. Estimations de densité par noyaux",sub="Fenêtre h=0,7");
# lines(density(data$V1,bw=.7,adjust=1,kernel="triangular"),col="green4");
# lines(density(data$V1,bw=.7,adjust=1,kernel="gaussian"),col="blue");
# legend(x="topright",y=NULL,legend=c("Unif.","Trian.","gauss.",""),text.col=c("red","green4","blue"));
# 
# 
# plot(density(data$V1,bw=.7,adjust=1,kernel="cosine"),col="green4",xlim=c(0,5));
# lines(density(data$V1,bw=.7,adjust=1,kernel="biweight"),col="blue");
# lines(density(data$V1,bw=.7,adjust=1,kernel="epanechnikov"),col="red",xlim=c(0,5));
# 
# a <- density(devA$X1, kernel = "gaussian", bw = "SJ")
# b <- density(devA$X1)
# 
# c <- boundedDensity(x = devA$X1, densities =  a$x[1:200], lower.limit = min(devA$X1), upper.limit = max(devA$X1))
# d <-  boundedDensity(x = devA$X1, densities =  b$x[1:200], lower.limit = min(devA$X1), upper.limit = max(devA$X1))
# 
# mise(c,d)
# 
# mise(data,kernel,discreteApproximation=TRUE)
# MISE(x=a, xgrid=data$V1,lambda = 0,)
# 
# 
# 
# 
