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

a <- density(devA$X1, kernel = "gaussian", bw = "SJ")
b <- density(devA$X1)

c <- boundedDensity(x = devA$X1, densities =  a$x[1:200], lower.limit = min(devA$X1), upper.limit = max(devA$X1))
d <-  boundedDensity(x = devA$X1, densities =  b$x[1:200], lower.limit = min(devA$X1), upper.limit = max(devA$X1))

mise(c,d)

mise(data,kernel,discreteApproximation=TRUE)
MISE(x=a, xgrid=data$V1,lambda = 0,)




# PArtie B ----------------------------------------------------------------

dataB<-read.table("donnees_source/devB.txt")

summary(dataB)
barplot(dataB$V1)


hist(dataB$V1)

a<-mean(ksmooth(dataB$V2,dataB$V1, kernel = "box", bandwidth = 1,
           range.x = range(dataB$V2)))
#problme de NA



plot(ksmooth(dataB$V2,dataB$V1, kernel = "box", bandwidth ="Rule of thumb",
        range.x = range(dataB$V2)),col="red")
lines(ksmooth(dataB$V2,dataB$V1, kernel = "normal", bandwidth = 1,
              range.x = range(dataB$V2)),col="blue")
#je ne trouve pas et ne sait pas quelle bw choisir 

#trouvé sur internet 
x <- xx <- dataB$V1
x[i.out <- sample(length(x), 10)] <- NA
doR <- density(x, bw = 0.15, na.rm = TRUE)
plot(doR, col = "blue")
points(xx[i.out], rep(0.01, 10))


plot(ksmooth(dataB$V2,xx, kernel = "box", bandwidth =1,
             range.x = range(dataB$V2)),col="red")
lines(ksmooth(dataB$V2,xx, kernel = "normal", bandwidth = 1,
              range.x = range(dataB$V2)),col="blue")

