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
             range.x = range(dataB$V2)),ylim=c(min(dataB$V1),max(dataB$V1)),type="l",col="red")
lines(ksmooth(dataB$V2,dataB$V1, kernel = "normal", bandwidth = 1,
              range.x = range(dataB$V2)),col="blue")
lines(ksmooth(dataB$V2,dataB$V1, kernel = "normal", bandwidth = 0.5,
              range.x = range(dataB$V2)),col="green")
lines(ksmooth(dataB$V2,dataB$V1, kernel = "normal", bandwidth = 2,
              range.x = range(dataB$V2)),col="skyblue")
lines(ksmooth(dataB$V2,dataB$V1, kernel = "normal", bandwidth = 5,
              range.x = range(dataB$V2)),col="pink")
lines(ksmooth(dataB$V2,dataB$V1, kernel = "normal", bandwidth = 115,
              range.x = range(dataB$V2)),col="grey")
abline(h=0.5,col="black")



sum(dataB$V1)/200

plot(ksmooth(dataB$V2,dataB$V1, kernel = "normal", bandwidth =1.4,
             range.x = range(dataB$V2)),ylim=c(min(dataB$V1),max(dataB$V1)),type="l",col="red")
plot(density(dataB$V1,kernel="gaussian",bw=1.4))
# Estimateur E(y)  --------------------------------------------------------


NW<-function(x,h=5){
  a<-c()
  kernel<-c()
  
  for (i in 1:200){
    if (is.na(dataB$V1[i])){
      kernel[i]<-0
      a[i]<-0}
    
    else{
      kernel[i]<-exp((((x-dataB$V2[i])/h)**2)/2)/sqrt(2*3.14)
      a[i]<-dataB$V1[i]*kernel[i]}
    
    
  }
  
  numerateur= sum(a)
  denominateur= sum(kernel)
  NaWa=numerateur/denominateur
  
  return(NaWa)
}


p_hat<-function(x,h=5){
  kernel<-c()
  kernel2<-c()
  
  for (i in 1:200){
    if (is.na(dataB$V1[i])){
      kernel[i]<-0
      }
    
    else{
      kernel[i]<-exp((((x-dataB$V2[i])/h)**2)/2)/sqrt(2*3.14)
      
      }
    
    kernel2[i]<-exp((((x-dataB$V2[i])/h)**2)/2)/sqrt(2*3.14)
    
  }
  
  numerateur= sum(kernel)
  denominateur= sum(kernel2)
  NaWa=numerateur/denominateur
  
  return(NaWa)
}
phat<-sapply(dataB$V2,p_hat)
Nadi<-sapply(dataB$V2, NW)

theta_tild1<-sum(Nadi)/200


theta_tild2<-c()
for (i in 1:200){
  if (is.na(dataB$V1[i])){
    theta_tild2[i]<-Nadi[i]
  }
  else{
    theta_tild2[i]<-dataB$V1[i]/phat[i]+(1-1/phat[i])*Nadi[i]
  }
  
}
thetat_tild2<-sum(theta_tild2)/200



# estimation h ------------------------------------------------------------


NW_moinsI<-function(x,h=1,k){
  a<-c()
  kernel<-c()
  
  for (i in 1:200){
    if (is.na(dataB$V1[i])){
      kernel[i]<-0
      a[i]<-0}
    
    else{
      kernel[i]<-exp((((x-dataB$V2[i])/h)**2)/2)/sqrt(2*3.14)
      a[i]<-dataB$V1[i]*kernel[i]}
    }
    
  a<-a[-k]
  kernel <- kernel[-k]
  
  numerateur= sum(a)
  denominateur= sum(kernel)
  NaWa=numerateur/denominateur
  
  return(NaWa)
}


h_cv<-function(h){
  summum<-c()
  for (k in 1:200){
    if (is.na(dataB$V1[k])){
      summum[k]<-0 # pmoinsI ^2 
    }
    else{  
      summum[k]<-(dataB$V1[k]-NW_moinsI(dataB$V2[k],h,k))**2 # (1 - pmoinsI)^2
    }}
  summum=summum[summum!=0] #
  return(sum(summum)/length(summum))
}
sequence<-seq(1,10,0.1)
estim_hcv<-sapply(sequence,h_cv)


min_hcv<-estim_hcv[1]
h_min<-sequence[1]
for (i in 1:length(estim_hcv)){
  
  if (estim_hcv[i]<min_hcv){
    min_hcv<-estim_hcv[i]
    h_min<-sequence[i]
  } 
}
print(h_min)

sequence<-seq(2,4,0.01)
sequence<-seq(3.10,3.12,0.0001)
h_validcroisee<-h_min



# final estim e(Y) --------------------------------------------------------

NW<-function(x,h=h_validcroisee){
  a<-c()
  kernel<-c()
  
  for (i in 1:200){
    if (is.na(dataB$V1[i])){
      kernel[i]<-0
      a[i]<-0}
    
    else{
      kernel[i]<-exp((((x-dataB$V2[i])/h)**2)/2)/sqrt(2*3.14)
      a[i]<-dataB$V1[i]*kernel[i]}
    
    
  }
  
  numerateur= sum(a)
  denominateur= sum(kernel)
  NaWa=numerateur/denominateur
  
  return(NaWa)
}


p_hat<-function(x,h=h_validcroisee){
  kernel<-c()
  kernel2<-c()
  
  for (i in 1:200){
    if (is.na(dataB$V1[i])){
      kernel[i]<-0
    }
    
    else{
      kernel[i]<-exp((((x-dataB$V2[i])/h)**2)/2)/sqrt(2*3.14)
      
    }
    
    kernel2[i]<-exp((((x-dataB$V2[i])/h)**2)/2)/sqrt(2*3.14)
    
  }
  
  numerateur= sum(kernel)
  denominateur= sum(kernel2)
  NaWa=numerateur/denominateur
  
  return(NaWa)
}
phat<-sapply(dataB$V2,p_hat)
Nadi<-sapply(dataB$V2,NW)

theta_tild1<-sum(Nadi)/200


theta_tild2<-c()
for (i in 1:200){
  if (is.na(dataB$V1[i])){
    theta_tild2[i]<-Nadi[i]
  }
  else{
    theta_tild2[i]<-dataB$V1[i]/phat[i]+(1-1/phat[i])*Nadi[i]
  }
  
}
theta_tild2<-sum(theta_tild2)/200

