library(readr)
devA <- read_csv("donnees_source/devA.txt", col_names = FALSE, na = "NA", col_types = cols(X1 = col_number()))

##### QUestion 1) #####

# https://nobelis.eu/photis/Estimat/densite.html

hist(devA$X1, freq = F, breaks = 4, col = "red")

summary(devA$X1)
sd(devA$X1)

plot(density(devA$X1, kernel = "gaussian", bw = "ucv"), main = "Estimation de la densité")


plot(density(devA$X1,bw=.7,adjust=1,kernel="rectangular"),xlim=c(0,5),
     col="red",xlab="t",ylab="f(t)",main="Fig. 1. Estimations de densité par noyaux",sub="Fenêtre h=0,7");
lines(density(devA$X1,bw=.7,adjust=1,kernel="triangular"),col="green4");
lines(density(devA$X1,bw=.7,adjust=1,kernel="gaussian"),col="blue");
legend(x="topright",y=NULL,legend=c("Unif.","Trian.","gauss.",""),text.col=c("red","green4","blue"));


plot(density(devA$X1,bw=.7,adjust=1,kernel="cosine"),col="green4",xlim=c(0,5));
lines(density(devA$X1,bw=.7,adjust=1,kernel="biweight"),col="blue");
lines(density(devA$X1,bw=.7,adjust=1,kernel="epanechnikov"),col="red",xlim=c(0,5));

# On cherche à minimiser l'écart entre deux densités

library(bde)
library(CHsharp)



a <- density(devA$X1, kernel = "gaussian", bw = 0.5)
a_2 <- density(devA$X1, kernel = "cosine", bw = 1)
a_3 <- density(devA$X1, kernel = "triangular", bw = 2)
a_4 <- density(devA$X1, kernel = "epanechnikov", bw = 3)
a_5 <- density(devA$X1, kernel = "rectangular", bw = 4)

b <- density(devA$X1)
b_2 <- hist(devA$X1)
b$x

plot(a)
plot(a_2)
plot(a_3)
plot(a_4)
plot(a_5)


c_1 <- boundedDensity(x = b$x, densities =  a$x, lower.limit = min(-1000), upper.limit = max(1000))
c_2 <- boundedDensity(x = b$x, densities =  a_2$x, lower.limit = min(-1000), upper.limit = max(1000))
c_3 <- boundedDensity(x = b$x, densities =  a_3$x, lower.limit = min(-1000), upper.limit = max(1000))
c_4 <- boundedDensity(x = b$x, densities =  a_4$x, lower.limit = min(-1000), upper.limit = max(1000))
c_5 <- boundedDensity(x = b$x, densities =  a_5$x, lower.limit = min(-1000), upper.limit = max(1000))

mise(c_2,c_1); mise(c_1,c_2); mise(c_1,c_3); mise(c_1,c_4);  
mise(c_3,c_1); mise(c_3,c_2); mise(c_2,c_3); mise(c_3,c_4); 
mise(c_4,c_1); mise(c_4,c_2); mise(c_4,c_3); mise(c_4,c_4); 
mise(c_5,c_1); mise(c_5,c_2); mise(c_5,c_3); mise(c_5,c_4)



devA$X1
qqnorm(devA$X1)
qqline(devA$X1)

deva$X1 <- 