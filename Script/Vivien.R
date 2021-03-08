library(readr)
devA <- read_csv("donnees_source/devA.txt", col_names = FALSE, na = "NA", col_types = cols(X1 = col_number()))

# https://nobelis.eu/photis/Estimat/densite.html

hist(devA$X1, freq = F, breaks = 4, col = "red")

summary(devA$X1)
sd(devA$X1)

plot(density(devA$X1, kernel = "gaussian", bw = "SJ"))

library(btb)

kernelSmoothing(dfObservations = devA$X1,iCellSize = 1.2, iBandwidth = 5)

library(bde)

a <- density(devA$X1, kernel = "gaussian", bw = "SJ")
b <- density(devA$X1)

c <- boundedDensity(x = devA$X1, densities =  a$x[1:200], lower.limit = min(devA$X1), upper.limit = max(devA$X1))
d <-  boundedDensity(x = devA$X1, densities =  b$x[1:200], lower.limit = min(devA$X1), upper.limit = max(devA$X1))

mise(c,d)
