rm = ls();


set.seed(18012021);

# set.seed(24012021);

n = 1000 ;
x <- seq(-10, 12, 0.01); 
y <- 1/3 * dnorm(x, mean=-5, sd=1) + 2/3 * dnorm(x, mean=2, sd=3);
u = runif(n);
b = (u < 1/3);
rn = b * rnorm(n, mean=-5, sd=1) + (1 - b) * rnorm(n, mean=2, sd=3);

a = 1;

par(mfrow = c(2,3));
dens = density(x=rn, bw="nrd0", adjust=a)
plot(dens, col=2,
     xlim=c(-10, 12), ylim=c(0, 0.15));
lines(x, y);

dens = density(x=rn, bw="nrd", adjust=a)
plot(dens, col=2,
     xlim=c(-10, 12), ylim=c(0, 0.15));
lines(x, y);

dens = density(x=rn, width=1, adjust=a)
plot(dens, col=2,
     xlim=c(-10, 12), ylim=c(0, 0.15));
lines(x, y);

dens = density(x=rn, bw="ucv", adjust=a)
plot(dens, col=2,
     xlim=c(-10, 12), ylim=c(0, 0.15));
lines(x, y);

dens = density(x=rn, bw="bcv", adjust=a)
plot(dens, col=2,
     xlim=c(-10, 12), ylim=c(0, 0.15));
lines(x, y);

dens = density(x=rn, bw="SJ", adjust=a)
plot(dens, col=2,
     xlim=c(-10, 12), ylim=c(0, 0.15));
lines(x, y);



a = 1;

par(mfrow = c(2,2));
dens = density(x=rn, bw="ucv", kernel="gaussian", adjust=a)
plot(dens, col=2,
     xlim=c(-10, 12), ylim=c(0, 0.15));
lines(x, y);

dens = density(x=rn, bw="ucv", kernel="epanechnikov", adjust=a)
plot(dens, col=2,
     xlim=c(-10, 12), ylim=c(0, 0.15));
lines(x, y);

dens = density(x=rn, bw="ucv", kernel="biweight", adjust=a)
plot(dens, col=2,
     xlim=c(-10, 12), ylim=c(0, 0.15));
lines(x, y);

dens = density(x=rn, bw="ucv", kernel="cosine", adjust=a)
plot(dens, col=2,
     xlim=c(-10, 12), ylim=c(0, 0.15));
lines(x, y);
