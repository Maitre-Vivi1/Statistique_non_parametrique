library(ks);
data(unicef);
# H.scv <- Hscv(unicef);
H.scv <- Hscv.diag(unicef);
fhat <- kde(unicef, H= 20 * H.scv);
plot(fhat);
plot(fhat, display="persp");
plot(fhat, display="image", col=rev(heat.colors(100)))
