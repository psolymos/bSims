## comparing distance sampling with plain Binomial
#rm(list=ls())
library(bSims)
library(dclone)
library(rjags)
library(unmarked)

## settings
tint <- c(2,4,6,8,10)
rint <- c(0.5, 1, 1.5)
phi <- 0.5
tau <- 1
dur <- 10
l <- bsims_init()
D <- 1
p <- 1-exp(-2*phi)
q <- (tau^2/max(rint)^2) * (1-exp(-(max(rint)/tau)^2))
P <- p * q

## simulated data
Yq1 <- NULL
Yq2 <- NULL
Yq3 <- NULL
f <- function(d, tau, ...) q
for (i in 1:1000) {
  n <- bsims_populate(l, D)
  a <- bsims_animate(n, vocal_rate=phi, duration=dur)
  o <- bsims_detect(a, tau=tau)
  ## distance: detectability is function of distance
  x1 <- bsims_transcribe(o, tint, rint)
  Yq1 <- rbind(Yq1, colSums(x1$visits))
  ## binomial: detectability does not depend on distance
  x2 <- bsims_transcribe(a, tint, rint)
  Yq2 <- rbind(Yq2, rbinom(5, colSums(x2$visits), q))
  ## constant and <1 distance function
  o3 <- bsims_detect(a, tau=tau, dist_fun = f)
  x3 <- bsims_transcribe(o3, tint, rint)
  Yq3 <- rbind(Yq3, colSums(x3$visits))
}
c(mean(Yq1), mean(Yq2), mean(Yq3), sd(Yq1), sd(Yq2), sd(Yq3))
D*max(rint)^2*pi*p*q

## density estimation
model <- custommodel("model {
    for (i in 1:n) {
        N[i] ~ dpois(D*A)
        for (j in 1:J) {
            Y[i,j] ~ dbin(p, N[i])
        }
    }
    p ~ dunif(0.001, 0.999)
    D ~ dlnorm(0, 0.001)
}")

## distance
dat1 <- list(Y = Yq1, A=max(rint)^2*pi, n = nrow(Yq1), J = ncol(Yq1))
ini1 <- list(N = apply(Yq1, 1, max) + 1)
fit1 <- jags.fit(data = dat1, params = c("p", "D"),
    n.update = 1000,
    model = model, inits = ini1)
umf1 <- unmarkedFramePCount(Yq1)
pc1 <- pcount(~ 1 ~ 1, umf1, K=100*D*2)
coef1 <- c(exp(coef(pc1)[1]), plogis(coef(pc1)[2]))

## binomial
dat2 <- list(Y = Yq2, A=max(rint)^2*pi, n = nrow(Yq2), J = ncol(Yq2))
ini2 <- list(N = apply(Yq2, 1, max) + 1)
fit2 <- jags.fit(data = dat2, params = c("p", "D"),
    n.update = 1000,
    model = model, inits = ini2)
umf2 <- unmarkedFramePCount(Yq2)
pc2 <- pcount(~ 1 ~ 1, umf2, K=100*D*2)
coef2 <- c(exp(coef(pc2)[1]), plogis(coef(pc2)[2]))

## constant dfun
dat3 <- list(Y = Yq3, A=max(rint)^2*pi, n = nrow(Yq3), J = ncol(Yq3))
ini3 <- list(N = apply(Yq3, 1, max) + 1)
fit3 <- jags.fit(data = dat3, params = c("p", "D"),
    n.update = 1000,
    model = model, inits = ini3)
umf3 <- unmarkedFramePCount(Yq3)
pc3 <- pcount(~ 1 ~ 1, umf3, K=100*D*2)
coef3 <- c(exp(coef(pc3)[1]), plogis(coef(pc3)[2]))

## summaries
c(mean(Yq2)/(max(rint)^2*pi*p*q), mean(Yq1)/(max(rint)^2*pi*p*q), mean(Yq3)/(max(rint)^2*pi*p*q))
cbind(True=c(D=D, pq=p*q), Nmix1=coef(fit1), Nmix2=coef(fit2), Nmix3=coef(fit3))
cbind(True=c(D=D, pq=p*q), Nmix1=coef1, Nmix2=coef2, Nmix3=coef3)
