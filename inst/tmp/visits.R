## comparing distance sampling with plain Binomial
#rm(list=ls())
library(bSims)
library(dclone)
library(rjags)
library(pbapply)

# Estimation
fit_fun <- function(Y, rint, D, p) {
  f <- jags.fit(
    data = list(Y = Y, A=max(rint)^2*pi, n = nrow(Y), J = ncol(Y)),
    params = c("D", "p"),
    n.update = 1000,
    model = custommodel("model {
        for (i in 1:n) {
            N[i] ~ dpois(D*A)
            for (j in 1:J) {
                Y[i,j] ~ dbin(p, N[i])
            }
        }
        p ~ dunif(0.001, 0.999)
        D ~ dlnorm(0, 0.001)
    }"),
    inits = list(N = apply(Y, 1, max) + 1))
  cbind(True=c(D=D, p=p),
        JAGS=unname(coef(f)))
}

## Settings
n <- 100
ti <- 2
tint <- 1:5 * ti
rint <- c(0.5, 1, 1.5)
phi <- 0.5
tau <- 1
dur <- 10
l <- bsims_init()
D <- 1
p <- 1-exp(-ti*phi)
q <- (tau^2/max(rint)^2) * (1-exp(-(max(rint)/tau)^2))
#q <- tau^2 * (1 - exp(-max(rint)^2/tau^2))/max(rint)^2
P <- p * q


## Distance: detectability is function of distance, using event1
## underestimate
Y1 <- t(pbreplicate(n, {
  u <- bsims_populate(l, D)
  a <- bsims_animate(u, vocal_rate=phi, duration=dur)
  o <- bsims_detect(a, tau=tau)
  x <- bsims_transcribe(o, tint, rint, condition="event1")
  colSums(x$visits)
}))
r1 <- fit_fun(Y1, rint, D, p*q)

## Distance: detectability is function of distance, using det1
## underestimate ??? sometimes D is OK but p is high ???
Y2 <- t(pbreplicate(n, {
  u <- bsims_populate(l, D)
  a <- bsims_animate(u, vocal_rate=phi, duration=dur)
  o <- bsims_detect(a, tau=tau)
  x <- bsims_transcribe(o, tint, rint, condition="det1")
  colSums(x$visits)
}))
r2 <- fit_fun(Y2, rint, D, p*q)

## binomial: detectability does not depend on distance
## OK
Y3 <- t(pbreplicate(n, {
  u <- bsims_populate(l, D)
  a <- bsims_animate(u, vocal_rate=phi, duration=dur)
  x <- bsims_transcribe(a, tint, rint)
  rbinom(length(tint), colSums(x$visits), q)
}))
r3 <- fit_fun(Y3, rint, D, p*q)

## constant and <1 distance function
## OK
Y4 <- t(pbreplicate(n, {
  u <- bsims_populate(l, D)
  a <- bsims_animate(u, vocal_rate=phi, duration=dur)
  o <- bsims_detect(a, tau=tau, dist_fun = function(d, tau, ...) q)
  x <- bsims_transcribe(o, tint, rint)
  colSums(x$visits)
}))
r4 <- fit_fun(Y4, rint, D, p*q)

## only removal, no distance (q=1)
## OK
Y5 <- t(pbreplicate(n, {
  u <- bsims_populate(l, D)
  a <- bsims_animate(u, vocal_rate=phi, duration=dur)
  x <- bsims_transcribe(a, tint, rint)
  colSums(x$visits)
}))
r5 <- fit_fun(Y5, rint, D, p)

## same as 1 but visits are not consecutive but independent
## underestimate
Y6 <- t(pbreplicate(n, {
  u <- bsims_populate(l, D)
  sapply(seq_along(tint), function(i) {
    a <- bsims_animate(u, vocal_rate=phi, duration=tint[1])
    o <- bsims_detect(a, tau=tau)
    x <- bsims_transcribe(o, tint[1], rint, condition="event1")
    sum(x$visits)
  })
}))
r6 <- fit_fun(Y6, rint, D, p*q)

## same as 1, unlimited distance, using event1
## underestimate
Y7 <- t(pbreplicate(n, {
  u <- bsims_populate(l, D)
  a <- bsims_animate(u, vocal_rate=phi, duration=dur)
  o <- bsims_detect(a, tau=tau)
  x <- bsims_transcribe(o, tint, Inf, condition="event1")
  colSums(x$visits)
}))
r7 <- fit_fun(Y7, rint, D, NA)

## only distance, no removal (p=1)
## underestimate
Y8 <- t(pbreplicate(n, {
  u <- bsims_populate(l, D)
  a <- bsims_animate(u, vocal_rate=phi*10, duration=dur)
  o <- bsims_detect(a, tau=tau)
  x <- bsims_transcribe(o, tint, rint)
  colSums(x$visits)
}))
r8 <- fit_fun(Y8, rint, D, q)


colMeans(Y1)
colMeans(Y2)
colMeans(Y3)
colMeans(Y4)
colMeans(Y5)
colMeans(Y6)
colMeans(Y7)
colMeans(Y8)

mean(Y1)/(max(rint)^2*pi*p*q)
mean(Y2)/(max(rint)^2*pi*p*q)
mean(Y3)/(max(rint)^2*pi*p*q)
mean(Y4)/(max(rint)^2*pi*p*q)
mean(Y5)/(max(rint)^2*pi*p)
mean(Y6)/(max(rint)^2*pi*p*q)
mean(Y7)/(tau^2*pi*p)
mean(Y8)/(max(rint)^2*pi*q)

r1
r2
r3
r4
r5
r6
r7
r8

D <- 1
Y9 <- t(pbreplicate(n, {
  u <- bsims_populate(l, D)
  Nest <- get_nests(u)
  Dist <- sqrt(Nest$x^2 + Nest$y^2)
  a <- bsims_animate(u, vocal_rate=phi, duration=dur)
  o <- bsims_detect(a, tau=tau)
  x <- bsims_transcribe(o, tint, rint, condition="event1")
  c(colSums(x$visits), All=sum(Dist <= max(rint)))
}))
YY9 <- NULL
for (i in 1:5) {
  YY9 <- cbind(YY9, rbinom(nrow(Y9), Y9[,"All"], p*q))
}
YY9 <- cbind(YY9, Y9[,"All"])
summary(Y9)
summary(YY9)
sd(Y9[,1:5])
sd(YY9[,1:5])
plot(as.numeric(table(as.numeric(Y9[,1:5]))),
as.numeric(table(as.numeric(YY9[,1:5]))))
abline(0,1)

  u <- bsims_populate(l, D)
  Nest <- get_nests(u)
  Dist <- sqrt(Nest$x^2 + Nest$y^2)
  plot(y ~ x, Nest, col=ifelse(Dist <= max(rint), 2, 1), pch=3, asp=1)
  abline(h=0,v=0,col=2)
  bSims:::.draw_ellipse(0, 0, a=max(rint),b=max(rint), border=2)



##--

## simulated data
Yq0 <- NULL
Yq1 <- NULL
Yq2 <- NULL
Yq3 <- NULL
f <- function(d, tau, ...) q

## step function with distance???
## try unlimited counts???

for (i in 1:n) {
  n <- bsims_populate(l, D)
  a <- bsims_animate(n, vocal_rate=phi, duration=dur)
  o <- bsims_detect(a, tau=tau)
  ## distance: detectability is function of distance
  x0 <- bsims_transcribe(o, tint, rint, condition="event1")
  Yq0 <- rbind(Yq0, colSums(x0$visits))
  ## distance: detectability is function of distance
  x1 <- bsims_transcribe(o, tint, rint, condition="det1")
  Yq1 <- rbind(Yq1, colSums(x1$visits))
  ## binomial: detectability does not depend on distance
  x2 <- bsims_transcribe(a, tint, rint)
  Yq2 <- rbind(Yq2, rbinom(length(tint), colSums(x2$visits), q))
  ## constant and <1 distance function
  o3 <- bsims_detect(a, tau=tau, dist_fun = f)
  x3 <- bsims_transcribe(o3, tint, rint)
  Yq3 <- rbind(Yq3, colSums(x3$visits))
}
c(mean(Yq0), mean(Yq1), mean(Yq2), mean(Yq3), sd(Yq1), sd(Yq2), sd(Yq3))
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

## distance event1
dat0 <- list(Y = Yq0, A=max(rint)^2*pi, n = nrow(Yq0), J = ncol(Yq0))
ini0 <- list(N = apply(Yq0, 1, max) + 1)
fit0 <- jags.fit(data = dat0, params = c("p", "D"),
    n.update = 1000,
    model = model, inits = ini0)
umf0 <- unmarkedFramePCount(Yq0)
pc0 <- pcount(~ 1 ~ 1, umf0, K=100*D*2)
coef0 <- c(exp(coef(pc0)[1]), plogis(coef(pc0)[2]))

## distance det1
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
c(mean(Yq0)/(max(rint)^2*pi*p*q),
  mean(Yq1)/(max(rint)^2*pi*p*q),
  mean(Yq2)/(max(rint)^2*pi*p*q),
  mean(Yq3)/(max(rint)^2*pi*p*q))
cbind(True=c(D=D, pq=p*q),
      Nmix0=coef(fit0),
      Nmix1=coef(fit1),
      Nmix2=coef(fit2),
      Nmix3=coef(fit3))
cbind(True=c(D=D, pq=p*q),
      Nmix0=coef0,
      Nmix1=coef1,
      Nmix2=coef2,
      Nmix3=coef3)
