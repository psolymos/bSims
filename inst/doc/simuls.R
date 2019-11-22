library(bSims)
library(parallel)

## common settings
s1 <- list(
  density = 2,
  xy_fun = NULL,
  move_rate = 0,
  movement = 0,
  tint = c(5, 10),
  rint = c(0.5, 1, 1.5)
)
s4 <- s3 <- s2 <- s1
xy_fun <- function (d) (1 - exp(-d^2/1^2) + dlnorm(d, 2)/dlnorm(2, 2))/2
s2$xy_fun <- xy_fun
s4$xy_fun <- s2$xy_fun
s3$move_rate <- 1
s3$movement <- 0.2
s4$move_rate <- s3$move_rate
s4$movement <- s4$movement

b1 <- bsims_all(s1)
b2 <- bsims_all(s2)
b3 <- bsims_all(s3)
b4 <- bsims_all(s4)

bb <- b1$replicate(2)

z <- bb[[1]]
z$removal
z$visits

B <- 100
nc <- 10

cl <- makeCluster(nc)
tmp <- clusterEvalQ(cl, library(bSims))

bb1 <- b1$replicate(B, cl=cl)
bb2 <- b2$replicate(B, cl=cl)
bb3 <- b3$replicate(B, cl=cl)
bb4 <- b4$replicate(B, cl=cl)

stopCluster(cl)




## comparing distance sampling with plain Binomial
#rm(list=ls())
library(bSims)
library(dclone)
library(rjags)
model <- custommodel("model {
    for (i in 1:n) {
        N[i] ~ dpois(D*A)
        for (t in 1:T) {
            Y[i,t] ~ dbin(p, N[i])
        }
    }
    p ~ dunif(0.001, 0.999)
    D ~ dlnorm(0, 0.001)
}")
phi <- 0.5
tau <- 1
dur <- 10
l <- bsims_init()
D <- 1
p <- 1-exp(-2*phi)
q <- (tau^2/1.5^2) * (1-exp(-(1.5/tau)^2))
P <- p * q

Yq1 <- NULL
Yq2 <- NULL
for (i in 1:1000) {
  n <- bsims_populate(l, D)
  a <- bsims_animate(n, vocal_rate=phi, duration=dur)
  o <- bsims_detect(a, tau=tau)
  ## distance
  x1 <- bsims_transcribe(o, c(2,4,6,8,10), c(0.5, 1, 1.5))
  Yq1 <- rbind(Yq1, colSums(x1$visits))
  ## binomial
  x2 <- bsims_transcribe(a, c(2,4,6,8,10), c(0.5, 1, 1.5))
  Yq2 <- rbind(Yq2, rbinom(5, colSums(x2$visits), q))
}
c(mean(Yq1), mean(Yq2), sd(Yq1), sd(Yq2))
D*1.5^2*pi*p*q

## distance
Y <- Yq1
dat <- list(Y = Y, A=1.5^2*pi, n = nrow(Y), T = ncol(Y))
ini <- list(N = apply(Y, 1, max) + 1)
fit <- jags.fit(data = dat, params = c("p", "D"),
    n.update = 1000,
    model = model, inits = ini)
coef(fit)
D
mean(Y)/(1.5^2*pi*p*q)
## binomial
Y <- Yq2
dat <- list(Y = Y, A=1.5^2*pi, n = nrow(Y), T = ncol(Y))
ini <- list(N = apply(Y, 1, max) + 1)
fit <- jags.fit(data = dat, params = c("p", "D"),
    n.update = 1000,
    model = model, inits = ini)
coef(fit)
D
mean(Y)/(1.5^2*pi*p*q)



