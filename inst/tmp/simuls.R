#remotes::install_github("psolymos/bSims")
library(bSims)
library(parallel)

# roadside bias stuff
# - road width -- vary
# - density: 1,1,0
# - behav: 0.8, lower/higher/same, 0 -- vary
# - EDR: 1,1,2 (same)

rint <- c(0.5, 1, Inf)
tint <- c(3, 5, 10)
s <- expand_list(
  road = c(0, 0.1, 0.2),
  edge = 0.5,
  density = list(c(1, 1, 0)),
  vocal_rate = list(c(0.5, 0.25, 0), c(0.5, 0.5, 0), c(0.5, 1, 0)),
  tau = list(c(1, 1, 2)),
  rint = list(rint),
  tint = list(tint)
)
## no edge without road
for (i in seq_along(s))
  if (s[[i]]$road == 0)
    s[[i]]$edge <- 0
b <- lapply(s, bsims_all)
B <- 500
nc <- 4
cl <- makeCluster(nc)
tmp <- clusterEvalQ(cl, library(bSims))
bb <- lapply(b, function(z) z$replicate(B, cl=cl))
stopCluster(cl)

yy <- lapply(bb, function(z) lapply(z, get_table))

Mr <- matrix(rint, nrow=B, ncol=length(rint), byrow=TRUE)
tau <- sapply(yy, function(z) {
  Y <- t(sapply(z, rowSums))
  exp(detect::cmulti.fit(Y, Mr, type="dis")$coef)
})

Mt <- matrix(tint, nrow=B, ncol=length(tint), byrow=TRUE)
phi <- sapply(yy, function(z) {
  Y <- t(sapply(z, colSums))
  exp(detect::cmulti.fit(Y, Mt, type="rem")$coef)
})


Ybar <- sapply(yy, function(z) mean(sapply(z, sum)))
p <- 1-exp(-phi*max(tint))
Dhat <- Ybar / (p * tau^2*pi)

op <- par(mfrow=c(3,3), mar=c(1,1,1,1))
for (i in seq_along(s))
plot(bb[[i]][[1]],
  main=paste(round(c(Ybar[i], phi[i], tau[i], Dhat[i]), 2), collapse=" "))
par(op)


## layers

rint <- c(seq(1, 5, 1), Inf)

# landscape init
s <- expand_list(
  density = list(c(1, 1, 0)),
  tau = list(c(2, 2, 4)),
  road = c(0, 0.25, 0.5),
  rint = list(rint)
)

# populate
s <- expand_list(
  density = 1,
  tau = 2,
  rint = list(rint), # needed as a list to keep vector together
  xy_fun = list(
    NULL,
    function(d) { (1-exp(-d^2/1^2) + dlnorm(d, 2)/dlnorm(exp(2-1),2)) / 2 },
    function(d) { exp(-d^2/1^2) + 0.5*(1-exp(-d^2/4^2)) }
  )
)

# animate
s <- expand_list(
  density = 1,
  tau = 2,
  move_rate = 2,
  movement = c(0, 0.1, 0.2),
  rint = list(rint)
)


b <- lapply(s, bsims_all)
B <- 100
nc <- 4
cl <- makeCluster(nc)
tmp <- clusterEvalQ(cl, library(bSims))
bb <- lapply(b, function(z) z$replicate(B, cl=cl))
stopCluster(cl)

op <- par(mfrow=c(1,3))
plot(bb[[1]][[1]])
plot(bb[[2]][[1]])
plot(bb[[3]][[1]])
par(op)

f <- function(x) {
  y <- drop(get_table(x, "removal"))
  y / ifelse(sum(y)==0, 1, sum(y))
}

yy <- lapply(bb, function(z) t(sapply(z, f)))

yhat <- sapply(yy, colMeans)

plot(spline(c(0, rint[-length(rint)]), yhat[,1]), type="l", ylim=c(0, max(yhat)),
  xlab="Distance band", ylab=expression(pi))
lines(spline(c(0, rint[-length(rint)]), yhat[,2]), col=2)
lines(spline(c(0, rint[-length(rint)]), yhat[,3]), col=4)

plot(c(0, rint[-length(rint)]), yhat[,1], type="l", ylim=c(0, max(yhat)),
  xlab="Distance band", ylab=expression(pi))
lines(c(0, rint[-length(rint)]), yhat[,2], col=2)
lines(c(0, rint[-length(rint)]), yhat[,3], col=4)

D <- matrix(s[[1]]$rint, nrow=B, ncol=length(s[[1]]$rint), byrow=TRUE)
exp(detect::cmulti.fit(yy[[1]], D, type="dis")$coef)
exp(detect::cmulti.fit(yy[[2]], D, type="dis")$coef)
exp(detect::cmulti.fit(yy[[3]], D, type="dis")$coef)

h <- function(x, a=2, b=0.5) {
  b * (1-exp(-x^2/a^2)) + (1-b) * dlnorm(x, a)/dlnorm(exp(a-1),a)
}
bv <- 0.8
curve(h(x, a=2, b=bv), 0, 5, 1001, ylim=c(0,1))
curve(h(x,a=1, b=bv), 0, 5, 1001, add=TRUE, col=2)
curve(h(x,a=0.5, b=bv), 0, 5, 1001, add=TRUE, col=4)

hfun <- function(d, a=2, b=0.5) {
  function(d) {
    b * (1-exp(-d^2/a^2)) + (1-b) * dlnorm(d, a)/dlnorm(exp(a-1),a)
  }
}
l <- bsims_init()
p1 <- bsims_populate(l, density=2, xy_fun=NULL)
p2 <- bsims_populate(l, density=2, xy_fun=hfun(x,a=0.5, b=bv))
p3 <- bsims_populate(l, density=2, xy_fun=hfun(x,a=1, b=bv))
p4 <- bsims_populate(l, density=2, xy_fun=hfun(x,a=2, b=bv))


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


