library(bSims)
library(intrval)
library(MASS)
#library(ADPclust)
library(mefa4)
#library(spatstat)



## spatial patterns
## stupid
f <- function(d) ifelse(d > 0, 0, 0)
acceptreject(2, f)
try(acceptreject(2, f, fail=TRUE))
acceptreject(2, NULL)


## random
f <- function(d) ifelse(d > 0, 1, 1)
plot(seq(0,1,0.01), f(seq(0,1,0.01)), type="l")
plot(acceptreject(100, f))
nrow(acceptreject(10, f))

## systematic
f <- function(d) 1-exp(-d^2/0.1^2)
plot(seq(0,1,0.01), f(seq(0,1,0.01)), type="l")
plot(acceptreject(100, f, m=1))

## bimodal/clustered
f <- function(d) pmax(ifelse(d < 0.1, 1, 0), 0.5*(1-exp(-d^2/0.5^2)))
plot(seq(0,1,0.01), f(seq(0,1,0.01)), type="l")
plot(acceptreject(100, f, m=1))



(bsims_init())
(l <- bsims_init(3, 0.05, 0.05))
plot(l)
x <- bsims_populate(l, c(2,1,0))
plot(x)

plot(bsims_populate(l, 10))

## systematic
f <- function(d) 1-exp(-d^2/0.3^2)
plot(seq(0,1,0.01), f(seq(0,1,0.01)), type="l")
plot(x <- bsims_populate(l, 10, xy_fun=f, margin=1))

## clustered -- need to watch maxit
f <- function(d) pmax(ifelse(d < 0.2, 1, 0), 0.5*(1-exp(-d^2/1^2)))
plot(seq(0,1,0.01), f(seq(0,1,0.01)), type="l")
plot(x <- bsims_populate(l, 10, xy_fun=f, margin=1))


dim(x$nests)
sum(x$abundance)


library(detect)
library(magrittr)

## check that abundance is right
l <- bsims_init(10)
summary(replicate(1000, sum(bsims_populate(l, 10)$abundance)) - 10^3)

## check vocal rates: no mixture
phi <- 0.5
br <- c(3, 5, 10)
#br <- 1:10
l <- bsims_init(10)
p <- bsims_populate(l, 1)
a <- bsims_animate(p, vocal_rate=phi)
o <- bsims_detect(a, tau=Inf) # detect all
d <- get_detections(o, first_only=TRUE)
i <- cut(d$t, c(0, br), include.lowest = TRUE)
table(i)
Y1 <- matrix(as.numeric(table(i)), nrow=1)
D1 <- matrix(br, nrow=1)
(phihat <- exp(cmulti.fit(Y1, D1, type="rem")$coef))
plot(stepfun(d$t, (0:nrow(d))/nrow(d)), do.points=FALSE, xlim=c(0,10))
curve(1-exp(-phi*x), add=TRUE, col=2)
points(br, cumsum(table(i))/sum(table(i)), cex=2, col=4)
curve(1-exp(-phihat*x), add=TRUE, col=4)

## check vocal rates: finite mixture
phi <- c(10, 0.5)
mix <- c(0.2, 0.8)
#br <- c(3, 5, 10)
br <- 1:10
l <- bsims_init(10)
p <- bsims_populate(l, 10)
a <- bsims_animate(p, vocal_rate=phi, mixture=mix)
o <- bsims_detect(a, tau=Inf) # detect all
d <- get_detections(o, first_only=TRUE)
i <- cut(d$t, c(0, br), include.lowest = TRUE)
table(i)
Y1 <- matrix(as.numeric(table(i)), nrow=1)
D1 <- matrix(br, nrow=1)
cf <- cmulti.fit(Y1, D1, type="mix")$coef # log.phi, logit.c
(phihat <- exp(cf[1]))
(mixhat <- c(1-plogis(cf[2]), plogis(cf[2])))


plot(stepfun(d$t, (0:nrow(d))/nrow(d)), do.points=FALSE, xlim=c(0,10))
curve(1-mix[2]*exp(-phi[2]*x), add=TRUE, col=2)
points(br, cumsum(table(i))/sum(table(i)), cex=2, col=4)
curve(1-mixhat[2]*exp(-phihat*x), add=TRUE, col=4)


## EDR

## check vocal rates: no mixture
phi <- 1
tau <- 0.8
br <- c(0.5, 1, 1.5, Inf)
l <- bsims_init(10)
Y1 <- D1 <- NULL
for (i in 1:20) {
  p <- bsims_populate(l, 10)
  a <- bsims_animate(p, vocal_rate=phi)
  o <- bsims_detect(a, tau=tau) # detect all
  d <- get_detections(o, first_only=TRUE)
  i <- cut(d$d, c(0, br), include.lowest = TRUE)
  Y1 <- rbind(Y1, matrix(as.numeric(table(i)), nrow=1))
  D1 <- rbind(D1, matrix(br, nrow=1))
}
Y1
D1
(tauhat <- exp(cmulti.fit(Y1, D1, type="dis")$coef))

plot(stepfun(d$t, (0:nrow(d))/nrow(d)), do.points=FALSE, xlim=c(0,10))
curve(1-exp(-phi*x), add=TRUE, col=2)
points(br, cumsum(table(i))/sum(table(i)), cex=2, col=4)
curve(1-exp(-phihat*x), add=TRUE, col=4)

## simple simulations
phi <- 1
tau <- 1
br <- c(0.5, 1, 1.5, Inf)
n <- 1000
x <- runif(n, -5, 5)
y <- runif(n, -5, 5)
d <- sqrt(x^2 + y^2)
p <- exp(-d^2/tau^2)
k <- rbinom(n, 1, p)
plot(x, y, asp=1, col="grey")
points(x[k>0], y[k>0], pch=19)
abline(h=0,v=0,lty=2)

i <- cut(d[k>0], c(0, br), include.lowest = TRUE)
table(i)
Y1 <- matrix(as.numeric(table(i)), nrow=1)
D1 <- matrix(br, nrow=1)
(tauhat <- exp(cmulti.fit(Y1, D1, type="dis")$coef))

phi <- 0.25
dur <- 3
tau <- 1
br <- c(0.5, 1, 1.5, Inf)

testf <- function(phi, dur, tau=1, br=c(0.5, 1, 1.5, Inf), n=100) {
  res <- NULL
  for (iii in seq_len(n)) {
    l <- bsims_init(10)
    p <- bsims_populate(l, 10)
    a <- bsims_animate(p, vocal_rate=phi, duration=dur)
    o <- bsims_detect(a, tau=tau) # detect all

    x <- o$nests$x
    y <- o$nests$y
    d <- sqrt(x^2 + y^2)
    p <- exp(-d^2/tau^2)
    k <- rbinom(length(p), 1, p)
    i <- cut(d[k>0], c(0, br), include.lowest = TRUE)
    table(i)
    Y1 <- matrix(as.numeric(table(i)), nrow=1)
    D1 <- matrix(br, nrow=1)
    tauhat1 <- exp(cmulti.fit(Y1, D1, type="dis")$coef)

    z <- get_detections(o, first_only=TRUE)
    i <- cut(z$d, c(0, br), include.lowest = TRUE)
    table(i)
    Y1 <- matrix(as.numeric(table(i)), nrow=1)
    D1 <- matrix(br, nrow=1)
    tauhat2 <- exp(cmulti.fit(Y1, D1, type="dis")$coef)

    res <- rbind(res, c(nest=tauhat1, first=tauhat2))
  }
  list(phi=phi, dur=dur, tau=tau, br=br, res=res)
}

v <- expand.grid(phi=c(0.25, 0.5, 1), dur=c(3,5,10))

tt <- list()
for (j in 1:nrow(v)) {
  cat(j)
  tt[[j]] <- testf(phi=v$phi[j], dur=v$dur[j], n=10)
}

ttt <- t(sapply(tt, function(z) colMeans(z$res)))
cbind(v, ttt)
par(mfrow=c(3,3), mar=c(3,3,3,2))
for (j in 1:9) {
  boxplot(tt[[j]]$res/tau, main=paste(v$phi[j], v$dur[j]),
          ylim=c(0,1.3))
  abline(h=1,col=2)
}

## TODO:
## - use 1st vocal
## - use all vocals -> not very realistic in practice
## - use 1 randomly chosen vocal -> not very realistic in practice
## OK - estimate tau based on 3, 5, and 10 min duration

## both phi and tau
phi <- 0.5
tau <- 0.8
dur <- 10
rbr <- c(0.5, 1, 1.5, Inf)
tbr <- c(3, 5, 10)
l <- bsims_init(10)
p <- bsims_populate(l, 10)
a <- bsims_animate(p, vocal_rate=phi, duration=dur)
o <- bsims_detect(a, tau=tau) # detect all
x <- bsims_transcribe(o, tint=tbr, rint=rbr)
Y1 <- matrix(colSums(x$counts), nrow=1)
D1 <- matrix(x$tint, nrow=1)
Y2 <- matrix(rowSums(x$counts), nrow=1)
D2 <- matrix(x$rint, nrow=1)
exp(cmulti.fit(Y1, D1, type="rem")$coef)
exp(cmulti.fit(Y2, D2, type="dis")$coef)

## all vocals: OK
z <- get_detections(o, first_only=FALSE)
i <- cut(z$d, c(0, rbr), include.lowest = TRUE)
table(i)
Y1 <- matrix(as.numeric(table(i)), nrow=1)
D1 <- matrix(rbr, nrow=1)
exp(cmulti.fit(Y1, D1, type="dis")$coef)
## random vocal: biased for large phi & dur
z <- z[sample(nrow(z)),]
z <- z[!duplicated(z$i),]
i <- cut(z$d, c(0, rbr), include.lowest = TRUE)
table(i)
Y1 <- matrix(as.numeric(table(i)), nrow=1)
exp(cmulti.fit(Y1, D1, type="dis")$coef)
## 1st vocal: biased for large phi & dur
z <- get_detections(o, first_only=TRUE)
i <- cut(z$d, c(0, rbr), include.lowest = TRUE)
table(i)
Y1 <- matrix(as.numeric(table(i)), nrow=1)
exp(cmulti.fit(Y1, D1, type="dis")$coef)

## 3, 5, 10 min based EDR estimation: shorter the better
Y3 <- matrix(x$counts[,1], nrow=1)
Y5 <- matrix(rowSums(x$counts[,1:2]), nrow=1)
Y10 <- matrix(rowSums(x$counts), nrow=1)
D <- matrix(x$rint, nrow=1)
exp(cmulti.fit(Y3, D, type="dis")$coef)
exp(cmulti.fit(Y5, D, type="dis")$coef)
exp(cmulti.fit(Y10, D, type="dis")$coef)


## avoid R stratum
l <- bsims_init(10, 0.5, 0.5)
plot(l)
p <- bsims_populate(l, c(1,1,0))
plot(p)
a <- bsims_animate(p, movement=0.25, move_rate=1, avoid="R")
plot(a, tlim=c(5,10))
o <- bsims_detect(a, c(0.5,0))
plot(o, tlim=c(5,10))
## zoom in
plot(o, xlim=c(-3,3), ylim=c(-3,3))

## repel birds
l <- bsims_init(4, 0.5, 0.5)
p <- bsims_populate(l, 1)
a <- bsims_animate(p, movement=0)
o <- bsims_detect(a, c(0,0), repel=1)
plot(o)

## roadside EDR
l <- bsims_init(10, 0.5, 0.5)
p <- bsims_populate(l, 3)
a <- bsims_animate(p, movement=0)
o <- bsims_detect(a, c(0,0), tau=1:3)
plot(o, pch_vocal=NA)

library(magrittr)

p <- bsims_init(3) %>%
  bsims_populate(c(2,1,0)) %>%
  bsims_animate(movement=0.1)
plot(p)

rr <- 1
tt <- timetoevent(rr, 10)
op <- par(mfrow=c(1,2))
plot(ecdf(tt))
curve(1-exp(-rr*x), add=TRUE, col=2)

plot(stepfun(sort(tt), 0:length(tt)/length(tt)))
curve(1-exp(-rr*x), add=TRUE, col=2)
par(op)

## get coords
xy <- do.call(rbind, lapply(1:length(x$events), function(i) {
  cbind(x$events[[i]]$x+x$nests$x[i],x$events[[i]]$y+x$nests$y[i])
}))
## get individual id
i <- do.call(c, lapply(1:length(x$events), function(i)
  rep(i, nrow(x$events[[i]]))))
## fit ADP clustering
library(ADPclust)
ad <- adpclust(xy, dmethod = "euclidean")
## number of clusters found
ad$nclust
## classification
tab <- table(inds=i, clust=ad$clusters)


col2hex <- function(col, alpha = FALSE) {
  rgb <- col2rgb(col, alpha)
  if (alpha) {
    apply(rgb, 2, function(z) rgb(z[1], z[2], z[3], z[4], maxColorValue=255))
  } else {
    apply(rgb, 2, function(z) rgb(z[1], z[2], z[3], maxColorValue=255))
  }
}
#col2hex(c(blu = "royalblue", reddish = "tomato"), FALSE)
#col2hex(c(blu = "royalblue", reddish = "tomato"), TRUE)
color_fade <- function(col, n) {
  rgb <- col2rgb(col[1L], alpha=FALSE)
  sapply(seq(0, 255, length.out = n), function(z)
    rgb(rgb[1], rgb[2], rgb[3], z, maxColorValue=255))
}
#plot(1:10, col=color_fade("red", 10), pch=19)


library(magick)

set.seed(1234)
l <- bsims_init(15, 0.1, 0.5, 2.5)
#l <- bsims_init(10, 0.1, 0.5)
p <- bsims_populate(l, c(2, 1.5, 0))
a <- bsims_animate(p, duration=60, movement=0.2, move_rate=2, vocal_rate=2, avoid="R")
o <- bsims_detect(a, xy=c(2.5, 1), tau=c(1:3)) # detect all
plot(o, pch_nest=NA, pch_vocal=NA, first_only=FALSE, tlim=c(0,60))
mtext("bSims: highly scientific and utterly addictive", 1, -2)

plot(o, pch_nest=NA, pch_vocal=NA, first_only=FALSE, tlim=c(0,60), xlim=c(-4,-2), ylim=c(-4, -2))
plot(o, pch_nest=NA, pch_vocal=NA, first_only=FALSE, tlim=c(0,60), xlim=c(-4,4), ylim=c(-4, 4))

chr0 <- function(x, n) {
  x <- as.character(x)
  paste0(paste0(rep(0, n - nchar(x)), collapse=""), x, collapse="")
}
## quadratic ease in-out: https://gist.github.com/gre/1650294
g <- function(from, to, length.out) {
  t <- seq(0, 1, length.out = length.out)
  a <- ifelse(t < 0.5, 2*t*t, -1+(4-2*t)*t)
  a * (to-from) + from
}
fps <- 19
lgt <- 6
ti <- 3
nstep <- lgt*fps

x0 <- g(-4, -4, length.out = nstep)
x1 <- g(-2, 4, length.out = nstep)
y0 <- g(-4, -4, length.out = nstep)
y1 <- g(-2, 4, length.out = nstep)
t0 <- g(0, o$duration-ti, length.out = nstep)
t1 <- g(ti, o$duration, length.out = nstep)

par(mar=rep(0,4))
for (i in 1:nstep) {
  png(paste0("temp/plot", chr0(i,3), ".png"), width=400, heigh=400)
  plot(o, pch_nest=NA, pch_vocal=NA, first_only=FALSE,
    tlim=c(t0[i],t1[i]), xlim=c(x0[i],x1[i]), ylim=c(y0[i],y1[i]))
  mtext("bSims: highly scientific and utterly addictive", 1, 0)
  dev.off()
}

im <- image_read(paste0("temp/", list.files("temp", pattern=".png")))
an <- image_animate(im)
image_write(an, "temp/bsims.gif")
