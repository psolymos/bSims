library(bSims)
library(intrval)
library(MASS)
#library(ADPclust)
library(mefa4)
#library(spatstat)

devtools::install_github("psolymos/bSims")

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
#l <- bsims_init(4, 0.5, 0.5)
#p <- bsims_populate(l, 1)
#a <- bsims_animate(p, movement=0)
#o <- bsims_detect(a, c(0,0), repel=1)
#plot(o)

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


tau <- 0.8
gfun <- function(d) exp(-d^2/tau^2)
cfun <- function(d) pi*2*d
integrate(cfun, 0, 1)$value # = pi
f <- function(d) gfun(d) * cfun(d)
Sum <- integrate(f, 0, Inf)$value
qfun <- function(rmax) f(rmax) / Sum


integrate(qfun, 0, Inf)$value

## edr
tau^2*pi

tau^2*pi - integrate(f, 0, tau)$value
Sum - integrate(f, 0, tau)$value
## see what happens with neg exp: gfun <- function(d) exp(-d/tau^2)
## hazard rate: gfun <- function(d) 1-exp(-(d/tau)^-2)


## a,b major and minor axes, theta is angle
ellipse_r <- function(theta, a, b)
  a * b / sqrt(a^2 * sin(theta)^2 + b^2 * cos(theta)^2)


deg2rad <- function(deg)
  pi * deg / 180
rad2deg <- function(rad)
  180 * rad / pi
deg <- seq(0, 360, by=1)
plot(deg, ellipse_r(deg2rad(deg), 1, 2), type="l")


## see of obs --> bird vs bird --> obs attenuation is same

## in symmetric situation it is not exact but quite similar (b=c(1,2), max distance 4, half-normal)
## but e.g. it is different for neg exponential
## and also huge diffs when boundaries are not 'symmetrically' positioned
## but if tau is the same: no problemo
tau <- c(2,3,4)
#tau <- c(3,3,3)
b <- c(1, 2) # points at the HER boundaries
dmax <- 4
d <- seq(0, dmax, length.out = 101)
dist_fun <- function(d, tau) exp(-d^2/tau^2) # half normal

op <- par(mfrow=c(1,2))
qq <- dist_fun2(d, tau[c(3,2,1)], dist_fun, b)
plot(d, dist_fun2(d, tau[1], dist_fun), type="l", main=round(rev(qq)[1], 4), ylim=c(0,1))
lines(d, dist_fun2(d, tau[2], dist_fun))
lines(d, dist_fun2(d, tau[3], dist_fun))
abline(v=b)
#abline(h=rev(qq)[1], col=2, lty=2)
lines(d, qq, col=2, lwd=3)
arrows(0, rev(qq)[1], dmax, rev(qq)[1], col=4, lwd=2, angle=20)

qq <- rev(dist_fun2(d, tau, dist_fun, 4-b))
plot(d, rev(dist_fun2(d, tau[1], dist_fun)), type="l", main=round(qq[1], 4), ylim=c(0,1))
lines(d, rev(dist_fun2(d, tau[2], dist_fun)))
lines(d, rev(dist_fun2(d, tau[3], dist_fun)))
abline(v=b)
#abline(h=qq[1], col=2, lty=2)
lines(d, qq, col=2, lwd=3)
arrows(dmax, qq[1], 0, qq[1], col=4, lwd=2, angle=20)
par(op)
## add HER background colors, axis labels, colored lines, legends

set.seed(12345)
l <- bsims_init(20, 0.1, 0.5)
p <- bsims_populate(l, 20)
a <- bsims_animate(p)
o <- bsims_detect(a, tau=c(1,3,3), vocal_only = FALSE) # detect all
plot(o, pch_nest=NA, pch_vocal=NA, first_only=FALSE, tlim=c(0,60), col_det=NA)
lines.bsims_detections(o, col="#00000044")
# Eye of Sauron
library(plotrix)
draw.ellipse(0, 0, 1, 3, border="white")
lines(c(0,0), c(-3,3), col="white")
lines(c(-1,1), c(0,0), col="white")


## distance sampling
## https://cran.r-project.org/web/packages/DSsim/vignettes/Investigating_Covariates_and_Truncation.html

tau <- 2
rmax <- 5

h <- function(r) 2*r/rmax^2
g <- function(r) exp(-r^2/tau^2)

n <- 10^4
x <- runif(n, -10, 10)
y <- runif(n, -10, 10)
r <- sqrt(x^2 + y^2)
p <- g(r)
s <- rbinom(n, 1, p)

plot(x, y, pch=c(21, 19)[s+1], asp=1)
abline(h=0, v=0)

hist(r[r < rmax], freq=FALSE)
curve(2*x/rmax^2, add=TRUE, col=2)


f <- function(x) g(x) * h(x)
tot <- integrate(f, lower=0, upper=rmax)$value

hist(r[r < rmax & s > 0], freq=FALSE)
curve(g(x) * h(x) / tot, add=TRUE, col=2)


# using bSims
library(bSims)

tau <- 2
rmax <- 4

h <- function(r) 2*r/rmax^2
g <- function(r) exp(-r^2/tau^2)
f <- function(x) g(x) * h(x)
tot <- integrate(f, lower=0, upper=rmax)$value


set.seed(123)
l <- bsims_init()
a <- bsims_populate(l, density=10)
b <- bsims_animate(a, initial_location=TRUE)

d <- bsims_detect(b, tau=tau, vocal_only=FALSE)
dt <- get_detections(d)
ra <- sqrt(rowSums(a$nests[,c("x", "y")]^2))

op <- par(mfrow=c(1,2))
hist(ra[ra <= rmax], freq=FALSE, xlim=c(0, rmax))
curve(2*x/rmax^2, add=TRUE, col=2)

hist(dt$d[dt$d <= rmax], freq=FALSE, xlim=c(0, rmax))
curve(g(x) * h(x) / tot, add=TRUE, col=2)
par(op)


x=b
xy=c(0,0)
tau=1
dist_fun=NULL
#repel=0
vocal_only=FALSE


aa=do.call(rbind, x$events)
bb <- x$nests
bb$d <- sqrt(bb$x^2+bb$y^2)

hist(bb$d[bb$d <= rmax], freq=FALSE)
curve(2*x/rmax^2, add=TRUE, col=2)

hist(aa$d[aa$d <= rmax], freq=FALSE)
curve(g(x) * h(x) / tot, add=TRUE, col=2)

d <- bsims_detect(b, tau=2, vocal_only=FALSE)
dt <- get_detections(d)
ra <- sqrt(rowSums(a$nests[,c("x", "y")]^2))

hist(ra[ra <= rmax], freq=FALSE)
curve(2*x/rmax^2, add=TRUE, col=2)

hist(dt$d[dt$d <= rmax], freq=FALSE)
curve(g(x) * h(x) / tot, add=TRUE, col=2)



xy <- a$nests[,c("x", "y")]


tau <- 2
rmax <- 5

h <- function(r) 2*r/rmax^2
g <- function(r) exp(-r^2/tau^2)

n <- 10^4
x <- a$nests$x
y <- a$nests$y
r <- sqrt(x^2 + y^2)
p <- g(r)
s <- rbinom(length(p), 1, p)

plot(x, y, pch=c(21, 19)[s+1], asp=1)
abline(h=0, v=0)

hist(r[r < rmax], freq=FALSE)
curve(2*x/rmax^2, add=TRUE, col=2)


f <- function(x) g(x) * h(x)
tot <- integrate(f, lower=0, upper=rmax)$value

hist(r[r < rmax & s > 0], freq=FALSE)
curve(g(x) * h(x) / tot, add=TRUE, col=2)




rmax <- 4

## distances
hist(ra[ra < rmax], freq=FALSE)
curve(2*x/rmax^2, add=TRUE, col=2)

hist(dt$d[dt$d < rmax], freq=FALSE)
#hist(dt$d, freq=FALSE)
curve(f(x)/tot, 0,10,add=TRUE)

## -------
library(bSims)
library(detect)

phi <- 0.5
tau <- 1
Den <- 10

tint <- c(1, 2, 3)
rint <- c(0.5, 1, 1.5, 2, Inf) # unlimited

tint <- c(3, 5, 10)
rint <- c(0.5, 1, Inf) # unlimited

sim_fun <- function(type=c("pq", "p", "q")) {
  l <- bsims_init()
  a <- bsims_populate(l, density=Den)
  ## all
  if (type == "pq") {
    b <- bsims_animate(a, vocal_rate=phi)
    x <- bsims_detect(b, tau=tau)
  }
  ## skip detection
  if (type == "p") {
    x <- bsims_animate(a, vocal_rate=phi)
  }
  ## skip avail
  if (type == "q") {
    b <- bsims_animate(a, initial_location=TRUE)
    x <- bsims_detect(b, tau=tau)
  }
  bsims_transcribe(x, tint=tint, rint=rint)$rem
}

B <- 20
res <- pbapply::pbreplicate(B, sim_fun("p"), simplify=FALSE)
res <- pbapply::pbreplicate(B, sim_fun("q"), simplify=FALSE)
res <- pbapply::pbreplicate(B, sim_fun("pq"), simplify=FALSE)

table(sapply(res, sum))

## need one excluding unlimited bin
Ddur <- matrix(tint, B, length(tint), byrow=TRUE)
Ydur <- t(sapply(res, function(z) colSums(z)))
summary(Ddur)
summary(Ydur)
colSums(Ydur) / sum(Ydur)
fitp <- cmulti(Ydur | Ddur ~ 1, type="rem")
phihat <- unname(exp(coef(fitp)))
c(true=phi, estimate=phihat)


Ddis1 <- matrix(rint, B, length(rint), byrow=TRUE)
Ydis1 <- t(sapply(res, function(z) rowSums(z)))
colSums(Ydis1) / sum(Ydis1)
fitq1 <- cmulti(Ydis1 | Ddis1 ~ 1, type="dis")
tauhat1 <- unname(exp(fitq1$coef))

Ddis2 <- matrix(rint[-length(rint)], B, length(rint)-1, byrow=TRUE)
Ydis2 <- t(sapply(res, function(z) rowSums(z)[-length(rint)]))
colSums(Ydis2) / sum(Ydis2)
fitq2 <- cmulti(Ydis2 | Ddis2 ~ 1, type="dis")
tauhat2 <- unname(exp(fitq2$coef))

round(c(true=tau, unlimited=tauhat1, truncated=tauhat2), 4)



(p <- 1-exp(-max(tint)*phihat))
q1 <- 1
(q2 <- (tauhat2^2/max(rint[-length(rint)])^2) * (1-exp(-(max(rint[-length(rint)])/tauhat2)^2)))
(A1 <- pi * tauhat1^2)
(A2 <- pi * max(rint[-length(rint)])^2)

c(true=Den,
  unlimited=mean(rowSums(Ydis1)) / (A1 * p * q1),
  truncated=mean(rowSums(Ydis2)) / (A2 * p * q2))

library(bSims)

l <- bsims_init()
a <- bsims_populate(l, density=Den)
b <- bsims_animate(a, initial_location=TRUE)
#d <- bsims_detect(b, tau=Inf)
tr <- bsims_transcribe(b, tint=tint, rint=rint)
tr$removal

l <- bsims_init()
a <- bsims_populate(l, density=Den)
b <- bsims_animate(a, vocal_rate=phi)
d <- bsims_detect(b, tau=tau)
tr <- bsims_transcribe(d, tint=tint, rint=rint)
tr$removal

tr <- bsims_transcribe(b, tint=tint, rint=rint)
colSums(tr$removal)
exp(cmulti.fit(matrix(colSums(tr$removal), 1), matrix(tint, 1), type="rem")$coef)

x <- bsims_detect(b, tau=Inf)
(v <- colSums(bsims_transcribe(x, tint=tint, rint=rint)$removal))
exp(cmulti.fit(matrix(v, 1), matrix(tint, 1), type="rem")$coef)
xx <- bsims_detect(b, tau=2)
(vv <- colSums(bsims_transcribe(xx, tint=tint, rint=rint)$removal))
exp(cmulti.fit(matrix(vv, 1), matrix(tint, 1), type="rem")$coef)

aa <- get_detections(xx, first_only=FALSE, drop0=FALSE)
aa <- aa[!duplicated(aa$i),]
aa <- aa[!is.na(aa$d),]
#summary(aa)
i <- cut(aa$t, c(0, tint), include.lowest=TRUE, labels=FALSE)
table(i)
exp(cmulti.fit(matrix(table(i), 1), matrix(tint, 1), type="rem")$coef)

str(get_detections(x))
str(get_detections(xx))
plot(ecdf(get_detections(x)$t))
lines(ecdf(get_detections(xx)$t), col=2)
lines(ecdf(aa$t), col=3)

op <- par(mfrow=c(2,2))
image(kde2d(get_detections(x)$d, get_detections(x)$t))
contour(kde2d(get_detections(x)$d, get_detections(x)$t), add=TRUE)
image(kde2d(get_detections(xx)$d, get_detections(xx)$t))
contour(kde2d(get_detections(xx)$d, get_detections(xx)$t), add=TRUE)
image(kde2d(aa$d, aa$t))
contour(kde2d(aa$d, aa$t), add=TRUE)
par(op)

e <- get_events(b)
e <- e[!duplicated(e$i),]
table(cut(e$t, c(0, tint), include.lowest=TRUE, labels=FALSE))

e$d <- sqrt(e$x^2 + e$y^2)
e$g <- exp(-(e$r/tau)^2)
e$d <- rbinom(nrow(e), 1, e$g)
summary(e)
e1 <- e[e$d > 0,]
e1 <- e
i <- cut(e1$t, c(0, tint), include.lowest=TRUE, labels=FALSE)
table(i)
exp(cmulti.fit(matrix(table(i), 1), matrix(tint, 1), type="rem")$coef)


##--

tau <- 2

set.seed(123)
l <- bsims_init()
a <- bsims_populate(l, density=10)
b <- bsims_animate(a, initial_location=TRUE)

(o <- bsims_detect(b, tau=tau, vocal_only=FALSE))
head(dt <- get_detections(o))



tau <- 2

set.seed(123)
l <- bsims_init()
a <- bsims_populate(l, density=10)
b <- bsims_animate(a, vocal_rate = 1, move_rate = 1, movement = 0.2)
o <- bsims_detect(b, tau=tau, event_type = "both")

nrow(get_detections(o, event_type="vocal"))
nrow(get_detections(o, event_type="move"))
nrow(get_detections(o, event_type="both"))


## roadside EDR
l <- bsims_init(10, 0.5, 0.5)
p <- bsims_populate(l, 3)
a <- bsims_animate(p, movement=0)
o <- bsims_detect(a, c(0,0), tau=1:3)
plot(o, pch_vocal=NA)


library(bSims)
library(magrittr)

p <- bsims_init(5) %>%
  bsims_populate(5) %>%
  bsims_animate(
    move_rate=0.5,
    movement=0.1) %>%
  bsims_detect()
plot(p)


## tessellation

#remotes::install_github("psolymos/bSims")
library(bSims)
library(deldir)

l <- bsims_init(2)
set.seed(1)
p <- bsims_populate(l, 5)
x <- bsims_animate(p, vocal_rate=0, duration=5, move_rate=1, movement=0.1, allow_overlap=FALSE)

#d <- bsims_detect(x, event_type = "move")
#plot(d)

dd <- x$tess
u1 <- dd$dirsgs[, 1]
v1 <- dd$dirsgs[, 2]
u2 <- dd$dirsgs[, 3]
v2 <- dd$dirsgs[, 4]


plot(x)
segments(u1, v1, u2, v2, lty=2, col="grey")
#lines(xy)

i <- 1
e <- x$events[[i]]
lines(t(t(e[,c("x", "y")])+as.numeric(x$nests[i,c("x", "y")])))
e$ti
e


bsims_all <- function(...) {
  Settings <- list(...)
  Functions <- list(
    ini=bsims_init,
    pop=bsims_populate,
    ani=bsims_animate,
    det=bsims_detect,
    tra=bsims_transcribe)
  Formals <- lapply(Functions, formals)
  Formals <- lapply(Formals, function(z) z[names(z) != "..."])
  Formals <- lapply(Formals, function(z) z[names(z) != "x"])

  x <- bsims_init()
  Call <- x$call
  for (i in seq_len(length(Formals))) {
    if (i == 2)
      x <- bsims_init()
    if (i > 1)
      Call[["x"]] <- as.name(x)
    for (j in names(Settings)) {
      if (j %in% names(Formals[[i]])) {
        Formals[[i]][[j]] <- Settings[[j]]
        Call[[j]] <- Settings[[j]]
      }
    }
    x <- eval(Call)
    # evaluate the call
    # update conditionally by mutating c0 here
  }
  Formals
}

## start from this call and update conditionally by mutating c0


