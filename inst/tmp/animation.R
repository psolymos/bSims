library(bSims)
library(intrval)
library(magick)

set.seed(1234)
l <- bsims_init(15, 0.1, 0.5, 2.5)
#l <- bsims_init(10, 0.1, 0.5)
p <- bsims_populate(l, c(2, 1.5, 0))
a <- bsims_animate(p, duration=60, movement=0.2, move_rate=2, vocal_rate=2, avoid="R")
o <- bsims_detect(a, xy=c(2.5, 1), tau=c(1:3)) # detect all
plot(o, pch_nest=NA, pch_vocal=NA, tlim=c(0,60), condition="alldet")
mtext("bSims: highly scientific and utterly addictive", 1, -2)

plot(o, pch_nest=NA, pch_vocal=NA, condition="alldet", tlim=c(0,60), xlim=c(-4,-2), ylim=c(-4, -2))
plot(o, pch_nest=NA, pch_vocal=NA, condition="alldet", tlim=c(0,60), xlim=c(-4,4), ylim=c(-4, 4))

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

if (!dir.exists("_temp"))
  dir.create("_temp")

par(mar=rep(0,4))
for (i in 1:nstep) {
  png(paste0("_temp/plot", chr0(i,3), ".png"), width=400, heigh=400)
  plot(o, pch_nest=NA, pch_vocal=NA, condition="alldet",
    tlim=c(t0[i],t1[i]), xlim=c(x0[i],x1[i]), ylim=c(y0[i],y1[i]))
  mtext("bSims: highly scientific and utterly addictive", 1, 0)
  dev.off()
}

im <- image_read(paste0("_temp/", list.files("_temp", pattern=".png")))
an <- image_animate(im)
image_write(an, "_temp/bsims.gif")

