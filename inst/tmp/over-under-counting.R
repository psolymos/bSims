library(bSims)

phi <- 0.5
B <- 10
perc <- seq(0.5, 1.5, 0.1)

l <- expand_list(
  abund_fun = list(identity),
  duration = 10,
  vocal_rate = phi,
  tau = Inf,
  tint = list(c(3, 5, 10)),
  perception = perc)
str(l[1:2])
b <- lapply(l, bsims_all)

s <- lapply(b, function(z) {
  z$replicate(B, cl=4)
})

## this is using the removal
phi_hat <- t(sapply(s, function(r) sapply(r, estimate)["phi",]))

matplot(perc, phi_hat, lty=1, type="l", col="grey", ylim=c(0, max(phi_hat)))
lines(perc, apply(phi_hat, 1, median), lwd=2)
abline(h=phi)

matplot(perc, 1-exp(-1*phi_hat), lty=1, type="l", col="grey", ylim=c(0,1))
lines(perc, 1-exp(-1*apply(phi_hat, 1, median)), lwd=2)
abline(h=1-exp(-1*phi), lty=2)

## fit model to t21 data
x <- s[[1]][[1]]
v <- get_events(x)
fitdistr(v$t[!duplicated(v$i)], "exponential")$estimate
v <- get_detections(x)
fitdistr(v$t[!duplicated(v$j)], "exponential")$estimate
## same bias because individuals are wrong

## --------- multiple locations -------------------

library(bSims)
library(magrittr)
library(survival)
library(pbapply)
library(detect)

phi <- 0.5
perc <- c(0.5, 1, 1.5)
tint <- c(3, 5, 10)
den <- 1

if (FALSE) {
n <- 100
l <- expand_list(
  density=den,
  abund_fun = list(identity),
  duration = max(tint),
  vocal_rate = phi,
  tau = 0.5,
  tint = list(tint),
  rint = 0.5,
  condition="det1",
  perception = perc)
str(l[1:2])
b <- lapply(l, bsims_all)

s <- lapply(b, function(z) z$replicate(n, cl=4))
}

n <- 200
b <- bsims_init() %>%
  bsims_populate(
    density=den,
    abund_fun = identity)
s <- list(pblapply(1:n, function(i) {
  bsims_animate(
    b,
    duration = max(tint),
    vocal_rate = phi) %>%
  bsims_transcribe(tint = tint)
}))


## abundance and detections
lapply(s, function(r) summary(t(sapply(r, function(x) {
  c(length(unique(get_events(x)$i)), length(unique(get_detections(x)$i)))
}))))

## removal model
D <- matrix(tint, nrow=n, ncol=length(tint), byrow=TRUE)
Y <- lapply(s, function(r) t(sapply(r, get_table)))
phi_hat1 <- sapply(Y, function(y)
  exp(cmulti(y | D ~ 1, type="rem")$coef))

## Picking the 1st event of a random individual
T21r <- lapply(s, function(r) sapply(r, function(x) {
  v <- get_events(x)
  ## 1st event random (1st) individual
  v$t[v$i==1][1]
}))
phi_hat2 <- sapply(T21r, function(t21) {
  fitdistr(t21[!is.na(t21)], "exponential")$estimate
})

## picking the 1st detection (low count situation)
## but we get lots of NAs but we know true abundance is >0
## thus we need censoring
T21d <- lapply(s, function(r) sapply(r, function(x) {
  ## 1st detected event of the 1st individual
  get_detections(x)$t[1]
}))
phi_hat3 <- sapply(T21d, function(t21) {
  fitdistr(t21[!is.na(t21)], "exponential")$estimate
})

survfun <- function(t21, MAX=10) {
  t21[!is.na(t21) & t21 > MAX] <- NA
  y01 <- ifelse(is.na(t21), 0, 1)
  t21[is.na(t21)] <- MAX
  #time cannot be 0, so we use 1 sec instead
  t21[t21 == 0] <- 0.001
  sv <- Surv(t21, y01)
  m <- survreg(sv ~ 1, dist="exponential")
  1/exp(coef(m))
}
phi_hat4 <- sapply(T21d, survfun, MAX=1)

cbind(Perc=perc, Rem=phi_hat1, T21r=phi_hat2, T21d=phi_hat3, Surv=phi_hat4)
## correcting for size bias
data.frame(est=c(Rem=phi_hat1,
                 T21r=phi_hat2,
                 T21d=phi_hat3,
                 Surv=phi_hat4,
                 T21dCorr=phi_hat3/sum(b$abundance),
                 SurvCorr=phi_hat4/sum(b$abundance)))



x <- bsims_all()$new()
bSims:::.get_detections(x, condition="event1")
bSims:::.get_detections(x, condition="det1")
bSims:::.get_detections(x, condition="alldet")

## Lessons:
## density needs to be low enough to lead to small counts
##




t21 <- pbreplicate(10, {
  v <- bsims_init() %>%
    bsims_populate() %>%
    bsims_animate(vocal_rate = 0.5) %>%
    get_events()
  v$t[1]
})
1/mean(t21)

v <- bsims_init() %>%
    bsims_populate() %>%
    bsims_animate(vocal_rate = 0.5) %>%
    get_events()
1/mean(v$t[!duplicated(v$i)])

t21 <- pbreplicate(100, {
  v <- bsims_init() %>%
    bsims_populate() %>%
    bsims_animate(vocal_rate = 0.5) %>%
    get_events()
  min(v$t[v$i==1])
})
1/mean(t21)
## need to pick random individual
## also means that abundance cannot be too high
## otherwise we get substantial bias
## lockily counts are small

range(sapply(t21, function(z) 1/mean(z)))
range(sapply(t21, function(z) z[1])-sapply(t21, function(z) min(z)))
1/mean(sapply(t21, function(z) z[1]))
1/mean(sapply(t21, function(z) sample(z, 1)))
1/mean(unlist(t21))

range(sapply(t21, function(z) mean(z)))
mean(sapply(t21, function(z) z[1]))
mean(unlist(t21))

