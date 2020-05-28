#remotes::install_github("psolymos/bSims")
library(bSims)
library(parallel)
library(detect)

# distance estimation error --------------------------------------

rint <- c(0.5, Inf)
tint <- c(3, 5, 10)
phi <- 0.5
tau <- 1
D <- 1
## define runs
s <- expand_list(
  tau = c(c(tau*0.5, tau, tau*2)),
  error = c(0, 0.1, 0.2),
  rint = list(rint),
  tint = list(tint)
)
## get wrapper objects ready
b <- lapply(s, bsims_all)
## test run before running more extensive runs
tmp <- lapply(b, function(z) z$new())


B <- 100 # number of times to replicate experiment
n <- 50 # sample size in each replicate

nc <- 4
cl <- makeCluster(nc)
tmp <- clusterEvalQ(cl, library(bSims))

bb <- lapply(b, function(z) {
  zz <- z$replicate(B * n, cl=cl)
  lapply(zz, get_table)
})

stopCluster(cl)

Mt <- matrix(tint, nrow=n, ncol=length(tint), byrow=TRUE)
Mr <- matrix(rint, nrow=n, ncol=length(rint), byrow=TRUE)

TAU <- PHI <- DEN <- matrix(NA, length(s), B)

for (i in seq_len(B)) {
  ii <- ((i-1)*n+1):(i*n)
  yy <- lapply(bb, function(z) z[ii])

  phihat <- sapply(yy, function(z) {
    Y <- t(sapply(z, colSums))
    exp(detect::cmulti.fit(Y, Mt, type="rem")$coef)
  })

  tauhat <- sapply(yy, function(z) {
    Y <- t(sapply(z, rowSums))
    out <- try(detect::cmulti.fit(Y, Mr, type="dis"))
    if (inherits(out, "try-error"))
      NA else exp(out$coef)
  })


  Ybar <- sapply(yy, function(z) mean(sapply(z, sum)))
  Dhat <- Ybar / ((1-exp(-phi*max(tint))) * tau^2*pi)

  TAU[,i] <- tauhat
  PHI[,i] <- phihat
  DEN[,i] <- Dhat
}

boxplot(t(TAU/sapply(s, "[[", "tau")))
abline(h=1, col=2)

op <- par(mfrow=c(3,3))
for (j in seq_along(s)) {
  hist(TAU[j,]*100,
    border="grey", col="grey",
    xlab="Estimated EDR (m)",
    main=paste0("EDR=", s[[j]]$tau*100,"m, SD=", s[[j]]$error))
  abline(v=s[[j]]$tau*100, col=2, lwd=2)
  abline(v=mean(TAU[j,]*100), col=1, lwd=2)
}
par(op)

## might have to add plot showing how true and percieved distances look like
DIST <- sort(runif(10000, 0, 200))
plot(DIST, rlnorm2(length(DIST), DIST, 0.2), col=2, type="l",
  ylab="Percieved distance (m)", xlab="True distance (m)")
lines(DIST, rlnorm2(length(DIST), DIST, 0.1), col=4)
lines(DIST, rlnorm2(length(DIST), DIST, 0), lwd=2)
legend("topleft", bty="n", lty=1, col=c(1,2,4),
  title="SD", legend=c("0", "0.1", "0.2"))

