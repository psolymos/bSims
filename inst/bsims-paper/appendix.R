## ----------------- ROADSIDE EXAMPLE ----------------

## Appendix from the paper
## Solymos, P. 2023. Agent-based simulations improve abundance estimation.
## Biologia Futura, DOI 10.1007/s42977-023-00183-2

## ---------------- Load packages & setup ------------

library(bSims)
library(parallel)
library(pbapply)
library(detect)
library(ggplot2)
library(patchwork)

pboptions(type = "none")
set.seed(0)

## ---------------- Simulation settings ------------

B <- 100 # number of replicate runs
n <- 100 # number of samples for each run
D <- 1 # population density in habitat
phi <- 0.5 # cue rate in habitat
tau <- 1 # effective detection radius in habitat

w <- c(0.05, 0.1) # road half width
delta_phi <- c(0.5, 1, 2)
delta_tau <- c(1, 2)

## breaks for time and distance intervals
tbr <- 1:10 # time (minutes)
rbr <- c(0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2, 2.25, 2.5, Inf) # distance (100 m)

delta_phi <- c(0.5, 1, 2) # cue rate in edge relative to habitat
delta_tau <- c(1, 2) # detection distance in road relative to habitat

## reference setup: no road
b0 <- bsims_all(
    extent = 10,
    road = 0,
    edge = 0,
    density = c(D, D, D),
    vocal_rate = phi * c(1, 1, 1),
    move_rate = 0,
    movement = 0,
    mixture = 1,
    duration = 10,
    tau = tau * c(1, 1, 1),
    tint = tbr,
    rint = rbr)

## road half width >0 setups: full factorial
s <- expand_list(
    extent = 10,
    road = w,
    edge = 0.5,
    density = list(c(D, D, 0)),
    vocal_rate = lapply(delta_phi, \(x) phi * c(1, x, x)),
    move_rate = 0,
    movement = 0,
    mixture = 1,
    duration = 10,
    tau = lapply(delta_tau, \(x) tau * c(1, 1, x)),
    tint = list(tbr),
    rint = list(rbr))

## all 13 setup as a list
b <- c(list(b0), lapply(s, bsims_all))

## ---------------- Capture simulation results ------------

dat_fun <- function(x) {
    lapply(x, \(z) get_table(z, "removal"))
}
cl <- makeCluster(4)
dat <- NULL
for (i in seq_along(b)) {
    z <- b[[i]]
    dati <- NULL
    for (j in seq_len(B)) {
        message("Setup: ", i, " / ", length(b), " - Iter ", j, " / ", B, " --- ", Sys.time())
        x <- z$replicate(n, cl = cl)
        dij <- list(
            info = c(id=i,
                iter=j,
                road = z$settings()$road,
                phi = z$settings()$vocal_rate[3],
                tau = z$settings()$tau[3]),
            tabs = dat_fun(x))
        dati[[j]] <- dij
    }
    dat[[i]] <- list(i = i, object = z, results = dati)
}
stopCluster(cl)

## ---------------- Estimate population density ------------

get_tabs <- function(tab, gr, gt) {
    m <- matrix(0, max(gr, na.rm = TRUE), max(gt, na.rm = TRUE))
    for (j in seq_len(nrow(tab))) {
        for (k in seq_len(ncol(tab))) {
            m[gr[j],gt[k]] <- m[gr[j],gt[k]] + tab[j,k]
        }
    }
    m
}

est_fun <- function(x, type = 1) {
    if (type == 1) {
        gr <- c(1, 1, 2, 2, 3, 3, 4, 4, 4, 4, 4)
        gt <- c(1, 1, 1, 2, 2, 3, 3, 3, 3, 3)
    }
    if (type == 2) {
        gr <- c(1, 1, 2, 2, 3, 3, 4, 4, 4, 4, 4)
        gt <- c(1, 2, 3, NA, NA, NA, NA, NA, NA, NA)
    }
    if (type == 3) {
        gr <- c(1, 1, 2, 2, 3, 3, NA, NA, NA, NA, NA)
        gt <- c(1, 1, 1, 2, 2, 3, 3, 3, 3, 3)
    }
    if (type == 4) {
        gr <- c(1, 1, 2, 2, 3, 3, NA, NA, NA, NA, NA)
        gt <- c(1, 2, 3, NA, NA, NA, NA, NA, NA, NA)
    }
    x <- lapply(x, get_tabs, gr, gt)
    tval <- 1:10
    rval <- c(0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2, 2.25, 2.5, Inf)

    yrem <- t(sapply(x, \(z) colSums(z)))
    ydis <- t(sapply(x, \(z) rowSums(z)))
    tb <- rev(rev(tval[!is.na(gt)])[!duplicated(rev(gt[!is.na(gt)]))])
    rb <- rev(rev(rval[!is.na(gr)])[!duplicated(rev(gr[!is.na(gr)]))])

    fitp <- try(detect::cmulti.fit(
        yrem,
        matrix(tb, length(x), length(tb), byrow = TRUE),
        type="rem"), silent = TRUE)
    fitq <- try(detect::cmulti.fit(
        ydis,
        matrix(rb, length(x), length(rb), byrow = TRUE),
        type="dis"), silent = TRUE)
    phihat <- if (inherits(fitp, "try-error"))
        NA else exp(fitp$coef)
    tauhat <- if (inherits(fitq, "try-error"))
        NA else exp(fitq$coef)
    p <- 1-exp(-max(tb)*phihat)
    q <- if (is.infinite(max(rb)))
        1 else (tauhat^2/max(rb)^2) * (1-exp(-(max(rb)/tauhat)^2))
    A <- if (is.infinite(max(rb)))
        pi * tauhat^2 else pi * max(rb)^2
    Dhat <- mean(rowSums(yrem) / (A * p * q))
    c(ytot=sum(yrem), tmax=max(tb), rmax=max(rb), corr = A * p * q, phihat=phihat, tauhat=tauhat, density=Dhat)
}

res <- NULL
for (i in 1:length(dat)) {
    for (j in 1:length(dat[[i]]$results)) {
        message("Setup (steady): ", i, " - Iter ", j)

        info <- dat[[i]]$results[[j]]$info
        tabs <- dat[[i]]$results[[j]]$tabs

        est1 <- est_fun(tabs, type = 1)
        est2 <- est_fun(tabs, type = 2)
        est3 <- est_fun(tabs, type = 3)
        est4 <- est_fun(tabs, type = 4)

        info <- c(info, movement = 0)

        res <- rbind(res,
            rbind(
                c(info, est1),
                c(info, est2),
                c(info, est3),
                c(info, est4)))
    }
}

## ---------------- Create figure ------------

## filter results for figure
d <- data.frame(res)
d$width <- paste0("width=", 2*d$road*100)
d$width[d$width == "width=0"] <- "baseline"
d$protocol <- paste0(d$tmax, "/", d$rmax)
d <- d[d$road < 0.2,]
d <- d[d$protocol %in% c("10/Inf", "3/1.5"),]
d <- na.omit(d)

## define response variable
d$variable <- d$density
V <- 1

## plot
p1 <- d[d$protocol == "10/Inf" & d$movement == 0,] |>
    ggplot(aes(
        x = as.factor(tau*100),
        y = variable,
        fill = as.factor(phi)
    )) +
    geom_abline(intercept = V, slope = 0, col = "grey") +
    geom_boxplot(outlier.size = 0.5) +
    facet_wrap(vars(width), scales = "free_x") +
    ylim(0, 1.5) +
    scale_fill_grey(start = 0.5, end = 1) +
    labs(subtitle = "10 min/Unlimited", fill = "cue rate (edge)",
        x = "detection radius (road)", y = "estimated density") +
    theme_minimal() +
    theme(legend.position = "none")

p2 <- d[d$protocol == "3/1.5" & d$movement == 0,] |>
    ggplot(aes(
        x = as.factor(tau*100),
        y = variable,
        fill = as.factor(phi)
    )) +
    geom_abline(intercept = V, slope = 0, col = "grey") +
    geom_boxplot(outlier.size = 0.5) +
    facet_wrap(vars(width), scales = "free_x") +
    ylim(0, 1.5) +
    scale_fill_grey(start = 0.5, end = 1) +
    labs(subtitle = "3 min/150 m", fill = "cue rate (edge)",
        x = "detection radius (road)", y = "estimated density") +
    theme_minimal() +
    theme(legend.position = "none")

p1 + p2
