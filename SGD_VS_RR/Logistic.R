library(zeallot)
library(MASS)
library(scales)
library(glmnet)
library(shape)


sim_data_logistic <- function(n, d, 
                              mu,        
                              lb_sigma,  
                              ub_sigma  
                              ) {
  beta_aux <- rnorm(d)
  X <-  mvrnorm(n,
              rep(mu, d),
              diag(runif(d, lb_sigma, ub_sigma)))
  y <- runif(n) <= X %*% beta_aux
  list(X = X, y = 2 * y - 1)
}

n <- 1000
d <- 10
mu <- 0
lb_sigma <- 1
ub_sigma <- 10
c(X, y) %<-% sim_data_logistic(n, d, mu, lb_sigma, ub_sigma)

fsum_logistic <- function(X, y) {
  list(
    par0 = rep(0, ncol(X)),
    
    # Objective function
    f = function(beta, lambda)
      mean(log(1 + exp(-y * X %*% beta))) + lambda * drop(crossprod(beta)) / 2,
    
    # Gradient 
    grad = function(beta, i, lambda){
      xi <- X[i, , drop = FALSE]
      (- y[i] / (1 + drop(exp(y[i] * xi %*% beta)))) * t(xi) + lambda * beta
    },
    
    # Hessian
    hessian = function(beta, i, lambda) {
      xi <- X[i, , drop = FALSE]
      y[i]^2 / (2 + drop(exp(xi %*% beta)) * (exp(y[i]) + exp(-y[i]))) * crossprod(xi) + lambda
    }
  )
}
c(par0, f, grad_obs, hessian_obs) %<-% fsum_logistic(X, y)

y_01 <- (y + 1) / 2
lambda = 0.2
fit <- glmnet(X, y_01, family = "binomial", alpha = 0, lambda = lambda, intercept = FALSE, standardize = FALSE)
par_hat <- unname(as.matrix(coef(fit)))[-1, ,drop = FALSE]
f_min <- f(par_hat, lambda)

dist_x <- function(par) sum((par - par_hat)^2)
dist_to_opt_f <- function(par) (f(par) - f_min)^2


RR_SG <- function(
  par,
  grad,              # Function of parameter and observation index
  n,                 # Sample size
  alpha,             # Decay schedule or a fixed learning rate
  maxEpoch = 200,    # Max epoch iterations
  flipFlop = FALSE,  # Should FlipFlop be used in conjunction
  cb = NULL,
  ...
) {
  alpha <- if (is.function(alpha)) alpha(1:maxEpoch) else rep(alpha, maxEpoch)
  par_bar <- par
  msd_iter <- numeric(maxEpoch)
  for(k in 1:maxEpoch) {
    if(!is.null(cb)) cb()
    if(flipFlop && k %% 2 == 0)
      samp <- samp[n:1]
    else
      samp <- sample(n, ...)
    for(j in 1:n) {
      i <-  samp[j]
      # par <- par - alpha[k] * grad(par, i, lambda)
      par <- par - alpha[k] * grad(par, i)
    }
    par_bar <- (1 - 1/k) * par_bar + 1/k * par
    msd_iter[k] <- 10 * log10(sum((par_bar - par_hat)^2))
  }
  msd_iter
}

# w1 <- RR_SG(par0, grad_obs, n, alpha = 0.003, maxEpoch = 10)


DRR <- function(
  par,
  grad,
  hessian,
  n,
  alpha,
  maxEpoch = 100,
  q = 1,
  cb = NULL
) {
  d <- length(par)
  par_bar <- numeric(d)
  alpha_bar <- 0
  v_hat <- numeric(d)
  H_hat <- matrix(0, nrow = d, ncol = d)
  iter_q <- ceiling((1 - q) * maxEpoch -1)
  alpha <- if (is.function(alpha)) alpha(1:maxEpoch) else rep(alpha, maxEpoch)
  msd_iter <- numeric(maxEpoch)
  for(k in 0:(maxEpoch-1)) {
    if(!is.null(cb)) cb()
    par_bar <- (k * par_bar + par) / (k + 1)
    alpha_bar <- (k * alpha_bar + alpha[k + 1]) / (k + 1)
    if(k == iter_q) {
      par_bar_q <- par_bar
      alpha_bar_q <- alpha_bar
    }
    samp <- sample(n)
    for(j in 1:n) {
      i <- samp[j]
      if(k == maxEpoch - 1) {
        # v_hat <- v_hat + hessian(par, i, lambda) %*% grad(par, i, lambda) / 2
        # H_hat <- H_hat + hessian(par, i, lambda)
        v_hat <- v_hat + hessian(par, i) %*% grad(par, i) / 2
        H_hat <- H_hat + hessian(par, i)
      }
      # par <- par - alpha[k + 1] * grad(par, i, lambda)
      par <- par - alpha[k + 1] * grad(par, i)
    }
    msd_iter[k+1] <- 10 * log10(sum((par_bar - par_hat)^2))
  }
  if(q != 1) {
    par_bar <- (par_bar - (1 - q) * par_bar_q) / q
    alpha_bar <- (par_bar - (1 - q) * alpha_bar_q) / q
  }
  msd_value = c(msd_iter[-maxEpoch], 10 * log10(sum((par_bar + alpha_bar * solve(H_hat) %*% v_hat - par_hat)^2)))
  # par_bar + alpha_bar * solve(H_hat) %*% v_hat
}

decay_scheduler <- function(R, s) {
  function(k) R / k^s
}

rate <- decay_scheduler(1/3 * 1e-3, 0.75)

MSD_SG <- replicate(500, RR_SG(par0, grad_obs, n, alpha = rate, maxEpoch = 200, replace = TRUE))
MSD_SG <- apply(MSD_SG, 1, mean)
MSD_RR <- replicate(500, RR_SG(par0, grad_obs, n, alpha = rate, maxEpoch = 200))
MSD_RR <- apply(MSD_RR, 1, mean)
MSD_RR_FF <- replicate(500, RR_SG(par0, grad_obs, n, alpha = rate, maxEpoch = 200, flipFlop = TRUE))
MSD_RR_FF <- apply(MSD_RR_FF, 1, mean)
MSD_DRR <- replicate(500, DRR(par0, grad_obs, hessian_obs, n, alpha = rate, maxEpoch = 200))
MSD_DRR <- apply(MSD_DRR, 1, mean)


MSD_SG <- RR_SG(par0, grad_obs, n, alpha = rate, maxEpoch = 2000, replace = TRUE)
MSD_RR <- RR_SG(par0, grad_obs, n, alpha = rate, maxEpoch = 2000)
MSD_RR_FF <- RR_SG(par0, grad_obs, n, alpha = rate, maxEpoch = 2000, flipFlop = TRUE)
MSD_DRR <- DRR(par0, grad_obs, hessian_obs, n, alpha = rate, maxEpoch = 2000)

# MSD_SG_s <- replicate(100, RR_SG(par0, grad_obs, n, alpha = 0.0003, maxEpoch = 1000, lambda = 0.2, replace = TRUE))
# MSD_RR_s <- replicate(100, RR_SG(par0, grad_obs, n, alpha = 0.0003, maxEpoch = 1000, lambda = 0.2))


plot(1:2000, MSD_SG, type = "l", col = alpha("black", 0.6), ylim = c(-90,-20),
      ylab = "Mean-square-deviation (dB)", lwd = 1.5, xlab = "Iterations") 
# axis(1, at=c(0, 20*(1:5)*n), 2*(0:5))
# mtext(expression(" \u00D7"~10^5), side=1, line=2.5, at=97000)
lines(1:2000, MSD_RR, type = "l", col = alpha("blue", 0.9), lwd = 1.8) 
lines(1:2000, MSD_RR_FF, type = "l", col = alpha("red", 0.6), lwd = 1.5) 
lines(1:2000, MSD_DRR, type = "l", col = alpha("green", 0.6), lwd = 1.5) 
# points(n*(1:10), MSD_RR[n*(1:10)], pch = 16, cex = 1.2, col = alpha("blue", 0.6))
# points(n*(1:10), MSD_RR_FF[n*(1:10)], pch = 16, cex = 1.2, col = alpha("red", 0.6))
# points(1, MSD_RR[1], pch = 16, cex = 1.2, col = alpha("blue", 0.6))
# points(1, MSD_RR_FF[1], pch = 16, cex = 1.2, col = alpha("red", 0.6))
# points(n*(1:10), MSD_RR[n*(1:10)], pch = 16, cex = 1.2, col = alpha("blue", 0.6))
# lines(n*(1:10), MSD_RR[n*(1:10)], col = alpha("blue", 0.6), lty = 1)
# points(n*(1:10), MSD_RR_FF[n*(1:10)], pch = 16, cex = 1.2, col = alpha("red", 0.6)) 
grid()
legend(70000, 0, legend=c("SG","RR","RR with FlipFlop"),
       col=c(alpha("black", 0.6), alpha("blue", 0.9), alpha("red", 0.6)),
       pt.cex=2, pch=15, 
       cex = 0.8, inset = 0.02,
       y.intersp = 1.2,
       x.intersp = 1)




hist(MSD_SG, breaks= 10, xlim=c(-50, -20), col = alpha("black", 0.3),
     xlab="", ylab = "", main = "Histogram of mean-square-deviation", ylim = c(0, 40))
mtext("dB", side=1, line=2.5, at=-20)
abline(v = mean(MSD_SG), col = "black")
hist(MSD_RR, breaks=10, xlim=c(-50, -20), col = alpha("blue", 0.6), add=T)
abline(v = mean(MSD_RR), col = "blue")
hist(MSD_SG_s, breaks=5, xlim=c(-50, -20), col = alpha("black", 0.3), add=T)
abline(v = mean(MSD_SG_s), col = "black")
hist(MSD_RR_s, breaks=5, xlim=c(-50, -20), col = alpha("blue", 0.6), add=T)
abline(v = mean(MSD_RR_s), col = "blue")
text(-30, 40, expression(alpha==0.003))
Arrows(-30,38,-27.5,26, arr.type="triangle", arr.width=0.1)
Arrows(-30,38,-24.5,26, arr.type="triangle", arr.width=0.1)
text(-40, 40, expression(alpha==0.0003))
Arrows(-40,38,-34.5,26, arr.type="triangle", arr.width=0.1)
Arrows(-40,38,-46,26, arr.type="triangle", arr.width=0.1)

legend(-22, 40, legend=c("SG","RR"),
       col=c(alpha("black", 0.3), alpha("blue", 0.6)),
       pt.cex=2, pch=15, 
       cex = 0.8, inset = 0.02,
       y.intersp = 1.2,
       x.intersp = 1)
box()




par(mar = c(0, 0, 0, 0))
hist(MSD_SG, breaks= 10, xlim=c(-50, -20), col = "#646464",
     xlab="", ylab = "", main = "", ylim = c(0, 40), xaxt = "n", yaxt = "n")
mtext("dB", side=1, line=2.5, at=-20)
abline(v = mean(MSD_SG), col = "#646464")
hist(MSD_RR, breaks=10, xlim=c(-50, -20), col = "#46743C", add=T)
abline(v = mean(MSD_RR), col = "#46743C")
hist(MSD_SG_s, breaks=5, xlim=c(-50, -20), col = "#646464", add=T)
abline(v = mean(MSD_SG_s), col = "#646464")
hist(MSD_RR_s, breaks=5, xlim=c(-50, -20), col = "#46743C", add=T)
abline(v = mean(MSD_RR_s), col = "#46743C")
text(-30, 40, expression(alpha==0.003))
Arrows(-30,38,-27.5,26, arr.type="triangle", arr.width=0.1)
Arrows(-30,38,-24.5,26, arr.type="triangle", arr.width=0.1)
text(-40, 40, expression(alpha==0.0003))
Arrows(-40,38,-34.5,26, arr.type="triangle", arr.width=0.1)
Arrows(-40,38,-46,26, arr.type="triangle", arr.width=0.1)

legend(-22, 40, legend=c("SG","RR"),
       col=c(alpha("black", 0.3), alpha("blue", 0.6)),
       pt.cex=2, pch=15, 
       cex = 0.8, inset = 0.02,
       y.intersp = 1.2,
       x.intersp = 1)
box()



















# last_exp <- function(alpha, num_epoch) {
#   msd_rr <- numeric(length(alpha))
#   for (i in 1:length(alpha)) {
#     msd_rr[i] <- mean(replicate(50, RR_SG(par0, grad_obs, n, alpha = alpha[i], maxEpoch = num_epoch[i], lambda = 0.2)))
#   }
#   msd_rr
# }
# 
# alpha_seq <- c(2*(1:5)*1e-4, 2*(1:5)*1e-5)
# epoch_seq <- rep(40000, 10)
# 
# msd_rr_1 <- last_exp(alpha_seq, epoch_seq)
# 
# msd_rr_all <- c()
# 
# 
# dat <- data.frame(x = c(2*(1:5)*1e-4, 2*(1:5)*1e-5),
#                   y = c(-65.69337,
#                         -62.62403,
#                         -60.16624,
#                         -58.15443,
#                         -56.48205,
#                         -69.48569,
#                         -69.11805,
#                         -68.73624,
#                         -68.33550,
#                         -67.94283))
# 
# MSD_lm <- data.frame(x = c(2*(5:1)*1e-4, 2*(5:1)*1e-5),
#                      y = msd_rr_1[c(5:1, 10:6)],
#                      z = 10^(msd_rr_1[c(5:1, 10:6)] / 10))
# 
# MSD_lm <- MSD_lm[c(1:5), ]
# fit_msd_lm <- lm(z ~ 0 + x, data = MSD_lm)

