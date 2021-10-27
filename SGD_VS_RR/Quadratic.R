library(zeallot)
library(CSwR)
library(scales)

# Experiment in Chap.6 ----------------------------------------------------

sim_data_quadratic <- function(n, d, 
                               ub_R = 50, # Upper bound on entries of R
                               ub_q = 50, # Upper bound on entries of q
                               ub_r = 1,  # Upper bound on entries of r
                               lambda = 5) {
  Ri <- array(runif(d * d * n, -ub_R, ub_R), c(d, d, n))
  Ai <- array(0, dim(Ri))
  Ai[] <- apply(Ri, 3, function(x) x %*% t(x) / d + lambda * diag(d))
  qi <- array(runif(d * n, -ub_q, ub_q), c(d, 1, n))
  ri <- runif(n, -ub_r, ub_r)
  list(Ai = Ai, qi = qi, ri = ri)
}

data <- sim_data_quadratic(50, 20)

fsum_quadratic <- function(data) {
  c(Ai, qi, ri) %<-% data
  A <- rowSums(Ai, dims = 2)
  q <- rowSums(qi, dims = 2)
  c(d,...,n) %<-% dim(Ai)
  list(
    par0 = rep(0, d),
    
    par_hat <- solve(2 * A, -q),
    
    # Objective function
    f = function(x)
      drop(crossprod(x, A %*% x) + crossprod(q, x)) + sum(ri),
    
    # Gradient 
    grad = function(x, i)                
      2 * Ai[ , , i] %*% x + qi[ , ,i],
    
    # Hessian
    hessian = function(x, i)
      2 * Ai[ , , i]
  )
}

c(par0, par_hat, f, grad_obs, hessian_obs) %<-% fsum_quadratic(data)
f_min <- f(par_hat)

dist_to_opt_x <- function(par) sum((par - par_hat)^2)
dist_to_opt_f <- function(par) (f(par) - f_min)^2


# The_implementation_of_3_methods -----------------------------------------

RR_SG <- function(
  par,
  grad,              # Function of parameter and observation index
  n,                 # Sample size
  alpha,             # Decay schedule or a fixed learning rate
  maxEpoch = 200,     # Max epoch iterations
  flipFlop = FALSE,  # Should FlipFlop be used in conjunction
  cb = NULL,
  ...
) {
  alpha <- if (is.function(alpha)) alpha(1:maxEpoch) else rep(alpha, maxEpoch)
  par_bar <- par
  obj_value <- numeric(maxEpoch)
  pot_value <- numeric(maxEpoch)
  for(k in 1:maxEpoch) {
    if(!is.null(cb)) cb()
    if(flipFlop && k %% 2 == 0)
      samp <- samp[n:1]
    else
      samp <- sample(n, ...)
    for(j in 1:n) {
      i <-  samp[j]
      par <- par - alpha[k] * grad(par, i)
    }
    par_bar <- (1 - 1/k) * par_bar + 1/k * par
    obj_value[k] <- 10 * log10(f(par_bar) - f_min)
    pot_value[k] <- 10 * log10(sum((par_bar - par_hat)^2))
  }
  list(obj_value = obj_value, pot_value = pot_value)
}

DRR <- function(
  par,
  grad,
  hessian,
  m,
  alpha,
  maxEpoch = 100,
  q = 1,
  cb = NULL
) {
  n <- length(par)
  par_bar <- numeric(n)
  alpha_bar <- 0
  v_hat <- numeric(n)
  H_hat <- matrix(0, nrow = n, ncol = n)
  iter_q <- ceiling((1 - q) * maxEpoch -1)
  alpha <- if (is.function(alpha)) alpha(1:maxEpoch) else rep(alpha, maxEpoch)
  obj_value <- numeric(maxEpoch)
  pot_value <- numeric(maxEpoch)
  for(k in 0:(maxEpoch-1)) {
    if(!is.null(cb)) cb()
    par_bar <- (k * par_bar + par) / (k + 1)
    alpha_bar <- (k * alpha_bar + alpha[k + 1]) / (k + 1)
    if(k == iter_q) {
      par_bar_q <- par_bar
      alpha_bar_q <- alpha_bar
    }
    samp <- sample(m)
    for(j in 1:m) {
      i <- samp[j]
      if(k == maxEpoch - 1) {
        v_hat <- v_hat + hessian(par, i) %*% grad(par, i) / 2
        H_hat <- H_hat + hessian(par, i)
      }
      par <- par - alpha[k + 1] * grad(par, i)
    }
    obj_value[k+1] <- 10 * log10(f(par_bar) - f_min)
    pot_value[k+1] <- 10 * log10(sum((par_bar - par_hat)^2))
  }
  if(q != 1) {
    par_bar <- (par_bar - (1 - q) * par_bar_q) / q
    alpha_bar <- (par_bar - (1 - q) * alpha_bar_q) / q
  }
  list(obj_value = c(obj_value[-maxEpoch], 10 * log10(f(par_bar + alpha_bar * solve(H_hat) %*% v_hat) - f_min)),
       pot_value = c(pot_value[-maxEpoch], 10 * log10(sum((par_bar + alpha_bar * solve(H_hat) %*% v_hat - par_hat)^2))))
}

decay_scheduler <- function(R, s) {
  function(k) R / k^s
}

rate <- decay_scheduler(1/3 * 1e-3, 0.75)

n <- length(data$ri)


# 500_sample_path_200epoch_and_2000epoch ---------------------------------

iter_SG <- replicate(500, RR_SG(par0, grad_obs, n, rate, maxEpoch = 200, replace = TRUE))
iter_RR <- replicate(500, RR_SG(par0, grad_obs, n, rate, maxEpoch = 200))
iter_RR_FF <- replicate(500, RR_SG(par0, grad_obs, n, rate, flipFlop = TRUE, maxEpoch = 200))
iter_DRR <- replicate(500, DRR(par0, grad_obs, hessian_obs, n, rate, maxEpoch = 200))


##___Distance_to_minimizer_________________________________________________________________

dist_SG_x <- sapply(1:500, function(i)dist_to_opt_x(iter_SG[,,i])) * 10^6 
dist_RR_x <- sapply(1:500, function(i)dist_to_opt_x(iter_RR[,,i])) * 10^6
dist_RR_FF_x <- sapply(1:500, function(i)dist_to_opt_x(iter_RR_FF[,,i])) * 10^6
dist_DRR_x <- sapply(1:500, function(i)dist_to_opt_x(iter_DRR[,,i])) * 10^6

hist(dist_SG_x, breaks= 10, xlim=c(0, 2.5), col = alpha("black", 0.3),
     xlab="", ylab = "", main = "Histogram of distance to minimizer", ylim = c(0, 150))
mtext(expression(" \u00D7"~10^-6), side=1, line=2.5, at=2.4)
abline(v = mean(dist_SG_x), col = "black")
hist(dist_RR_x, breaks=10, xlim=c(0, 2.5), col = alpha("blue", 0.9), add=T)
abline(v = mean(dist_RR_x), col = "blue")
hist(dist_DRR_x, breaks=10, xlim=c(0, 2.5), col = alpha("green", 0.6), add=T)
abline(v = mean(dist_DRR_x), col = "green")
hist(dist_RR_FF_x, breaks=10, xlim=c(0, 2.5), col = alpha("red", 0.3), add=T)
abline(v = mean(dist_RR_FF_x), col = "red")
legend(1.7, 155, legend=c("SG","RR","De-biased RR", "RR with FlipFlop"),
       col=c(alpha("black", 0.3), alpha("blue", 0.9), alpha("green", 0.6), col = alpha("red", 0.3)),
       pt.cex=2, pch=15, 
       cex = 0.8, inset = 0.02,
       y.intersp = 1.2,
       x.intersp = 1)
box()

hist(dist_RR_x*10, breaks= 10, xlim=c(0, 8), col = alpha("blue", 0.9),
     xlab="", ylab = "", main = "Histogram of distance to minimizer", ylim = c(0, 150))
mtext(expression(" \u00D7"~10^-7), side=1, line=2.5, at=7.6)
abline(v = mean(dist_RR_x*10), col = "blue")
hist(dist_DRR_x*10, breaks=10, xlim=c(0, 8), col = alpha("green", 0.6), add=T)
abline(v = mean(dist_DRR_x*10), col = "green")
hist(dist_RR_FF_x*10, breaks=10, xlim=c(0, 8), col = alpha("red", 0.3), add=T)
abline(v = mean(dist_RR_FF_x*10), col = "red")
legend(5.5, 155, legend=c("RR","De-biased RR", "RR with FlipFlop"),
       col=c(alpha("blue", 0.9), alpha("green", 0.6), col = alpha("red", 0.3)), pt.cex=2, pch=15, 
       cex = 0.8, inset = 0.02,
       y.intersp = 1.2,
       x.intersp = 1)
box()

##___Suboptimality_in_objective____________________________________________________________________

dist_SG_f <- sapply(1:500, function(i)dist_to_opt_f(iter_SG[,,i])) * 10^3  
dist_RR_f <- sapply(1:500, function(i)dist_to_opt_f(iter_RR[,,i])) * 10^3
dist_RR_FF_f <- sapply(1:500, function(i)dist_to_opt_f(iter_RR_FF[,,i])) * 10^3
dist_DRR_f <- sapply(1:500, function(i)dist_to_opt_f(iter_DRR[,,i])) * 10^3

hist(dist_SG_f, breaks= 10, xlim=c(0, 10), col = alpha("black", 0.3),
     xlab="", ylab = "", main = "Histogram of the suboptimality in objective", ylim = c(0, 200))
mtext(expression(" \u00D7"~10^-3), side=1, line=2.5, at=9.5)
abline(v = mean(dist_SG_f), col = "black")
hist(dist_RR_f, breaks=10, xlim=c(0, 10), col = alpha("blue", 0.9), add=T)
abline(v = mean(dist_RR_f), col = "blue")
hist(dist_DRR_f, breaks=10, xlim=c(0, 10), col = alpha("green", 0.6), add=T)
abline(v = mean(dist_DRR_f), col = "green")
hist(dist_RR_FF_f, breaks=10, xlim=c(0, 10), col = alpha("red", 0.3), add=T)
abline(v = mean(dist_RR_FF_f), col = "red")
legend(7, 205, legend=c("SG","RR","De-biased RR", "RR with FlipFlop"),
       col=c(alpha("black", 0.3), alpha("blue", 0.9), alpha("green", 0.6), col = alpha("red", 0.3)),
       pt.cex=2, pch=15, 
       cex = 0.8, inset = 0.02,
       y.intersp = 1.2,
       x.intersp = 1)
box()

hist(dist_RR_f*10, breaks= 10, xlim=c(0, 10), col = alpha("blue", 0.9),
     xlab="", ylab = "", main = "Histogram of the suboptimality in objective", ylim = c(0, 200))
mtext(expression(" \u00D7"~10^-4), side=1, line=2.5, at=9.5)
abline(v = mean(dist_RR_f*10), col = "blue")
hist(dist_DRR_f*10, breaks=10, xlim=c(0, 10), col = alpha("green", 0.6), add=T)
abline(v = mean(dist_DRR_f*10), col = "green")
hist(dist_RR_FF_f*10, breaks=10, xlim=c(0, 10), col = alpha("red", 0.3), add=T)
abline(v = mean(dist_RR_FF_f*10), col = "red")
legend(7, 205, legend=c("RR","De-biased RR", "RR with FlipFlop"),
       col=c(alpha("blue", 0.9), alpha("green", 0.6), col = alpha("red", 0.3)), pt.cex=2, pch=15, 
       cex = 0.8, inset = 0.02,
       y.intersp = 1.2,
       x.intersp = 1)
box()

##___________________________________________________________________________________________________



# 1000_epoch_suboptimality ------------------------------------------------
subopt_SG <- RR_SG(par0, grad_obs, n, rate, maxEpoch = 2000, replace = TRUE)
subopt_RR <- RR_SG(par0, grad_obs, n, rate, maxEpoch = 2000)
subopt_RR_FF <- RR_SG(par0, grad_obs, n, rate, maxEpoch = 2000, flipFlop = TRUE)
subopt_DRR <- DRR(par0, grad_obs, hessian_obs, n, rate, maxEpoch = 2000)

plot(1:2000, subopt_SG$obj_value, type = "l", col = alpha("black", 0.3), ylim = c(-45,20),
     xlab = "Epochs", ylab = "Expected suboptimality (dB)", lwd = 2)  
lines(1:2000, subopt_RR$obj_value, type = "l", col = alpha("blue", 0.9), lwd = 2) 
lines(1:2000, subopt_RR_FF$obj_value, type = "l", col = alpha("red", 0.3), lwd = 2) 
lines(1:2000, subopt_DRR$obj_value, type = "l", col = alpha("green", 0.6), lwd = 2) 
grid()
legend(1500, 20, legend=c("SG","RR","De-biased RR", "RR with FlipFlop"),
       col=c(alpha("black", 0.3), alpha("blue", 0.9), alpha("green", 0.6), col = alpha("red", 0.3)),
       pt.cex=2, pch=15, 
       cex = 0.8, inset = 0.02,
       y.intersp = 1.2,
       x.intersp = 1)

plot(1:2000, subopt_SG$pot_value, type = "l", col = alpha("black", 0.3), ylim = c(-90,-20),
     xlab = "Epochs", ylab = "Mean-square-deviation (dB)", lwd = 2)  
lines(1:2000, subopt_RR$pot_value, type = "l", col = alpha("blue", 0.9), lwd = 2) 
lines(1:2000, subopt_RR_FF$pot_value, type = "l", col = alpha("red", 0.3), lwd = 2) 
lines(1:2000, subopt_DRR$pot_value, type = "l", col = alpha("green", 0.6), lwd = 2) 
grid()
legend(1500, -20, legend=c("SG","RR","De-biased RR", "RR with FlipFlop"),
       col=c(alpha("black", 0.3), alpha("blue", 0.9), alpha("green", 0.6), col = alpha("red", 0.3)),
       pt.cex=2, pch=15, 
       cex = 0.8, inset = 0.02,
       y.intersp = 1.2,
       x.intersp = 1)



# count_to_N <- function(runningTime = 0.05) {
#   startTime <- as.numeric(Sys.time())
#   for (i in 1:10000) {
#     print(i)
#     if (as.numeric(Sys.time() - startTime >= runningTime)) break
#   }
# }
# 
# 
# test_fn <- function(runTime = 0.05) {
#   sum <- 0
#   stop <- FALSE
#   startTime <- as.numeric(Sys.time())
#   for(j in 1:100) {
#     for (i in 1:100) {
#       if (as.numeric(Sys.time() - startTime >= runTime)) {
#         stop <- TRUE
#         break
#       }
#       if(i %% 100 == 0) print(i)
#     }
#     sum <- sum + j
#     cat("j= ", j, "sum=", sum, "\n")
#     if(stop) break
#   }
#   sum
# }
# 
# test_gn <- function(runTime = 0.05) {
#   sum <- 0
#   startTime <- as.numeric(Sys.time())
#   for(j in 1:100) {
#     for (i in 1:100) {
#       if (as.numeric(Sys.time() - startTime >= runTime)) break
#       print(i)
#     }
#     sum <- sum + j
#   }
#   sum
# }








