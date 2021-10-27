  library(tidyverse)
  library(e1071)

  dat <- read.csv("./ionosphere.data", header = FALSE)
  dat$V35 <- ifelse(dat$V35 == "g", 1, -1)
  

# CV-SVM ------------------------------------------------------------------
  set.seed(20200712)
  id <- sample(1:nrow(dat), 200)
  train <- dat[id, -35]
  train_label <- dat[id, 35] %>% as.factor
  test <- dat[-id, -35]
  test_label <- dat[-id, 35] %>% as.factor
  ## calculating the distance of any two points having different labels
  opt_gamma <- function(train, train_label) {
    Dist <- dist(rbind(
      train[train_label == -1, ],
      train[train_label == 1, ]
    ), diag = TRUE, upper = TRUE) %>% as.matrix
    ## Find the median value
    n <- nrow(Dist)
    n_0 <- nrow(train[train_label == -1, ])
    G <- c(apply(Dist[(n_0 + 1) : n, (1 : n_0)], 2, min), 
           apply(Dist[(1 : n_0), ((1 + n_0) : n)], 2, min))
    sigma_Jaa <- median(G)
    return(1 / (2*sigma_Jaa^2))
  }
  gamma_Jaa <- opt_gamma(train = train, train_label = train_label)
  ## Grid search
  ## Set the number of folds of CV
  tc <- tune.control(cross = 5)
  ## Execute the grid search using the instructions in exam paper
  time_0 <- Sys.time()
  tune_svm <- tune.svm(train, train_label, kernel = "radial",
                       cost = 10^(-3:3),
                       gamma = gamma_Jaa * (10^seq(-4, 4, by = 2)),
                       tunecontrol = tc)
  ## fit a svm classifier with the hyperparameters chosen from above step
  fit <- svm(train, train_label, kernel = "radial",
             cost = as.numeric(tune_svm$best.parameters[2]),
             gamma = as.numeric(tune_svm$best.parameters[1]))
  time_1 <- Sys.time()
  ## do classification and calculate the accuracy rate
  pred_test <- predict(fit, test)
  loss_base <- mean(pred_test != test_label)  
  time_base <- time_1 - time_0

# PAC-Bayes_lambda --------------------------------------------------------

  
  KL_div <- function(p, q) weighted.mean(log(p/q), p)
  
  opt_rho <- function(m, n, r, delta) {
    L_val = c()
    pred = c()
    time_0 = Sys.time()
    for (i in 1:m) {
      id = sample(1:n, r)
      train_sub = train[id, ]
      train_label_sub = train_label[id]
      valid = train[-id, ]
      valid_label = train_label[-id]
      gamma = sample(gamma_Jaa * (10^seq(-4, 4, by = 2)), 1)
      fit = svm(train_sub, train_label_sub, kernel = "radial", gamma = gamma)
      L_val = c(L_val, mean(predict(fit, valid) != valid_label))
      pred = cbind(pred, as.numeric(as.character(predict(fit, test))))
    }
    lambda = 0.5
    rho_0 = numeric(m)
    z = TRUE
    eps = 1e-10
    while (z) {
      rho_unmer = exp(-lambda*(n-r)*(L_val-min(L_val)))
      rho = (rho_unmer/mean(rho_unmer))/m
      lambda_deno = (2*(n-r)*weighted.mean(L_val, rho))/(KL_div(rho, rep(1/m,m))+log((2*sqrt(n-r))/delta))
      lambda = 2/(sqrt(lambda_deno+1) +1)
      z = dist(rbind(rho, rho_0)) > eps
      rho_0 = rho
    }
    time = Sys.time() - time_0
    pred = sign(pred%*%rho)
    L_test = mean(pred != test_label)
    bound = weighted.mean(L_val, rho)/(1-lambda/2)+
      (KL_div(rho, rep(1/m,m))+log((2*sqrt(n-r))/delta))/(lambda*(1-lambda/2)*(n-r))
    list(loss = L_test, bound = bound, time = time)
  }
    
  f <- function(num) {
    loss = c()
    bound = c()
    time = c()
    for (i in 1:num) {
      tmp = opt_rho(m = i,n = 200,r = 35,delta = 0.05)
      loss = c(loss, tmp$loss)
      bound = c(bound, tmp$bound)
      time = c(time, tmp$time)
    }
    list(loss = loss, bound = bound, time = time)
  }

  g <- function(num_exp, num_m) {
    loss = c()
    bound = c()
    time = c()
    for (i in 1:num_exp) {
      df = f(num_m)
      loss = rbind(loss, df$loss)
      bound = rbind(bound, df$bound)
      time = rbind(time, df$time)
    }
    mean_loss = apply(loss, 2, mean)
    sd_loss = apply(loss, 2, sd)
    mean_bound = apply(bound, 2, mean)
    sd_bound = apply(bound, 2, sd)
    mean_time = apply(time, 2, mean)
    sd_time = apply(time, 2, sd)
    list(loss = mean_loss+sd_loss, 
         bound = mean_bound+sd_bound,
         time = mean_time+sd_time)
  }
  
  df <- g(num_exp = 30, num_m = 24)
  
  par(mar = c(4, 4, 1, 4))
  plot(1:24, df$loss, type = "l", col = "black", ylim = c(0,0.8),
       xlab = "", ylab = "")
  mtext("m", side = 1, line = 2)
  mtext("Test loss", side = 2, line = 2)
  lines(1:24, rep(loss_base,24), type = "l", col = "red") 
  lines(1:24, df$bound, type = "l", col = "blue") 
  par(new = TRUE)
  plot(1:24, df$time, type = "l", xaxt = "n", yaxt = "n",
       ylab = "", xlab = "", col = "black", lty = 2, ylim = c(0,0.8))
  lines(1:24, rep(time_base,24), type = "l", lty = 2, col = "red")
  axis(side = 4)
  mtext("Runtime (s)", side = 4, line = 2)
  grid()
  legend("topright", 
         c("Their Method",
           "CV SVM",
           "Bound",
           expression(t[m]),
           expression(t[cv])),
         col = c("black", "red", "blue", "black", "red"),
         lty = c(1, 1, 1, 2, 2),
         cex = 0.8, inset = 0.02,
         y.intersp = 0.6,
         x.intersp = 0.6) 
  
  
  
  
  
  
  
    