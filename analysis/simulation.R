#################### Simulation

### Simple design from Freedman (Weighting Regressions by P Scores)
### We assume a treatment effect b which depends on W1: b = 1 if W1 > 0.75, b=-1 if W1 < 0.75
### Y = a + bT + c1W1 + c2W2 + dU is the response model
### T = I(e1 + f1W1 + f2W2 + V > 0) is the selection model for treatment when not randomized
### S = I(e2 + g1W1 + g2W2 + g3W3 + R > 0) is the model for selection into the RCT
### C = I(e3 + h2W2 + h3W3 + Q > 0) is the model for compliance
### U, V, R, T are N(0,1); U, V, R, T, (W1, W2, W3) are mutually independent


rm(list=ls())
library(Matching)
library(MASS)
library(randomForest)
library(rpart)

## Setup

sim_estimates <- function(sims = 100, e1= -1, e2 = 0.5, e3 = 1){
  # e1 controls number in the population who are eligible for treatment
  # e2 controls number eligible to be in RCT
  # e3 controls compliance

  # set up storage
  tpatt <- true_patt <- rct_sate <- tpatt_unadj <- rep(0, sims)
  rateC <- rateT <- rateS <- rep(0, sims)
  
  for(i in 1:sims){
    # Pick target sample size
    popsize <- 30000
    samplesize <- 5000
    rctsample <- sample(1:popsize, samplesize)
    observsample <- (1:popsize)[!(1:popsize %in% rctsample)]
    nrtsample <- sample(observsample, samplesize)
    
    # (U, V, R, Q, W1, W2, W3) are multivariate normal. Set parameters
    a <- c1 <- d <- 1
    c2 <- 2
    #  e1 <- -1
    #  e2 <-  0.5
    #  e3 <- 1
    f1 <- g2 <- 0.25
    f2 <- g3 <- 0.75
    g1 <- h2 <- h3 <- 0.5
    Sigma <- diag(rep(1,7))
    Sigma[5,5] <- 2
    Sigma[6,6] <- 1
    Sigma[7,7] <- 3
    Sigma[5,6] <- Sigma[6,5] <- 1
    Sigma[5,7] <- Sigma[7,5] <- Sigma[7,6] <- Sigma[6,7] <- 0.5
    mu <- c(0, 0, 0, 0, 0.5, 1, -1)
    # Data for the whole population
    var <- mvrnorm(popsize, mu=mu, Sigma=Sigma, empirical = F)
    U <- var[,1]
    V <- var[,2]
    R <- var[,3]
    Q <- var[,4]
    W1 <- var[,5]
    W2 <- var[,6]
    W3 <- var[,7]
    b <- ifelse(W1 > 0.75, 4, 1)
    T = as.numeric((e1 + f1*W1 + f2*W2 + V) > 0 )
    S = as.numeric(e2 + g1*W1 + g2*W2 + g3*W3 + R > 0) 
    T[S == 1] <- sample(c(0,1), sum(S==1), replace = TRUE)
    C <- as.numeric(e3 + h2*W2 + h3*W3 + Q > 0)
    D <- ifelse(C == 1, T, 0)
    
    Y <- a + b*D + c1*W1 + c2*W2 + d*U
    dat <- data.frame(Y, T, D, S, C, W1, W2, W3)
#    print(i)
#     if(i == 1){
#       cat("Compliance rate is approximately ", mean(C), "\n")
#       cat("Proportion eligible for RCT is approximately ", mean(S), "\n")
#       cat("Proportion eligible for treatment is approximately ", mean(X), "\n")
#     }
    rateC[i] <- mean(C)
    rateS[i] <- mean(S)
    rateT[i] <- mean(T)
    
    # Set up the RCT
    rct <- dat[rctsample,]
    rct <- rct[rct$S == 1,]
    
    # Set up the non-randomized trial. 
    nrt <- dat[nrtsample,]
    nrt <- nrt[nrt$S==0,]
    nrt <- nrt[,-2]; colnames(nrt)[2] <- "T"
    
    # Predict who is a complier in the control group (T=0) using W1, W2, W3
    #  suppressWarnings(complier_mod <- randomForest(C~W1+W2+W3, data = rct)) # estimate propensity of compliance
    complier_mod <- glm(C~W1+W2+W3, data = rct, family = "binomial")
    rct$C_pscore <- predict(complier_mod, rct, type = "response")
    rct$Chat <- rep(1, nrow(rct))
    rct$Chat[rct$D == 0] <- as.numeric(rct$C_pscore[rct$D == 0] >= 0.5)
    rct_compliers <- rct[rct$Chat == 1,]
    
    nrt$Chat <- rep(1, nrow(nrt))
    nrt$C_pscore <- predict(complier_mod, nrt, type = "response")
    nrt$Chat[nrt$T == 0] <- as.numeric(nrt$C_pscore[nrt$T == 0] >= 0.5)
    nrt_compliers <- nrt[nrt$Chat == 1,]
    
    # Fit a regression to the compliers in the RCT, use it to predict response in population "compliers"
    response_mod <- randomForest(Y~T + W1 + W2 + W3, data = rct_compliers)
    #response_mod <- lm(Y~T+W1+W2+W3, data = rct_compliers)
    nrt_tr_counterfactual <- cbind(nrt_compliers[,c("W1", "W2", "W3")], "T" = rep(1, nrow(nrt_compliers)))
    nrt_ctrl_counterfactual <- cbind(nrt_compliers[,c("W1", "W2", "W3")], "T" = rep(0, nrow(nrt_compliers)))
    nrt_compliers$Yhat_1 <- predict(response_mod, nrt_tr_counterfactual)
    nrt_compliers$Yhat_0 <- predict(response_mod, nrt_ctrl_counterfactual)
    
    
    
    # Compute the estimator
    #  term1 <- mean(nrt_compliers$C_pscore[nrt_compliers$T == 1]*nrt_compliers$Yhat_1[nrt_compliers$T==1])
    #  term2 <- mean(nrt_compliers$C_pscore[nrt_compliers$T == 1]*nrt_compliers$Yhat_0[nrt_compliers$T==1])
    term1 <- mean(nrt_compliers$Yhat_1[nrt_compliers$T==1])
    term2 <- mean(nrt_compliers$Yhat_0[nrt_compliers$T==1])
    tpatt[i] <- term1 - term2
    
    # Compare to other estimators
    true_patt[i] <- mean(b[T == 1 & S == 0])
    # SATE
    rct_sate[i] <- (mean(rct$Y[rct$T == 1]) - mean(rct$Y[rct$T==0]))/mean(rct$C[rct$T==1])
    # Hartman et al - doesn't account for noncompliance
    response_mod2 <- randomForest(Y~T + W1 + W2 + W3, data = rct)
    #response_mod2 <- lm(Y~T+W1+W2+W3, data = rct)
    nrt_tr_counterfactual <- cbind(nrt[,c("W1", "W2", "W3")], "T" = rep(1, nrow(nrt)))
    nrt_ctrl_counterfactual <- cbind(nrt[,c("W1", "W2", "W3")], "T" = rep(0, nrow(nrt)))
    nrt$Yhat_1 <- predict(response_mod2, nrt_tr_counterfactual)
    nrt$Yhat_0 <- predict(response_mod2, nrt_ctrl_counterfactual)
    term1 <- mean(nrt$Yhat_1[nrt$T==1])
    term2 <- mean(nrt$Yhat_0[nrt$T==1])
    tpatt_unadj[i] <- term1 - term2
    
  }
res <- cbind(true_patt, tpatt, tpatt_unadj, rct_sate, rateC, rateS, rateT)
return(res)
}

# res1 <- sim_estimates(10)
# sapply(2:4, function(x) mean((res1[,1]-res1[,x])^2))
# 
# res2 <- sim_estimates(10, e3 = 2)
# sapply(2:4, function(x) mean((res2[,1]-res2[,x])^2))
# 
# res3 <- sim_estimates(10, e2 = -0.5)
# sapply(2:4, function(x) mean((res3[,1]-res3[,x])^2))
# 
# res4 <- sim_estimates(10, e1 = -2, e3 = 2)
# sapply(2:4, function(x) mean((res4[,1]-res4[,x])^2))

e <- seq(-2, 2, by = 1)
#e <- 1:2
e <- expand.grid(e,e,e)
res <- t(sapply(1:nrow(e), function(x){print(x);sim_estimates(1,e[x,1],e[x,2],e[x,3])}))
colnames(res) <- c("true_patt","tpatt","tpatt_unadj","rct_sate","rateC","rateS","rateX")
mse <- sapply(2:4, function(x) ((res[,1]-res[,x])^2))
colnames(mse) <- c("mse_tpatt", "mse_tpatt_unadj", "mse_rct_sate")
res <- cbind(res, mse)
res <- as.data.frame(res)

library(ggplot2)
p1 <- ggplot(res, aes(as.factor(round(rateC,1)), as.factor(round(rateT,1)))) + geom_tile(aes(fill = mse_tpatt),     colour = "yellow")+ scale_fill_gradient(low = "yellow", high = "red")
p1
p2 <- ggplot(res, aes(as.factor(round(rateC,1)), as.factor(round(rateS,1)))) + geom_tile(aes(fill = mse_tpatt),     colour = "yellow", size = 3)+ scale_fill_gradient(low = "yellow", high = "red")
p2
p3 <- ggplot(res, aes(as.factor(round(rateT,1)), as.factor(round(rateS,1)))) + geom_tile(aes(fill = mse_tpatt),     colour = "yellow", size = 3)+ scale_fill_gradient(low = "yellow", high = "red")
p3
good <- mse[,1]<mse[,2]