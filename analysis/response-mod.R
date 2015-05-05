## Run this script on SCF

# Set WD
setwd("~/Documents/stat215b-final-project")

# Load R workspace
load("analysis.RData")

# Source superlearner scripts to ensure libraries attached
source("SuperLearner.R")

# Fit a regression to the compliers in the RCT
y.col <- 1:ncol(Y.ohie) # number of responses
Y.ohie.response <- Y.ohie[which(rct.compliers$complier==1),]
X.ohie.response <- data.frame("treatment"=treatment.ohie[which(rct.compliers$complier==1)],
                              X.ohie[which(rct.compliers$complier==1),])
# Run response model
set.seed(42)
response.mod <- lapply(y.col, function (i) SuperLearner(Y=Y.ohie.response[,i], 
                                                        X=X.ohie.response, 
                                                        SL.library=SL.library.class,
                                                        family="binomial"))

names(response.mod) <- colnames(Y.ohie.response) # name each element of list

response.mod # summarize

# Use response model to estimate potential outcomes for population "compliers" on medicaid
nrt.tr.counterfactual <- cbind("treatment" = rep(1, length(which(insurance.nhis==1))),
                               X.nhis[which(insurance.nhis==1),])
nrt.ctrl.counterfactual <- cbind("treatment" = rep(0, length(which(insurance.nhis==1))),
                                 X.nhis[which(insurance.nhis==1),])

Y.hat.1 <- lapply(y.col, function (i) predict(response.mod[[i]], nrt.tr.counterfactual)$pred) # extract only SL predictions
Y.hat.0 <- lapply(y.col, function (i) predict(response.mod[[i]], nrt.ctrl.counterfactual)$pred)

# Compute PATT estimator
t.patt <- lapply(y.col, function (i) mean(Y.hat.1[[i]]) - mean(Y.hat.0[[i]]))

# Compute unadjusted PATT
Y.ohie.response.unadj <- Y.ohie[which(rct.compliers$complier==1 | rct.compliers$complier==0),]
X.ohie.response.unadj <- data.frame("treatment"=treatment.ohie,
                                    X.ohie)
set.seed(42)
response.mod2 <- lapply(y.col, function (i) SuperLearner(Y=Y.ohie.response.unadj[,i], 
                                                         X=X.ohie.response.unadj, 
                                                         SL.library=SL.library.class,
                                                         family="binomial"))

names(response.mod2) <- colnames(Y.ohie) # name each element of list

response.mod2 # summarize

nrt.tr.counterfactual.unadj <- cbind("treatment" = rep(1, length(which(insurance.nhis==1 | insurance.nhis==0))),
                                     X.nhis[which(insurance.nhis==1| insurance.nhis==0),])
nrt.ctrl.counterfactual.unadj <- cbind("treatment" = rep(0, length(which(insurance.nhis==1 | insurance.nhis==0))),
                                       X.nhis[which(insurance.nhis==1 | insurance.nhis==0),])

Y.hat.1.unadj <- lapply(y.col, function (i) predict(response.mod2[[i]], nrt.tr.counterfactual.unadj)$pred)
Y.hat.0.unadj <- lapply(y.col, function (i) predict(response.mod2[[i]], nrt.ctrl.counterfactual.unadj)$pred)

t.patt.unadj <- lapply(y.col, function (i) mean(Y.hat.1.unadj[[i]]) - mean(Y.hat.0.unadj[[i]]))

# Compute unadjusted SATT
rct.tr.counterfactual.unadj <- cbind("treatment" = rep(1, length(which(insurance.ohie==1 | insurance.ohie==0))),
                                     X.ohie[which(insurance.ohie==1| insurance.ohie==0),])
rct.ctrl.counterfactual.unadj <- cbind("treatment" = rep(0, length(which(insurance.ohie==1 | insurance.ohie==0))),
                                       X.ohie[which(insurance.ohie==1 | insurance.ohie==0),])

Y.hat.1.unadj.rct <- lapply(y.col, function (i) predict(response.mod2[[i]], rct.tr.counterfactual.unadj)$pred)
Y.hat.0.unadj.rct <- lapply(y.col, function (i) predict(response.mod2[[i]], rct.ctrl.counterfactual.unadj)$pred)

t.satt.unadj <- lapply(y.col, function (i) mean(Y.hat.1.unadj.rct[[i]]) - mean(Y.hat.0.unadj.rct[[i]]))

# Compute SATE
rct.sate <- lapply(y.col, function (i) (mean(Y.ohie[[i]][which(treatment.ohie==1)]) - # Num. is ITT effect
                                          mean(Y.ohie[[i]][which(treatment.ohie==0)])) 
                   /mean(rct.compliers$complier[which(treatment.ohie==1)])) # Denom. is true RCT compliance rate

# Save workspace
save.image(file.path(directory,"analysis-pred.Rdata"))
