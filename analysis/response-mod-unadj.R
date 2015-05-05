## Run this script on SCF

# Set WD
setwd("~/Documents/stat215b-final-project")

# Load R workspace
load("analysis.RData")

# Source superlearner scripts to ensure libraries attached
source("SuperLearner.R")

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

Y.hat.1.unadj <- lapply(y.col, function (i) predict(response.mod2[[i]], nrt.tr.counterfactual.unadj))
Y.hat.0.unadj <- lapply(y.col, function (i) predict(response.mod2[[i]], nrt.ctrl.counterfactual.unadj))

# Output predictions as .txt file
lapply(y.col, function (i) write.table(cbind(Y.hat.1.unadj[[i]],Y.hat.0.unadj[[i]]), 
                                       paste("Y.hat.unadj-",names(response.mod2)[i], ".txt"),  row.names=FALSE))

# Compute unadjusted SATT
rct.tr.counterfactual.unadj <- cbind("treatment" = rep(1, length(which(insurance.ohie==1 | insurance.ohie==0))),
                                     X.ohie[which(insurance.ohie==1| insurance.ohie==0),])
rct.ctrl.counterfactual.unadj <- cbind("treatment" = rep(0, length(which(insurance.ohie==1 | insurance.ohie==0))),
                                       X.ohie[which(insurance.ohie==1 | insurance.ohie==0),])

Y.hat.1.unadj.rct <- lapply(y.col, function (i) predict(response.mod2[[i]], rct.tr.counterfactual.unadj))
Y.hat.0.unadj.rct <- lapply(y.col, function (i) predict(response.mod2[[i]], rct.ctrl.counterfactual.unadj))

# Output predictions as .txt file
lapply(y.col, function (i) write.table(cbind(Y.hat.1.unadj.rct[[i]],Y.hat.0.unadj.rct[[i]]), 
                                       paste("Y.hat.unadj.rct-",names(response.mod2)[i], ".txt"),  row.names=FALSE))
