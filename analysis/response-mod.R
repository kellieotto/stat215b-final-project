## Run this script on SCF

# Set WD
setwd("~/Documents/stat215b-final-project")

# Load R workspace
load("analysis.RData")

# Source superlearner scripts to ensure libraries attached
source("SuperLearner.R")

# Predict who is a complier in the control group
set.seed(42)
response.mod <- lapply(y.col, SuperLearner(Y=Y.ohie.response[,i], 
                             X=X.ohie.response, 
                             SL.library=SL.library.class,
                             family="binomial"))
names(response.mod) <- colnames(Y.ohie.response) # name each element of list

summary(response.mod) # summarize

# Use response model to estimate potential outcomes for population "compliers" on medicaid
nrt.tr.counterfactual <- cbind("treatment" = rep(1, length(which(insurance.nhis==1))),
                               X.nhis[which(insurance.nhis==1),])
nrt.ctrl.counterfactual <- cbind("treatment" = rep(0, length(which(insurance.nhis==1))),
                                 X.nhis[which(insurance.nhis==1),])

Y.hat.1 <- lapply(y.col, function (i) predict(response.mod[[i]], nrt.tr.counterfactual))
Y.hat.0 <- lapply(y.col, function (i) predict(response.mod[[i]], nrt.ctrl.counterfactual))

# Output predictions as .txt file
write.table(cbind(Y.hat.1,Y.hat.0), "response-mod-pred.txt",  row.names=FALSE, col.names=FALSE)