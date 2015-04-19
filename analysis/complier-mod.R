## Run this script on SCF

# Load R workspace
load("~/analysis.RData")

# Predict who is a complier in the control group
set.seed(42)
complier.mod <- SuperLearner(Y=Y.insurance, # estimate propensity of compliance
                             X=X.ohie, 
                             SL.library=SL.library.class,
                             family=binomial(), # glmnet response is 2-level factor
                             method="method.NNLS",
                             cvControl=list(stratifyCV=TRUE))

# Store predictions
C.pscore <- complier.mod$SL.predict
# Output predictions as .txt file
write.table(C.pscore, "C.pscore.txt",  row.names=FALSE, col.names=FALSE)