## Estimate conditional expectation of responses in RCT 
## Then, use response model to estimate population members' outcomes given their covariates.
## These estimates will be used to estimate the PATT.

# Libraries

# Define directory for analysis 
directory <- "~/Dropbox/github/stat215b-final-project/analysis"

# Source scripts
source(file.path(directory,"prepare-analysis.R"))
source(file.path(directory,"SuperLearner.R"))

## RCT: Create dfs for common features and responses

Y.ohie <- na.omit(data.frame("any.visit"=any.visit, # remove missing rows
                             "num.visit"=num.visit,
                             "any.out"=any.out,
                             "num.out"=num.out)) 

X.ohie <-   na.omit(data.frame(n.hh,  # remove missing rows
                     gender, 
                     age.20to49,
                     age.50to64,
                     white,
                     black,
                     hisp,
                     diabetes,
                     asthma,
                     bp,
                     heart,
                     education,
                     income))

# Impute missing feature values using proximity from randomForest
y.col <- 1:ncol(Y.ohie) # number of responses
set.seed(42)
X.ohie.response <- lapply(y.col, rfImpute(X.ohie[as.numeric(rownames(Y.ohie)),], # feature rows match response rows
                                           y=Y.ohie[,y.col]))

set.seed(42)
X.ohie.insurance <- rfImpute(X.ohie, # impute missing feature values using proximity from randomForest
                             factor(insurance)) 
                             
# NRT: Create dfs for common features and reponses

X.nhis <-   na.omit(data.frame(n.hh.nhis, # need to omit rows containing any NA
                             gender.nhis, 
                             "age.20to49"=age.20to49.nhis,
                             "age.50to64"=age.50to64.nhis,
                             "white"=white.nhis,
                             "black"=black.nhis,
                             "hisp"=hisp.nhis,
                             "diabetes"=diabetes.nhis,
                             "asthma"=asthma.nhis,
                             "bp"=bp.nhis,
                             "heart"=heart.nhis,
                             education.nhis,
                             income.nhis))

Y.nhis <- data.frame("any.visit"=nhis.any.visit[as.numeric(rownames(X.nhis))], # remove rows with missing predictors
                     "num.visit"=nhis.num.visit[as.numeric(rownames(X.nhis))],
                     "any.out"=nhis.any.out[as.numeric(rownames(X.nhis))],
                     "num.out"=nhis.num.out[as.numeric(rownames(X.nhis))]) 

# Predict who is a complier in RCT and NRT
run <- FALSE
if(run){
  set.seed(42)
  complier.mod <- SuperLearner(Y=insurance[as.numeric(rownames(X.ohie))], # estimate propensity of compliance
                               X=X.ohie, 
                               SL.library=SL.library.class,
                               family=binomial(), # glmnet response is 2-level factor
                               method="method.NNLS",
                               cvControl=list(stratifyCV=TRUE))
  C.pscore <- complier.mod$SL.predict   # Store predictions
}
complier.mod <- suppressWarnings(randomForest(X.ohie.insurance,
                                              insurance)) # use rf regression for now
rct.compliers <- data.frame("treatment"=treatment[as.numeric(rownames(X.ohie))],
                            "insurance"=insurance[as.numeric(rownames(X.ohie))],
                            "C.pscore"=complier.mod$predicted, # change to complier.mod$SL.predict
                            "C.hat"=ifelse(complier.mod$predicted>=0.5,1,0),
                            "complier"=0)
rct.compliers$complier[rct.compliers$treatment==1 & rct.compliers$insurance==1] <- 1 # true compliers in the treatment group
rct.compliers$complier[rct.compliers$treatment==0 & rct.compliers$C.hat==1] <- 1 # predicted compliers from the control group

nrt.compliers <- data.frame("C.pscore"=predict(complier.mod, X.ohie, type = "response"),
                            "C.hat"=ifelse(complier.mod$predicted>=0.5,1,0))

# Fit a regression to the compliers in the RCT, use it to predict response in population "compliers"
response.mod <- lapply(y.col, randomForest, x=cbind(rct.compliers$treatment[rct.compliers$complier==1],
                                                    X.ohie.response[rct.compliers$complier==1,]),
                                                    y=Y.ohie[as.numeric(rownames(X.ohie[rct.compliers$complier==1,])),y.col])

response_mod <- randomForest(Y~Tt + W1 + W2 + W3, data = rct_compliers)
nrt_tr_counterfactual <- cbind(nrt_compliers[,c("W1", "W2", "W3")], "Tt" = rep(1, nrow(nrt_compliers)))
nrt_ctrl_counterfactual <- cbind(nrt_compliers[,c("W1", "W2", "W3")], "Tt" = rep(0, nrow(nrt_compliers)))
nrt_compliers$Yhat_1 <- predict(response_mod, nrt_tr_counterfactual)
nrt_compliers$Yhat_0 <- predict(response_mod, nrt_ctrl_counterfactual)