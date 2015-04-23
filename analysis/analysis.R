## Estimate conditional expectation of responses in RCT 
## Then, use response model to estimate population members' outcomes given their covariates.
## These estimates will be used to estimate the PATT.

# Define directory for analysis 
directory <- "~/Dropbox/github/stat215b-final-project/analysis"

# Source scripts
source(file.path(directory,"prepare-analysis.R"))
#source(file.path(directory,"SuperLearner.R"))
library(randomForest)

# Create dfs containing common features for RCT and observational study
X.ohie <- na.omit(data.frame(n.hh,  # need to omit rows containing any NA
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

# Create vectors for treatment and compliance 
treatment.ohie <- treatment[as.numeric(rownames(X.ohie))]

insurance.ohie <- insurance[as.numeric(rownames(X.ohie))]
insurance.nhis <- medicaid[as.numeric(rownames(X.nhis))]

# Create dfs for outcomes 
Y.ohie <- na.omit(data.frame("any.visit"=any.visit, # need to omit rows containing any NA
                    "num.visit"=num.visit,
                  #  "any.hosp"=any.hosp,
                  #  "num.hosp"=num.hosp,
                    "any.out"=any.out,
                    "num.out"=num.out))

Y.nhis <- na.omit(data.frame("any.visit"=nhis.any.visit, # need to omit rows containing any NA
                     "num.visit"=nhis.num.visit,
                #     "any.hosp"=nhis.any.hosp,
                     "any.out"=nhis.any.out,
                     "num.out"=nhis.num.out))

# Train compliance model on RCT treated. Use model to predict P(insurance == 1|covariates) on controls. 
complier.mod <- suppressWarnings(randomForest(x=X.ohie[treatment.ohie == 1,], 
                                              y=insurance.ohie[treatment.ohie==1])) 
rct.compliers <- data.frame("treatment"=treatment.ohie,
                            "insurance"=insurance.ohie,
                            "C.pscore"=predict(complier.mod, X.ohie), 
                            "C.hat"=ifelse(predict(complier.mod, X.ohie)>=0.5,1,0),
                            "complier"=0)
rct.compliers$complier[rct.compliers$treatment==1 & rct.compliers$insurance==1] <- 1 # true compliers in the treatment group
rct.compliers$complier[rct.compliers$treatment==0 & rct.compliers$C.hat==1] <- 1 # predicted compliers from the control group

# Predict who is a complier in NRT
nrt.compliers <- data.frame("C.pscore"=predict(complier.mod, X.nhis),
                            "C.hat"=ifelse(predict(complier.mod, X.nhis)>=0.5,1,0))

# Fit a regression to the compliers in the RCT
y.col <- 1:ncol(Y.ohie) # number of responses
Y.ohie.response <- Y.ohie[which(rct.compliers$complier==1),]
X.ohie.response <- data.frame("treatment"=rct.compliers$treatment[which(rct.compliers$complier==1)],
                         X.ohie[which(rct.compliers$complier==1),])
response.mod <- lapply(y.col, function(i) randomForest(x=X.ohie.response,
                                                    y=Y.ohie.response[,i]))
names(response.mod) <- colnames(Y.ohie.response) # name each element of list

# Use response model to estimate potential outcomes for population "compliers" on medicaid
nrt.tr.counterfactual <- cbind("treatment" = rep(1, length(which(insurance.nhis==1))),
                               X.nhis[which(insurance.nhis==1),])
nrt.ctrl.counterfactual <- cbind("treatment" = rep(0, length(which(insurance.nhis==1))),
                                 X.nhis[which(insurance.nhis==1),])

Yhat.1 <- lapply(y.col, function (i) predict(response.mod[[i]], nrt.tr.counterfactual))
Yhat.0 <- lapply(y.col, function (i) predict(response.mod[[i]], nrt.ctrl.counterfactual))

# Compute the estimator
term1 <- lapply(y.col, function (i) mean(Yhat.1[[i]]))
term2 <- lapply(y.col, function (i) mean(Yhat.0[[i]]))
tpatt <- lapply(y.col, function (i) term1[[i]] - term2[[i]])

# Compute SATE for comparison
rct.sate <- lapply(y.col, function (i) (mean(Y.ohie[[i]][which(treatment.ohie==1)]) - # Num. is ITT effect
                                             mean(Y.ohie[[i]][which(treatment.ohie==0)])) 
                   /mean(rct.compliers$complier[which(rct.compliers$treatment==1)])) # Denom. is true RCT compliance rate

