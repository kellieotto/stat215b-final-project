## Estimate conditional expectation of responses in RCT 
## Then, use response model to estimate population members' outcomes given their covariates.
## These estimates will be used to estimate the PATT.

# Define directory for analysis 
directory <- "~/Dropbox/github/stat215b-final-project/analysis"

# Source scripts
source(file.path(directory,"prepare-analysis.R"))
source(file.path(directory,"SuperLearner.R"))

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

# Create dfs for outcomes 
Y.ohie <- na.omit(data.frame("any.visit"=any.visit, # match response rows with predictors
                    "num.visit"=num.visit,
                    "any.out"=any.out,
                    "num.out"=num.out))

Y.nhis <- na.omit(data.frame("any.visit"=nhis.any.visit, # match response rows with predictors
                     "num.visit"=nhis.num.visit,
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

save(complier.mod, rct.compliers, file = "complier-mod-rf.RData") # save .Rdata

# Predict who is a complier in RCT
nrt.compliers <- data.frame("C.pscore"=predict(complier.mod, X.nhis),
                            "C.hat"=ifelse(predict(complier.mod, X.nhis)>=0.5,1,0))

# Fit a regression to the compliers in the RCT, use it to predict response in population "compliers"
y.col <- 1:ncol(Y.ohie) # number of responses
Y.ohie.response <- Y.ohie[which(rct.compliers$complier==1),]
X.ohie.response <- data.frame("treatment"=rct.compliers$treatment[which(rct.compliers$complier==1)],
                         X.ohie[which(rct.compliers$complier==1),])
response.mod <- lapply(y.col, function(i) randomForest(x=X.ohie.response,
                                                    y=Y.ohie.response[,i]))
names(response.mod) <- colnames(Y.ohie.response) # name each element of list

nrt_tr_counterfactual <- cbind(nrt_compliers[,c("W1", "W2", "W3")], "Tt" = rep(1, nrow(nrt_compliers)))
nrt_ctrl_counterfactual <- cbind(nrt_compliers[,c("W1", "W2", "W3")], "Tt" = rep(0, nrow(nrt_compliers)))
nrt_compliers$Yhat_1 <- predict(response_mod, nrt_tr_counterfactual)
nrt_compliers$Yhat_0 <- predict(response_mod, nrt_ctrl_counterfactual)