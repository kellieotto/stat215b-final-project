## Estimate conditional expectation of responses in RCT 
## Then, use response model to estimate population members' outcomes given their covariates.
## These estimates will be used to estimate the PATT.

# Libraries

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

# Create dfs for outcomes
Y.ohie <- data.frame("any.visit"=any.visit[as.numeric(rownames(X.ohie))], # remove rows with missing predictors
                    "num.visit"=num.visit[as.numeric(rownames(X.ohie))],
                    "any.hosp"=any.hosp[as.numeric(rownames(X.ohie))], 
                    "any.out"=any.out[as.numeric(rownames(X.ohie))],
                    "num.out"=num.out[as.numeric(rownames(X.ohie))]) 

Y.nhis <- data.frame("any.visit"=nhis.any.visit[as.numeric(rownames(X.nhis))], # remove rows with missing predictors
                     "num.visit"=nhis.num.visit[as.numeric(rownames(X.nhis))],
                     "any.hosp"=nhis.any.hosp[as.numeric(rownames(X.nhis))], 
                     "any.out"=nhis.any.out[as.numeric(rownames(X.nhis))],
                     "num.out"=nhis.num.out[as.numeric(rownames(X.nhis))]) 

# Predict who in the controls would have accepted treatment had they been assigned by fitting 
# model P(accept treatment | covariates) to the people randomly assigned to treatment
complier.mod <- randomForest(x=X.ohie,
                             y=insurance[as.numeric(rownames(X.ohie))])
rct.compliers <- data.frame("C.pscore"=complier.mod$predicted,
                            "C.hat"=ifelse(complier.mod$predicted>=0.5,1,0))

nrt$Chat <- rep(1, nrow(nrt))
nrt$C_pscore <- predict(complier_mod, nrt, type = "response")
nrt$Chat[nrt$Tt == 0] <- as.numeric(nrt$C_pscore[nrt$Tt == 0] >= 0.5)
nrt_compliers <- nrt[nrt$Chat == 1,]