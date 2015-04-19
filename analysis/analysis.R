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
X.nhis <- suppressWarnings(na.omit(data.frame(n.hh.nhis, # need to omit rows containing any NA
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
                             income.nhis)))

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

# Predict who is a complier in RCT control group (X=0) using common features
#  suppressWarnings(complier_mod <- randomForest(C~W1+W2+W3, data = rct)) # estimate propensity of compliance

rct$C_pscore <- predict(complier_mod, rct, type = "response")
rct$Chat <- rep(1, nrow(rct))
rct$Chat[rct$Xobs == 0] <- as.numeric(rct$C_pscore[rct$Xobs == 0] >= 0.5)
rct_compliers <- rct[rct$Chat == 1,]