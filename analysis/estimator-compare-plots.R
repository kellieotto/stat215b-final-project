# Create plots for each outcome variable comparing SATT vs. PATT overall and by covariate 

# Libraries
library(ggplot2)
library(reporttools)

# Calculate differences in potential outcomes for population treated compliers
nrt.pred <- lapply(y.col, function (i) data.frame("tau"=Y.hat.1[[i]]-Y.hat.0[[i]],
                                                  X.nhis[which(insurance.nhis==1),]))
                                                   
# Estimate PATT for each covariate group
covs <- colnames(X.nhis)[4:40] # exclude HH dummies

patt.het <- lapply(y.col, function (i) lapply(covs, function(x) mean(nrt.pred[[i]]$tau[nrt.pred[[i]][x]==1]) - 
                                             mean(nrt.pred[[i]]$tau[nrt.pred[[i]][x]==0]))) # heterogenous treatment effect on population treated compliers

# For comparison, calculate differences in potential outcomes for population treated 
nrt.pred.unadj <- lapply(y.col, function (i) data.frame("tau"=Y.hat.1.unadj[[i]]-Y.hat.0.unadj[[i]],
                                                        X.nhis[which(insurance.nhis==1 | insurance.nhis==0),]))

# Estimate unadjusted PATT for each covariate group
patt.unadj.het <- lapply(y.col, function (i) lapply(covs, function(x) mean(nrt.pred.unadj[[i]]$tau[nrt.pred.unadj[[i]][x]==1]) - 
                                                mean(nrt.pred.unadj[[i]]$tau[nrt.pred.unadj[[i]][x]==0])))  

# Estimate SATE for each covariate group
sate.het <- lapply(y.col, function (i) lapply(covs, function(x) (mean(Y.ohie[[i]][which(treatment.ohie==1)][X.ohie.response[x]==1]) - 
                                                                   mean(Y.ohie[[i]][which(treatment.ohie==0)][X.ohie.response[x]==1])) 
                                              /mean(rct.compliers$complier[which(treatment.ohie==1)][X.ohie.response[x]==1]))) # heterogenous treatment effect on sample treated compliers

# Estimate unadjusted SATT for each covariate group
rct.pred.unadj <- lapply(y.col, function (i) data.frame("tau"=Y.hat.1.unadj.rct[[i]]-Y.hat.0.unadj.rct[[i]],
                                                        X.ohie[which(insurance.ohie==1 | insurance.ohie==0),]))

satt.unadj.het <- lapply(y.col, function (i) lapply(covs, function(x) mean(rct.pred.unadj[[i]]$tau[rct.pred.unadj[[i]][x]==1]) - 
                                                      mean(rct.pred.unadj[[i]]$tau[rct.pred.unadj[[i]][x]==0])))  

# Create data for plot
Overall  <- c("Overall")
Sex <- c("Male", "Female")
Age <- c("20-49", "50-64")
Race.ethn <- c("White", "Black", "Hispanic")
Health.stat <- c( "Diabetes", "Asthma","High blood pressure", "Heart Condition")
Education <- c("Less than high school","High school disploma or GED",
               "Vocational training or 2-year degree","4-year college degree or more")
Income <- c("0","1-2500","2501-5000","5001-7500","7501-10000",
            "10001-12500","12501-15000","15001-17500","17501-20000","20001-22500","22501-25000","25001-27500","27501-30000",
            "30001-32500","32501-35000","35001-37500","37501-40000","400001-42500","42501-45000","45001-47500","47501-50000",">50000")
cov.groups <- c("Overall","Sex","Age","Race","Health status","Education","Income") 
cov.names <- c(Overall,Sex,Age,Race.ethn,Health.stat,Education,Income)

het.plot <- lapply(y.col, function (i) data.frame(x=factor(c(rep(cov.names,3)), levels=rev(cov.names)), # reverse order
                                     y = c(t.patt[[i]],unlist(patt.het[[i]]),
                                           t.patt.unadj[[i]],unlist(patt.unadj.het[[i]]),
                                           rct.sate[[i]],unlist(sate.het[i])), 
                                     Group = factor(rep(c(cov.groups[1],rep(cov.groups[2],length(Sex)),rep(cov.groups[3],length(Age)),
                                               rep(cov.groups[4],length(Race.ethn)),rep(cov.groups[5],length(Health.stat)),
                                               rep(cov.groups[6],length(Education)),rep(cov.groups[7],length(Income))),3), levels=cov.groups),
                                     Estimator= factor(c(rep("PATT (adjusted)",length(covs)+1),
                                                  rep("PATT (unadjusted)",length(covs)+1),
                                                  rep("SATE (adjusted)",length(covs)+1)))))
# Plot forest plot
het.plot.all <- lapply(y.col, function (i) 
                         ggplot(het.plot[[i]][het.plot[[i]]$Group!="Income",], aes(x=x, y=y,colour=Estimator)) +  # exclude income groups
                         geom_point(size=6,alpha=0.4) + 
                         scale_colour_manual(values=c("red","blue","green")) + # change colors for estimators
                         coord_flip() +
                         geom_line() +
                         geom_hline(aes(x=0), lty=2) +
                     #    facet_grid(Group~.) +
                         ylab("Treatment effect") +
                         xlab("")) #switch because of the coord_flip() above 

any.visit.plot<- het.plot.all[[1]] # any.visit
num.visit.plot <- het.plot.all[[2]] # num.visit
any.out.plot <- het.plot.all[[3]] # any.out
num.out.plot <- het.plot.all[[4]] # num.out

# Create table similar to Table 1 of Hartman et al. 
latex <- FALSE
if() {
  rct.nrt.tab <- rbind(cbind(study="OHIE",X.ohie.response,
                             Y.ohie.response[c("any.visit","any.out")]),
                       cbind(study="NHIS",nrt.tr.counterfactual,
                             Y.nhis[c("any.visit","any.out")][which(insurance.nhis==1),])) # create data for table
  rct.nrt.tab$group <- NA
  rct.nrt.tab$group[rct.nrt.tab$study=="OHIE" & rct.nrt.tab$treatment==0] <- 1
  rct.nrt.tab$group[rct.nrt.tab$study=="OHIE" & rct.nrt.tab$treatment==1] <- 2
  rct.nrt.tab$group[rct.nrt.tab$study=="NHIS" & rct.nrt.tab$treatment==1] <- 3
    
  tableNominal(vars = rct.nrt.tab, 
               group = rct.nrt.tab$group, 
         #      nams=c(cov.names[-1],"Any ER visit","Any primary care visit"),
               vertical=FALSE,
               prec = 3,cumsum=FALSE,lab = "rct-nrt-compare",
               cap="Pretreatment covariates and responses for the OHIE and for NHIS respondents who received Medicaid.") # RCT vs. NRT compliers
}


