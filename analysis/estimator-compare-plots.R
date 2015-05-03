# Create plots for each outcome variable comparing SATT vs. PATT overall and by covariate 

# Libraries
library(ggplot2)
library(reporttools)
library(randomForest)

het.effects <- function(boot = FALSE){
  if(boot==TRUE){
    boot.nrt <- sample(1:sum(insurance.nhis==1,na.rm=T), sum(insurance.nhis==1,na.rm=T), replace = T)
#     insurance.nhis <- insurance.nhis[boot.nrt]
#     X.nhis_boot <- X.nhis[boot.nrt,][which(insurance.nhis==1),]
#     X.nhis_unadjboot <- X.nhis[boot.nrt,][which(insurance.nhis==1 | insurance.nhis == 0),]
#     nrt.tr.counterfactual <- cbind("treatment" = rep(1, length(which(insurance.nhis==1))),
#                                    X.nhis_boot)
#     nrt.ctrl.counterfactual <- cbind("treatment" = rep(0, length(which(insurance.nhis==1))),
#                                      X.nhis_boot)
#     Y.hat.1 <- lapply(y.col, function (i) predict(response.mod[[i]], nrt.tr.counterfactual))
#     Y.hat.0 <- lapply(y.col, function (i) predict(response.mod[[i]], nrt.ctrl.counterfactual))
#     nrt.tr.counterfactual.unadj <- cbind("treatment" = rep(1, length(which(insurance.nhis==1 | insurance.nhis==0))), X.nhis[which(insurance.nhis==1| insurance.nhis==0),])
#     nrt.ctrl.counterfactual.unadj <- cbind("treatment" = rep(0, length(which(insurance.nhis==1 | insurance.nhis==0))), X.nhis[which(insurance.nhis==1 | insurance.nhis==0),])
#     Y.hat.1.unadj <- lapply(y.col, function (i) predict(response.mod2[[i]], nrt.tr.counterfactual.unadj))
#     Y.hat.0.unadj <- lapply(y.col, function (i) predict(response.mod2[[i]], nrt.ctrl.counterfactual.unadj))
#     
    
    Y.hat.1 <- lapply(y.col, function(i) Y.hat.1[[i]][boot.nrt])
    Y.hat.0 <- lapply(y.col, function(i) Y.hat.0[[i]][boot.nrt])
    X.nhis_boot <- X.nhis[which(insurance.nhis==1),][boot.nrt,]
    
    boot.nrt.unadj <- sample(1:sum(insurance.nhis==1 | insurance.nhis==0,na.rm=T), sum(insurance.nhis==1 | insurance.nhis==0,na.rm=T), replace = T)
    Y.hat.1.unadj <- lapply(y.col, function(i) Y.hat.1.unadj[[i]][boot.nrt.unadj])
    Y.hat.0.unadj <- lapply(y.col, function(i) Y.hat.0.unadj[[i]][boot.nrt.unadj])
    X.nhis_unadjboot <- X.nhis[which(insurance.nhis==1 | insurance.nhis == 0),][boot.nrt.unadj,]
    
    boot.rct <- sample(1:nrow(X.ohie), nrow(X.ohie), replace = T)
    treatment.ohie <- treatment.ohie[boot.rct]
    insurance.ohie <- insurance.ohie[boot.rct]
    Y.ohie <- Y.ohie[boot.rct,]
    Y.hat.1.unadj.rct <- lapply(y.col, function(i) Y.hat.1.unadj.rct[[i]][boot.rct])
    Y.hat.0.unadj.rct <- lapply(y.col, function(i) Y.hat.0.unadj.rct[[i]][boot.rct])
    X.ohie <- X.ohie[boot.rct,]
    rct.compliers <- rct.compliers[boot.rct,]
    X.ohie.response <- data.frame("treatment"=treatment.ohie[which(rct.compliers$complier==1)],
                                  X.ohie[which(rct.compliers$complier==1),])
  }else{
    X.nhis_boot      <- X.nhis[which(insurance.nhis==1),]
    X.nhis_unadjboot <- X.nhis[which(insurance.nhis==1 | insurance.nhis==0),]
  }
  
  
  
  # Calculate differences in potential outcomes for population treated compliers
  nrt.pred <- lapply(y.col, function (i) data.frame("tau"=Y.hat.1[[i]]-Y.hat.0[[i]], 
                                                    X.nhis_boot))
  
  # Estimate PATT for each covariate group
  covs <- colnames(X.nhis)[4:40] # exclude HH dummies
  
  patt.het <- lapply(y.col, function (i) lapply(covs, function(x) mean(nrt.pred[[i]]$tau[nrt.pred[[i]][x]==1]) - 
                                                  mean(nrt.pred[[i]]$tau[nrt.pred[[i]][x]==0]))) # heterogenous treatment effect on population treated compliers
  
  # For comparison, calculate differences in potential outcomes for population treated 
  nrt.pred.unadj <- lapply(y.col, function (i) data.frame("tau"=Y.hat.1.unadj[[i]]-Y.hat.0.unadj[[i]],
                                                          X.nhis_unadjboot))
  
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
  
return(list(patt.het, patt.unadj.het, sate.het, satt.unadj.het))
}

true_effect <- het.effects() # a list where true_effects[[1]] is patt.het, true_effects[[2]] is patt.unadj.het, etc
patt.het <- true_effect[[1]]
patt.unadj.het <- true_effect[[2]]
sate.het <- true_effect[[3]]
covs <- colnames(X.nhis)[4:40]
# boot_effect <- replicate(5, het.effects(boot=T))
# boot_effect <- lapply(1:ncol(boot_effect), function(b) lapply(boot_effect[,b], unlist))
# patt.het.bootdist <- do.call(cbind, lapply(boot_effect, "[[", 1)) # rows are for each het. effect
# patt.unadj.het.bootdist <- do.call(cbind, lapply(boot_effect, "[[", 2)) # rows are for each het. effect
# sate.het.bootdist <- do.call(cbind, lapply(boot_effect, "[[", 3)) # rows are for each het. effect
# satt.het.bootdist <- do.call(cbind, lapply(boot_effect, "[[", 4)) # rows are for each het. effect
# 

B <- 50
boot_effect <- replicate(B, het.effects(boot=T))
patt.het.boot.ci <- lapply(1:length(true_effect[[1]][[1]]), function(k) lapply(y.col, function(i) quantile(sapply(1:B, function(b) boot_effect[1,][[b]][[i]][[k]]), probs = c(0.025, 0.975), na.rm=T)))
patt.unadj.het.boot.ci <- lapply(1:length(true_effect[[1]][[1]]), function(k) lapply(y.col, function(i) quantile(sapply(1:B, function(b) boot_effect[2,][[b]][[i]][[k]]), probs = c(0.025, 0.975), na.rm=T)))
sate.het.boot.ci <- lapply(1:length(true_effect[[1]][[1]]), function(k) lapply(y.col, function(i) quantile(sapply(1:B, function(b) boot_effect[3,][[b]][[i]][[k]]), probs = c(0.025, 0.975), na.rm=T)))
satt.het.boot.ci <- lapply(1:length(true_effect[[1]][[1]]), function(k) lapply(y.col, function(i) quantile(sapply(1:B, function(b) boot_effect[4,][[b]][[i]][[k]]), probs = c(0.025, 0.975), na.rm=T)))
conf.int <- lapply(y.col, function(i){
                  ci.lower <- c(0, sapply(patt.het.boot.ci, "[[", i)[1,],  #### Put in 0 in place of "overall" confidence bounds for now
                                0, sapply(patt.unadj.het.boot.ci, "[[", i)[1,],
                                0, sapply(sate.het.boot.ci, "[[", i)[1,])
                  ci.upper <- c(0, sapply(patt.het.boot.ci, "[[", i)[2,],
                                0, sapply(patt.unadj.het.boot.ci, "[[", i)[2,],
                                0, sapply(sate.het.boot.ci, "[[", i)[2,])
                  cbind(ci.lower, ci.upper)
                  })


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
                                                  rep("SATE (adjusted)",length(covs)+1))),
                                      ci.lower = conf.int[[i]][,1],
                                      ci.upper = conf.int[[i]][,2]))
# Plot forest plot
het.plot.all <- lapply(y.col, function (i) 
                         ggplot(het.plot[[i]][het.plot[[i]]$Group!="Income" & het.plot[[i]]$x!="Male",], aes(x=x, y=y, ymin = ci.lower, ymax = ci.upper, colour=Estimator)) +
                     #    geom_point(size=6,alpha=0.4) + 
                         geom_pointrange() +
                         scale_colour_manual(values=c("red","blue","green")) + # change colors for estimators
                         coord_flip() +
                         geom_line() +
                         geom_hline(aes(x=0), lty=2) +
                     #    facet_grid(Group~.) +
                         ylab("Treatment effect") +
                         xlab("")) #switch because of the coord_flip() above 

het.plot.all[[1]] # any.visit
#het.plot.all[[2]] # num.visit
het.plot.all[[3]] # any.out
#het.plot.all[[4]] # num.out

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


