# Create plots for each outcome variable comparing SATT vs. PATT overall and by covariate 

# Libraries
library(ggplot2)

### Confidence intervals for the estimates
B <- 1000
t.patt.boot <- replicate(B, {
              samp <- sample(1:length(Y.hat.1[[1]]), length(Y.hat.1[[1]]), replace=T)  
              lapply(y.col, function (i) mean(Y.hat.1[[i]][samp]) - mean(Y.hat.0[[i]][samp]))
})
t.patt.ci <- lapply(y.col, function(i) quantile(unlist(t.patt.boot[i,]),probs = c(0.025, 0.975)))
t.patt.unadj.boot <- replicate(B, {
  samp <- sample(1:length(Y.hat.1.unadj[[1]]), length(Y.hat.1.unadj[[1]]), replace=T)  
  lapply(y.col, function (i) mean(Y.hat.1.unadj[[i]][samp]) - mean(Y.hat.0.unadj[[i]][samp]))
})
t.patt.unadj.ci <- lapply(y.col, function(i) quantile(unlist(t.patt.unadj.boot[i,]),probs = c(0.025, 0.975)))
t.satt.unadj.boot <- replicate(B, {
  samp <- sample(1:length(Y.hat.1.unadj.rct[[1]]), length(Y.hat.1.unadj.rct[[1]]), replace=T)  
  lapply(y.col, function (i) mean(Y.hat.1.unadj.rct[[i]][samp]) - mean(Y.hat.0.unadj.rct[[i]][samp]))
})
t.satt.unadj.ci <- lapply(y.col, function(i) quantile(unlist(t.satt.unadj.boot[i,]),probs = c(0.025, 0.975)))

t.sate.boot <- replicate(B,{
                samp <- sample(1:length(treatment.ohie), length(treatment.ohie), replace=T)  
                lapply(y.col, function (i) (mean(Y.ohie[[i]][samp][which(treatment.ohie[samp]==1)]) - # Num. is ITT effect
                                          mean(Y.ohie[[i]][samp][which(treatment.ohie[samp]==0)])) 
                   /mean(rct.compliers$complier[samp][which(treatment.ohie[samp]==1)]))
})
t.sate.ci <- lapply(y.col, function(i) quantile(unlist(t.sate.boot[i,]),probs = c(0.025, 0.975)))


### Function to get heterogeneous treatment effect estimates, using true data and bootstrapped data (set boot = TRUE)

het.effects <- function(covs, boot = FALSE){
  if(boot==TRUE){
    boot.nrt <- sample(1:sum(insurance.nhis==1,na.rm=T), sum(insurance.nhis==1,na.rm=T), replace = T)
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


### Estimate the heterogeneous effects
covs <- colnames(X.nhis)[5:21] # choose features to est. het effects

true_effect <- het.effects(covs) # a list where true_effect[[1]] is patt.het, true_effect[[2]] is patt.unadj.het, etc
patt.het <- true_effect[[1]]
patt.unadj.het <- true_effect[[2]]
sate.het <- true_effect[[3]]
satt.het <- true_effect[[4]]

B <- 50
boot_effect <- replicate(B, het.effects(covs,boot=TRUE))

### Compute the 95% confidence intervals based on the quantiles of the bootstrap distribution

patt.het.boot.ci <- lapply(1:length(true_effect[[1]][[1]]), function(k) lapply(y.col, function(i) quantile(sapply(1:B, function(b) boot_effect[1,][[b]][[i]][[k]]), probs = c(0.025, 0.975), na.rm=T)))
patt.unadj.het.boot.ci <- lapply(1:length(true_effect[[1]][[1]]), function(k) lapply(y.col, function(i) quantile(sapply(1:B, function(b) boot_effect[2,][[b]][[i]][[k]]), probs = c(0.025, 0.975), na.rm=T)))
sate.het.boot.ci <- lapply(1:length(true_effect[[1]][[1]]), function(k) lapply(y.col, function(i) quantile(sapply(1:B, function(b) boot_effect[3,][[b]][[i]][[k]]), probs = c(0.025, 0.975), na.rm=T)))
satt.het.boot.ci <- lapply(1:length(true_effect[[1]][[1]]), function(k) lapply(y.col, function(i) quantile(sapply(1:B, function(b) boot_effect[4,][[b]][[i]][[k]]), probs = c(0.025, 0.975), na.rm=T)))
conf.int <- lapply(y.col, function(i){
                  ci.lower <- c(t.patt.ci[[i]][1], sapply(patt.het.boot.ci, "[[", i)[1,],  #### Put in 0 in place of "overall" confidence bounds for now
                                t.patt.unadj.ci[[i]][1], sapply(patt.unadj.het.boot.ci, "[[", i)[1,],
                                t.satt.unadj.ci[[i]][1], sapply(satt.het.boot.ci, "[[", i)[1,])
                  ci.upper <- c(t.patt.ci[[i]][2], sapply(patt.het.boot.ci, "[[", i)[2,],
                                t.patt.unadj.ci[[i]][2], sapply(patt.unadj.het.boot.ci, "[[", i)[2,],
                                t.satt.unadj.ci[[i]][2], sapply(satt.het.boot.ci, "[[", i)[2,])
                  cbind(ci.lower, ci.upper)
                  })


# Create data for plot
Overall  <- c("Overall")
Sex <- c("Female")
Age <- c("Age 19-49", "Age 50-64")
Race.ethn <- c("White", "Black", "Hispanic")
Health.stat <- c( "Diabetes", "Asthma","High blood pressure", "Heart Condition")
Education <- c("Less than high school","High school disploma or GED",
               "Vocational training or 2-year degree","4-year college degree or more")
Income <- c("Income < $10k","Income $10k-$25k","Income > $25k")
cov.groups <- c("Overall","Sex","Age","Race","Health status","Education","Income") 
cov.names <- c(Overall,Sex,Age,Race.ethn,Health.stat,Education,Income)

het.plot <- lapply(y.col, function (i) data.frame(x=factor(c(rep(cov.names,3)), levels=rev(cov.names)), # reverse order
                                     y = c(t.patt[[i]],unlist(patt.het[[i]]),
                                           t.patt.unadj[[i]],unlist(patt.unadj.het[[i]]),
                                           t.satt.unadj[[i]],unlist(satt.het[i])), 
                                     Group = factor(rep(c(cov.groups[1],rep(cov.groups[2],length(Sex)),rep(cov.groups[3],length(Age)),
                                               rep(cov.groups[4],length(Race.ethn)),rep(cov.groups[5],length(Health.stat)),
                                               rep(cov.groups[6],length(Education)),rep(cov.groups[7],length(Income))),3), levels=cov.groups),
                                     Estimator= factor(c(rep("PATT (adjusted)",length(covs)+1),
                                                  rep("PATT (unadjusted)",length(covs)+1),
                                                  rep("SATT (unadjusted)",length(covs)+1))), 
                                      ci.lower = conf.int[[i]][,1],
                                      ci.upper = conf.int[[i]][,2]))
# Plot forest plot
het.plot.all <- lapply(y.col, function (i) 
                         ggplot(het.plot[[i]], aes(x=x, y=y, ymin = ci.lower, ymax = ci.upper, colour=Estimator)) +
                         geom_pointrange(size=1, alpha=0.6) +
                         scale_colour_manual(values=c("red","blue","green")) + # change colors for estimators
                         coord_flip() +
                         geom_line() +
                         geom_hline(aes(x=0), lty=2) +
                     #    facet_grid(Group~.) +
                         ylab("Treatment effect") +
                         xlab("")) #switch because of the coord_flip() above 

het.plot.all[[1]] # any.visit
het.plot.all[[2]] # any.out


