# Create plots for each outcome variable comparing SATT vs. PATT overall and by covariate 

# Libraries
library(ggplot2)

# Calculate differences in potentual outcomes for population treated compliers
nrt.pred <- lapply(y.col, function (i) data.frame("tau"=Yhat.1[[i]]-Yhat.0[[i]],
                                                  X.nhis[which(insurance.nhis==1),]))
                                                   
# Estimate PATT for each covariate group
covs <- colnames(X.nhis)[4:40] # exclude HH dummies

patt.het <- lapply(y.col, function (i) lapply(covs, function(x) mean(nrt.pred[[i]]$tau[nrt.pred[[i]][x]==1]) - 
                                             mean(nrt.pred[[i]]$tau[nrt.pred[[i]][x]==0])))

# Estimate SATT overall and for each covariate group
rct.pred <- lapply(y.col, function (i) data.frame("Y.hat"= response.mod[[i]]$predicted, # combine predictions with covariates
                                                  X.ohie.response))

#satt.overall <- lapply(y.col, function (i) mean(rct.pred[[i]]$Y.hat[rct.pred[[i]]$treatment==1]) -
 #                        mean(rct.pred[[i]]$Y.hat[rct.pred[[i]]$treatment==0])) # overall treatment effect on sample treated compliers

satt.het <- lapply(y.col, function (i) lapply(covs, function(x) 
                                              # heterogenous treatment effect on sample treated compliers
                                              

# Create data for plot
het.plot <- data.frame(x=rep(c("Low", "Medium", "High"),3), 
                       y = c(slaves.pre.low, slaves.pre.medium, slaves.pre.high,acres.pre.low, acres.pre.medium, acres.pre.high, ptax.pre.low, ptax.pre.medium, ptax.pre.high), 
                       Measure= c(rep("Slaves",3),rep("Land (acres)",3),rep("Person tax ($)",3)))
# Plot forest plot
het.plot$x <- factor(het.plot$x, levels=rev(het.plot$x)) # reverse order
ggplot(het.plot, aes(x=x, y=y,colour=Measure,group=Measure)) + 
  geom_point(size=4) + 
  coord_flip() +
  geom_line() +
  geom_hline(aes(x=0), lty=2) +
  facet_grid(Measure ~.) +
  theme(legend.position="none") +
  ylab("Treatment effect") +
  xlab("") #switch because of the coord_flip() above 