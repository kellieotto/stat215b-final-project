# Create plots for each outcome variable comparing SATT vs. PATT overall and by covariate 

# Libraries
library(ggplot2)

ResponseDiff <- function(x,y.hat) {
  #Calculates response surface differences for the treated compliers
  #
  # Args:
  #   x: Character string for covariate group. 
  #   y.hat: Vector containing estimated responses or differences in potential outcomes. 
  #
  # Returns:
  #   Numeric value for mean difference in y.hat for x. 
  return(mean(y.hat[x==1]) - mean(y.hat[x==0]))
}

# Calculate differences in potentual outcomes for population treated compliers
nrt.pred <- lapply(y.col, function (i) data.frame("tau"=Yhat.1[[i]]-Yhat.0[[i]],
                                                  X.nhis[which(insurance.nhis==1),]))
                                                   
# Compute PATT for each covariate group
covs <- colnames(X.nhis)[4:40] #select covariate names
patt.het <- lapply(y.col, function (i) lapply(covs, function(x) mean(nrt.pred[[i]]$tau[nrt.pred[[i]][x]==1]) - 
                                             mean(nrt.pred[[i]]$tau[nrt.pred[[i]][x]==0])))

# Compute SATE for each covariate group
  # combine predicted responses with RCT covariates

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