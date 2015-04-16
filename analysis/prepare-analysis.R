## Imports NHIS and OHIE datasets and creates outcome vectors and common covariates for the analysis 

# Libraries
library(weights)

# Define directory for analysis 
directory <- "~/Dropbox/github/stat215b-final-project/analysis"

# Source data prep scripts
source(file.path(directory,"prepare-ohie.R"))
source(file.path(directory,"prepare-nhis.R"))

## Create numeric vectors for treatment, # of HH members, and compliance status

# Treatment assignment
treatment <- ifelse(ohie$treatment=="Selected",1,0)

# Assignment is random only conditional on # of HH members on waiting list 
n.hh <- dummify(ohie$numhh_list)

# Compliance is "ever on Medicaid" during study period
# (variable used for analysis of hospital discharge data in Taubman et al. 2014)
insurance <- ifelse(ohie$ohp_all_ever_firstn_30sep2009=="Enrolled",1,0)

table(insurance, treatment) # there's two-way crossover?

## Create vectors for health care use outcomes


## Create vectors for common covariates

# create at or below 138% fpl dummy
x.sa$below.138.fpl = ifelse(x.sa$povrati3 <= 1380 , 1 , 0 ) 

# create family income categories
x.sa <- 
  transform( 
    x.sa , 
    
    # create a four-category family income variable
    fine.faminci2 =
      cut( 
        faminci2, 
        c( -Inf , summary(faminci2[below.138.fpl==1])[[2]] , summary(faminci2[below.138.fpl==1])[[3]], summary(faminci2[below.138.fpl==1])[[5]] , Inf ) ,
        labels = c( "Fam. income (<Q1)" , "Fam. income (>Q1 & <Q2)" , "Fam. income (>Q2 & <Q3)" , "Fam. income (>Q3)" )
      )
  )



