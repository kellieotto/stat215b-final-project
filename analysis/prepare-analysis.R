## Imports NHIS and OHIE datasets and creates outcome vectors and common covariates for the analysis 

# Libraries
library(weights)
library(plyr)

# Define directory for analysis 
directory <- "~/Dropbox/github/stat215b-final-project/analysis"

# Source data prep scripts
source(file.path(directory,"prepare-ohie.R"))
source(file.path(directory,"prepare-nhis.R")) # script merges person, sample adult, and imputed income files

## NHIS: sample selection

# Keep participants below 138% FPL
nhis[["2009"]] <- subset(nhis[["2009"]], nhis[["2009"]]$povrati2 <= 138) # diff'nt var
nhis[["2010"]] <- subset(nhis[["2010"]], nhis[["2010"]]$povrati3 <= 1380) 
nhis[["2011"]] <- subset(nhis[["2011"]], nhis[["2011"]]$povrati3 <= 1380)
nhis[["2012"]] <- subset(nhis[["2012"]], nhis[["2012"]]$povrati3 <= 1.380) # diffn't decimals
nhis[["2013"]] <- subset(nhis[["2013"]], nhis[["2013"]]$povrati3 <= 1.380)

## OHIE: create vectors for treatment, # of HH members, and compliance status

# Treatment assignment
treatment <- ifelse(ohie$treatment=="Selected",1,0)

# Assignment is random only conditional on # of HH members on waiting list 
n.hh <- dummify(ohie$numhh_list)

# Compliance is "ever on Medicaid" during study period
# (variable used for analysis of hospital discharge data in Taubman et al. 2014)
insurance <- ifelse(ohie$ohp_all_ever_firstn_30sep2009=="Enrolled",1,0) #Any ED visit in the study period

table(insurance, treatment) # there's two-way crossover?

## OHIE: create vectors for health care use outcomes 
# (outcomes used in Taubman et al (2014))

# Any ED visit in study period  
any.visit <- NA
any.visit[ohie$any_visit_ed=="Yes"] <- 1
any.visit[ohie$any_visit_ed=="No"] <- 0

# Number of ED visits in study period (censored)
num.visit <- ohie$num_visit_cens_ed

# Any ED visit resulting in a hospitalization the study period
any.hosp <- NA
any.hosp[ohie$any_hosp_ed=="Yes"] <- 1
any.hosp[ohie$any_hosp_ed=="No"] <- 0

# Number of ED visits resulting in a hospitalization in study period (censored)
num.hosp <- ohie$num_hosp_cens_ed

# Any Outpatient ED visit in study period
any.out <- NA
any.out[ohie$any_out_ed=="Yes"] <- 1
any.out[ohie$any_out_ed=="No"] <- 0

# Number of Outpatient ED visits in study period (censored)
num.out <- ohie$num_out_cens_ed

## Create NHIS outcome vectors

# Number of times in ER/ED, past 12 m
nhis.num.visit <- foreach(i=years, .combine=c) %do% {
  nhis[[as.character(i)]]$ahernoy2
}
nhis.num.visit[nhis.num.visit==97 | nhis.num.visit==98 | nhis.num.visit==99]  <- NA # make missing NA

# Any ER/ED visit in past 12 m
nhis.any.visit <- NA
nhis.any.visit[nhis.num.visit==0] <- 0
nhis.any.visit[nhis.num.visit>0] <- 1
  
# ER visit resulted in hospital admission 
nhis.any.hosp <- foreach(i=c(2011:2013), .combine=c) %do% { # available for 2011-13
  nhis[[as.character(i)]]$aerhos
}
nhis.any.hosp[nhis.any.hosp==2] <- 0 #recode
nhis.any.hosp[nhis.any.hosp==7 | nhis.any.hosp==8 | nhis.any.hosp==9] <- NA

# Total number of office visits, past 12 m 
nhis.num.out <- foreach(i=years, .combine=c) %do% {
  nhis[[as.character(i)]]$ahcnoyr
}
nhis.num.out[nhis.num.out==97 | nhis.num.out==98 | nhis.num.out==99]  <- NA # make missing NA

# Any office visit in last 12 m
nhis.any.out <- NA
nhis.any.out[nhis.num.out==0] <- 0
nhis.any.out[nhis.num.out>0] <- 1

## Create vectors for common covariates

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



