## Imports NHIS and OHIE datasets and creates outcome vectors and common covariates for the analysis 

# Libraries

# Define data directory
data.directory <- "~/Dropbox/github/stat215b-final-project/data/OHIE_Public_Use_Files/OHIE_Data"


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



