# Prepares Oregon Health Insurance Experiment (OHIE) data

# Libraries
library(foreign)

# Define data directory
data.directory <- "~/Dropbox/github/stat215b-final-project/data/OHIE_Public_Use_Files/OHIE_Data"

# Import data
f <- file.path(data.directory, c("oregonhie_descriptive_vars.dta",
                                 "oregonhie_ed_vars.dta",
                                 "oregonhie_inperson_vars.dta",
                                 "oregonhie_stateprograms_vars.dta",
                                 "oregonhie_survey0m_vars.dta",
                                 "oregonhie_survey12m_vars.dta"))
ohie <- lapply(f, read.dta) # read data to list
names(ohie) <- gsub(".*/oregonhie_(.*)\\..*", "\\1", f) # name elements

# Extract each element of list as a data frame
descriptive <- as.data.frame(ohie[["descriptive_vars"]])
ed <- as.data.frame(ohie[["ed_vars"]])
inperson <- as.data.frame(ohie[["inperson_vars"]])
stateprograms <- as.data.frame(ohie[["stateprograms_vars"]])
survey0m <- as.data.frame(ohie[["survey0m_vars"]])
survey12m <- as.data.frame(ohie[["survey12m_vars"]])

# Clean up workspace
rm(data.directory,f,ohie)