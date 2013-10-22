







## clean out any previous work
outputs <- c("lfsdat.csv",               # the clean data
             "WageTrendRegression.csv",  # The Table built from the second script 
             list.files(pattern = "*.png$"))
file.remove(outputs)

## run my two scripts from the beginning
source("01_preprocessData.R")   ## Reads the big dataset and makes the required changes and saves the clean data 
source("02_analysisPlots.R")    ## analyse global terrorism using plots
