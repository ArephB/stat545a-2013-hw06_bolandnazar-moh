## fine-tune the cleaning strategy to avoid this redoing this step in most
## scenarios; set nuke to TRUE if you want to re-clean and assemble the data
nuke <- FALSE

## clean out any previous work
if(nuke) {
  outputs <- c("lfsdat.csv",               # the clean data
               "WageTrendRegression.csv",  # The Table built from the second script 
               list.files(pattern = "*.png$"))
} else {
  outputs <- list.files(pattern = "*.png$")
}
## clean out previous work
file.remove(outputs)

## run my two scripts from the beginning
if(!file.exists("lfsdat.csv")) {
  ## Reads the big dataset and makes the required changes and saves the clean
  ## data
  source("01_preprocessData.R")
}
source("02_analysisPlots.R")    ## analyse and plot labor force data
