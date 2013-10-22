

# READING DATA AND CLEANNING


path  <- "LFSDATA/"
filenames <- list.files(path)    # this reads all the data files titled: 1997..2013


## A function for sampling from data files - 
## For argument x which is a file, picks 1000 entities, fairly randomly 
jFun <- function( x) {
    BIGDAT  <- read.csv( x,  na.strings = "")       #Resolves the empty cells with NA
    BIGDAT <- subset(BIGDAT, BIGDAT$FWEIGHT>500)    #ignores the less frequent entities 
    Edat <- BIGDAT[sample(nrow(BIGDAT), 1000, replace = FALSE, prob = NULL),]  # Sample 1000 rows out of current file
    fDat <- subset(Edat, select= c(SURVYEAR, PROV, AGE_12, SEX, MARSTAT, EDUC90,  NAICS_18, YABSENT,FTPTMAIN,UHRSMAIN,TENURE,HRLYEARN,UNION, FWEIGHT))  ## Selects some of the columns
    return(fDat)
}



## Apply the function above on all the data files
setwd("LFSDATA/")   ## goes in the input directory to read and combine
lfsRAW <- do.call("rbind", lapply(filenames,jFun))      
setwd("..")         ## come back to the main directory

## Check that we have correctly read the data, there should be 17000 observations
dim(lfsRAW)


## Our main data from now on is called "lfsdat"
lfsdat <- lfsRAW


## For each variable of the data, we give some tidier attribute name
## In this part we summarized some of the attributes, for example we 
## count "widowed" people as sinle
levels(lfsdat$MARSTAT) <- list(Married="Married or", Married= "Married", Single= "Single, ne", Single="Widowed", Separated= "Separated/",Separated= "Separated", Separated= "Divorced", Married= "Living in")
levels(lfsdat$PROV) <- list(Alberta= "Alberta", BritishColumbia= "British Co",Manitoba="Manitoba",NewBrunswick= "New Brunsw", Newfoundland= "Newfoundla", NovaScotia="Nova Scoti", Ontario="Ontario", PrinceEdward="Prince Edw", Quebec="Québec" , Saskatchewan="Saskatchew")
levels(lfsdat$EDUC90) <- list(A.EightYears= "0 to 8 yea", B.Secondary= "Some secon",C.Grade11to13= "Grade 11 t",D.PostSecondary="Post secon", D.PostSecondary= "Some post", E.University="University")
levels(lfsdat$YABSENT) <- list(Vacation= "Vacation", FamilyResponsibility= "Personal o", OwnIllness= "Own illnes",Other= "Other")


## Change the names of variables to a more meaningful one
c <- c("Year", "Province", "Age", "Sex", "MaritalStatus", "Educations","JobType", "JobAbsence", "JobStatus", "WorkHours", "Tenure", "HourlyEarn", "Union", "Frequency")
names(lfsdat) <- c

### We rank the entities of the data based on some of the variables
##  with most priority given to Year then Provinces and then Sex
lfsdat = arrange(lfsdat, Year, Province, Sex)


### Check for the outliers 
ggplot(lfsdat[!is.na(lfsdat$HourlyEarn),], aes(x = Province, y = HourlyEarn, color="orange")) + geom_jitter()


## Drop the outliers
lfsdat <- droplevels(subset(lfsdat, Province != "Manitoba"))


## Write the final and clean data set to a human-readable file
write.table(lfsdat, "lfsdat.csv", quote = FALSE, sep = ",", row.names = FALSE)
