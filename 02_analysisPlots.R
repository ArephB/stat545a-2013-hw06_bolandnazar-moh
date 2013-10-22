

## This is the main file for data analysis and making plots and tables



## Loading the clean data

lfsdat <- read.csv("lfsdat.csv")

## Loading Libraries

library(ggplot2)
library(xtable)
library(lattice)
library(plyr)
library(mgcv)       # for using "gam" in geom_smooth(...)   



## Workers in different Jobs

## NOTE : The first line which we will use again and again, actually ignores those Non Available
## observations in the variables that we are currently work/concentrate on. 
## for example here we want to talk about JobTypes. In order to be convinient
## we force R only work on available data in this part of program

lfstemp<- lfsdat[!is.na(lfsdat$JobType),]
lfstemp<- within( lfstemp, JobType <- reorder(JobType, Sex, length, order=TRUE)) # Reorder based on increasing count of each JobType

## NOTE: lftemp is a temporary data frame with a reordered and NA-cleared attributes for
## a temporary use in this part

q <- ggplot(lfstemp, aes(x = JobType, fill= Sex)) +
        geom_bar(position = "dodge")+
        theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))+
        ggtitle("Workers in different Jobs")
ggsave("WorkersInJobs.png")

## angle = 45 gives nicer labels on x axis (to avoid overlapping labels)





## Reason of Being Absent from the Main Job
## lftemp is a temporary data frame with a reordered and NA-cleared attributes for
## a temporary use in this part
lfstemp <- lfsdat[!is.na(lfsdat$JobAbsence), ]
lfstemp <- within( lfstemp, JobAbsence <- reorder( JobAbsence, Sex, length, order = TRUE))

q <- ggplot(lfstemp,aes(x=JobAbsence, fill= Sex)) +
       geom_bar(position = "dodge")+
       theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))+
       ggtitle("Reason of Being Absent from the Main Job")
ggsave("ReasonOfJobAbsence.png")

## angle = 45 gives nicer labels on x axis (to avoid overlapping labels)





## Distribution of Wage in Different Provinces

lfstemp<- within(lfsdat[!is.na(lfsdat$HourlyEarn),], Province <- reorder(Province, HourlyEarn, min, order=TRUE))  ## reorder provinces based on the minimum of wages 

q <- ggplot(lfstemp[!is.na(lfstemp$HourlyEarn),], aes(x = HourlyEarn, color = Province)) +
       geom_density(lwd = 1)+
       ggtitle("Distribution of Wage in Different Provinces")
ggsave("WageInProvinces.png")






## Boxplot of Hourly Wage over Time

q <- ggplot(lfsdat[!is.na(lfsdat$HourlyEarn),], aes(x = as.factor(Year), y= HourlyEarn)) +
        geom_boxplot(lwd = 1, trim= TRUE)+
        theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))+
        ggtitle("Hourly Wage over Time") + xlab("Year") 
ggsave("WageOverYear.png")





## Regression of Median Hourly Wage with respect to year (adjusted to zero at 1997)
## Summarize the results in a table

lfstemp <- lfsdat[!is.na(lfsdat$HourlyEarn),]   ## get rid of NA cells

MedWage <- ddply( lfstemp , ~Province + Year, summarize, MedianWage = median( HourlyEarn))
names(MedWage) <- c("Province", "Year", "MedWage")   ## giving proper names
yearMin <- 1997  ## to let it make more sense

RegWageYear <- function(x) {      ## This function finds the regression parameters  
  RegCoefs <- coef(lm(MedWage ~ I(Year - yearMin), x))
  names(RegCoefs) <- c("intercept", "slope")
  return(RegCoefs)
}
WageTrend <- ddply(MedWage, ~Province, RegWageYear)  ## Construct the data frame
WageTrendTable <- xtable(WageTrend)
print(WageTrendTable, type = "html", include.rownames = FALSE)
write.table(WageTrend, "WageTrendRegression.csv", quote = FALSE, sep = ",", row.names = FALSE)


## Scattering Hourly Wage over Years facetting different Provinces

q <- ggplot(lfsdat[!is.na(lfsdat$HourlyEarn),], aes(x = Year, y = HourlyEarn, color=Sex)) +
        geom_jitter( position = position_jitter(width = .1))  + 
        facet_wrap(~ Province)+
        geom_line(stat = "summary", fun.y = "median", col = "black", lwd = 1)+
        ggtitle("Hourly Wage over Time") + xlab("Year") 
ggsave("WageTrendsProvinces.png")



## Trend in Median Wage over Time facetting different Provinces

lfstemp <- lfsdat[!is.na(lfsdat$HourlyEarn),]  ##get rid of NA cells

MedianWage <- ddply(lfstemp, .(Year, Province), summarize,  MedWage = median(HourlyEarn))

q <- ggplot(MedianWage, aes(x = Year, y = MedWage)) + 
        facet_wrap(~Province)+ geom_point(cex = 1) +
        geom_line(lwd=1, color="blue")+ 
        geom_smooth(method = "lm", colour = "red",aes(group=1))+
        ggtitle("Growth in Median Wage over Time for different Provinces")
ggsave("MedianWageTrends.png")





## Wage levels vs. Educational levels


lfstemp <- lfsdat[!is.na(lfsdat$Educations),]   ## to get rid of NA parts
lfstemp <- lfstemp[!is.na(lfstemp$HourlyEarn),] ## to get rid of NA parts

lfstemp<- within(lfstemp, Province <- reorder(Province, HourlyEarn, min, order=TRUE))

q <- ggplot(lfstemp, aes(x = Educations, y = HourlyEarn, fill = Province, order = -as.numeric(Province))) + 
        geom_boxplot(alpha = 0.2) + 
        theme(legend.position = "bottom") +  
        xlab("Educational Levels") + ylab("Hourly Wage") +
        ggtitle("Wage levels among Educational levels")

ggsave("WageEducations.png")




## Median Wages for different Educational levels

lfstemp <- lfsdat[!is.na(lfsdat$HourlyEarn),]
lfstemp<- within(lfstemp, Province <- reorder(Province, HourlyEarn, min, order=TRUE))

MedianWage <- ddply(lfstemp, .(Educations, Sex), summarize,  MedWage = median(HourlyEarn))

q <- ggplot(MedianWage, aes(x = Educations, y = MedWage,col=Sex))+geom_point(cex = 4)  +
        geom_line(aes(group=Sex))+
        theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))+
        ggtitle("Median Wages for different Educational levels")
ggsave("MedianWageEducations.png")



## Wage for Different Ages

lfstemp <- lfsdat[!is.na(lfsdat$Age),]  # to get rid of NA cells
lfstemp <- lfstemp[!is.na(lfstemp$HourlyEarn),]

lfstemp<- within(lfstemp, JobStatus <- reorder(JobStatus, HourlyEarn, max, order=TRUE))

q <- ggplot(lfstemp, aes(x = Age, y= HourlyEarn, color = JobStatus, order= as.numeric(JobStatus))) + 
       geom_jitter(position = position_jitter(width = .1)) + ylab("Hourly Wage") + 
       ggtitle(" Wage for Different Ages") +
       theme(axis.text.x = element_text(angle = 45)) + 
       scale_fill_brewer("JobStatus", type = "qual", palette = 3)+
       geom_smooth(method = "gam", formula = y ~ s(x), colour = "purple",aes(group=1)) 
## raughly speaking geom_smooth adds a summary of the trend for the whole data  
ggsave("WageOnAge.png")





## Union Members in different Provinces

lfstemp <- lfsdat[!is.na(lfsdat$Union),]
lftemp<- within(lfstemp, Province <- reorder(Province, Union, length, order=TRUE))

q <- ggplot(lfstemp,aes(x=Province, fill= Union)) +
        geom_bar(position = "dodge")+facet_wrap(~Year)+
        theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))+
        ggtitle(" Union Members in different Provinces")+
        coord_flip()  ## makes a horizontal barchart 
ggsave("UnionMembersProvinces.png")





## Hours of Work in a week across age groups

lfstemp <- lfsdat[!is.na(lfsdat$WorkHours),]
lfstemp <- lfstemp[!is.na(lfstemp$Age),]

q <- ggplot(lfstemp, aes(x = Age, y = WorkHours)) +  facet_wrap(~Sex, ncol = 2) + geom_smooth(method = "gam", formula = y ~ s(x), colour = "purple",aes(group=1))+
          theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))+
          ggtitle(" Hours of Work in a week across age groups")
ggsave("WorkHoursAge.png")




## Distribution of Employee Tenures

lfstemp <- lfsdat[!is.na(lfsdat$Tenure),]
lfstemp <- within(lfstemp, Province <- reorder(Province, Tenure, min, order=TRUE))

q <- ggplot(lfstemp, aes(x = Tenure, color = Province))+
        geom_density(lwd = 1)+
        ggtitle("Distribution of Employee Tenures") 
ggsave("TenureDistProvinces.png")



## Hourly Wage for different Jobs

lfstemp <- lfsdat[!is.na(lfsdat$HourlyEarn),]
lfstemp <- within(lfstemp, JobType <- reorder(JobType, HourlyEarn, median, order=TRUE))

q <- ggplot(lfstemp, aes(x = as.factor(JobType), y= HourlyEarn)) +
        geom_boxplot(lwd = 1, trim= TRUE, color="brown")+theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))+
        ggtitle("Hourly Wage for different Jobs") + xlab("Job Type") 
ggsave("WagesOfJobs.png")

