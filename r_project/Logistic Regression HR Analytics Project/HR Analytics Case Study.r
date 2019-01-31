###########################################################################################################################
#-----------------------HR Analytics Group Case Study--------------------------------------------------------------------##         
###########################################################################################################################
#-----------------------Program Flow-------------------------------------------------------------------------------------##
#Business Understanding
#Data Understanding
#Data Preparation & EDA
#Model Building 
#Model Evaluation
###########################################################################################################################
#
#----------------------- Business Understanding--------------------------------------------------------------------------##
# A large company XYZ has 4000+ employees and it has 15% attrition. Management believes that this high attrition is bad for 
# them as it results into delay in existing projects, high recruitment efforts and training to be imparted to new employees.
# 
# Company wants to know what are the reasons for this attrition and accordingly want to make changes such that attrition is
# reduced. 
# 
# Objective is to carry out data driven analysis of a problem and find out the root causes. 
##########################################################################################################################
#------------------------------------------------------------------------------------------------------------------------#
# SET UP WORK DIRECTORY
#-------------------------------------------------------------------------------------------------------------------------#
#setwd("C:/Users/Desktop/PGDDS/Course 3 - Predictive Analytics 1/Group Project - HR Analytics Case Study/PA-I_Case_Study_HR_Analytics")
#-------------------------------------------------------------------------------------------------------------------------#
# INSTALL PACKAGES AND LOAD REQUIRED LIBRARIES
#-------------------------------------------------------------------------------------------------------------------------# 

library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(lubridate)
library(cowplot)
library(GGally)
library(MASS)
library(car)
library(e1071)
library(caret)
library(caTools)
library(ROCR)

#-------------------------------------------------------------------------------------------------------------------------#
#**********************DATA SOURCING**************************************************************************************#
#-------------------------------------------------------------------------------------------------------------------------#

emp_survey <- read.csv("employee_survey_data.csv",stringsAsFactors = F)
general_data <- read.csv("general_data.csv",stringsAsFactors = F)
in_time <- read.csv("in_time.csv",stringsAsFactors = F)
manager_survey <- read.csv("manager_survey_data.csv",stringsAsFactors=F)
out_time<- read.csv("out_time.csv",stringsAsFactors = F)

#------------------------------------------------------------------------------------------------------------------------#
# *********************DATA UNDERSTANDING AND PREPARATION****************************************************************#
#------------------------------------------------------------------------------------------------------------------------#

str(emp_survey)       # emp_survey has 4410 obs. of 4 variables
str(general_data)     # general data has 4410 obs. of  24 variables including target variable Attrition.
str(in_time)          # in_time has 4410 obs. of 262 variables
str(manager_survey)   # manager_survey has 4410 obs. of 3 variables
str(out_time)         # out_time has 4410 obs. of 262 variables

# Checking the unique key - EmployeeID
length(unique(emp_survey$EmployeeID)) #unique number of EmployeeID is 4410
length(unique(general_data$EmployeeID)) #unique number of EmployeeID is 4410
length(unique(in_time$X)) # Assumption: Unlabelled first column is considered as EmployeeID
length(unique(manager_survey$EmployeeID)) #unique number of EmployeeID is 4410
length(unique(out_time$X)) # Assumption: Unlabelled first column is considered as EmployeeID

# All datasets have same number of records. It is seen that intime and outtime datasets are for each employee howerver employee ID
# column needs to be renamed for consistency across datasets.
# We renamed the first column 'X' from each of the dataframes in_time and out_time data based on the assumption.
colnames(in_time)[1]<- "EmployeeID"
colnames(out_time)[1]<- "EmployeeID"

## All the dataset are having same number of unique EmployeeID.
# Here we check and compare the  unique keys in all the file.
setdiff(emp_survey$EmployeeID,general_data$EmployeeID) #returns 0 - identical EmployeeID across these datasets
setdiff(general_data$EmployeeID,in_time$EmployeeID)    #returns 0 - identical EmployeeID across these datasets
setdiff(in_time$EmployeeID,manager_survey$EmployeeID)  #returns 0 - identical EmployeeID across these datasets
setdiff(manager_survey$EmployeeID,out_time$EmployeeID) #returns 0 - identical EmployeeID across these datasets

#Hence all the files are having identical unique key-EmployeeID

#########################################################################################################
# Data Preparation
#
# 1. emp_survey, general_data, manager_survey dataframes are all having EmployeeId as unique key. Hence
#    for the analysis we will be merging them to get the emp_all_data dataframe.

#We will merege all the datasets based on EmployeeID key.
emp_all_data <- merge(emp_survey,general_data,by='EmployeeID')
emp_all_data <- merge(emp_all_data,manager_survey,by='EmployeeID')                 

# 2. There are few characher variables in the merged dataset which need to be reformatted and converted into 
#    factor type for the analysis. Hence we will reformat and factorised following variables.
#    - EnvironmentSatisfaction
#    - JobSatisfaction
#    - WorkLifeBalance
#    - BusinessTravel
#    - Department
#    - Education
#    - EducationField
#    - Gender
#    - JobLevel
#    - JobRole
#    - MaritalStatus
#    - Over18
#    - JobInvolvement
#    - PerformanceRating

#EnvironmentSatisfaction 
emp_all_data$EnvironmentSatisfaction <- sapply(emp_all_data$EnvironmentSatisfaction,function(x){  if(is.na(x)) NA else if(x==1) 'Low'  else if(x==2) 'Medium' else if(x==3) 'High' else if(x==4) 'Very High'})
emp_all_data$EnvironmentSatisfaction <- as.factor(emp_all_data$EnvironmentSatisfaction)

#JobSatisfaction
emp_all_data$JobSatisfaction <- sapply(emp_all_data$JobSatisfaction,function(x){  if(is.na(x)) NA else if(x==1) 'Low'  else if(x==2) 'Medium' else if(x==3) 'High' else if(x==4) 'Very High'})
emp_all_data$JobSatisfaction <- as.factor(emp_all_data$JobSatisfaction)

#WorkLifeBalance
emp_all_data$WorkLifeBalance <- sapply(emp_all_data$WorkLifeBalance,function(x){  if(is.na(x)) NA else if(x==1) 'Bad'  else if(x==2) 'Good' else if(x==3) 'Better' else if(x==4) 'Best'})
emp_all_data$WorkLifeBalance <- as.factor(emp_all_data$WorkLifeBalance)

#BusinessTravel
emp_all_data$BusinessTravel <- as.factor(emp_all_data$BusinessTravel)

#Department
emp_all_data$Department <- as.factor(emp_all_data$Department)

#Education
emp_all_data$Education <- sapply(emp_all_data$Education,function(x){  if(is.na(x)) NA else if(x==1) 'Below College'  else if(x==2) 'College' else if(x==3) 'Bachelor' else if(x==4) 'Master'  else if(x==5) 'Doctor'})
emp_all_data$Education <- as.factor(emp_all_data$Education)

#EducationField
emp_all_data$EducationField <- as.factor(emp_all_data$EducationField)

#Gender
emp_all_data$Gender <- as.factor(emp_all_data$Gender)

#JobLevel
emp_all_data$JobLevel <- as.factor(emp_all_data$JobLevel)

#JobRole
emp_all_data$JobRole <- as.factor(emp_all_data$JobRole)

#StockOptionLevel
emp_all_data$StockOptionLevel <- as.factor(emp_all_data$StockOptionLevel)

#MaritalStatus
emp_all_data$MaritalStatus <- as.factor(emp_all_data$MaritalStatus)

#Over18
emp_all_data$Over18 <- as.factor(emp_all_data$Over18)

#JobInvolvement
emp_all_data$JobInvolvement <- sapply(emp_all_data$JobInvolvement,function(x){  if(is.na(x)) NA else if(x==1) 'Low'  else if(x==2) 'Medium' else if(x==3) 'High' else if(x==4) 'Very High'})
emp_all_data$JobInvolvement <- as.factor(emp_all_data$JobInvolvement)

#PerformanceRating
emp_all_data$PerformanceRating <- sapply(emp_all_data$PerformanceRating,function(x){  if(is.na(x)) NA else if(x==1) 'Low'  else if(x==2) 'Good' else if(x==3) 'Excellent' else if(x==4) 'Outstanding'})
emp_all_data$PerformanceRating <- as.factor(emp_all_data$PerformanceRating)

#3. Time In Office Calculation  and Analysis. 
##  There are two more input datasets - in_time and out_time. These two files have employee's in time in office and out time from 
##  office since 01-01-2015 to 31-12-2015. We need to process these two files to get average time in office 
##  for all the employees during this 1 year period to check if this is a major drivng factor for employee attrition.

#Before merging all the files we will change the colnames for in_time and out_time files as both the files have same colnames
## gather data in_time_data 
in_time_gather_data <- gather(in_time,X2015.01.01:X2015.12.31, key = "date", value = "In_Time")
##Remove x from date
in_time_gather_data$date <- sapply(in_time_gather_data$date, function(x){str_split(x, "X", n = 2, simplify = TRUE)[2][1]}) 
## cast to date 
in_time_gather_data$date <- ymd(in_time_gather_data$date)
in_time_gather_data$In_Time <- ymd_hms(in_time_gather_data$In_Time)

sum(is.na(in_time_gather_data))/count(in_time_gather_data) # data ha NA , lets remove it
in_time_gather_data <- na.omit(in_time_gather_data)

## check the str
str(in_time_gather_data)
## check there is no missing value
sum(is.na(in_time_gather_data))

## duplicate check
length(unique(out_time$X)) # no duplicate record

## gather data out_time_data
out_time_gather_data <- gather(out_time,X2015.01.01:X2015.12.31, key = "date", value = "Out_Time")
##Remove x from date
out_time_gather_data$date <- sapply(out_time_gather_data$date, function(x){str_split(x, "X", n = 2, simplify = TRUE)[2][1]}) 
## cast to date 

out_time_gather_data$date <- ymd(out_time_gather_data$date)
out_time_gather_data$Out_Time <- ymd_hms(out_time_gather_data$Out_Time)

sum(is.na(out_time_gather_data))/count(out_time_gather_data) # data has NA , lets remove it
out_time_gather_data <- na.omit(out_time_gather_data)

## check the str
str(out_time_gather_data)
## check there is no missing value
sum(is.na(out_time_gather_data))

##merge in_time_gather_data and out_time_gather_data

login_time_data <- in_time_gather_data %>%
  left_join(out_time_gather_data, by=c("EmployeeID","date"))

str(login_time_data)

## derive metrics
## time_in_office
login_time_data$time_in_office <- as.integer(difftime(login_time_data$Out_Time,login_time_data$In_Time,units="hours"))

## Is_weekend
login_time_data$wday <- wday(login_time_data$date, label = TRUE, abbr = FALSE)
login_time_data$Is_weekend <- ifelse(login_time_data$wday=='Sunday' || login_time_data$wday=='Saturday',1,0)

unique(login_time_data$Is_weekend)
## we dont have weekend data , so we can ignore weekend analysis

##quarter
login_time_data$quarter <- quarter(login_time_data$date)
unique(login_time_data$quarter)
# We checked the quarter wise time in office for all the employees.
# in_out_data gives avgerage time office for the all employees and quarterwise average time in office.

in_out_data <- login_time_data %>%
  group_by(EmployeeID)%>%
  summarise(
    Avg_time_in_office = as.integer(mean(time_in_office, na.rm = T)),
    Avg_time_in_office_first_qrt = as.integer( mean(time_in_office[quarter==1], na.rm = T) ),
    Avg_time_in_office_second_qrt = as.integer( mean(time_in_office[quarter==2], na.rm = T)),
    Avg_time_in_office_third_qrt =  as.integer( mean(time_in_office[quarter==3], na.rm = T)),
    Avg_time_in_office_fourth_qrt = as.integer( mean(time_in_office[quarter==4], na.rm = T))
    
  )
str(in_out_data) ##4410 obs. of  6 variables:

# We merge Employee's average office time - in_out_data with emp_all_data dataframe
setdiff(emp_all_data$EmployeeID ,in_out_data$EmployeeID )
emp_all_data <- merge(emp_all_data,in_out_data,by = 'EmployeeID')

##-----End of Office Hours data preparation-------------------------------------------------------------------------------##

##---------------------------------------Basic Data cleaning -------------------------------------------------------------##

#Checking Missing values
sapply(emp_all_data, function(x) sum(is.na(x)))
# emp_all_data has missing values for the following variable:
# EnvironmentSatisfaction
# JobSatisfaction
# WorkLifeBalance
# NumCompaniesWorked
# TotalWorkingYears

#Removing remaining NA values from the dataset
sum(is.na(emp_all_data)) /count(emp_all_data) # 2% of employee data consists of NA value
emp_all_data <- na.omit(emp_all_data)
sum(is.na(emp_all_data)) # verify that there is no NA value left

#Checking for outliers in MonthlyIncome data
box <- boxplot.stats(emp_all_data$MonthlyIncome)
out <- box$out # 7.75% monthly income data are outliers

#Boxplot to check outliers in monthlyincome
ggplot(emp_all_data,aes(Attrition,MonthlyIncome,fill=Attrition))+geom_boxplot()

#Removing outliers from monthlyincome
emp_all_data_wo <- emp_all_data[!emp_all_data$MonthlyIncome %in% out,]
ggplot(emp_all_data_wo,aes(Attrition,MonthlyIncome,fill=Attrition))+geom_boxplot()

# Since both the factors EmployeeCount = 1 and StandardHours =8 ,Over18= 'Y' are having constant value, 
# we assume that they will not affect the attrition rate.
# Hence we are removing them from dataset. 

emp_all_data_wo <- emp_all_data_wo[,!(colnames(emp_all_data_wo) %in%c('EmployeeCount','StandardHours','Over18'))]

#summary of the dataset
summary(emp_all_data_wo)

##-------EDA on merged employee data set----------------------------------------------------------------##

# Barchart for categorical variables.
cat_bar<- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
                   legend.position = 'right')
plot_grid(ggplot(emp_all_data_wo, aes(EnvironmentSatisfaction,fill=as.factor(Attrition)))+geom_bar()+cat_bar,
          ggplot(emp_all_data_wo, aes(JobSatisfaction,fill=as.factor(Attrition)))+geom_bar()+cat_bar,
          ggplot(emp_all_data_wo, aes(WorkLifeBalance,fill=as.factor(Attrition)))+geom_bar()+cat_bar,
          ggplot(emp_all_data_wo, aes(BusinessTravel,fill=as.factor(Attrition)))+geom_bar()+cat_bar,
          ggplot(emp_all_data_wo, aes(JobInvolvement,fill=as.factor(Attrition)))+geom_bar()+cat_bar,
          ggplot(emp_all_data_wo, aes(MaritalStatus,fill=as.factor(Attrition)))+geom_bar()+cat_bar,
          ggplot(emp_all_data_wo, aes(PerformanceRating,fill=as.factor(Attrition)))+geom_bar()+cat_bar)
          
plot_grid(ggplot(emp_all_data_wo, aes(Department,fill=as.factor(Attrition)))+geom_bar()+cat_bar,
          ggplot(emp_all_data_wo, aes(Education,fill=as.factor(Attrition)))+geom_bar()+cat_bar,
          ggplot(emp_all_data_wo, aes(EducationField,fill=as.factor(Attrition)))+geom_bar()+cat_bar,
          ggplot(emp_all_data_wo, aes(Gender,fill=as.factor(Attrition)))+geom_bar()+cat_bar,
          ggplot(emp_all_data_wo, aes(JobLevel,fill=as.factor(Attrition)))+geom_bar()+cat_bar,
          ggplot(emp_all_data_wo, aes(StockOptionLevel,fill=as.factor(Attrition)))+geom_bar()+cat_bar,
          ggplot(emp_all_data_wo, aes(JobRole,fill=as.factor(Attrition)))+geom_bar()+cat_bar)

plot_grid(ggplot(emp_all_data_wo, aes(MaritalStatus,fill=as.factor(Attrition)))+geom_bar()+cat_bar)

# Here we observe that attrition level relates significantly for Business travel, Marital Status
# and lower values of employee / manager surveys. 

# Histogram and boxplot plot for numeric variables.

# Age,DistanceFromHome, MonthlyIncome, NumCompaniesWorked, PercentSalaryHike,TotalWorkingYears, 
# TrainingTimesLastYear, YearsAtCompany, YearsSinceLastPromotion, YearsWithCurrManager, Avg_time_in_Office

num_box <- theme(axis.line=element_blank(),axis.title=element_blank(), 
                  axis.ticks=element_blank(), axis.text=element_blank())

plot_grid(ggplot(emp_all_data_wo, aes(Age,fill=as.factor(Attrition)))+ geom_bar(position = 'dodge'),
          ggplot(emp_all_data_wo, aes(x="",y=Age,fill=as.factor(Attrition)))+ geom_boxplot(width=0.1)+num_box, 
          align = "v",ncol = 1)

plot_grid(ggplot(emp_all_data_wo, aes(DistanceFromHome,fill=as.factor(Attrition)))+ geom_histogram(binwidth = 5),
          ggplot(emp_all_data_wo, aes(x="",y=DistanceFromHome,fill=as.factor(Attrition)))+ geom_boxplot(width=0.1)+num_box, 
          align = "v",ncol = 1)

plot_grid(ggplot(emp_all_data_wo, aes(MonthlyIncome,fill=as.factor(Attrition)))+ geom_histogram(binwidth = 5),
          ggplot(emp_all_data_wo, aes(x="",y=MonthlyIncome,fill=as.factor(Attrition)))+ geom_boxplot(width=0.1)+num_box, 
          align = "v",ncol = 1)

plot_grid(ggplot(emp_all_data_wo, aes(NumCompaniesWorked,fill=as.factor(Attrition)))+ geom_histogram(binwidth = 5),
          ggplot(emp_all_data_wo, aes(x="",y=NumCompaniesWorked,fill=as.factor(Attrition)))+ geom_boxplot(width=0.1)+num_box, 
          align = "v",ncol = 1)

plot_grid(ggplot(emp_all_data_wo, aes(PercentSalaryHike,fill=as.factor(Attrition)))+ geom_histogram(binwidth = 5),
          ggplot(emp_all_data_wo, aes(x="",y=PercentSalaryHike,fill=as.factor(Attrition)))+ geom_boxplot(width=0.1)+num_box, 
          align = "v",ncol = 1)

plot_grid(ggplot(emp_all_data_wo, aes(YearsAtCompany,fill=as.factor(Attrition)))+ geom_histogram(binwidth = 5),
          ggplot(emp_all_data_wo, aes(x="",y=YearsAtCompany,fill=as.factor(Attrition)))+ geom_boxplot(width=0.1)+num_box, 
          align = "v",ncol = 1)

plot_grid(ggplot(emp_all_data_wo, aes(TrainingTimesLastYear,fill=as.factor(Attrition)))+ geom_histogram(binwidth = 5),
          ggplot(emp_all_data_wo, aes(x="",y=TrainingTimesLastYear,fill=as.factor(Attrition)))+ geom_boxplot(width=0.1)+num_box, 
          align = "v",ncol = 1)

plot_grid(ggplot(emp_all_data_wo, aes(TotalWorkingYears,fill=as.factor(Attrition)))+ geom_histogram(binwidth = 5),
          ggplot(emp_all_data_wo, aes(x="",y=TotalWorkingYears,fill=as.factor(Attrition)))+ geom_boxplot(width=0.1)+num_box, 
          align = "v",ncol = 1)

plot_grid(ggplot(emp_all_data_wo, aes(YearsSinceLastPromotion,fill=as.factor(Attrition)))+ geom_histogram(binwidth = 5),
          ggplot(emp_all_data_wo, aes(x="",y=YearsSinceLastPromotion,fill=as.factor(Attrition)))+ geom_boxplot(width=0.1)+num_box, 
          align = "v",ncol = 1)

plot_grid(ggplot(emp_all_data_wo, aes(YearsWithCurrManager,fill=as.factor(Attrition)))+ geom_histogram(binwidth = 5),
          ggplot(emp_all_data_wo, aes(x="",y=YearsWithCurrManager,fill=as.factor(Attrition)))+ geom_boxplot(width=0.1)+num_box, 
          align = "v",ncol = 1)

plot_grid(ggplot(emp_all_data_wo, aes(Avg_time_in_office,fill=as.factor(Attrition)))+ geom_bar(position = 'fill'),
          ggplot(emp_all_data_wo, aes(x="",y=Avg_time_in_office,fill=as.factor(Attrition)))+ geom_boxplot(width=0.1)+num_box, 
          align = "v",ncol = 1)

plot_grid(ggplot(emp_all_data_wo, aes(Avg_time_in_office_first_qrt,fill=as.factor(Attrition)))+ geom_bar(position = 'fill'),
          ggplot(emp_all_data_wo, aes(x="",y=Avg_time_in_office_first_qrt,fill=as.factor(Attrition)))+ geom_boxplot(width=0.1)+num_box, 
          align = "v",ncol = 1)

plot_grid(ggplot(emp_all_data_wo, aes(Avg_time_in_office_second_qrt,fill=as.factor(Attrition)))+ geom_bar(position = 'fill'),
          ggplot(emp_all_data_wo, aes(x="",y=Avg_time_in_office_second_qrt,fill=as.factor(Attrition)))+ geom_boxplot(width=0.1)+num_box, 
          align = "v",ncol = 1)

plot_grid(ggplot(emp_all_data_wo, aes(Avg_time_in_office_third_qrt,fill=as.factor(Attrition)))+ geom_bar(position = 'fill'),
          ggplot(emp_all_data_wo, aes(x="",y=Avg_time_in_office_third_qrt,fill=as.factor(Attrition)))+ geom_boxplot(width=0.1)+num_box, 
          align = "v",ncol = 1)

plot_grid(ggplot(emp_all_data_wo, aes(Avg_time_in_office_fourth_qrt,fill=as.factor(Attrition)))+ geom_bar(position = 'fill'),
          ggplot(emp_all_data_wo, aes(x="",y=Avg_time_in_office_fourth_qrt,fill=as.factor(Attrition)))+ geom_boxplot(width=0.1)+num_box, 
          align = "v",ncol = 1)

### Correlation between numeric variables

emp_all_data_num_corr <- round(cor(emp_all_data_wo[, c("Age", "DistanceFromHome", "MonthlyIncome", "NumCompaniesWorked", "PercentSalaryHike", 
                        "TotalWorkingYears","TrainingTimesLastYear","YearsAtCompany","YearsSinceLastPromotion","YearsWithCurrManager","Avg_time_in_office","Avg_time_in_office_first_qrt","Avg_time_in_office_second_qrt","Avg_time_in_office_third_qrt","Avg_time_in_office_fourth_qrt")]),2)

# We observed high correlation in the following factors:
# YearWithCurrManager,YearsAtCompany  -> corr = 0.76
# Age,TotalWorkingYears - > corr = 0.68
# YearsSinceLastPromotion, YearsAtCompany -> corr = 0.61
# As expected Avg_time_in_office, Avg_time_in_office_first_qrt, Avg_time_in_office_second_qrt, Avg_time_in_office_third_qrt, Avg_time_in_office_fourth_qrt are highly correlated.

# We observed that for few continuous variables we can bin them into different segments by categorizing them.
# 
# Categorize Age into different groups.
emp_all_data_wo$Age <- ifelse(emp_all_data_wo$Age < 30, "YOUNG", ifelse(emp_all_data_wo$Age >= 30 & emp_all_data_wo$Age <= 45, "MIDAGE", "OLD"))
emp_all_data_wo$Age <- as.factor(emp_all_data_wo$Age) # convert from character to factor.

# Categorize DistanceFromHome into different groups.
emp_all_data_wo$DistanceFromHome <- ifelse(emp_all_data_wo$DistanceFromHome < 5, "NEAR", ifelse(emp_all_data_wo$DistanceFromHome >= 5 & emp_all_data_wo$DistanceFromHome <= 15, "NORMAL", "FAR"))
emp_all_data_wo$DistanceFromHome <- as.factor(emp_all_data_wo$DistanceFromHome) # convert from character to factor.

# Categorize number of companies worked into different groups. 
emp_all_data_wo$NumCompaniesWorked <- ifelse(emp_all_data_wo$NumCompaniesWorked == 0, "FRESHER", ifelse((emp_all_data_wo$TotalWorkingYears/emp_all_data_wo$NumCompaniesWorked) <= 5, "HOPPER", "STABLE"))
emp_all_data_wo$NumCompaniesWorked <- as.factor(emp_all_data_wo$NumCompaniesWorked) # convert from character to factor.

# Categorize total working years ininto different groups.
emp_all_data_wo$TotalWorkingYears <- ifelse(emp_all_data_wo$TotalWorkingYears < 5, "LOWEXP", ifelse(emp_all_data_wo$TotalWorkingYears >= 5 & emp_all_data_wo$TotalWorkingYears <= 15, "MIDEXP", "HIGHEXP"))
emp_all_data_wo$TotalWorkingYears <- as.factor(emp_all_data_wo$TotalWorkingYears) # convert from character to factor.

# Categorize working years in company into different groups.                                        
emp_all_data_wo$YearsAtCompany <- ifelse(emp_all_data_wo$YearsAtCompany < 5, "JUNIOR", ifelse(emp_all_data_wo$YearsAtCompany >= 5 & emp_all_data_wo$YearsAtCompany <= 15, "SENIOR", "VSENIOR"))
emp_all_data_wo$YearsAtCompany <- as.factor(emp_all_data_wo$YearsAtCompany) # convert from character to factor.

# Categorize percent salary hike into different groups.                                                                           
emp_all_data_wo$PercentSalaryHike <- ifelse(emp_all_data_wo$PercentSalaryHike < 15, "LOW", ifelse(emp_all_data_wo$PercentSalaryHike >= 15 & emp_all_data_wo$PercentSalaryHike <= 20, "MEDIUM", "HIGH"))
emp_all_data_wo$PercentSalaryHike <- as.factor(emp_all_data_wo$PercentSalaryHike) # convert from character to factor.

# Categorize total years since last promotion into different groups.                                                                                                                      
emp_all_data_wo$YearsSinceLastPromotion <- ifelse(emp_all_data_wo$YearsSinceLastPromotion < 5, "RECENT", ifelse(emp_all_data_wo$YearsSinceLastPromotion >= 5 & emp_all_data_wo$YearsSinceLastPromotion <= 15, "DUE", "OVERDUE"))
emp_all_data_wo$YearsSinceLastPromotion <- as.factor(emp_all_data_wo$YearsSinceLastPromotion) # convert from character to factor.

# Categorize total years with current manager into different groups.
emp_all_data_wo$YearsWithCurrManager <- ifelse(emp_all_data_wo$YearsWithCurrManager < 5, "SHORT", ifelse(emp_all_data_wo$YearsWithCurrManager >= 5 & emp_all_data_wo$YearsWithCurrManager <= 15, "MEDIUM", "LONG"))
emp_all_data_wo$YearsWithCurrManager <- as.factor(emp_all_data_wo$YearsWithCurrManager) # convert from character to factor.

# Categorize per annum trainings into different groups.
emp_all_data_wo$TrainingTimesLastYear <- ifelse(emp_all_data_wo$TrainingTimesLastYear < 2, "LESS", ifelse(emp_all_data_wo$TrainingTimesLastYear >= 2 & emp_all_data_wo$TrainingTimesLastYear <= 4, "REGULAR", "MORE"))
emp_all_data_wo$TrainingTimesLastYear <- as.factor(emp_all_data_wo$TrainingTimesLastYear) # convert from character to factor.

##############################################################################################################################
# Feature standardisation

# We convert target variable - Attrition from char to factorwith levels 0/1 
emp_all_data_wo$Attrition <- ifelse(emp_all_data_wo$Attrition=="Yes",1,0)

# We Check Attrition rate of the employee
attrition_rate <- sum(emp_all_data_wo$Attrition)/nrow(emp_all_data_wo)
attrition_rate # 16.35% employee attrition rate.

# We scale all the continuousvariable - Monthly Income as varies in a wide range but this variable is an important parameter to decide attrition.
# Also we nomalised Avg_time_in_office and Avg_time_in_office in different quarters.

emp_all_data_wo$MonthlyIncome <- scale(emp_all_data_wo$MonthlyIncome)
emp_all_data_wo$Avg_time_in_office <- scale(emp_all_data_wo$Avg_time_in_office)
emp_all_data_wo$Avg_time_in_office_first_qrt <- scale(emp_all_data_wo$Avg_time_in_office_first_qrt)
emp_all_data_wo$Avg_time_in_office_second_qrt <- scale(emp_all_data_wo$Avg_time_in_office_second_qrt)
emp_all_data_wo$Avg_time_in_office_third_qrt <- scale(emp_all_data_wo$Avg_time_in_office_third_qrt)
emp_all_data_wo$Avg_time_in_office_fourth_qrt <- scale(emp_all_data_wo$Avg_time_in_office_fourth_qrt)

# Creating dataframe for factor variables

emp_all_data_wo_catg <- data.frame(emp_all_data_wo[,-c(1,6,16,27,28,29,30,31)])

# We will create dummy variables from factor variables having 2 or more number of levels
dummy_var <- data.frame(sapply(emp_all_data_wo_catg, 
                            function(x) data.frame(model.matrix(~x-1,data =emp_all_data_wo))[,-1]))

# combining dummy_var with rest of the numerical variables
emp_all_data_norm <- cbind(emp_all_data_wo[,c("Attrition","MonthlyIncome","Avg_time_in_office","Avg_time_in_office_first_qrt","Avg_time_in_office_second_qrt","Avg_time_in_office_third_qrt","Avg_time_in_office_fourth_qrt")],dummy_var)

# Now we will run logistic regression algorith on the final dataset-emp_all_data_norm 
# It has 3969 observation and 68 variables

##-----------------End of Data Preparation-----------------------------------------------------------------------------------##
#----------------------------------------------------------------------------------------------------------------------------##
# PREPARE TRAINING AND TEST DATA
#----------------------------------------------------------------------------------------------------------------------------##
# We split the data between train_data and test_data
set.seed(100)

indices_attrn = sample.split(emp_all_data_norm$Attrition, SplitRatio = 0.7)

train_data = emp_all_data_norm[indices_attrn,]

test_data = emp_all_data_norm[!(indices_attrn),]
#----------------------------------------------------------------------------------------------------------------------------##
# BUILD LOGISTIC REGRESSION MODEL
#----------------------------------------------------------------------------------------------------------------------------##
# Base model for logistic regression

model_1_emp = glm(Attrition~.,data = train_data,family = 'binomial')
summary(model_1_emp)

#summary :  model_1_emp 
# Null deviance: 2474.1  on 2777  degrees of freedom
# Residual deviance: 1808.5  on 2710  degrees of freedom
# AIC: 1944.5

# There are multiple variable in the model which do not have any significance 
# We now run StepAIC on the base model to eliminate insignificant variables from the dataset
model_2_emp = stepAIC(model_1_emp,direction = 'both')
summary(model_2_emp)

#Summary : model_2_emp    
# Null deviance: 2474.1  on 2777  degrees of freedom
# Residual deviance: 1826.4  on 2743  degrees of freedom
# AIC: 1896.4

vif(model_2_emp)
#Variables with high VIF are highly significant.Hence in this step we are ignoring VIF values.
#EnvironmentSatisfaction.xVery.High is insignificant .Hence removing it to create a new model.

model_3_emp = glm(formula = Attrition ~ Avg_time_in_office_second_qrt + EnvironmentSatisfaction.xLow + 
                    JobSatisfaction.xLow + 
                    JobSatisfaction.xVery.High + WorkLifeBalance.xBest + WorkLifeBalance.xBetter + 
                    WorkLifeBalance.xGood + Age.xOLD + Age.xYOUNG + BusinessTravel.xTravel_Frequently + 
                    BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                    Department.xSales + Education.xCollege + JobLevel.x2 + JobLevel.x5 + 
                    JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                    JobRole.xSales.Representative + MaritalStatus.xMarried + 
                    MaritalStatus.xSingle + NumCompaniesWorked.xHOPPER + NumCompaniesWorked.xSTABLE + 
                    TotalWorkingYears.xLOWEXP + TotalWorkingYears.xMIDEXP + TrainingTimesLastYear.xMORE + 
                    YearsAtCompany.xSENIOR + YearsSinceLastPromotion + JobInvolvement.xLow + 
                    JobInvolvement.xMedium + JobInvolvement.xVery.High + EducationField.xLife.Sciences, 
                  family = "binomial", data = train_data)
summary(model_3_emp)

#summary: model_3_emp
# Null deviance: 2474.1  on 2777  degrees of freedom
# Residual deviance: 1828.7  on 2744  degrees of freedom
# AIC: 1896.7

vif(model_3_emp)

#Variables with high VIF are highly significant.Hence in this step we are ignoring VIF values.
#EducationField.xLife.Sciences is insignificant .Hence removing it to create a new model.


model_4_emp <- glm(formula = Attrition ~ Avg_time_in_office_second_qrt + EnvironmentSatisfaction.xLow + 
                     JobSatisfaction.xLow + 
                     JobSatisfaction.xVery.High + WorkLifeBalance.xBest + WorkLifeBalance.xBetter + 
                     WorkLifeBalance.xGood + Age.xOLD + Age.xYOUNG + BusinessTravel.xTravel_Frequently + 
                     BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                     Department.xSales + Education.xCollege + JobLevel.x2 + JobLevel.x5 + 
                     JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                     JobRole.xSales.Representative + MaritalStatus.xMarried + 
                     MaritalStatus.xSingle + NumCompaniesWorked.xHOPPER + NumCompaniesWorked.xSTABLE + 
                     TotalWorkingYears.xLOWEXP + TotalWorkingYears.xMIDEXP + TrainingTimesLastYear.xMORE + 
                     YearsAtCompany.xSENIOR + YearsSinceLastPromotion + JobInvolvement.xLow + 
                     JobInvolvement.xMedium + JobInvolvement.xVery.High, 
                   family = "binomial", data = train_data)
summary(model_4_emp)

#summary : model_4_emp
# Null deviance: 2474.1  on 2777  degrees of freedom
# Residual deviance: 1830.9  on 2745  degrees of freedom
# AIC: 1896.9

vif(model_4_emp)
#Variables with high VIF are highly significant.Hence in this step we are ignoring VIF values.
#JobLevel.x2 is insignificant .Hence removing it to create a new model.

model_5_emp <- glm(formula = Attrition ~ Avg_time_in_office_second_qrt + EnvironmentSatisfaction.xLow + 
                     JobSatisfaction.xLow + 
                     JobSatisfaction.xVery.High + WorkLifeBalance.xBest + WorkLifeBalance.xBetter + 
                     WorkLifeBalance.xGood + Age.xOLD + Age.xYOUNG + BusinessTravel.xTravel_Frequently + 
                     BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                     Department.xSales + Education.xCollege + JobLevel.x5 + 
                     JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                     JobRole.xSales.Representative + MaritalStatus.xMarried + 
                     MaritalStatus.xSingle + NumCompaniesWorked.xHOPPER + NumCompaniesWorked.xSTABLE + 
                     TotalWorkingYears.xLOWEXP + TotalWorkingYears.xMIDEXP + TrainingTimesLastYear.xMORE + 
                     YearsAtCompany.xSENIOR + YearsSinceLastPromotion + JobInvolvement.xLow + 
                     JobInvolvement.xMedium + JobInvolvement.xVery.High, 
                   family = "binomial", data = train_data)
summary(model_5_emp)
#Summary:
# Null deviance: 2474.1  on 2777  degrees of freedom
# Residual deviance: 1833.4  on 2746  degrees of freedom
# AIC: 1897.4

vif(model_5_emp)

#Variables with high VIF are highly significant.Hence in this step we are ignoring VIF values.
#Age.xOLD is less significant .Hence removing it to create a new model.

model_6_emp <- glm(formula = Attrition ~ Avg_time_in_office_second_qrt + EnvironmentSatisfaction.xLow + 
                     JobSatisfaction.xLow + 
                     JobSatisfaction.xVery.High + WorkLifeBalance.xBest + WorkLifeBalance.xBetter + 
                     WorkLifeBalance.xGood + Age.xYOUNG + BusinessTravel.xTravel_Frequently + 
                     BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                     Department.xSales + Education.xCollege + JobLevel.x5 + 
                     JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                     JobRole.xSales.Representative + MaritalStatus.xMarried + 
                     MaritalStatus.xSingle + NumCompaniesWorked.xHOPPER + NumCompaniesWorked.xSTABLE + 
                     TotalWorkingYears.xLOWEXP + TotalWorkingYears.xMIDEXP + TrainingTimesLastYear.xMORE + 
                     YearsAtCompany.xSENIOR + YearsSinceLastPromotion + JobInvolvement.xLow + 
                     JobInvolvement.xMedium + JobInvolvement.xVery.High, 
                   family = "binomial", data = train_data)
summary(model_6_emp)
#Summary:
# Null deviance: 2474.1  on 2777  degrees of freedom
# Residual deviance: 1836.5  on 2747  degrees of freedom
# AIC: 1898.5

vif(model_6_emp)

#Variables with high VIF are highly significant.Hence in this step we are ignoring VIF values.
#JobLevel.x5 is less significant .Hence removing it to create a new model.

model_7_emp <- glm(formula = Attrition ~ Avg_time_in_office_second_qrt + EnvironmentSatisfaction.xLow + 
                     JobSatisfaction.xLow + 
                     JobSatisfaction.xVery.High + WorkLifeBalance.xBest + WorkLifeBalance.xBetter + 
                     WorkLifeBalance.xGood + Age.xYOUNG + BusinessTravel.xTravel_Frequently + 
                     BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                     Department.xSales + Education.xCollege + 
                     JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                     JobRole.xSales.Representative + MaritalStatus.xMarried + 
                     MaritalStatus.xSingle + NumCompaniesWorked.xHOPPER + NumCompaniesWorked.xSTABLE + 
                     TotalWorkingYears.xLOWEXP + TotalWorkingYears.xMIDEXP + TrainingTimesLastYear.xMORE + 
                     YearsAtCompany.xSENIOR + YearsSinceLastPromotion + JobInvolvement.xLow + 
                     JobInvolvement.xMedium + JobInvolvement.xVery.High, 
                   family = "binomial", data = train_data)
summary(model_7_emp)
#Summary:
# Null deviance: 2474.1  on 2777  degrees of freedom
# Residual deviance: 1840.1  on 2748  degrees of freedom
# AIC: 1900.1

vif(model_7_emp)

#Variables with high VIF are highly significant.Hence in this step we are ignoring VIF values.
#YearsAtCompany.xSENIOR is less significant .Hence removing it to create a new model.

model_8_emp <- glm(formula = Attrition ~ Avg_time_in_office_second_qrt + EnvironmentSatisfaction.xLow + 
                     JobSatisfaction.xLow + 
                     JobSatisfaction.xVery.High + WorkLifeBalance.xBest + WorkLifeBalance.xBetter + 
                     WorkLifeBalance.xGood + Age.xYOUNG + BusinessTravel.xTravel_Frequently + 
                     BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                     Department.xSales + Education.xCollege + 
                     JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                     JobRole.xSales.Representative + MaritalStatus.xMarried + 
                     MaritalStatus.xSingle + NumCompaniesWorked.xHOPPER + NumCompaniesWorked.xSTABLE + 
                     TotalWorkingYears.xLOWEXP + TotalWorkingYears.xMIDEXP + TrainingTimesLastYear.xMORE + 
                     YearsSinceLastPromotion + JobInvolvement.xLow + 
                     JobInvolvement.xMedium + JobInvolvement.xVery.High, 
                   family = "binomial", data = train_data)
summary(model_8_emp)
#Summary:
# Null deviance: 2474.1  on 2777  degrees of freedom
# Residual deviance: 1843.7  on 2749  degrees of freedom
# AIC: 1901.7

vif(model_8_emp)

#Variables with high VIF are highly significant.Hence in this step we are ignoring VIF values.
#NumCompaniesWorked.xSTABLE is less significant .Hence removing it to create a new model.

model_9_emp <- glm(formula = Attrition ~ Avg_time_in_office_second_qrt + EnvironmentSatisfaction.xLow + 
                     JobSatisfaction.xLow + 
                     JobSatisfaction.xVery.High + WorkLifeBalance.xBest + WorkLifeBalance.xBetter + 
                     WorkLifeBalance.xGood + Age.xYOUNG + BusinessTravel.xTravel_Frequently + 
                     BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                     Department.xSales + Education.xCollege + 
                     JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                     JobRole.xSales.Representative + MaritalStatus.xMarried + 
                     MaritalStatus.xSingle + NumCompaniesWorked.xHOPPER +  
                     TotalWorkingYears.xLOWEXP + TotalWorkingYears.xMIDEXP + TrainingTimesLastYear.xMORE + 
                     YearsSinceLastPromotion + JobInvolvement.xLow + 
                     JobInvolvement.xMedium + JobInvolvement.xVery.High, 
                   family = "binomial", data = train_data)

summary(model_9_emp)
#Summary:
# Null deviance: 2474.1  on 2777  degrees of freedom
# Residual deviance: 1847.1  on 2750  degrees of freedom
# AIC: 1903.1

vif(model_9_emp)

#Variables with high VIF are highly significant.Hence in this step we are ignoring VIF values.
# JobInvolvement.xMedium is also less significant .Hence removing it to create a new model.

model_10_emp <- glm(formula = Attrition ~ Avg_time_in_office_second_qrt + EnvironmentSatisfaction.xLow + 
                      JobSatisfaction.xLow + 
                      JobSatisfaction.xVery.High + WorkLifeBalance.xBest + WorkLifeBalance.xBetter + 
                      WorkLifeBalance.xGood + Age.xYOUNG + BusinessTravel.xTravel_Frequently + 
                      BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                      Department.xSales + Education.xCollege + 
                      JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                      JobRole.xSales.Representative + MaritalStatus.xMarried + 
                      MaritalStatus.xSingle + NumCompaniesWorked.xHOPPER +  
                      TotalWorkingYears.xLOWEXP + TotalWorkingYears.xMIDEXP + TrainingTimesLastYear.xMORE + 
                      YearsSinceLastPromotion + JobInvolvement.xLow + 
                      JobInvolvement.xVery.High, 
                    family = "binomial", data = train_data)

summary(model_10_emp)
#Summary:
# Null deviance: 2474.1  on 2777  degrees of freedom
# Residual deviance: 1851.1  on 2751  degrees of freedom
# AIC: 1905.1

vif(model_10_emp)

# Variable with high VIF value are highly significant. Hence ignoring VIF here.
# JobInvolvement.xVery.High is less significant. Hence removing it to create a new model.

model_11_emp <- glm(formula = Attrition ~ Avg_time_in_office_second_qrt + EnvironmentSatisfaction.xLow + 
                      JobSatisfaction.xLow + 
                      JobSatisfaction.xVery.High + WorkLifeBalance.xBest + WorkLifeBalance.xBetter + 
                      WorkLifeBalance.xGood + Age.xYOUNG + BusinessTravel.xTravel_Frequently + 
                      BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                      Department.xSales + Education.xCollege + 
                      JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                      JobRole.xSales.Representative + MaritalStatus.xMarried + 
                      MaritalStatus.xSingle + NumCompaniesWorked.xHOPPER +  
                      TotalWorkingYears.xLOWEXP + TotalWorkingYears.xMIDEXP + TrainingTimesLastYear.xMORE + 
                      YearsSinceLastPromotion + JobInvolvement.xLow,
                      family = "binomial", data = train_data)


summary(model_11_emp)
#Summary:
# Null deviance: 2474.1  on 2777  degrees of freedom
# Residual deviance: 1854.0  on 2752  degrees of freedom
# AIC: 1906

vif(model_11_emp)

#Variables with high VIF are highly significant.Hence in this step we are ignoring VIF values.
# JobRole.xManufacturing.Director is less significant .Hence removing it to create a new model.

model_12_emp <- glm(formula = Attrition ~ Avg_time_in_office_second_qrt + EnvironmentSatisfaction.xLow + 
                      JobSatisfaction.xLow + 
                      JobSatisfaction.xVery.High + WorkLifeBalance.xBest + WorkLifeBalance.xBetter + 
                      WorkLifeBalance.xGood + Age.xYOUNG + BusinessTravel.xTravel_Frequently + 
                      BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                      Department.xSales + Education.xCollege + 
                      JobRole.xManager + JobRole.xResearch.Director + 
                      JobRole.xSales.Representative + MaritalStatus.xMarried + 
                      MaritalStatus.xSingle + NumCompaniesWorked.xHOPPER +  
                      TotalWorkingYears.xLOWEXP + TotalWorkingYears.xMIDEXP + TrainingTimesLastYear.xMORE + 
                      YearsSinceLastPromotion + JobInvolvement.xLow,
                    family = "binomial", data = train_data)

summary(model_12_emp)
#Summary: 
# Null deviance: 2474.1  on 2777  degrees of freedom
# Residual deviance: 1858.6  on 2753  degrees of freedom
# AIC: 1908.6

vif(model_12_emp)

# JobRole.xSales.Representative  is less significant. Hence removing it.

model_13_emp <- glm(formula = Attrition ~ Avg_time_in_office_second_qrt + EnvironmentSatisfaction.xLow + 
                      JobSatisfaction.xLow + 
                      JobSatisfaction.xVery.High + WorkLifeBalance.xBest + WorkLifeBalance.xBetter + 
                      WorkLifeBalance.xGood + Age.xYOUNG + BusinessTravel.xTravel_Frequently + 
                      BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                      Department.xSales + Education.xCollege + 
                      JobRole.xManager + JobRole.xResearch.Director + 
                      MaritalStatus.xMarried + 
                      MaritalStatus.xSingle + NumCompaniesWorked.xHOPPER +  
                      TotalWorkingYears.xLOWEXP + TotalWorkingYears.xMIDEXP + TrainingTimesLastYear.xMORE + 
                      YearsSinceLastPromotion + JobInvolvement.xLow,
                    family = "binomial", data = train_data)
summary(model_13_emp)

#Summary :
# Null deviance: 2474.1  on 2777  degrees of freedom
# Residual deviance: 1863.3  on 2754  degrees of freedom
# AIC: 1911.3

vif(model_13_emp)

#JobRole.xManager is less significant.Hence removing it.

model_14_emp <- glm(formula = Attrition ~ Avg_time_in_office_second_qrt + EnvironmentSatisfaction.xLow + 
                      JobSatisfaction.xLow + 
                      JobSatisfaction.xVery.High + WorkLifeBalance.xBest + WorkLifeBalance.xBetter + 
                      WorkLifeBalance.xGood + Age.xYOUNG + BusinessTravel.xTravel_Frequently + 
                      BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                      Department.xSales + Education.xCollege + 
                      JobRole.xResearch.Director + 
                      MaritalStatus.xMarried + 
                      MaritalStatus.xSingle + NumCompaniesWorked.xHOPPER +  
                      TotalWorkingYears.xLOWEXP + TotalWorkingYears.xMIDEXP + TrainingTimesLastYear.xMORE + 
                      YearsSinceLastPromotion + JobInvolvement.xLow,
                    family = "binomial", data = train_data)
summary(model_14_emp)

#Summary :
# Null deviance: 2474.1  on 2777  degrees of freedom
# Residual deviance: 1868.3  on 2755  degrees of freedom
# AIC: 1914.3

vif(model_14_emp)

#BusinessTravel.xTravel_Rarely has high VIF and less significant. Hence removing it to create new  model.

model_15_emp <- glm(formula = Attrition ~ Avg_time_in_office_second_qrt + EnvironmentSatisfaction.xLow + 
                      JobSatisfaction.xLow + 
                      JobSatisfaction.xVery.High + WorkLifeBalance.xBest + WorkLifeBalance.xBetter + 
                      WorkLifeBalance.xGood + Age.xYOUNG + BusinessTravel.xTravel_Frequently + 
                      Department.xResearch...Development + 
                      Department.xSales + Education.xCollege + 
                      JobRole.xResearch.Director + 
                      MaritalStatus.xMarried + 
                      MaritalStatus.xSingle + NumCompaniesWorked.xHOPPER +  
                      TotalWorkingYears.xLOWEXP + TotalWorkingYears.xMIDEXP + TrainingTimesLastYear.xMORE + 
                      YearsSinceLastPromotion + JobInvolvement.xLow,
                    family = "binomial", data = train_data)
summary(model_15_emp)

#Summary: 
# Null deviance: 2474.1  on 2777  degrees of freedom
# Residual deviance: 1874.3  on 2756  degrees of freedom
# AIC: 1918.3

vif(model_15_emp)

# All variables with high VIF are highliy significant. Hence ignoring VIF values here.
# TrainingTimesLastYear.xMORE is  less significant. Hence removing it to build new model.

model_16_emp <- glm(formula = Attrition ~ Avg_time_in_office_second_qrt + EnvironmentSatisfaction.xLow + 
                      JobSatisfaction.xLow + 
                      JobSatisfaction.xVery.High + WorkLifeBalance.xBest + WorkLifeBalance.xBetter + 
                      WorkLifeBalance.xGood + Age.xYOUNG + BusinessTravel.xTravel_Frequently + 
                      Department.xResearch...Development + 
                      Department.xSales + Education.xCollege + 
                      JobRole.xResearch.Director + 
                      MaritalStatus.xMarried + 
                      MaritalStatus.xSingle + NumCompaniesWorked.xHOPPER +  
                      TotalWorkingYears.xLOWEXP + TotalWorkingYears.xMIDEXP + 
                      YearsSinceLastPromotion + JobInvolvement.xLow,
                    family = "binomial", data = train_data)
summary(model_16_emp)

#summary :
# Null deviance: 2474.1  on 2777  degrees of freedom
# Residual deviance: 1880.2  on 2757  degrees of freedom
# AIC: 1922.7

vif(model_16_emp)

#Education.xCollege is  less significant. Hence removing it.

model_17_emp <- glm(formula = Attrition ~ Avg_time_in_office_second_qrt + EnvironmentSatisfaction.xLow + 
                      JobSatisfaction.xLow + 
                      JobSatisfaction.xVery.High + WorkLifeBalance.xBest + WorkLifeBalance.xBetter + 
                      WorkLifeBalance.xGood + Age.xYOUNG + BusinessTravel.xTravel_Frequently + 
                      Department.xResearch...Development + 
                      Department.xSales + JobInvolvement.xLow +
                      JobRole.xResearch.Director + 
                      MaritalStatus.xMarried + 
                      MaritalStatus.xSingle + NumCompaniesWorked.xHOPPER +  
                      TotalWorkingYears.xLOWEXP + TotalWorkingYears.xMIDEXP + 
                      YearsSinceLastPromotion,
                    family = "binomial", data = train_data)
summary(model_17_emp)
# Null deviance: 2474.1  on 2777  degrees of freedom
# Residual deviance: 1887.5  on 2758  degrees of freedom
# AIC: 1927.5

vif(model_17_emp)

# MaritalStatus.xMarried is less significant and has high VIF.Hence removing it.
model_18_emp <- glm(formula = Attrition ~ Avg_time_in_office_second_qrt + EnvironmentSatisfaction.xLow + 
                      JobSatisfaction.xLow + 
                      JobSatisfaction.xVery.High + WorkLifeBalance.xBest + WorkLifeBalance.xBetter + 
                      WorkLifeBalance.xGood + Age.xYOUNG + BusinessTravel.xTravel_Frequently + 
                      Department.xResearch...Development + 
                      Department.xSales + JobInvolvement.xLow +
                      JobRole.xResearch.Director + 
                      MaritalStatus.xSingle + NumCompaniesWorked.xHOPPER +  
                      TotalWorkingYears.xLOWEXP + TotalWorkingYears.xMIDEXP + 
                      YearsSinceLastPromotion,
                    family = "binomial", data = train_data)
summary(model_18_emp)

#Summary:
# Null deviance: 2474.1  on 2777  degrees of freedom
# Residual deviance: 1896.1  on 2759  degrees of freedom
# AIC: 1934.1

vif(model_18_emp)

#JobInvolvement.xLow is less significant.Hence removing it.

model_19_emp <- glm(formula = Attrition ~ Avg_time_in_office_second_qrt + EnvironmentSatisfaction.xLow + 
                      JobSatisfaction.xLow + 
                      JobSatisfaction.xVery.High + WorkLifeBalance.xBest + WorkLifeBalance.xBetter + 
                      WorkLifeBalance.xGood + Age.xYOUNG + BusinessTravel.xTravel_Frequently + 
                      Department.xResearch...Development + 
                      Department.xSales +  
                      JobRole.xResearch.Director + 
                      MaritalStatus.xSingle + NumCompaniesWorked.xHOPPER +  
                      TotalWorkingYears.xLOWEXP + TotalWorkingYears.xMIDEXP + 
                      YearsSinceLastPromotion,
                    family = "binomial", data = train_data)
summary(model_19_emp)

#Summary:
#Null deviance: 2474.1  on 2777  degrees of freedom
#Residual deviance: 1904.1  on 2760  degrees of freedom
#AIC: 1940.1
vif(model_19_emp)

#All the variables are  significant. But there is multicolinearity.
# Department.xSales has high VIF. Hence removing it.
model_20_emp <- glm(formula = Attrition ~ Avg_time_in_office_second_qrt + EnvironmentSatisfaction.xLow + 
                      JobSatisfaction.xLow + 
                      JobSatisfaction.xVery.High + WorkLifeBalance.xBest + WorkLifeBalance.xBetter + 
                      WorkLifeBalance.xGood + Age.xYOUNG + BusinessTravel.xTravel_Frequently + 
                      Department.xResearch...Development + 
                      JobRole.xResearch.Director + 
                      MaritalStatus.xSingle + NumCompaniesWorked.xHOPPER +  
                      TotalWorkingYears.xLOWEXP + TotalWorkingYears.xMIDEXP + 
                      YearsSinceLastPromotion,
                    family = "binomial", data = train_data)
summary(model_20_emp)
#Summary
# Null deviance: 2474.1  on 2777  degrees of freedom
#Residual deviance: 1923.1  on 2761  degrees of freedom
# AIC: 1957.1
vif(model_20_emp)

#Department.xResearch...Development has become  insignificant. Hence removing it.
model_21_emp <- glm(formula = Attrition ~ Avg_time_in_office_second_qrt + EnvironmentSatisfaction.xLow + 
                      JobSatisfaction.xLow + 
                      JobSatisfaction.xVery.High + WorkLifeBalance.xBest + WorkLifeBalance.xBetter + 
                      WorkLifeBalance.xGood + Age.xYOUNG + BusinessTravel.xTravel_Frequently + 
                      JobRole.xResearch.Director + 
                      MaritalStatus.xSingle + NumCompaniesWorked.xHOPPER +  
                      TotalWorkingYears.xLOWEXP + TotalWorkingYears.xMIDEXP + 
                      YearsSinceLastPromotion,
                    family = "binomial", data = train_data)
summary(model_21_emp)
#Summary
#Null deviance: 2474.1  on 2777  degrees of freedom
#Residual deviance: 1923.2  on 2762  degrees of freedom
#AIC: 1955.2

vif(model_21_emp)

#JobRole.xResearch.Director  has  b.ecome less significant. Hence removing it.
model_22_emp <- glm(formula = Attrition ~ Avg_time_in_office_second_qrt + EnvironmentSatisfaction.xLow + 
                      JobSatisfaction.xLow + 
                      JobSatisfaction.xVery.High + WorkLifeBalance.xBest + WorkLifeBalance.xBetter + 
                      WorkLifeBalance.xGood + Age.xYOUNG + BusinessTravel.xTravel_Frequently + 
                      MaritalStatus.xSingle + NumCompaniesWorked.xHOPPER +  
                      TotalWorkingYears.xLOWEXP + TotalWorkingYears.xMIDEXP + 
                      YearsSinceLastPromotion,
                    family = "binomial", data = train_data)
summary(model_22_emp)
#Null deviance: 2474.1  on 2777  degrees of freedom
#Residual deviance: 1932.8  on 2763  degrees of freedom
#AIC: 1962.8

vif(model_22_emp)

#Though all the variables are significant now, they have multicollinearity.
#WorkLifeBalance.xBetter  has  high VIF. Hence removing it.
model_23_emp <- glm(formula = Attrition ~ Avg_time_in_office_second_qrt + EnvironmentSatisfaction.xLow + 
                      JobSatisfaction.xLow + 
                      JobSatisfaction.xVery.High + WorkLifeBalance.xBest +  
                      WorkLifeBalance.xGood + Age.xYOUNG + BusinessTravel.xTravel_Frequently + 
                      MaritalStatus.xSingle + NumCompaniesWorked.xHOPPER +  
                      TotalWorkingYears.xLOWEXP + TotalWorkingYears.xMIDEXP + 
                      YearsSinceLastPromotion,
                    family = "binomial", data = train_data)
summary(model_23_emp)

#Null deviance: 2474.1  on 2777  degrees of freedom
#Residual deviance: 1967.6  on 2764  degrees of freedom
#AIC: 1995.6

vif(model_23_emp)

#WorkLifeBalance.xBest is insignificant now. Hence removing it.

model_24_emp <- glm(formula = Attrition ~ Avg_time_in_office_second_qrt + EnvironmentSatisfaction.xLow + 
                      JobSatisfaction.xLow + 
                      JobSatisfaction.xVery.High +   
                      WorkLifeBalance.xGood + Age.xYOUNG + BusinessTravel.xTravel_Frequently + 
                      MaritalStatus.xSingle + NumCompaniesWorked.xHOPPER +  
                      TotalWorkingYears.xLOWEXP + TotalWorkingYears.xMIDEXP + 
                      YearsSinceLastPromotion,
                    family = "binomial", data = train_data)
summary(model_24_emp)
#Null deviance: 2474.1  on 2777  degrees of freedom
#Residual deviance: 1968.3  on 2765  degrees of freedom
#AIC: 1994.3

vif(model_24_emp)

#WorkLifeBalance.xGood is  insignificant now. Hence removing it.
model_25_emp <- glm(formula = Attrition ~ Avg_time_in_office_second_qrt + EnvironmentSatisfaction.xLow + 
                      JobSatisfaction.xLow + 
                      JobSatisfaction.xVery.High +   
                      Age.xYOUNG + BusinessTravel.xTravel_Frequently + 
                      MaritalStatus.xSingle + NumCompaniesWorked.xHOPPER +  
                      TotalWorkingYears.xLOWEXP + TotalWorkingYears.xMIDEXP + 
                      YearsSinceLastPromotion,
                    family = "binomial", data = train_data)
summary(model_25_emp)
#Null deviance: 2474.1  on 2777  degrees of freedom
#Residual deviance: 1970.3  on 2766  degrees of freedom
#AIC: 1994.3

vif(model_25_emp)

# All the variables are significant now, but there is multicollinearity.
# TotalWorkingYears.xLOWEXP has comparatively high VIF values. Hence removing it.

model_26_emp <- glm(formula = Attrition ~ Avg_time_in_office_second_qrt + EnvironmentSatisfaction.xLow + 
                      JobSatisfaction.xLow + 
                      JobSatisfaction.xVery.High +   
                      Age.xYOUNG + BusinessTravel.xTravel_Frequently + 
                      MaritalStatus.xSingle + NumCompaniesWorked.xHOPPER +  
                      TotalWorkingYears.xMIDEXP + 
                      YearsSinceLastPromotion,
                    family = "binomial", data = train_data)
summary(model_26_emp)
#Null deviance: 2474.1  on 2777  degrees of freedom
#Residual deviance: 2034.0  on 2767  degrees of freedom
#AIC: 2056

vif(model_26_emp)

# TotalWorkingYears.xMIDEXP has  become less significant. Hence removing it.
model_27_emp <- glm(formula = Attrition ~ Avg_time_in_office_second_qrt + EnvironmentSatisfaction.xLow + 
                      JobSatisfaction.xLow + 
                      JobSatisfaction.xVery.High +   
                      Age.xYOUNG + BusinessTravel.xTravel_Frequently + 
                      MaritalStatus.xSingle + NumCompaniesWorked.xHOPPER +  
                      YearsSinceLastPromotion,
                    family = "binomial", data = train_data)
summary(model_27_emp)

#Summary:
#Null deviance: 2474.1  on 2777  degrees of freedom
#Residual deviance: 2038.1  on 2768  degrees of freedom
#AIC: 2058.1

vif(model_27_emp)

# All variables are having low VIF. But JobSatisfaction.xVery.High is less significant.
# Hence removing it.

model_28_emp <- glm(formula = Attrition ~ Avg_time_in_office_second_qrt + EnvironmentSatisfaction.xLow + 
                      JobSatisfaction.xLow + 
                      Age.xYOUNG + BusinessTravel.xTravel_Frequently + 
                      MaritalStatus.xSingle + NumCompaniesWorked.xHOPPER +  
                      YearsSinceLastPromotion,
                    family = "binomial", data = train_data)
summary(model_28_emp)

#Summary:
#Null deviance: 2474.1  on 2777  degrees of freedom
#Residual deviance: 2047.2  on 2769  degrees of freedom
#AIC: 2065.2

vif(model_28_emp)

# Since there is very high correlation between average time in office and average time in office in second
# quarter. it is better to use that. Hence replacing it back and checking the model. 

model_29_emp <- glm(formula = Attrition ~ Avg_time_in_office + EnvironmentSatisfaction.xLow + 
                      JobSatisfaction.xLow + 
                      Age.xYOUNG + BusinessTravel.xTravel_Frequently + 
                      MaritalStatus.xSingle + NumCompaniesWorked.xHOPPER +  
                      YearsSinceLastPromotion,
                    family = "binomial", data = train_data)
summary(model_29_emp)

#Summary:
#Null deviance: 2474.1  on 2777  degrees of freedom
#Residual deviance: 2048.8  on 2769  degrees of freedom
#AIC: 2066.8

vif(model_29_emp) # all VIF values are low.

# Now all the variables are highly significant and have low VIF values. 
# Hence we  conclude this model_29_emp as  our final  model.

Final_model <- model_29_emp

#Final driving variables:

#Avg_time_in_office 
#EnvironmentSatisfaction.xLow 
#JobSatisfaction.xLow
#Age.xYOUNG
#BusinessTravel.xTravel_Frequently 
#MaritalStatus.xSingle
#NumCompaniesWorked.xHOPPER
#YearsSinceLastPromotion 

#----------------------------------------------------------------------------------------------------------------------------##
# MODEL TESTING AND VALIDATION
#----------------------------------------------------------------------------------------------------------------------------##

#Prediction of Employee attrition on test data set

emp_all_data_test_prediction <- predict(Final_model, type = "response", newdata = test_data[,-1])

#Summary of prediction:
summary(emp_all_data_test_prediction)

#Min.     1st Qu.   Median    Mean      3rd Qu.    Max. 
#0.01332  0.05298   0.11370   0.16471   0.21936    0.88351

#We merge predicted value to test_data set
test_data$Predicted_probality <- emp_all_data_test_prediction

#We need to find  out optimal cutoff points. Following function  will calculate  the optimal cutoff.
Actual_emp_attrition <- factor(ifelse(test_data$Attrition==1,"Yes","No"))
Find_cut_off_optimal <- function(cutoff_value) 
{
  predicted_emp_attrition <- factor(ifelse(emp_all_data_test_prediction >= cutoff_value, "Yes", "No"))
  attrition_confusion_matrix <- confusionMatrix(predicted_emp_attrition, Actual_emp_attrition, positive = "Yes")
  
  attrition_sensitivity <- attrition_confusion_matrix$byClass[1]
  attrition_specificity <- attrition_confusion_matrix$byClass[2]
  attrition_accuracy <- attrition_confusion_matrix$overall[1]
  
  emp_attrition_output <- t(as.matrix(c(attrition_sensitivity, attrition_specificity, attrition_accuracy))) 
  colnames(emp_attrition_output) <- c("sensitivity", "specificity", "accuracy")
  return(emp_attrition_output)
}

#Now  we will run this  function for a  sequence of  cutoff to  plot sigmoid  curve.

seq_initiation = seq(.01,.80,length=100)
out_dataset = matrix(0,100,3)

for(index_num in 1:100)
{
  out_dataset[index_num,] = Find_cut_off_optimal(seq_initiation[index_num])
} 

plot(seq_initiation, out_dataset[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(seq_initiation,out_dataset[,2],col="darkgreen",lwd=2)
lines(seq_initiation,out_dataset[,3],col=4,lwd=2)
box()
legend(-0.1,.001,col=c(2,"yellow",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

cutoff_prob_value <- seq_initiation[which(abs(out_dataset[,1]-out_dataset[,2])<0.1)]
cutoff_prob_value #0.1775758

#So we choose cut off as 0.1775758 in the model
emp_test_data_attrition_prediction <- factor(ifelse(emp_all_data_test_prediction >=0.1775758, "Yes", "No"))
#Now  we need to check conf
test_data_confusionmatrix <- confusionMatrix(emp_test_data_attrition_prediction, Actual_emp_attrition, positive = "Yes")

#Hence acuracy,sensitivity and specificity for the model is as follows:
accuracy_model <- test_data_confusionmatrix$overall[1]
accuracy_model #Accuracy -> 0.7271201

sensitivity_model <- test_data_confusionmatrix$byClass[1]
sensitivity_model #Sensitivity -> 0.6769231

specificity_model <- test_data_confusionmatrix$byClass[2]
specificity_model #Specificity -> 0.7369478

##---Calculation of KS  statistics  for the test dataset --------------------------------------------------------------------##

emp_test_data_attrition_prediction <- ifelse(emp_test_data_attrition_prediction=="Yes",1,0)
Actual_emp_attrition <- ifelse(Actual_emp_attrition=="Yes",1,0)

#We run prediction on emp_test_data_attrition_prediction and Actual_emp_attrition
pred_test_emp_model_data <- prediction(emp_test_data_attrition_prediction, Actual_emp_attrition)

#Performance of the model
emp_model_data_performance_measures_test<- performance(pred_test_emp_model_data, "tpr", "fpr")

ks_table_emp_model_test <- attr(emp_model_data_performance_measures_test, "y.values")[[1]] - 
  (attr(emp_model_data_performance_measures_test, "x.values")[[1]])

# Here we check the maximum KS  stat value
max(ks_table_emp_model_test) #KS Stat - > 0.4138709

##--------------Lift & Gain Chart  for the  model-----------------------------------------------------------------------------##
# 
# plotting the lift chart

find_logistic_model_lift <- function(var_name , predicted_prob_val,groups=10) {
  
  if(is.factor(var_name)) var_name  <- as.integer(as.character(var_name ))
  if(is.factor(predicted_prob_val)) predicted_prob_val <- as.integer(as.character(predicted_prob_val))
  helper = data.frame(cbind(var_name , predicted_prob_val))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob_val"], groups)
  gain_table = helper %>% group_by(bucket)  %>% summarise_at(vars(var_name ), funs(total = n(),totalresp=sum(., na.rm = TRUE))) %>%
           mutate(Cum_resp_val = cumsum(totalresp),
           Gain=Cum_resp_val/sum(totalresp)*100,
           Cum_lift=Gain/(bucket*(100/groups))) 
  return(gain_table)
}

Attrition_decile = find_logistic_model_lift(Actual_emp_attrition, emp_all_data_test_prediction, groups = 10)

#Result:
## A table: 10 x 6
#     bucket total  totalresp  Cum_resp_val  Gain    Cum_lift
#     <int> <int>      <dbl>       <dbl>     <dbl>    <dbl>
# 1      1   120        58           58      29.7     2.97
# 2      2   119        37           95      48.7     2.44
# 3      3   119        31          126      64.6     2.15
# 4      4   119        16          142      72.8     1.82
# 5      5   119        14          156      80       1.6 
# 6      6   119         7          163      83.6     1.39
# 7      7   119         8          171      87.7     1.25
# 8      8   119         9          180      92.3     1.15
# 9      9   119         5          185      94.9     1.05
#10     10   119        10          195      100      1   

# Gain graph
ggplot(Attrition_decile,aes(x=bucket,y=(Gain/100)))+geom_line()+ylab("Gain")

# Lift Graph
ggplot(Attrition_decile,aes(x=bucket,y=Cum_lift))+geom_line() +ylab("Lift")

# ROC Curve
ROCData <- data.frame(out_dataset)
ggplot(ROCData,aes(x=X1,y=1-(X2)))+geom_line() +
  ylab("sensitivity")+
  xlab("1- specificity")

#-----------------------------------------------------End of Model Evaluation-------------------------------------------------##
# Model Interpretation
# 1. Model has good level of Accuracy = 72.71% , Sensitivity = 67.69% and Specificity = 73.70% at optimal cut-off probability of 0.1775758
# 2. It has a decent Gain and Lift as compared to random model.
# 3. It has KS Statistics of 41.39% ( >= 40% ) at 3rd decile.
# Hence this model is quite stable and robust.
##-------------------------------------------------End of HR analytics Case Study---------------------------------------------##