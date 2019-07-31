###########################################################################################################################
#-----------------------CAPSTONE PROJECT---------------------------------------------------------------------------------##    
###########################################################################################################################
#-------------------------------------------------------------------------------------------------------------------------#
# PROCESS FLOW
#-------------------------------------------------------------------------------------------------------------------------#
# 1. Business Understanding
# 2. Data Understanding  
# 3. Data Preparation
# 4. Modelling
# 5. Model Evaluation
# 6. Application Score Card Preparation
# 7. Recommendations and Presentation to Management
#-------------------------------------------------------------------------------------------------------------------------#
# BUSINESS UNDERSTANDING
#-------------------------------------------------------------------------------------------------------------------------#
# CredX is a leading credit card provider that gets thousands of credit card applicants every year. 
# But in the past few years, it has experienced an increase in credit loss. The CEO believes that the best strategy to 
# mitigate credit risk is to ?acquire the right customers?.

# In this project, we will help CredX identify the right customers using predictive models. Using past data of the bank?s 
# applicants (demographic and credit bureau information), we will determine the factors affecting credit risk, create 
# strategies to mitigate the acquisition risk and assess the financial benefit. Accordingly we shall present the our 
# findings and recommendations to the management. 
#-------------------------------------------------------------------------------------------------------------------------#
# SET UP WORK DIRECTORY
#-------------------------------------------------------------------------------------------------------------------------#
setwd("D:/Chetan/PGDDS/Module 7 - Capstone Project")
#-------------------------------------------------------------------------------------------------------------------------#
# INSTALL PACKAGES AND LOAD REQUIRED LIBRARIES
#-------------------------------------------------------------------------------------------------------------------------# 
library(dplyr)
library(ggplot2)
library(tidyr)
library(gridExtra)
library(caret)
library(caTools)
library(e1071)
library(MASS)
#-------------------------------------------------------------------------------------------------------------------------#
# DATA SOURCING
#-------------------------------------------------------------------------------------------------------------------------#
bureau_data <- read.csv("Credit Bureau data.csv",stringsAsFactors = FALSE)
demographic_data <- read.csv("Demographic data.csv",stringsAsFactors = FALSE)
#-------------------------------------------------------------------------------------------------------------------------#
# DATA UNDERSTANDING, CLEANING AND PREPARATION
#-------------------------------------------------------------------------------------------------------------------------#

############################################### ATTENTION ################################################################
# Data set has performance tag indicating whether applicant defaulted or not. Tag 1 is default case and Tag 0 is 
# non-default case. Though out this entire code, we will follow this definition only.
##########################################################################################################################

# Check number of rows and columns for data set
dim(bureau_data) #71295 observation and 19 variables.
dim(demographic_data) #71295 observation and 12 variables.

# Check structure of data
str(bureau_data)
str(demographic_data)

# Checking the unique key - Application.ID and remove duplicate records
bureau_data <- bureau_data %>% distinct(Application.ID, .keep_all = TRUE)
demographic_data <- demographic_data %>% distinct(Application.ID, .keep_all = TRUE)

# 3 duplicate records are removed from each dataset. Total applicant counts now is 71292.

# Let's compare both datasets and verify that both datasets have identical applicants ids.
setdiff(bureau_data$Application.ID,demographic_data$Application.ID) #returns 0 - identical Application.ID across both the datasets.

# Creating single dataframe merging both datasets.
master_df <- merge(demographic_data,bureau_data,by="Application.ID")

# After merging, performance tag columns are duplicated. Let's remove one column for performance tag.
master_df <- master_df[,-12]

# Change the column name for performance tag
colnames(master_df)[29] <- "Performance.Tag"

summary(master_df) # There are 1425 records without performance tags. We shall drop these records.

# Dropping records having null/blank performance tags
master_df_with_pTag <- master_df[!is.na(master_df$Performance.Tag),] # Total records in dataframe now is 69867.

summary(master_df_with_pTag) # Verifying that all records are now having valid performance tag.

sum(is.na(master_df_with_pTag)) # Checking if any continuous variable has null value which needs treatment. 

# Avgas.CC.Utilization.in.last.12.months has 1023 NA values
# No.of.trades.opened.in.last.6.months has 1 NA value
# Presence.of.open.home.loan has 272 NA values
# Outstanding.Balance has 272 NA values
# No.of.dependents has 3 NA values

# Removing records having NA values.
master_df_with_pTag <- na.omit(master_df_with_pTag) # Remaining number of records in dataframe is now 68841.

sum(is.na(master_df_with_pTag)) # Verifying that there is no NA values left in the dataframe for continuous variables. 

sapply(master_df_with_pTag, function(x) unique(x)) # Checking unique values, particularly for categorical variables. 

# All the five categorical variables have blank values.Let's remove them as they are smaller in numbers.

master_df_with_pTag <- master_df_with_pTag[!(master_df_with_pTag$Gender == ""), ] # 2 records with blank Gender values removed.
master_df_with_pTag <- master_df_with_pTag[!(master_df_with_pTag$Education == ""), ] # 117 records with blank Education values removed.
master_df_with_pTag <- master_df_with_pTag[!(master_df_with_pTag$Profession == ""), ] # 13 records with blank Profession values removed.
master_df_with_pTag <- master_df_with_pTag[!(master_df_with_pTag$Type.of.residence == ""), ] # 8 records with blank Type of Residence values removed.
master_df_with_pTag <- master_df_with_pTag[!(master_df_with_pTag$Marital.Status..at.the.time.of.application. == ""), ] # 5 records with blak Marital Status values removed.

sapply(master_df_with_pTag, function(x) unique(x)) # Verifying replacement of blank values. 

# Check if any variable has zero or negative value which is unreasonable.
names(master_df_with_pTag)[sapply(master_df_with_pTag, function(x) min(x))<=0]

# Age and Income variable has zero or negative value which is data anamoly.Need to further confirm and remove them.

# Checking Age Variable for zero or negative value. 
master_df_with_pTag[which(master_df_with_pTag$Age<=0),] 

# There few rows with zero or negative Age values.Let's remove them.
master_df_with_pTag <- master_df_with_pTag %>% filter(Age>0) # 17 records removed.

# Checking Income Variable for zero or negative value. 
master_df_with_pTag[which(master_df_with_pTag$Income <=0),] 

# There few rows with zero or negative Income values.Let's remove them.
master_df_with_pTag <- master_df_with_pTag %>% filter(Income>0) # 104 records removed.

names(master_df_with_pTag)[sapply(master_df_with_pTag, function(x) min(x))<=0] # Verified that no zero or negative values are left for Age and Income.

# Converting categorical variable from Character to Factors.
fact_col <- c('Gender','Marital.Status..at.the.time.of.application.','Education','Profession','Type.of.residence')
master_df_with_pTag[fact_col] <- lapply(master_df_with_pTag[fact_col] , factor)

# Checking successful convertion to factors.
str(master_df_with_pTag)
dim(master_df_with_pTag) # Finally 68575 clean records with 29 variable remain in merged data set.

########### Let's now take each variable one by one, carry out exploratory analysis and treat outliers if required ####################

#----Let's take Age variable first-----------------------------------------------------------------------------------------------------

# Plotting age histogram
ggplot(master_df_with_pTag,aes(Age))+geom_histogram()  # data is widely distributed over age. 

# Let's check the outlier in the variables 
quantile(master_df_with_pTag$Age,seq(0,1,0.01))  
# 65 and below is the age with 99 percentile and maximum Age is 65. Hence there is no outlier.

# Box plot 
boxplot(master_df_with_pTag$Age)  # Box plot also confirms that there is no outliers.

# Binning the Age variable and store it into "Binning.Age".
master_df_with_pTag$Binning.Age <- as.factor(cut(master_df_with_pTag$Age, breaks = c(10,15, 20, 30, 40, 50, 60,70)))

sum(is.na(master_df_with_pTag$Binning.Age)) # Ensuring that no record is missed out for age binning. 

# Check the default rate in each bucket
agg_age <- merge(aggregate(Performance.Tag ~ Binning.Age, master_df_with_pTag, mean),aggregate(Performance.Tag ~ Binning.Age, master_df_with_pTag, sum),by = "Binning.Age") 

# Adding Number of Applicants
count <- data.frame(table(master_df_with_pTag$Binning.Age))
count <- count[,-1]
agg_age <- cbind(agg_age,count)

# changing column name of each variables in agg_age dataframe
colnames(agg_age) <- c("Age", "Default.Rate", "Count.Default","Number.of.Applicants")

# Round Off the values
agg_age$Default.Rate <- format(round(agg_age$Default.Rate, 3))
agg_age

#   Age      Default.Rate     Count.Default Number.of.Applicants
#1 (10,15]        0.000             0                   10
#2 (15,20]        0.029             3                  102
#3 (20,30]        0.041           233                 5671
#4 (30,40]        0.045           818                18334
#5 (40,50]        0.042           936                22457
#6 (50,60]        0.041           706                17239
#7 (60,70]        0.041           196                 4762


# Let's see the default rate of each age bucket in the plot
ggplot(agg_age, aes(Age, Number.of.Applicants,label = Default.Rate)) + 
  geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  geom_text(size = 3, vjust = -0.5)

# Result shows that default rate is uniformly distributed across age groups, except for below 20 agre group where it is lower. However
# number of records in below 20 age group is very low as compared to others. 

#----Let's now take Gender variable ---------------------------------------------------------------------------------------------------

# Checking the levels of the Gender
levels(master_df_with_pTag$Gender)

# Plottng the bar graph

# Writing a function "plot_default" to do the same task for each variable
plot_default <- function(cat_var, var_name){
  a <- aggregate(Performance.Tag~cat_var, master_df_with_pTag, mean)
  count <- data.frame(table(cat_var))
  count <- count[,-1]
  agg_default <- cbind(a, count)
  
  colnames(agg_default) <- c(var_name, "Default.Rate","Number.of.Applicants")
  agg_default[, 3] <- format(round(agg_default[, 3], 3))
  
  ggplot(agg_default, aes(agg_default[, 1], count, label = Default.Rate)) + geom_bar(stat = 'identity') + 
    theme(axis.text.x = element_text(angle = 60, hjust = 1)) + geom_text(size = 3, vjust = -0.5) + xlab(var_name)
  
}

plot_default(master_df_with_pTag$Gender, "Gender")

# Default rate does not vary too much based on Gender.


#----Let's now take Marital Status  ---------------------------------------------------------------------------------------------------

# Checking the levels of the marital status
levels(master_df_with_pTag$Marital.Status..at.the.time.of.application.)

plot_default(master_df_with_pTag$Marital.Status..at.the.time.of.application., "Marital.Status..at.the.time.of.application.")

# Default rate does not vary much with different marital status.


#----Let's now take Education variable ------------------------------------------------------------------------------------------------

# Checking the levels of the Education
levels(master_df_with_pTag$Education)

plot_default(master_df_with_pTag$Education, "Education")

# Default rate is almost same for all types of education level, except of "Others" where it is higher.


#----Let's now take Profession variable -----------------------------------------------------------------------------------------------

# Checking the levels of the Profession
levels(master_df_with_pTag$Profession)

plot_default(master_df_with_pTag$Profession, "Profession")

# Default rate is not varying widely with change of profession. 

#----Let's now take Type of Residence variable ----------------------------------------------------------------------------------------

# Checking the levels of type of residence
levels(master_df_with_pTag$Type.of.residence)

plot_default(master_df_with_pTag$Type.of.residence, "Type.of.residence")

# Default rate is lower for "Others" category as compared remaining types of residence. However population of others categoroy is insignificant.

str(master_df_with_pTag)

## Now let's work on continuous variables one by one. 

#----Let's now take No.of Dependants variable -----------------------------------------------------------------------------------------

plot_default(as.factor(master_df_with_pTag$No.of.dependents), "No.of.dependents")

# Some variation in default rate is noticed for different values of no.of dependents. However it is not clear indicator of default. 

# Let's covert this variable to factor one.

master_df_with_pTag$No.of.dependents <- as.factor(master_df_with_pTag$No.of.dependents)

#----Let's now take Income variable ---------------------------------------------------------------------------------------------------

# Let's check the outlier in the variables 
quantile(master_df_with_pTag$Income,seq(0,1,0.01))  # 59 and below is the income with 99 percentile.

boxplot(master_df_with_pTag$Income)
# There is no outlier.

# 59 is at 99 percentile and highest value is 60. So no outlier treatment is required. Box plot also confirms the same.

summary(master_df_with_pTag$Binning.Income)

# Binning the Income variable and store it into "Binning.Income".
master_df_with_pTag$Binning.Income <- as.factor(cut(master_df_with_pTag$Income, breaks = c(0.01, 10, 20, 30, 40, 50, 60)))

sum(is.na(master_df_with_pTag$Binning.Income)) # Ensuring that no record is missed out for income binning.

# Check the default rate in each bucket
agg_income <- merge(aggregate(Performance.Tag ~ Binning.Income, master_df_with_pTag, mean),aggregate(Performance.Tag ~ Binning.Income, master_df_with_pTag, sum),by = "Binning.Income") 

# Adding Number of Applicants
count <- data.frame(table(master_df_with_pTag$Binning.Income))
count <- count[,-1]
agg_income <- cbind(agg_income,count)

# changing column name of each variables in agg_age dataframe
colnames(agg_income) <- c("Income", "Default.Rate", "Count.Default","Number.of.Applicants")

# Round Off the values
agg_income$Default.Rate <- format(round(agg_income$Default.Rate, 3))
agg_income

#     Income      Default.Rate  Count.Default Number.of.Applicants
#1 (0.01,10]        0.056           696                12519
#2   (10,20]        0.046           599                13145
#3   (20,30]        0.044           584                13373
#4   (30,40]        0.036           479                13402
#5   (40,50]        0.035           377                10640
#6   (50,60]        0.029           157                 5496

plot_default(as.factor(master_df_with_pTag$Income), "Income")

# Default rate decreases with increase in income. 

#----Let's now take No.of Months in Current Residence variable ------------------------------------------------------------------------

summary(master_df_with_pTag$No.of.months.in.current.residence)

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#6.00    6.00   10.00   34.55   61.00  126.00 

quantile(master_df_with_pTag$No.of.months.in.current.residence,seq(0,1,0.01))

boxplot(master_df_with_pTag$No.of.months.in.current.residence)

# 1st qurtile is below 1 year and 3rd quartile is upto 5 years.Value for 99 percential is quite near to max value and box
# plot also connfirms that there is no outlier.

# Let's factorize this variable and convert it to a fewer buckets / levels. 

# Let's bucket this into 3 levels
master_df_with_pTag$No.of.months.in.current.residence <- as.factor(master_df_with_pTag$No.of.months.in.current.residence)

levels(master_df_with_pTag$No.of.months.in.current.residence) # Checking initial levels

levels(master_df_with_pTag$No.of.months.in.current.residence)[1:6]<-"less_than_one_year"

levels(master_df_with_pTag$No.of.months.in.current.residence) # Cheking revised levels

levels(master_df_with_pTag$No.of.months.in.current.residence)[2:50] <- "one_year_to_five_years"

levels(master_df_with_pTag$No.of.months.in.current.residence) # Checking revised levels

levels(master_df_with_pTag$No.of.months.in.current.residence)[3:68] <- "more_than_five_years"

levels(master_df_with_pTag$No.of.months.in.current.residence) # Checking revised levels

# Plotting default rate again with new levels of variable. 
plot_default(as.factor(master_df_with_pTag$No.of.months.in.current.residence), "No.of.months.in.current.residence")

# Default rate varies significantly for different levels of number of months in current residence.

#----Let's now take No.of Months in Current Company variable ------------------------------------------------------------------------

summary(master_df_with_pTag$No.of.months.in.current.company)

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 3.00   17.00   34.00   34.24   51.00  133.00 

quantile(master_df_with_pTag$No.of.months.in.current.company,seq(0,1,0.01)) # 74 is at 99 percentile while max value is 133

boxplot(master_df_with_pTag$No.of.months.in.current.company)

# There are outliers and need treatment.

# Capping the upper values with 74 to treat outliers.
master_df_with_pTag[(which(master_df_with_pTag$No.of.months.in.current.company>74)),]$No.of.months.in.current.company <- 74

boxplot(master_df_with_pTag$No.of.months.in.current.company) # Verifying that outlier are properly treated.

summary(master_df_with_pTag$No.of.months.in.current.company)

# Let's bucket this into 3 levels
master_df_with_pTag$No.of.months.in.current.company <- as.factor(master_df_with_pTag$No.of.months.in.current.company)

levels(master_df_with_pTag$No.of.months.in.current.company) # Checking initial levels

levels(master_df_with_pTag$No.of.months.in.current.company)[1:22]<-"0_2 years"

levels(master_df_with_pTag$No.of.months.in.current.company) # Verifying revised levels

levels(master_df_with_pTag$No.of.months.in.current.company)[2:25] <- "2_4 years"

levels(master_df_with_pTag$No.of.months.in.current.company) # Verifying revised levels

levels(master_df_with_pTag$No.of.months.in.current.company)[3:28] <- "more_than_4 years"

levels(master_df_with_pTag$No.of.months.in.current.company) # Verifying final levels

# Plotting default rate again with new levels of variable. 
plot_default(as.factor(master_df_with_pTag$No.of.months.in.current.company), "No.of.months.in.current.company")

# Default rate decreases with increase in number of months in current company.

#----Let's now take other vriables having number times events happening ---------------------------------------------------------------

plot_default(as.factor(master_df_with_pTag$No.of.times.90.DPD.or.worse.in.last.6.months), "No.of.times.90.DPD.or.worse.in.last.6.months")
plot_default(as.factor(master_df_with_pTag$No.of.times.60.DPD.or.worse.in.last.6.months), "No.of.times.60.DPD.or.worse.in.last.6.months")
plot_default(as.factor(master_df_with_pTag$No.of.times.30.DPD.or.worse.in.last.6.months), "No.of.times.30.DPD.or.worse.in.last.6.months")

plot_default(as.factor(master_df_with_pTag$No.of.times.90.DPD.or.worse.in.last.12.months), "No.of.times.90.DPD.or.worse.in.last.12.months")
plot_default(as.factor(master_df_with_pTag$No.of.times.60.DPD.or.worse.in.last.12.months), "No.of.times.60.DPD.or.worse.in.last.12.months")
plot_default(as.factor(master_df_with_pTag$No.of.times.30.DPD.or.worse.in.last.12.months), "No.of.times.30.DPD.or.worse.in.last.12.months")


plot_default(as.factor(master_df_with_pTag$No.of.trades.opened.in.last.6.months), "No.of.trades.opened.in.last.6.months")
plot_default(as.factor(master_df_with_pTag$No.of.trades.opened.in.last.12.months), "No.of.trades.opened.in.last.12.months")

plot_default(as.factor(master_df_with_pTag$No.of.PL.trades.opened.in.last.6.months), "No.of.PL.trades.opened.in.last.6.months")
plot_default(as.factor(master_df_with_pTag$No.of.PL.trades.opened.in.last.12.months), "No.of.PL.trades.opened.in.last.12.months")

plot_default(as.factor(master_df_with_pTag$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.), "No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.")
plot_default(as.factor(master_df_with_pTag$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.), "No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.")

# Default rate varies significantly with number of events for each of the above variables. Generally it increases with the number of events. 

#----Let's now take loan variables ------------------------------------------------------------------------------------------------------

plot_default(as.factor(master_df_with_pTag$Presence.of.open.home.loan),"Presence.of.open.home.loan")
plot_default(as.factor(master_df_with_pTag$Presence.of.open.auto.loan),"Presence.of.open.auto.loan")

# Interestingly default rate is more for applicants who do not have any other loan as compared to ones who have other loans.However,
# population of applicants having loan is lesser comparatively.

#----Let's now take variable on average card utilization --------------------------------------------------------------------------------

summary(master_df_with_pTag$Avgas.CC.Utilization.in.last.12.months)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.00    8.00   15.00   29.29   45.00  113.00 
quantile(master_df_with_pTag$Avgas.CC.Utilization.in.last.12.months,seq(0,1,0.01)) # 113 is at 99 percentile which is max value as well.
boxplot(master_df_with_pTag$Avgas.CC.Utilization.in.last.12.months) # Box plot suggest outliers.

# Hoever max value is at 96 percentile as well.So we would not treat this for outlier. This will be taken care in WoE transformation.

# Let's bucket this into 3 levels
master_df_with_pTag$Avgas.CC.Utilization.in.last.12.months <- as.factor(master_df_with_pTag$Avgas.CC.Utilization.in.last.12.months)

levels(master_df_with_pTag$Avgas.CC.Utilization.in.last.12.months) # Checking initial levels

levels(master_df_with_pTag$Avgas.CC.Utilization.in.last.12.months)[1:13]<-"avg_max_once_in_month"

levels(master_df_with_pTag$Avgas.CC.Utilization.in.last.12.months) # Verifying revised levels

levels(master_df_with_pTag$Avgas.CC.Utilization.in.last.12.months)[2:37] <- "avg_4_times_a_month"

levels(master_df_with_pTag$Avgas.CC.Utilization.in.last.12.months) # Verifying revised levels

levels(master_df_with_pTag$Avgas.CC.Utilization.in.last.12.months)[3:67] <- "avg_more_than_4_times_month"

levels(master_df_with_pTag$Avgas.CC.Utilization.in.last.12.months) # Verifying final levels

# Plotting default rate again with new levels of variable. 
plot_default(as.factor(master_df_with_pTag$Avgas.CC.Utilization.in.last.12.months), "Avgas.CC.Utilization.in.last.12.months")

# More the number of times card is utilized, more is default rate.

#----Let's now take variable on total number of trades --------------------------------------------------------------------------------

summary(master_df_with_pTag$Total.No.of.Trades)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.000   3.000   6.000   8.301  10.000  44.000 

quantile(master_df_with_pTag$Total.No.of.Trades,seq(0,1,0.01)) # 31 is at 99 percentile while max value is 44.
boxplot(master_df_with_pTag$Total.No.of.Trades) # Plot suggests outliers.

# Let's cap value at 99 percentile which is 31.
master_df_with_pTag[(which(master_df_with_pTag$Total.No.of.Trades>31)),]$Total.No.of.Trades <- 31

boxplot(master_df_with_pTag$Total.No.of.Trades) # Still plot shows outlier, however distribution of values is uniform. 

# We will not carry out outlier treatment further. WoE transformation shall take care. 

summary(master_df_with_pTag$Total.No.of.Trades)

# Let's bucket this into levels
master_df_with_pTag$Total.No.of.Trades <- as.factor(master_df_with_pTag$Total.No.of.Trades)

levels(master_df_with_pTag$Total.No.of.Trades) # Checking initial levels

levels(master_df_with_pTag$Total.No.of.Trades)[1:5]<-"1to5_times"

levels(master_df_with_pTag$Total.No.of.Trades) # Verifying revised levels

levels(master_df_with_pTag$Total.No.of.Trades)[2:6] <- "6to10_times"

levels(master_df_with_pTag$Total.No.of.Trades) # Verifying revised levels

levels(master_df_with_pTag$Total.No.of.Trades)[3:7] <- "11to15_times"

levels(master_df_with_pTag$Total.No.of.Trades) # Verifying revised levels

levels(master_df_with_pTag$Total.No.of.Trades)[4:8] <- "16to20_times"

levels(master_df_with_pTag$Total.No.of.Trades) # Verifying revised levels

levels(master_df_with_pTag$Total.No.of.Trades)[5:9] <- "21to25_times"

levels(master_df_with_pTag$Total.No.of.Trades) # Verifying revised levels

levels(master_df_with_pTag$Total.No.of.Trades)[6:11] <- "Above25_times"

levels(master_df_with_pTag$Total.No.of.Trades) # Verifying final levels

# Plotting default rate again with new levels of variable. 
plot_default(as.factor(master_df_with_pTag$Total.No.of.Trades), "Total.No.of.Trades")

# Number of total trades has a significant impact on default rate. Generally it is more when total trades are more.

#----Let's now take variable on Outstanding Balance --------------------------------------------------------------------------------

summary(master_df_with_pTag$Outstanding.Balance)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0  213600  777912 1265964 2929738 5218801 

# Let's check the outlier in the variables 
boxplot(master_df_with_pTag$Outstanding.Balance)
# There is no outlier.

# Binning the outstanding balance variable and store it into "Binning.Balance".
master_df_with_pTag$Binning.Balance <- as.factor(cut(master_df_with_pTag$Outstanding.Balance, breaks = c(-0.01, 200000, 1000000, 2000000, 3000000, 4000000, 5000000, 6000000)))

sum(is.na(master_df_with_pTag$Binning.Balance)) # Ensuring that no record is missed out for binning.

# Check the default rate in each bucket
agg_balance <- merge(aggregate(Performance.Tag ~ Binning.Balance, master_df_with_pTag, mean),aggregate(Performance.Tag ~ Binning.Balance, master_df_with_pTag, sum),by = "Binning.Balance") 

# Adding Number of Applicants
count <- data.frame(table(master_df_with_pTag$Binning.Balance))
count <- count[,-1]
agg_balance <- cbind(agg_balance,count)

# changing column name of each variables in agg_age dataframe
colnames(agg_balance) <- c("O/Balance", "Default.Rate", "Count.Default","Number.of.Applicants")

# Round Off the values
agg_balance$Default.Rate <- format(round(agg_balance$Default.Rate, 3))
agg_balance

#O/Balance           Default.Rate Count.Default Number.of.Applicants
#1 (-0.01,2e+05]        0.019           287                15428
#2 (1e+06,2e+06]        0.058           477                26918
#3 (2e+05,1e+06]        0.057          1525                 8164
#4 (2e+06,3e+06]        0.016           133                 8239
#5 (3e+06,4e+06]        0.049           406                 8341
#6 (4e+06,5e+06]        0.045            67                 1473
#7 (5e+06,6e+06]        0.167             2                   12

plot_default(as.factor(master_df_with_pTag$Outstanding.Balance), "O/Balance")

# There is a variation in default rate for different bins of outstanding balance. However, there is no clear indication due to binning. 

# Converting loan status to factors.

master_df_with_pTag$Presence.of.open.auto.loan <- ifelse(master_df_with_pTag$Presence.of.open.auto.loan == 1, "Yes", "No")
master_df_with_pTag$Presence.of.open.auto.loan <- as.factor(master_df_with_pTag$Presence.of.open.auto.loan)

master_df_with_pTag$Presence.of.open.home.loan <- ifelse(master_df_with_pTag$Presence.of.open.home.loan == 1, "Yes", "No")
master_df_with_pTag$Presence.of.open.home.loan <- as.factor(master_df_with_pTag$Presence.of.open.home.loan)

str(master_df_with_pTag)

# Check co-relation between numeric variables.

num_var <- c('No.of.times.90.DPD.or.worse.in.last.6.months',
             'No.of.times.60.DPD.or.worse.in.last.6.months',
             'No.of.times.30.DPD.or.worse.in.last.6.months',
             'No.of.times.90.DPD.or.worse.in.last.12.months',
             'No.of.times.60.DPD.or.worse.in.last.12.months',
             'No.of.times.30.DPD.or.worse.in.last.12.months',
             'No.of.trades.opened.in.last.6.months',
             'No.of.trades.opened.in.last.12.months',
             'No.of.PL.trades.opened.in.last.6.months',
             'No.of.PL.trades.opened.in.last.12.months',
             'No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.',
             'No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.',
             'Outstanding.Balance',
             'Age',
             'Income')



master_num_corr <- round(cor(master_df_with_pTag[,colnames(master_df_with_pTag) %in% num_var]),2)
master_num_corr

# Export for analysis on correlation matrix

write.csv(master_num_corr, "correlation_matrix_bfs5.csv")

# Findings

# Number of times customer has not payed dues since 90/60/30 days in last 6/12 months are highly correlated with each other.
# Presence of home loan and outstanding balance are highly correlated
# Total number of trades highly correlated with number of trades/PL trades opened in last 6/12 months.It has also high correlation 
# with number of queries made in last 6/12 months excluding home/auto loans

# Export for graphical analysis in tableau.

write.csv(master_df_with_pTag, 'capstone_bfs5.csv')

#-------------------Calculate Weigh of Evidence(WoE) and IV Values------------------------------------------------------------------------

library(Information)

master_df_with_pTag_woe <- master_df_with_pTag

master_df_with_pTag_woe <- master_df_with_pTag_woe[,-(30:32)]

summary(master_df_with_pTag_woe)

#Let us remove Application.ID , Age_bracket and Job_change_freq from the this dataset before doing WOE analysis.
master_df_with_pTag_woe <- master_df_with_pTag_woe[,-1]

str(master_df_with_pTag_woe)
dim(master_df_with_pTag_woe)

master_df_with_pTag_woe_contvar <- master_df_with_pTag_woe[,c('Performance.Tag',
                                                              'Age',
                                                              'Income',
                                                              'No.of.times.60.DPD.or.worse.in.last.6.months',
                                                              'No.of.times.30.DPD.or.worse.in.last.6.months',
                                                              'No.of.times.90.DPD.or.worse.in.last.6.months',
                                                              'No.of.times.90.DPD.or.worse.in.last.12.months',
                                                              'No.of.times.60.DPD.or.worse.in.last.12.months',
                                                              'No.of.times.30.DPD.or.worse.in.last.12.months',
                                                              'No.of.trades.opened.in.last.6.months',
                                                              'No.of.trades.opened.in.last.12.months',
                                                              'No.of.PL.trades.opened.in.last.6.months',
                                                              'No.of.PL.trades.opened.in.last.12.months',
                                                              'No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.',
                                                              'No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.',
                                                              'Outstanding.Balance')]


iv_cont_master <- create_infotables(master_df_with_pTag_woe_contvar,y="Performance.Tag",bins=10,parallel = FALSE)

iv_cont_bin10 <- data.frame(iv_cont_master$Summary)
iv_cont_bin10

#   Variable                                                        IV
#  No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. 0.316772497
#                         No.of.PL.trades.opened.in.last.12.months 0.312148867
#                            No.of.trades.opened.in.last.12.months 0.311654449
#                                              Outstanding.Balance 0.257279267
#                     No.of.times.30.DPD.or.worse.in.last.6.months 0.246925567
#                          No.of.PL.trades.opened.in.last.6.months 0.228619335
#                    No.of.times.90.DPD.or.worse.in.last.12.months 0.218480301
#   No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. 0.214394504
#                     No.of.times.60.DPD.or.worse.in.last.6.months 0.210201099
#                    No.of.times.30.DPD.or.worse.in.last.12.months 0.202475032
#                             No.of.trades.opened.in.last.6.months 0.195264106
#                    No.of.times.60.DPD.or.worse.in.last.12.months 0.188954740
#                     No.of.times.90.DPD.or.worse.in.last.6.months 0.163722943
#                                                           Income 0.042571328
#                                                              Age 0.003728238

master_df_with_pTag_woe_catvar <- master_df_with_pTag_woe[,c('Performance.Tag',
                                                             'Gender',
                                                             'Marital.Status..at.the.time.of.application.',
                                                             'No.of.dependents',
                                                             'Education','Profession','Type.of.residence',
                                                             'No.of.months.in.current.residence',
                                                             'No.of.months.in.current.company',
                                                             'Avgas.CC.Utilization.in.last.12.months',
                                                             'Presence.of.open.home.loan',
                                                             'Total.No.of.Trades',
                                                             'Presence.of.open.auto.loan')]

iv_cat_master <- create_infotables(master_df_with_pTag_woe_catvar,y="Performance.Tag",bins=10,parallel = FALSE)
iv_cat_bin10 <- data.frame(iv_cat_master$Summary)
iv_cat_bin10

#                               Variable           IV
#       Avgas.CC.Utilization.in.last.12.months 0.2610204093
#                           Total.No.of.Trades 0.2089818314
#            No.of.months.in.current.residence 0.0649495645
#                   Presence.of.open.home.loan 0.0169166671
#              No.of.months.in.current.company 0.0131186314
#                             No.of.dependents 0.0029249685
#                                   Profession 0.0020202656
#                   Presence.of.open.auto.loan 0.0015484526
#                            Type.of.residence 0.0008705523
#                                    Education 0.0008287873
#                                       Gender 0.0002698331
#  Marital.Status..at.the.time.of.application. 0.0001039180

# Plot WOE scores for variables having IV >= 0.2, having medium to strong predictive power.

plot_infotables(iv_cat_master,"Avgas.CC.Utilization.in.last.12.months")
plot_infotables(iv_cat_master,"Total.No.of.Trades")
plot_infotables(iv_cont_master,"No.of.trades.opened.in.last.12.months")
plot_infotables(iv_cont_master,"No.of.PL.trades.opened.in.last.12.months")
plot_infotables(iv_cont_master,"No.of.PL.trades.opened.in.last.6.months")
plot_infotables(iv_cont_master,"No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.")
plot_infotables(iv_cont_master,"No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.")
plot_infotables(iv_cont_master,"No.of.times.30.DPD.or.worse.in.last.6.months")
plot_infotables(iv_cont_master,"No.of.times.30.DPD.or.worse.in.last.12.months")
plot_infotables(iv_cont_master,"No.of.times.90.DPD.or.worse.in.last.12.months")
plot_infotables(iv_cont_master,"No.of.times.60.DPD.or.worse.in.last.6.months")
plot_infotables(iv_cont_master,"Outstanding.Balance")

# Above are the variables which have medium to strong predictive power and hence they are important variables from modelling perspective.

#----------WoE Transformation using Score Card Package-------------------------------------------------------------------------------------

library(scorecard)

# Check iv values for all variables
scorecard::iv(dt =master_df_with_pTag_woe, y='Performance.Tag')

#                           variable                               info_value
#1:  No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. 0.3222742841
#2:                            No.of.trades.opened.in.last.12.months 0.3214464795
#3:                         No.of.PL.trades.opened.in.last.12.months 0.3151355011
#4:                           Avgas.CC.Utilization.in.last.12.months 0.2610204093
#5:                     No.of.times.30.DPD.or.worse.in.last.6.months 0.2496995319
#6:                          No.of.PL.trades.opened.in.last.6.months 0.2330899248
#7:                    No.of.times.30.DPD.or.worse.in.last.12.months 0.2235467580
#8:                    No.of.times.90.DPD.or.worse.in.last.12.months 0.2202165136
#9:   No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. 0.2185134495
#10:                    No.of.times.60.DPD.or.worse.in.last.6.months 0.2154891635
#11:                                              Total.No.of.Trades 0.2089818314
#12:                            No.of.trades.opened.in.last.6.months 0.2007686474
#13:                   No.of.times.60.DPD.or.worse.in.last.12.months 0.1919274752
#14:                    No.of.times.90.DPD.or.worse.in.last.6.months 0.1660361868
#15:                               No.of.months.in.current.residence 0.0649495645
#16:                                                          Income 0.0642820059
#17:                                             Outstanding.Balance 0.0586406616
#18:                                                             Age 0.0175297443
#19:                                      Presence.of.open.home.loan 0.0169166671
#20:                                 No.of.months.in.current.company 0.0131186314
#21:                                                No.of.dependents 0.0029249685
#22:                                                      Profession 0.0020202656
#23:                                      Presence.of.open.auto.loan 0.0015484526
#24:                                               Type.of.residence 0.0008705523
#25:                                                       Education 0.0008287873
#26:                                                          Gender 0.0002698331
#27:                     Marital.Status..at.the.time.of.application. 0.0001039180

# Filtering based on IV values.
dim(master_df_with_pTag_woe) # 68575 records with 27 predicting variables based on IV values.
dt_f = var_filter(master_df_with_pTag_woe, "Performance.Tag")

dim(dt_f) # 68575 records with 17 variables having significant predictive power.
colnames(dt_f) # Listing down variable having significant predictive power. 

# Breaking dt into train and test
dt_list = split_df(dt_f, "Performance.Tag")

View(dt_list)
View(dt_list$train)
View(dt_list$test)

# Keeping original Outstanding Balance intact in separate dataset before woe transformation to use it for financial analyis.
test_data_balance <- as.data.frame(dt_list$test$Outstanding.Balance)
colnames(test_data_balance)[1] <- "Outstanding.Balance"
View(test_data_balance)

label_list = lapply(dt_list, function(x) x$Performance.Tag)
View(label_list)

# Carry out WoE Binning
bins = woebin(dt_list$train, "Performance.Tag")

# Converting train and test into WoE values
dt_woe_list = lapply(dt_list, function(x) woebin_ply(x, bins))

View(dt_woe_list$train)
View(dt_woe_list$test)

#------------------------------------------------------------------------------------------------------------------------##
# BUILDING, TESTING AND VALIDATING MODELS
#------------------------------------------------------------------------------------------------------------------------##

# We will have 3 types of model building.

# 1. Model building only for Demographic data to understand the predictive power of application data. This will be done
# using raw data without any WoE transformation.

# 2. Model building on merged dataset on biased data set with WoE tranformed values.

# 3. Model building on merged dataset on un-biased data set (using smote) with WoE transformed values.

# Let's start it one by one.

##########################################################################################################################
#                MODEL FOR DEMOGRAPHIC DATA ONLY
##########################################################################################################################

##-- LOGISTIC REGRESSION -----------------------------------------------------------------------------------------------##

# Prepare final demographic dataset.
demo_data <- cbind(master_df_with_pTag[,(2:11)],master_df_with_pTag$Performance.Tag)
colnames(demo_data)[11] <- "Performance.Tag"

library(dummies)

demo_data <- dummy.data.frame(demo_data)
demo_data$Performance.Tag <- as.factor(ifelse(demo_data$Performance.Tag == 1, "yes", "no"))

# Prepare train and test data.
set.seed(1)

split_indices <- sample.split(demo_data$Performance.Tag, SplitRatio = 0.70)

demo_train <- demo_data[split_indices, ]

demo_test <- demo_data[!split_indices, ]

# Confirming split in 70:30 ratio

nrow(demo_train)/nrow(demo_data)
nrow(demo_test)/nrow(demo_data)

summary(demo_train)
str(demo_train)

# Base model for logistic regression

demo_log_1 <- glm(Performance.Tag ~ ., family = "binomial", data = demo_train)
summary(demo_log_1)

#Null deviance: 16778  on 48001  degrees of freedom
#Residual deviance: 16544  on 47979  degrees of freedom
#AIC: 16590

# We now run StepAIC on the base model to eliminate insignificant variables from the dataset

demo_log_2 <- stepAIC(demo_log_1, direction = "both")
summary(demo_log_2)
vif(demo_log_2)

#Null deviance: 16778  on 48001  degrees of freedom
#Residual deviance: 16550  on 47994  degrees of freedom
#AIC: 16566

# "No.of.dependents2" is having high p value and less significance, hence removing it.

demo_log_3 <- glm(formula = Performance.Tag ~ Income + EducationOthers +
                    No.of.months.in.current.residenceless_than_one_year +
                    No.of.months.in.current.residenceone_year_to_five_years +
                    `No.of.months.in.current.company2_4 years`, family = "binomial", data = demo_train)

summary(demo_log_3)
vif(demo_log_3)

#Null deviance: 16778  on 48001  degrees of freedom
#Residual deviance: 16578  on 47996  degrees of freedom
#AIC: 16590

# Dropping "`No.of.months.in.current.company2_4 years`" due to lesser significance and high p value.

demo_log_4 <- glm(formula = Performance.Tag ~ Income + EducationOthers +
                    No.of.months.in.current.residenceless_than_one_year +
                    No.of.months.in.current.residenceone_year_to_five_years, family = "binomial", data = demo_train)

summary(demo_log_4)
vif(demo_log_4)

#Null deviance: 16778  on 48001  degrees of freedom
#Residual deviance: 16579  on 47997  degrees of freedom
#AIC: 16589

# Dropping "EducationOthers" due to lesser significance and higher p value.

demo_log_5 <- glm(formula = Performance.Tag ~ Income + 
                    No.of.months.in.current.residenceless_than_one_year +
                    No.of.months.in.current.residenceone_year_to_five_years, family = "binomial", data = demo_train)

summary(demo_log_5)
vif(demo_log_5)

#Null deviance: 16778  on 48001  degrees of freedom
#Residual deviance: 16581  on 47998  degrees of freedom
#AIC: 16589

# Now there are few variables and all of them have high significance and very low p values. This model would be taken as final.

demo_log_final <- demo_log_5

#### Model Testing.

# Predicting probabilities of default for the test data

predictions_logit <- predict(demo_log_final, newdata = demo_test[, -31], type = "response")
summary(predictions_logit)

#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.02277 0.03157 0.03909 0.04209 0.05030 0.07405

# Let's use the probability cutoff of 0.03885 at median and check.

predicted_response <- factor(ifelse(predictions_logit >= 0.03909, "yes", "no"))

# Creating confusion matrix for identifying the model evaluation.

conf <- confusionMatrix(predicted_response, demo_test$Performance.Tag, positive = "yes")
conf

# Accuracy:    0.51060
# Sensitivity: 0.61060
# Specificity: 0.50622

# Let's find out the optimal probalility cutoff 

perform_fn <- function(cutoff) 
{
  predicted_response <- factor(ifelse(predictions_logit >= cutoff, "yes", "no"))
  conf <- confusionMatrix(predicted_response, demo_test$Performance.Tag, positive = "yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Creating cutoff values from 0.01 to 0.99 for plotting and initiallizing a matrix of 1000 X 4.

s = seq(.01,.99,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 


# plotting cutoffs 
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

cutoff_prob_value <- s[which(abs(OUT[,1]-OUT[,2])<0.2)]
cutoff_prob_value

# Cut-off probabity value 0.03969697

# Let's choose a cutoff value of 0.03969697 for final model

predicted_response <- factor(ifelse(predictions_logit >= 0.03969697, "yes", "no"))

conf_final <- confusionMatrix(predicted_response, demo_test$Performance.Tag, positive = "yes")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc # 0.5246196 

sens # 0.5990783 

spec # 0.5213398 

# Model is having accuracy, sensitivity and specificity on lower side.

# Calculate KS Statistics
library(ROCR)

predicted_defaulter <- ifelse(predicted_response =="yes",1,0)
test_actual_defaulter <- ifelse(demo_test$Performance.Tag == "yes",1,0)

predict_test_logistic <- prediction(predicted_defaulter,test_actual_defaulter)
performance_measure_test_logistic <- performance(predict_test_logistic,"tpr","fpr")
ks_table <- attr(performance_measure_test_logistic,"y.values")[[1]]-attr(performance_measure_test_logistic,"x.values")[[1]]

max(ks_table)
#0.1204181

# Lift & Gain Chart 

# plotting the lift chart

# Loading dplyr package 
require(dplyr)
library(dplyr)

lift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

default_decile = lift(test_actual_defaulter, predicted_defaulter, groups = 10)
default_decile

#bucket total totalresp Cumresp  Gain Cumlift
#<int> <int>     <dbl>   <dbl> <dbl>   <dbl>
#1      1  2058       105     105  12.1    1.21
#2      2  2057       114     219  25.2    1.26
#3      3  2057       115     334  38.5    1.28
#4      4  2058       106     440  50.7    1.27
#5      5  2057        97     537  61.9    1.24
#6      6  2057        56     593  68.3    1.14
#7      7  2058        83     676  77.9    1.11
#8      8  2057        61     737  84.9    1.06
#9      9  2057        61     798  91.9    1.02
#10    10  2057        70     868 100      1   

# Gain graph
ggplot(default_decile,aes(x=bucket,y=(Gain/100)))+geom_line()+ylab("Gain")

# Lift Graph
ggplot(default_decile,aes(x=bucket,y=Cumlift))+geom_line() +ylab("Lift")

# ROC Curve
ROCData <- data.frame(OUT)
ggplot(ROCData,aes(x=X1,y=1-(X2)))+geom_line() + ylab("sensitivity")+ xlab("1- specificity")

# We will not do any other modelling on demographic data as it is only to understand the key variables in applicant's data,
# having strong predictive power. Final model would be on merged data where vaious models shall be built.

# From logistic regression model on demographic data, it is observed that 3 variables are key parameters impacting
# default rate.

# These variables are:
#1. Income
#2. No. of months in current residence < 1 year
#3. No. of months in current residence 1 to 5 years

###----------------------End of  Demograhic data only model---------------------------------------------------------###

#######################################################################################################################
#
#-------------------Model building on merged dataset on biased data set with WoE transformed values-----------------###
#
#######################################################################################################################
#
# We will here perform modelling with biased data set.

### -------Logistic Regression Model_Biased Data -------------------------------------------------------------------###

library(dummies)

master_df_logit_train <- dt_woe_list$train    # Train Data
master_df_logit_test <- dt_woe_list$test      # Test Data

model_1 <- glm(Performance.Tag ~ . , family = "binomial", data = master_df_logit_train)

summary(model_1)  # initial model has total 17 variables
vif(model_1)


#Null deviance: 16642  on 47874  degrees of freedom
#Residual deviance: 15846  on 47857  degrees of freedom
#AIC: 15882

# Using stepwise algorithm for removing insignificant variables 

model_stepAIC <- stepAIC(model_1, direction = "both")
summary(model_stepAIC)

#Null deviance: 16642  on 47874  degrees of freedom
#Residual deviance: 15852  on 47868  degrees of freedom
#AIC: 15866

# Removing "Income_woe" having lesser significance and higher p value.
model_2 <- glm(formula = Performance.Tag ~  No.of.times.90.DPD.or.worse.in.last.12.months_woe + 
                 No.of.times.30.DPD.or.worse.in.last.12.months_woe + 
                 Avgas.CC.Utilization.in.last.12.months_woe + 
                 No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._woe + 
                 Outstanding.Balance_woe,
                 family = "binomial", data = master_df_logit_train)
summary(model_2)
vif(model_2)

#Null deviance: 16642  on 47874  degrees of freedom
#Residual deviance: 15855  on 47869  degrees of freedom
#AIC: 15867

# Removing "No.of.times.90.DPD.or.worse.in.last.12.months_woe" due to comparatively less significance and higher p value. 
model_3 <- glm(formula = Performance.Tag ~  No.of.times.30.DPD.or.worse.in.last.12.months_woe + 
                 Avgas.CC.Utilization.in.last.12.months_woe + 
                 No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._woe + 
                 Outstanding.Balance_woe,
                 family = "binomial", data = master_df_logit_train)
summary(model_3)
vif(model_3)

#Null deviance: 16642  on 47874  degrees of freedom
#Residual deviance: 15862  on 47870  degrees of freedom
#AIC: 15872

# Removing "Outstanding.Balance_woe" due to comparatively lesser significance and higher p value. 
model_4 <- glm(formula = Performance.Tag ~  No.of.times.30.DPD.or.worse.in.last.12.months_woe + 
                 Avgas.CC.Utilization.in.last.12.months_woe + 
                 No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._woe, 
                 family = "binomial", data = master_df_logit_train)
summary(model_4)
vif(model_4)

#Null deviance: 16642  on 47874  degrees of freedom
#Residual deviance: 15873  on 47871  degrees of freedom
#AIC: 15881

# Now there are 3 variables left with very high significance and low p value. We will take this as a final model.

logistic_biased_final <- model_4

# Predict response as probabilities for test data
predictions_logit <- predict(logistic_biased_final, newdata = master_df_logit_test[, -1], type = "response")
View(predictions_logit)

summary(predictions_logit)

#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.01241 0.01633 0.03581 0.04182 0.05661 0.09453 

master_df_logit_test$Performance.Tag <- ifelse(master_df_logit_test$Performance.Tag==1,"yes","no")
master_df_logit_test$Performance.Tag <- as.factor(master_df_logit_test$Performance.Tag)

# Evaluate logistic regression model

# Let's first take the median value of probability for cut off and check
predicted_response <- factor(ifelse(predictions_logit >= 0.03581, "yes", "no"))

conf <- confusionMatrix(predicted_response, master_df_logit_test$Performance.Tag, positive = "yes")
conf

#Accuracy : 0.52650
#Sensitivity : 0.72297   
#Specificity : 0.51767


# Let's find out the optimal probalility cutoff 

perform_fn <- function(cutoff) 
{
  predicted_response <- factor(ifelse(predictions_logit >= cutoff, "yes", "no"))
  conf <- confusionMatrix(predicted_response, master_df_logit_test$Performance.Tag, positive = "yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Creating cutoff values from 0.01 to 0.99 for plotting and initiallizing a matrix of 1000 X 4.

s = seq(.01,.99,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 


# plotting cutoffs 
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

cutoff_prob_value <- s[which(abs(OUT[,1]-OUT[,2])<0.25)]
cutoff_prob_value

# 0.03969697 0.04959596

# Let's choose median value of above proposed cutoff values as our cut off value.

predicted_response <- factor(ifelse(predictions_logit >= 0.045, "yes", "no"))

conf_final <- confusionMatrix(predicted_response, master_df_logit_test$Performance.Tag, positive = "yes")
conf_final


#      Reference
# Prediction    no   yes
# no          12577   337
# yes          7235   551

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc # 0.6342029

sens # 0.6204955

spec # 0.6348173

# Calculate KS Statistics
library(ROCR)

predicted_defaulter <- ifelse(predicted_response =="yes",1,0)
test_actual_defaulter <- ifelse(master_df_logit_test$Performance.Tag == "yes",1,0)

predict_test_logistic <- prediction(predicted_defaulter,test_actual_defaulter)
performance_measure_test_logistic <- performance(predict_test_logistic,"tpr","fpr")
ks_table <- attr(performance_measure_test_logistic,"y.values")[[1]]-attr(performance_measure_test_logistic,"x.values")[[1]]

max(ks_table)
#0.2553128

# Lift & Gain Chart 

# plotting the lift chart

# Loading dplyr package 
require(dplyr)
library(dplyr)

lift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

default_decile = lift(test_actual_defaulter, predicted_defaulter, groups = 10)
default_decile

#bucket total totalresp Cumresp  Gain Cumlift
#<int> <int>     <dbl>   <dbl> <dbl>   <dbl>
#1      1  2070       131     131  14.8    1.48
#2      2  2070       147     278  31.3    1.57
#3      3  2070       151     429  48.3    1.61
#4      4  2070       132     561  63.2    1.58
#5      5  2070        60     621  69.9    1.40
#6      6  2070        54     675  76.0    1.27
#7      7  2070        48     723  81.4    1.16
#8      8  2070        45     768  86.5    1.08
#9      9  2070        65     833  93.8    1.04
#10    10  2070        55     888 100      1  

# Gain graph
ggplot(default_decile,aes(x=bucket,y=(Gain/100)))+geom_line()+ylab("Gain")

# Lift Graph
ggplot(default_decile,aes(x=bucket,y=Cumlift))+geom_line() +ylab("Lift")

# ROC Curve
ROCData <- data.frame(OUT)
ggplot(ROCData,aes(x=X1,y=1-(X2)))+geom_line() + ylab("sensitivity")+ xlab("1- specificity")

### -------Random Forest Model_Biased Data -------------------------------------------------------------------------###

library(randomForest)
set.seed(71)
data.rf <- randomForest(Performance.Tag ~ ., data=master_df_logit_train, proximity=FALSE,ntree=1000, mtry=4, do.trace=TRUE, na.action=na.omit)

data.rf

predictions_rf <- predict(data.rf, newdata=master_df_logit_test)

summary(predictions_rf)

#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#0.002035 0.015865 0.029111 0.043260 0.055260 0.420206 

# Let's median value as cut off and check first.

predicted_response <- factor(ifelse(predictions_rf >= 0.029111, "yes", "no"))

# Creating confusion matrix for identifying the model evaluation.

conf <- confusionMatrix(predicted_response, master_df_logit_test$Performance.Tag, positive = "yes")
conf

# Accuracy:    0.51860
# Sensitivity: 0.71847	
# Specificity: 0.50969

# Let's find out the optimal probalility cutoff 

perform_fn <- function(cutoff) 
{
  predicted_response <- factor(ifelse(predictions_rf >= cutoff, "yes", "no"))
  conf <- confusionMatrix(predicted_response, master_df_logit_test$Performance.Tag, positive = "yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Creating cutoff values from 0.01 to 0.99 for plotting and initiallizing a matrix of 1000 X 4.

s = seq(.01,.99,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 


# plotting cutoffs 
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

cutoff_prob_value <- s[which(abs(OUT[,1]-OUT[,2])<0.7)]
cutoff_prob_value

#0.01989899 0.02979798 0.03969697 0.04959596 0.05949495 0.06939394 0.07929293 0.08919192

# After chosing various values of probability cut off, 0.03880000 gives optimal balance. Let's take that as cut off. 

predicted_response <- factor(ifelse(predictions_rf >= 0.03880000, "yes", "no"))

conf_final <- confusionMatrix(predicted_response, master_df_logit_test$Performance.Tag, positive = "yes")
conf_final

#       Reference
# Prediction    no   yes
#   no       12348   346
#  yes        7464   542

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc # 0.6227053

sens # 0.6103604

spec # 0.6232586 

# Calculate KS Statistics
library(ROCR)

predicted_defaulter <- ifelse(predicted_response =="yes",1,0)
test_actual_defaulter <- ifelse(master_df_logit_test$Performance.Tag == "yes",1,0)

predict_test_rf <- prediction(predicted_defaulter,test_actual_defaulter)
performance_measure_test_rf <- performance(predict_test_rf,"tpr","fpr")
ks_table <- attr(performance_measure_test_rf,"y.values")[[1]]-attr(performance_measure_test_rf,"x.values")[[1]]

max(ks_table)
#0.233619

# Lift & Gain Chart 

# plotting the lift chart
	
# Loading dplyr package 
require(dplyr)
library(dplyr)

lift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

default_decile = lift(test_actual_defaulter, predicted_defaulter, groups = 10)
default_decile

#bucket total totalresp Cumresp  Gain Cumlift
#<int> <int>     <dbl>   <dbl> <dbl>   <dbl>
#1      1  2070       128     128  14.4    1.44
#2      2  2070       130     258  29.1    1.45
#3      3  2070       151     409  46.1    1.54
#4      4  2070       140     549  61.8    1.55
#5      5  2070        65     614  69.1    1.38
#6      6  2070        51     665  74.9    1.25
#7      7  2070        58     723  81.4    1.16
#8      8  2070        42     765  86.1    1.08
#9      9  2070        67     832  93.7    1.04
#10    10  2070        56     888 100      1   

# Gain graph
ggplot(default_decile,aes(x=bucket,y=(Gain/100)))+geom_line()+ylab("Gain")

# Lift Graph
ggplot(default_decile,aes(x=bucket,y=Cumlift))+geom_line() +ylab("Lift")

# ROC Curve
ROCData <- data.frame(OUT)
ggplot(ROCData,aes(x=X1,y=1-(X2)))+geom_line() + ylab("sensitivity")+ xlab("1- specificity")


#------------------------------End of Modeling on Biased data set---------------------------------------------------###

#######################################################################################################################
#
#----Model building on merged dataset on un-biased data set (using smote) with WoE transformed values---------------###
#
#######################################################################################################################
#
# We will here perform modelling with unbiased data set.Using SMOTE we are oversampling the defaulter data.
#
master_df_smote_train <- dt_woe_list$train

table(master_df_smote_train$Performance.Tag)
#    0     1 
# 45871  2004 
prop.table(table(master_df_smote_train$Performance.Tag))

#     0          1 
# 0.95814099 0.04185901

# SMOTTED train data set

library(DMwR)

master_df_smote_train$Performance.Tag <- as.factor(master_df_smote_train$Performance.Tag)
master_df_smote_train <- SMOTE(Performance.Tag~.,master_df_smote_train,perc.over = 100,perc.under = 200,k=5)

table(master_df_smote_train$Performance.Tag)
#   0    1 
#4008 4008

prop.table(table(master_df_smote_train$Performance.Tag))
#  1   2 
#0.5 0.5
#
##----Logistic Regression Model with smoted data-----------------------------------------------------------------------###
#
str(master_df_smote_train)
model_1 <- glm(Performance.Tag~.,data=master_df_smote_train,family = binomial)
summary(model_1)

#Null deviance: 11113  on 8015  degrees of freedom
#Residual deviance: 10282  on 7998  degrees of freedom
#AIC: 10318

model_2 <- stepAIC(model_1, direction="both")
summary(model_2)
vif(model_2)

#Null deviance: 11113  on 8015  degrees of freedom
#Residual deviance: 10286  on 8007  degrees of freedom
#AIC: 10304

# Removing "No.of.months.in.current.residence_woe" due to less significance and higher p value. 
model_3 <- glm(Performance.Tag ~ Income_woe +
                 No.of.times.90.DPD.or.worse.in.last.12.months_woe +
                 No.of.times.30.DPD.or.worse.in.last.12.months_woe +  
                 Avgas.CC.Utilization.in.last.12.months_woe + 
                 No.of.trades.opened.in.last.6.months_woe +
                 No.of.PL.trades.opened.in.last.12.months_woe +
                 No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._woe,
                 data=master_df_smote_train,family = binomial)

summary(model_3)
vif(model_3)

#Null deviance: 11113  on 8015  degrees of freedom
#Residual deviance: 10289  on 8008  degrees of freedom
#AIC: 10305

# Removing "No.of.trades.opened.in.last.6.months_woe" due to less significance and higher p value. 
model_4 <- glm(Performance.Tag ~ Income_woe +
                 No.of.times.90.DPD.or.worse.in.last.12.months_woe +
                 No.of.times.30.DPD.or.worse.in.last.12.months_woe +  
                 Avgas.CC.Utilization.in.last.12.months_woe +
                 No.of.PL.trades.opened.in.last.12.months_woe +
                 No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._woe,
                 data=master_df_smote_train,family = binomial)

summary(model_4)
vif(model_4)

#Null deviance: 11113  on 8015  degrees of freedom
#Residual deviance: 10292  on 8009  degrees of freedom
#AIC: 10306

# Removing "Income_woe" due to less significance and higher p value. 
model_5 <- glm(Performance.Tag ~  No.of.times.90.DPD.or.worse.in.last.12.months_woe +
                 No.of.times.30.DPD.or.worse.in.last.12.months_woe +  
                 Avgas.CC.Utilization.in.last.12.months_woe +
                 No.of.PL.trades.opened.in.last.12.months_woe +
                 No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._woe,
                 data=master_df_smote_train,family = binomial)

summary(model_5)
vif(model_5)

#Null deviance: 11113  on 8015  degrees of freedom
#Residual deviance: 10301  on 8010  degrees of freedom
#AIC: 10313

# Removing "Avgas.CC.Utilization.in.last.12.months_woe" due to less significance and higher p value. 
model_6 <- glm(Performance.Tag ~  No.of.times.90.DPD.or.worse.in.last.12.months_woe +
                 No.of.times.30.DPD.or.worse.in.last.12.months_woe +
                 No.of.PL.trades.opened.in.last.12.months_woe +
                 No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._woe,
                 data=master_df_smote_train,family = binomial)

summary(model_6)
vif(model_6)

#Null deviance: 11113  on 8015  degrees of freedom
#Residual deviance: 10311  on 8011  degrees of freedom
#AIC: 10321

#All variables are significant with low p value. So we will take this model as final one.
final_logit_smote_model <- model_6

# Model Evaluation_Logistic Regression_Unbised Smote Data

master_df_smote_test = dt_woe_list$test

master_df_smote_test$Performance.Tag <- ifelse(master_df_smote_test$Performance.Tag==1,"yes","no")

predictions_logit <- predict(final_logit_smote_model,newdata=master_df_smote_test,type='response')

summary(predictions_logit)

#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.2284  0.2597  0.4625  0.4544  0.5897  0.6942 

## Create Confusion Matrix with 0.5 cutoff
predicted_response <- factor(ifelse(predictions_logit >= 0.5, "yes", "no"))

# Creating confusion matrix for identifying the model evaluation.

conf <- confusionMatrix(predicted_response, as.factor(master_df_smote_test$Performance.Tag), positive = "yes")
conf

# Accuracy:    0.54680
# Sensitivity: 0.70946	
# Specificity: 0.53947

# Let's find out the optimal probalility cutoff 

perform_fn <- function(cutoff) 
{
  predicted_response <- factor(ifelse(predictions_logit >= cutoff, "yes", "no"))
  conf <- confusionMatrix(predicted_response, as.factor(master_df_smote_test$Performance.Tag), positive = "yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Creating cutoff values from 0.01 to 0.99 for plotting and initiallizing a matrix of 1000 X 4.

s = seq(.01,.99,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 


# plotting cutoffs 
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

cutoff_prob_value <- s[which(abs(OUT[,1]-OUT[,2])<0.7)]
cutoff_prob_value

#[1] 0.2376768 0.2475758 0.2574747 0.2673737 0.2772727 0.2871717 0.2970707 0.3069697 0.3168687 0.3267677 0.3366667 0.3465657 0.3564646
#[14] 0.3663636 0.3762626 0.3861616 0.3960606 0.4059596 0.4158586 0.4257576 0.4356566 0.4455556 0.4554545 0.4653535 0.4752525 0.4851515
#[27] 0.4950505 0.5049495 0.5148485 0.5247475 0.5346465 0.5445455 0.5544444 0.5643434 0.5742424 0.5841414 0.5940404 0.6039394 0.6138384
#[40] 0.6237374 0.6336364 0.6435354 0.6534343 0.6633333

# After trials with various cut off values, 0.5346465 chosen as final one which gives optimum balance. 

predicted_response <- factor(ifelse(predictions_logit >= 0.5346465, "yes", "no"))

conf_final <- confusionMatrix(predicted_response, as.factor(master_df_smote_test$Performance.Tag), positive = "yes")
conf_final


#     Reference
# Prediction    no   yes
#   no       13573   396
#  yes        6239   492

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc # 0.6794686 

sens # 0.5540541

spec # 0.6850898

# Calculate KS Statistics
library(ROCR)

predicted_defaulter <- ifelse(predicted_response =="yes",1,0)
test_actual_defaulter <- ifelse(master_df_smote_test$Performance.Tag == "yes",1,0)

predict_test_logistic <- prediction(predicted_defaulter,test_actual_defaulter)
performance_measure_test_logistic <- performance(predict_test_logistic,"tpr","fpr")
ks_table <- attr(performance_measure_test_logistic,"y.values")[[1]]-attr(performance_measure_test_logistic,"x.values")[[1]]

max(ks_table)
# 0.2391439

# Lift & Gain Chart 

# plotting the lift chart

# Loading dplyr package 
require(dplyr)
library(dplyr)

lift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

default_decile = lift(test_actual_defaulter, predicted_defaulter, groups = 10)
default_decile

#bucket total totalresp Cumresp  Gain Cumlift
#<int> <int>     <dbl>   <dbl> <dbl>   <dbl>
#1      1  2070       147     147  16.6    1.66
#2      2  2070       147     294  33.1    1.66
#3      3  2070       162     456  51.4    1.71
#4      4  2070        77     533  60.0    1.50
#5      5  2070        59     592  66.7    1.33
#6      6  2070        62     654  73.6    1.23
#7      7  2070        47     701  78.9    1.13
#8      8  2070        64     765  86.1    1.08
#9      9  2070        64     829  93.4    1.04
#10    10  2070        59     888 100      1 

# Gain graph
ggplot(default_decile,aes(x=bucket,y=(Gain/100)))+geom_line()+ylab("Gain")

# Lift Graph
ggplot(default_decile,aes(x=bucket,y=Cumlift))+geom_line() +ylab("Lift")

# ROC Curve
ROCData <- data.frame(OUT)
ggplot(ROCData,aes(x=X1,y=1-(X2)))+geom_line() + ylab("sensitivity")+ xlab("1- specificity")

##----Random Forest Model with SMOTE and without Cross Validation------------------------------------------------------###

library("randomForest")

master_df_smote.RForest_train <- dt_woe_list$train

master_df_smote.RForest_train$Performance.Tag <- as.factor(master_df_smote.RForest_train$Performance.Tag)
master_df_smote.RForest_train <- SMOTE(Performance.Tag~.,master_df_smote.RForest_train,perc.over = 100,perc.under = 200,k=5)

table(master_df_smote.RForest_train$Performance.Tag)
#   0    1 
#4008 4008

prop.table(table(master_df_smote.RForest_train$Performance.Tag))

#  0   1 
#0.5 0.5

#Initial random forest  model with mtry-4 and smoted data
str(master_df_smote.RForest_train)
model_rf_smote <- randomForest(Performance.Tag~.,data=master_df_smote.RForest_train,importance=TRUE,
                               ntree=5000,mtry=4,do.trace=TRUE,proximity=FALSE,allowParallel=TRUE)

# Prediction on test data
master_df_smote.RForest_test <- dt_woe_list$test

predictions_rf <- predict(model_rf_smote,master_df_smote.RForest_test,type='prob')

# Model Evaluation.

#Let us find the cut off

test_actual_defaulter <- factor(ifelse(master_df_smote.RForest_test$Performance.Tag==1,"yes","no"))

perform_fn <- function(cutoff)

{
  predicted_defaulter <- factor(ifelse(predictions_rf[,2] >= cutoff, "yes", "no"))
  conf <- confusionMatrix(predicted_defaulter, test_actual_defaulter, positive = "yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Creating cutoff values from 0.01 to 0.80 for plotting and initiallizing a matrix of 100 X 3.

s = seq(.01,.80,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.5)]
cutoff 


#[1] 0.09777778 0.10575758 0.11373737 0.12171717 0.12969697 0.13767677 0.14565657 0.15363636 0.16161616 0.16959596 0.17757576
#[12] 0.18555556 0.19353535 0.20151515 0.20949495 0.21747475 0.22545455 0.23343434 0.24141414 0.24939394 0.25737374 0.26535354
#[23] 0.27333333 0.28131313 0.28929293 0.29727273 0.30525253 0.31323232 0.32121212 0.32919192 0.33717172 0.34515152 0.35313131
#[34] 0.36111111 0.36909091 0.37707071 0.38505051 0.39303030 0.40101010 0.40898990 0.41696970 0.42494949 0.43292929 0.44090909
#[45] 0.44888889 0.45686869 0.46484848 0.47282828 0.48080808 0.48878788 0.49676768 0.50474747 0.51272727 0.52070707 0.52868687
#[56] 0.53666667

# Based on plot and trying values around ideal cut off, 0.36111111 is the best. Chosen as final one. 

#Confusion matrix
master_df_rforest_defaulter <- factor(ifelse(predictions_rf[,2] >=0.36111111, "yes", "no"))

master_df_rforest_confusionmatrix <- confusionMatrix(master_df_rforest_defaulter,test_actual_defaulter,positive="yes")
master_df_rforest_confusionmatrix

#  Prediction    no   yes
#       no     12532   364
#      yes      7280   524

#Accuracy
master_df_rforest_confusionmatrix$overall[1]
#0.6307246

#Sensitivity
master_df_rforest_confusionmatrix$byClass[1]
#0.5900901

#Specificity
master_df_rforest_confusionmatrix$byClass[2]
#0.6325459

# KS Statistics

library(ROCR)
#on testing  data

predictions_rf <- ifelse(master_df_rforest_defaulter =="yes",1,0)
test_actual_defaulter <- ifelse(test_actual_defaulter == "yes",1,0)

predict_def_test_object_rfoest <- prediction(predictions_rf,test_actual_defaulter)
performance_measure_test_rfoest <- performance(predict_def_test_object_rfoest,"tpr","fpr")
ks_table_test_rfoest <- attr(performance_measure_test_rfoest,"y.values")[[1]]-attr(performance_measure_test_rfoest,"x.values")[[1]]

max(ks_table_test_rfoest)
#0.222636

# Lift & Gain Chart 

# plotting the lift chart

# Loading dplyr package 
require(dplyr)
library(dplyr)

lift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

default_decile = lift(test_actual_defaulter, predictions_rf, groups = 10)
default_decile

#bucket total totalresp Cumresp  Gain Cumlift
#<int> <int>     <dbl>   <dbl> <dbl>   <dbl>
#1      1  2070       128     128  14.4    1.44
#2      2  2070       124     252  28.4    1.42
#3      3  2070       157     409  46.1    1.54
#4      4  2070       125     534  60.1    1.50
#5      5  2070        64     598  67.3    1.35
#6      6  2070        59     657  74.0    1.23
#7      7  2070        64     721  81.2    1.16
#8      8  2070        47     768  86.5    1.08
#9      9  2070        60     828  93.2    1.04
#10    10  2070        60     888 100      1  

# Gain graph
ggplot(default_decile,aes(x=bucket,y=(Gain/100)))+geom_line()+ylab("Gain")

# Lift Graph
ggplot(default_decile,aes(x=bucket,y=Cumlift))+geom_line() +ylab("Lift")

# ROC Curve
ROCData <- data.frame(OUT)
ggplot(ROCData,aes(x=X1,y=1-(X2)))+geom_line() + ylab("sensitivity")+ xlab("1- specificity")

##----Random Forest Model with SMOTE and with Cross Validation---------------------------------------------------------###

##Let us tune the random forest model 

library(doSNOW)
library(parallel)

# insert serial backend, otherwise error in repetetive tasks
registerDoSEQ()
master_df_smote.RForest_tune_train <- dt_woe_list$train

master_df_smote.RForest_tune_train$Performance.Tag <- as.factor(master_df_smote.RForest_tune_train$Performance.Tag)
master_df_smote.RForest_tune_train <- SMOTE(Performance.Tag~.,master_df_smote.RForest_tune_train,perc.over = 100,perc.under = 200,k=5)

library(caret)

control <- trainControl(method="repeatedcv",number=10,repeats=3)
set.seed(7)
tunegrid <- expand.grid(.mtry=c(1:10))
model_rf_smote_cv <- caret::train(Performance.Tag~.,data=master_df_smote.RForest_tune_train,method="rf",
                                  metric="Accuracy",tuneGrid=tunegrid,trControl=control,allowParallel=TRUE)
model_rf_smote_cv

#mtry  Accuracy   Kappa    
#1    0.6579766  0.3159501
#2    0.6925296  0.3850569
#3    0.7110746  0.4221487
#4    0.7154817  0.4309648
#5    0.7180590  0.4361200
#6    0.7183935  0.4367884
#7    0.7187259  0.4374541
#8    0.7189340  0.4378693
#9    0.7189754  0.4379535
#10   0.7198901  0.4397825

plot(model_rf_smote_cv)

# Accuracy was used to select the optimal model using the largest value.
# The final value used for the model was mtry = 10.

# Constructing final random forest model with mtry=10 and ntree=1000
gc()
model_rf_smote_cv1 <- randomForest(Performance.Tag~.,data=master_df_smote.RForest_train,importance=TRUE,
                                   ntree=1000,mtry=10,do.trace=TRUE,proximity=FALSE)


#Prediction on test data
master_df_smote.RForest_tune_test <- dt_woe_list$test
master_df_rforest_tune_pred <- predict(model_rf_smote_cv1,master_df_smote.RForest_tune_test,type = 'prob')

# Model Evaluation 

# Model evaluation confusion matrix
# Let us find the cut off
test_actual_defaulter <- factor(ifelse(master_df_smote.RForest_tune_test$Performance.Tag==1,"yes","no"))
perform_fn <- function(cutoff) 
{
  predicted_defaulter <- factor(ifelse(master_df_rforest_tune_pred[,2] >= cutoff, "yes", "no"))
  conf <- confusionMatrix(predicted_defaulter, test_actual_defaulter, positive = "yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Creating cutoff values from 0.01 to 0.80 for plotting and initiallizing a matrix of 100 X 3.

# Summary of test probability

#summary(master_df_rforest_pred)

s = seq(.01,.80,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.50)]
cutoff 

#[1] 0.08181818 0.08979798 0.09777778 0.10575758 0.11373737 0.12171717 0.12969697 0.13767677 0.14565657 0.15363636 0.16161616
#[12] 0.16959596 0.17757576 0.18555556 0.19353535 0.20151515 0.20949495 0.21747475 0.22545455 0.23343434 0.24141414 0.24939394
#[23] 0.25737374 0.26535354 0.27333333 0.28131313 0.28929293 0.29727273 0.30525253 0.31323232 0.32121212 0.32919192 0.33717172
#[34] 0.34515152 0.35313131 0.36111111 0.36909091 0.37707071 0.38505051 0.39303030 0.40101010 0.40898990 0.41696970 0.42494949
#[45] 0.43292929 0.44090909 0.44888889 0.45686869 0.46484848 0.47282828 0.48080808 0.48878788 0.49676768 0.50474747 0.51272727
#[56] 0.52070707 0.52868687 0.53666667 0.54464646 0.55262626

# Based on plot and checking with various values around cut off point, 0.3451515 is chosen as final one. 

#Confusion matrix
master_df_rforest_tune_defaulter <- factor(ifelse(master_df_rforest_tune_pred[,2] >=0.32919192, "yes", "no"))

master_df_rforest_tune_confusionmatrix <- confusionMatrix(master_df_rforest_tune_defaulter,test_actual_defaulter,positive="yes")
master_df_rforest_tune_confusionmatrix

#            Reference
#Prediction    no   yes
#    no     11977   371
#   yes      7835   517

#Accuracy
master_df_rforest_tune_confusionmatrix$overall[1]
#0.6035749

#Sensitivity
master_df_rforest_tune_confusionmatrix$byClass[1]
#0.5822072

#Specificity
master_df_rforest_tune_confusionmatrix$byClass[2]

#0.6045326

# KS Statistics

library(ROCR)
#on testing  data

predictions_rf <- ifelse(master_df_rforest_tune_defaulter =="yes",1,0)
test_actual_defaulter <- ifelse(test_actual_defaulter == "yes",1,0)

predict_def_test_object_rfoest <- prediction(predictions_rf,test_actual_defaulter)

performance_measure_test_rfoest <- performance(predict_def_test_object_rfoest,"tpr","fpr")
ks_table_test_rfoest <- attr(performance_measure_test_rfoest,"y.values")[[1]]-attr(performance_measure_test_rfoest,"x.values")[[1]]

max(ks_table_test_rfoest)
#0.1867398

## Lift and Gain Chart of the Model
# 
# Plotting the lift chart

find_rforest_tune_model_lift <- function(var_name , predicted_prob_val,groups=10) {
  
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

defaulter_decile = find_rforest_tune_model_lift(test_actual_defaulter, predictions_rf, groups = 10)
defaulter_decile

#bucket total totalresp Cum_resp_val  Gain Cum_lift
#<int> <int>     <dbl>        <dbl> <dbl>    <dbl>
#1      1  2070       120          120  13.5     1.35
#2      2  2070       118          238  26.8     1.34
#3      3  2070       140          378  42.6     1.42
#4      4  2070       138          516  58.1     1.45
#5      5  2070        60          576  64.9     1.30
#6      6  2070        65          641  72.2     1.20
#7      7  2070        61          702  79.1     1.13
#8      8  2070        54          756  85.1     1.06
#9      9  2070        60          816  91.9     1.02
#10     10  2070        72          888 100       1  

# Gain graph
ggplot(defaulter_decile,aes(x=bucket,y=(Gain/100)))+geom_line()+ylab("Gain")

# Lift Graph
ggplot(defaulter_decile,aes(x=bucket,y=Cum_lift))+geom_line() +ylab("Lift")

# ROC Curve
ROCData <- data.frame(OUT)
ggplot(ROCData,aes(x=X1,y=1-(X2)))+geom_line() + ylab("sensitivity")+ xlab("1- specificity")

# Summary of all the models is as below:
#---------------------------------------------------------------------------------------------------------------------------------
# Model Type						Accuracy	Sensitivity	Specificity	KS Statistic	ProbCutoff
#---------------------------------------------------------------------------------------------------------------------------------
#Biased Data - Logistic Regression	    		0.6342029	0.6204955	0.6348173	0.2553128	0.0450000
#Biased Data - Random Forest				0.6227053	0.6103604	0.6232586	0.2336190	0.0388000
#Unbiased Data - SMOTE - Logistic Regression		0.6794686	0.5540541	0.6850898	0.2391439	0.5346465
#Unbiased Data - SMOTE - Random Forest without CV	0.6307246	0.5900901	0.6325459 	0.2226360	0.3611111
#Unbiased Data - SMOTE - Random Forest with CV		0.6035749	0.5822072	0.6045326	0.1867398	0.3291919
#---------------------------------------------------------------------------------------------------------------------------------

# Considering all above models, Random Forest without CV on unbised data has been chosen.

#-------------------------------------------------------------------------------------------------------------------------#
# Score Card Preparation and Financial Analysis on Test Data Set
#-------------------------------------------------------------------------------------------------------------------------#

# Application Score Card and Financial Analysis for Random Forest Unbised Data without CV.
Factor <- (20/log(2))
offset <- 400 - (Factor*log(10))

# Random Forest - Unbiased without CV

rf_unbiased_odd <- ( 1 - 0.36111111) / 0.36111111 # Based on cut off probability we derived for the model.
rf_unbiased_logodd <- log(rf_unbiased_odd)
rf_unbiased_cutoff_score <- (offset + Factor*rf_unbiased_logodd)
rf_unbiased_cutoff_score
# Cut off score for RF unbiased data model without CV is 350.0239 for approval / rejection. Applicants having more than this 
# score shall only be granted a credit card. 

# Create master df for score card calculation and financial benefit calculation.
rf_unbiased_actual_default <- data.frame(master_df_smote.RForest_tune_test$Performance.Tag)
View(rf_unbiased_actual_default)

rf_unbiased_predicted_default <- data.frame(master_df_rforest_defaulter)
rf_unbiased_predicted_default <- ifelse(rf_unbiased_predicted_default=="yes",1,0)
View(rf_unbiased_predicted_default)

rf_unbiased_probability <- predict(model_rf_smote,master_df_smote.RForest_test,type='prob')
View(rf_unbiased_probability)

rf_unbiased_scorecard <- data.frame(cbind(rf_unbiased_actual_default,rf_unbiased_predicted_default,rf_unbiased_probability[,2]))
colnames(rf_unbiased_scorecard) <- c("actual.default","predicted.default","default.probability")

# Check confusion matrix and verify that it matches with what we derived during RF modelling.
rf_unbiased_confusionmatrix <- confusionMatrix(factor(rf_unbiased_scorecard$predicted.default),factor(rf_unbiased_scorecard$actual.default),positive="1")
rf_unbiased_confusionmatrix

#Prediction     
#     0     1
#0 12532   364
#1  7280   524
# Confusion Matrix matches the expectations.

# Change the value of 0 and 1 probability such that odds is not infinity.
rf_unbiased_scorecard$default.probability <- ifelse(rf_unbiased_scorecard$default.probability==0.000,0.0001,rf_unbiased_scorecard$default.probability)
rf_unbiased_scorecard$default.probability <- ifelse(rf_unbiased_scorecard$default.probability==1.000,0.9999,rf_unbiased_scorecard$default.probability)
summary(rf_unbiased_scorecard)

#Let us calculate Odds  based on probability of the events
rf_unbiased_scorecard <- rf_unbiased_scorecard %>% mutate(Odds = (1-rf_unbiased_scorecard$default.probability)/rf_unbiased_scorecard$default.probability)

rf_unbiased_scorecard <- rf_unbiased_scorecard %>% mutate(Log_Odds = log(rf_unbiased_scorecard$Odds))

rf_unbiased_scorecard <- rf_unbiased_scorecard %>% mutate(Score = (offset + (Factor*rf_unbiased_scorecard$Log_Odds)))

rf_unbiased_scorecard$Outstanding.Balance <- (test_data_balance$Outstanding.Balance)

rf_unbiased_scorecard$lossatdefault <- (rf_unbiased_scorecard$Outstanding.Balance * rf_unbiased_scorecard$default.probability)

summary(rf_unbiased_scorecard$Score)

#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#164.0   336.7   364.8   408.9   462.8   599.3

View(rf_unbiased_scorecard)

#Histogram plot
ggplot(rf_unbiased_scorecard,aes(Score))+geom_histogram() +geom_vline(xintercept= 350.0239,col="blue")

View(rf_unbiased_scorecard)

write.csv(rf_unbiased_scorecard, "rf_unbiased_scorecard.csv") # Exporting score card for any data validation / graphical analysis.

#-------------------------------------------------------------------------------------------------------------------------
# Applicants who are actually defaulted could have been avoided by applying model based scoring criteria.
#-------------------------------------------------------------------------------------------------------------------------

# Total population of test data.

total_test_applicants <- count(rf_unbiased_scorecard)
total_test_applicants # There are 20700 applicants in test data.

# Let's first check total number of defaulted applicants. 

total_defaulted_applicants <- (rf_unbiased_scorecard[rf_unbiased_scorecard$actual.default == 1,])
count_defaulted_applicants <- count(total_defaulted_applicants) 
count_defaulted_applicants # There are total 888 applicants defaulted.
actual_loss_at_default <- sum(total_defaulted_applicants$Outstanding.Balance)
actual_loss_at_default # Total 1134143324 is outstanding balance for all defaulted applicants. 

# Let's see how many of them model could have rejected based on cut off score.

total_rejected_applicants <- (total_defaulted_applicants[total_defaulted_applicants$Score < 350.0239,])

count_rejected_applicants <- count(total_rejected_applicants)
count_rejected_applicants # There are total 524 applicants defaulted.

percent_avoided_by_model <- (count_rejected_applicants / count_defaulted_applicants) * 100
percent_avoided_by_model
# ~59.00% of total default applicants could have been avoided had model cut off score would have been applied.

#--------------------------------------------------------------------------------------------------------------------------
# Reduce Credit Loss - Outstanding balance of the applicants who are actually defaulted and model could have rejected 
# based on scoring criteria as percentage of total outstanding balance of defaulted applicants.
#-------------------------------------------------------------------------------------------------------------------------

# Here credit loss for model is outstanding balance for the cases where score is lower than cut-off score. 

saved_credit_loss <- sum(total_rejected_applicants$Outstanding.Balance)
saved_credit_loss # 387037869

saved_credit_loss_for_default_applicants_percent <- ( saved_credit_loss / actual_loss_at_default ) * 100
saved_credit_loss_for_default_applicants_percent
# 59.51572% of total credit loss as per actual default could have been avoided by model by applying cut-off score. 

#--------------------------------------------------------------------------------------------------------------------------
# Applicants who are actually not defaulted but model could have rejected based on score (potential revenue loss due to
# rejection of good customers)
#-------------------------------------------------------------------------------------------------------------------------

# Let's first check total number of non-defaulted applicants. 

total_nondefaulted_applicants <- (rf_unbiased_scorecard[rf_unbiased_scorecard$actual.default == 0,])
count_nondefaulted_applicants <- count(total_nondefaulted_applicants) 
count_nondefaulted_applicants # There are total 19812 applicants who have not defaulted.

# Let's see how many of them model could have rejected based on cut off score.

count_rejected_applicants_nondefault <- nrow(total_nondefaulted_applicants[total_nondefaulted_applicants$Score < 350.0239,])
count_rejected_applicants_nondefault # There are total 7280 applicants who would have been rejected by model.

good_customers_rejected <- (count_rejected_applicants_nondefault / count_nondefaulted_applicants) * 100
good_customers_rejected
# 36.74541% good customers were rejected by model based on cutoff scoring criteria. This is a potential revenue loss due 
# to use of this model. 

######################################################################################################################
# Score Card Preparation on Rejected Population
######################################################################################################################

# Creating data set for rejected population.
master_df_without_pTag_rej <- master_df[is.na(master_df$Performance.Tag),]
summary(master_df_without_pTag_rej)

# Replacing NA values for Performance Tag
master_df_without_pTag_rej$Performance.Tag[is.na(master_df_without_pTag_rej$Performance.Tag)] <- ""
summary(master_df_without_pTag_rej)

# Let's now create classification of some of the variables in line with what has been done with accepted population.

# Converting number of dependents variable to factor.
master_df_without_pTag_rej$No.of.dependents <- as.factor(master_df_without_pTag_rej$No.of.dependents)

#No.of.months.in.current.residence. Let's bucket this into 3 levels
master_df_without_pTag_rej$No.of.months.in.current.residence <- as.factor(master_df_without_pTag_rej$No.of.months.in.current.residence)

levels(master_df_without_pTag_rej$No.of.months.in.current.residence) # Checking initial levels

levels(master_df_without_pTag_rej$No.of.months.in.current.residence)[1:6]<-"less_than_one_year"
levels(master_df_without_pTag_rej$No.of.months.in.current.residence) # Verifying revised levels

levels(master_df_without_pTag_rej$No.of.months.in.current.residence)[2:50] <- "one_year_to_five_years"
levels(master_df_without_pTag_rej$No.of.months.in.current.residence) # Verifying revised levels

levels(master_df_without_pTag_rej$No.of.months.in.current.residence)[3:65] <- "more_than_five_years"
levels(master_df_without_pTag_rej$No.of.months.in.current.residence) # Verifying final levels


#No.of.months.in.current.company. Let's bucket this into 3 levels
master_df_without_pTag_rej$No.of.months.in.current.company <- as.factor(master_df_without_pTag_rej$No.of.months.in.current.company)

levels(master_df_without_pTag_rej$No.of.months.in.current.company) # Checking initial levels

levels(master_df_without_pTag_rej$No.of.months.in.current.company)[1:22]<-"0_2 years"
levels(master_df_without_pTag_rej$No.of.months.in.current.company) # Verifying revised levels

levels(master_df_without_pTag_rej$No.of.months.in.current.company)[2:25] <- "2_4 years"
levels(master_df_without_pTag_rej$No.of.months.in.current.company) # Verifying revised levels

levels(master_df_without_pTag_rej$No.of.months.in.current.company)[3:29] <- "more_than_4 years"
levels(master_df_without_pTag_rej$No.of.months.in.current.company) # Verifying final levels


#Avgas.CC.Utilization.in.last.12.months.Let's bucket this into 3 levels
master_df_without_pTag_rej$Avgas.CC.Utilization.in.last.12.months <- as.factor(master_df_without_pTag_rej$Avgas.CC.Utilization.in.last.12.months)

levels(master_df_without_pTag_rej$Avgas.CC.Utilization.in.last.12.months) # Checking initial levels

levels(master_df_without_pTag_rej$Avgas.CC.Utilization.in.last.12.months)[1:9]<-"avg_max_once_in_month"
levels(master_df_without_pTag_rej$Avgas.CC.Utilization.in.last.12.months) # Verifying revised levels

levels(master_df_without_pTag_rej$Avgas.CC.Utilization.in.last.12.months)[2:37] <- "avg_4_times_a_month"
levels(master_df_without_pTag_rej$Avgas.CC.Utilization.in.last.12.months) # Verifying revised levels

levels(master_df_without_pTag_rej$Avgas.CC.Utilization.in.last.12.months)[3:53] <- "avg_more_than_4_times_month"
levels(master_df_without_pTag_rej$Avgas.CC.Utilization.in.last.12.months) # Verifying final levels

#Total.No.of.Trades.Let's bucket this into levels
master_df_without_pTag_rej$Total.No.of.Trades <- as.factor(master_df_without_pTag_rej$Total.No.of.Trades)

levels(master_df_without_pTag_rej$Total.No.of.Trades) # Checking initial levels

levels(master_df_without_pTag_rej$Total.No.of.Trades)[1:5]<-"1to5_times"
levels(master_df_without_pTag_rej$Total.No.of.Trades) # Verifying revised levels

levels(master_df_without_pTag_rej$Total.No.of.Trades)[2:6] <- "6to10_times"
levels(master_df_without_pTag_rej$Total.No.of.Trades) # Verifying revised levels

levels(master_df_without_pTag_rej$Total.No.of.Trades)[3:7] <- "11to15_times"
levels(master_df_without_pTag_rej$Total.No.of.Trades) # Verifying revised levels

levels(master_df_without_pTag_rej$Total.No.of.Trades)[4:8] <- "16to20_times"
levels(master_df_without_pTag_rej$Total.No.of.Trades) # Verifying revised levels

levels(master_df_without_pTag_rej$Total.No.of.Trades)[5:9] <- "21to25_times"
levels(master_df_without_pTag_rej$Total.No.of.Trades) # Verifying revised levels

levels(master_df_without_pTag_rej$Total.No.of.Trades)[6:11] <- "Above25_times"
levels(master_df_without_pTag_rej$Total.No.of.Trades) # Verifying final levels

# Converting Loan Status to factors.

master_df_without_pTag_rej$Presence.of.open.auto.loan <- ifelse(master_df_without_pTag_rej$Presence.of.open.auto.loan == 1, "Yes", "No")
master_df_without_pTag_rej$Presence.of.open.auto.loan <- as.factor(master_df_without_pTag_rej$Presence.of.open.auto.loan)

master_df_without_pTag_rej$Presence.of.open.home.loan <- ifelse(master_df_without_pTag_rej$Presence.of.open.home.loan == 1, "Yes", "No")
master_df_without_pTag_rej$Presence.of.open.home.loan <- as.factor(master_df_without_pTag_rej$Presence.of.open.home.loan)

# Converting categorical variables to factors.
master_df_without_pTag_rej$Gender <- as.factor(master_df_without_pTag_rej$Gender)
master_df_without_pTag_rej$Profession <- as.factor(master_df_without_pTag_rej$Profession)
master_df_without_pTag_rej$Education <- as.factor(master_df_without_pTag_rej$Education)
master_df_without_pTag_rej$Type.of.residence <- as.factor(master_df_without_pTag_rej$Type.of.residence)
master_df_without_pTag_rej$Marital.Status..at.the.time.of.application. <- as.factor(master_df_without_pTag_rej$Marital.Status..at.the.time.of.application.)

str(master_df_without_pTag_rej)
summary(master_df_without_pTag_rej)

# WOE transformation

master_filter_rej = master_df_without_pTag_rej[colnames(master_df_without_pTag_rej) %in% colnames(dt_f)]
master_filter_rej <- master_filter_rej[!colnames(master_filter_rej) %in% c("Performance.Tag")]

summary(master_filter_rej)

# Converting into WoE values
dt_woe_list_rej = data.frame(woebin_ply(master_filter_rej,bins))
summary(dt_woe_list_rej)

#We will run the model on WOe transformed data
rejected_rforest_pred <- predict(model_rf_smote,dt_woe_list_rej,type = 'prob')
summary(rejected_rforest_pred)

# Let's prepare the score card
head(rejected_rforest_pred[,2])
master_df_without_pTag_rej$pred_prob_nondef <- rejected_rforest_pred[,2]

summary(master_df_without_pTag_rej$pred_prob_nondef)

#Let us calculate Odds  based on probability of the events
master_df_without_pTag_rej <- master_df_without_pTag_rej %>% mutate(Odds = (1-pred_prob_nondef)/pred_prob_nondef)
head(master_df_without_pTag_rej)

master_df_without_pTag_rej <- master_df_without_pTag_rej %>% mutate(Log_Odds = log(Odds))
head(master_df_without_pTag_rej)

Factor <- (20/log(2))
offset <- 400 - (Factor*log(10))
master_df_without_pTag_rej <- master_df_without_pTag_rej %>% mutate(Score = (offset + (Factor*master_df_without_pTag_rej$Log_Odds)))

#Histogram plot with score cut off of 350.0239
ggplot(master_df_without_pTag_rej,aes(Score))+geom_histogram() +geom_vline(xintercept= 350.0239,col="blue")
#-------------End of Assignment----------------------------------------------------------------------------------------------#