#Clear environment variables
remove (list=ls())

# load the library
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(stringr)
library(reshape2)
library(chron)

#Set Working directory
#setwd("H:/PG Diploma IN Data Science IIITB/Course 2/EDA group case study")
#getwd()

#Read UBER Request Data csv file
loan_data <- read.csv('loan.csv',stringsAsFactors = F)

#Check the structure of the data set
str(loan_data)
head(loan_data)
tail(loan_data)

##Data Clean up------##

#1.Following variables are having only NA values.Hence we will remove them.

#mths_since_last_major_derog
#annual_inc_joint
#dti_joint
#verification_status_joint
#tot_coll_amt
#tot_cur_bal	
#open_acc_6m	
#open_il_6m	
#open_il_12m	
#open_il_24m	
#mths_since_rcnt_il	
#total_bal_il	
#il_util	
#open_rv_12m	
#open_rv_24m	
#max_bal_bc	
#all_util	
#total_rev_hi_lim	
#inq_fi	
#total_cu_tl
#inq_last_12m	
#acc_open_past_24mths	
#avg_cur_bal	
#bc_open_to_buy	
#bc_util
#mo_sin_old_il_acct	
#mo_sin_old_rev_tl_op	
#mo_sin_rcnt_rev_tl_op	
#mo_sin_rcnt_tl	
#mort_acc	
#mths_since_recent_bc	
#mths_since_recent_bc_dlq	
#mths_since_recent_inq	
#mths_since_recent_revol_delinq	
#num_accts_ever_120_pd	
#num_actv_bc_tl	
#num_actv_rev_tl	
#num_bc_sats	
#num_bc_tl	
#num_il_tl	
#num_op_rev_tl
#num_rev_accts	
#num_rev_tl_bal_gt_0	
#num_sats	
#num_tl_120dpd_2m	
#num_tl_30dpd	
#num_tl_90g_dpd_24m	
#num_tl_op_past_12m	
#pct_tl_nvr_dlq	
#percent_bc_gt_75
#tot_hi_cred_lim	
#total_bal_ex_mort	
#total_bc_limit	
#total_il_high_credit_limit

loan_data <- loan_data[,!(names(loan_data) %in% c("mths_since_last_major_derog",
                                                  "annual_inc_joint",
                                                  "dti_joint",
                                                  "verification_status_joint",
                                                  "tot_coll_amt",
                                                  "tot_cur_bal",
                                                  "open_acc_6m",
                                                  "open_il_6m",
                                                  "open_il_12m",
                                                  "open_il_24m",
                                                  "mths_since_rcnt_il",
                                                  "total_bal_il",
                                                  "il_util",
                                                  "open_rv_12m",
                                                  "open_rv_24m",
                                                  "max_bal_bc",
                                                  "all_util",
                                                  "total_rev_hi_lim",
                                                  "inq_fi",
                                                  "total_cu_tl",
                                                  "inq_last_12m",
                                                  "acc_open_past_24mths",
                                                  "avg_cur_bal",
                                                  "bc_open_to_buy",
                                                  "bc_util",
                                                  "mo_sin_old_il_acct",
                                                  "mo_sin_old_rev_tl_op",
                                                  "mo_sin_rcnt_rev_tl_op",
                                                  "mo_sin_rcnt_tl",
                                                  "mort_acc",
                                                  "mths_since_recent_bc",
                                                  "mths_since_recent_bc_dlq",
                                                  "mths_since_recent_inq",
                                                  "mths_since_recent_revol_delinq",
                                                  "num_accts_ever_120_pd",
                                                  "num_actv_bc_tl",
                                                  "num_actv_rev_tl",
                                                  "num_bc_sats",
                                                  "num_bc_tl",
                                                  "num_il_tl",
                                                  "num_op_rev_tl",
                                                  "num_rev_accts",
                                                  "num_rev_tl_bal_gt_0",
                                                  "num_sats",
                                                  "num_tl_120dpd_2m",
                                                  "num_tl_30dpd",
                                                  "num_tl_90g_dpd_24m",
                                                  "num_tl_op_past_12m",
                                                  "pct_tl_nvr_dlq",
                                                  "percent_bc_gt_75",
                                                  "tot_hi_cred_lim",
                                                  "total_bal_ex_mort",
                                                  "total_bc_limit",
                                                  "total_il_high_credit_limit"))]

#2. Following variables are having single value in our current dataset
#initial_list_status = "f"
#policy_code	= 1
#application_type	= "INDIVIDUAL"
#acc_now_delinq = 0
#delinq_amnt = 0
#pymnt_plan = "n"

#Here we assume, our analysis scope is limited to 'fractional'(f) initial list  status for 'Individual' application type and payment plan as 'n'.
#with policy code 1 .There is zero delinquent account now with delinquent amount zero.
#Hence we remove these variables from our dataset.

loan_data <- loan_data[,!(names(loan_data) %in% c("initial_list_status",
                                                  "policy_code",
                                                  "application_type",
                                                  "acc_now_delinq",
                                                  "delinq_amnt",
                                                  "pymnt_plan"))]

str(loan_data)

#3.Missing value issues.
#There are few blank values in some variables and we will convert them all into NA value.

is.na(loan_data) <- loan_data==""
which(is.na(loan_data$title))
#4.Some monetory variables are having more than 2 decimal places.Hence we will be rounding them
#of to two digit places to make it standardised monetary value.
#funded_amnt_inv
#total_pymnt
#total_rec_late_fee
#collection_recovery_fee

loan_data$funded_amnt_inv <- round(loan_data$funded_amnt_inv,2)
loan_data$total_pymnt <- round(loan_data$total_pymnt,2)
loan_data$total_rec_late_fee <- round(loan_data$total_rec_late_fee,2)
loan_data$collection_recovery_fee <- round(loan_data$collection_recovery_fee,2)

#5.Following variables are having value as NA for less than equal to 0.1% records
#and for rest of the records they are having value as zero.
#Hence we are assuming these variables are having same value for all the records and 
#do not have significant effect on the decision.Hence we will remove them from our analysis data frame
#collections_12_mths_ex_med = 0
#chargeoff_within_12_mths = 0
#tax_liens = 0

loan_data <- loan_data[,!(names(loan_data) %in% c("collections_12_mths_ex_med",
                                                  "chargeoff_within_12_mths",
                                                  "tax_liens"))]

#
#write.csv(loan_data,"loan_data_cleaned.csv")
str(loan_data)

#6.Following variables are to represent dates but are in character format.
#Hence for analysis purpose we will be converting them into date formats
#
#issue_d
#earliest_cr_line
#last_pymnt_d
#last_credit_pull_d
#next_pymnt_d

firstday <- "01"

loan_data$issue_d <- paste(firstday,loan_data$issue_d,sep="-")
loan_data$issue_d <- as.Date(loan_data$issue_d,"%d-%b-%y")

loan_data$earliest_cr_line <- paste(firstday,loan_data$earliest_cr_line,sep="-")

#used chron library to convert dates before 1970 into dateformat.Since earliest credit line has dates before 1970 and this variable is under analysis.
loan_data$earliest_cr_line <- as.Date(chron(format(as.Date(loan_data$earliest_cr_line,"%d-%b-%y"),"%m/%d/%Y")))


loan_data$last_pymnt_d <- paste(firstday,loan_data$last_pymnt_d,sep="-")
loan_data$last_pymnt_d <- as.Date(loan_data$last_pymnt_d,"%d-%b-%y")

loan_data$last_credit_pull_d <- paste(firstday,loan_data$last_credit_pull_d,sep="-")
loan_data$last_credit_pull_d <- as.Date(loan_data$last_credit_pull_d,"%d-%b-%y")

loan_data$next_pymnt_d <- paste(firstday,loan_data$next_pymnt_d,sep="-")
loan_data$next_pymnt_d <- as.Date(loan_data$next_pymnt_d,"%d-%b-%y")

#7.url variable has similar url for all records and it differs by loan id only.For Loan id we have separate variable already.
#Hence we consider url variable to be redundant.
#Also,we consider desc variable to be redundant as we already have pupose and title variable to segrgate loan category.
#We will be removing these two variable from our analysis data frame.

loan_data <- loan_data[,!(names(loan_data) %in% c("url",
                                                  "desc"))]

#8.Converting loan status as factor for ease of analysis
loan_data$loan_status <- as.factor(loan_data$loan_status)

#Uniqueness checking

#1. Unique loan id
loan_id_distinct <-nrow(distinct(loan_data,id)) #Hence each row indicates records for unique loan id

#2. Unique member id
member_id_distinct <-nrow(distinct(loan_data,member_id)) #Hence each row indicates records for unique member id

#3.Exploratory Data Analysis:
##

#a.installment per annual_inc/12 [monthly income] percentage and its effect on loan status
#Derived metrics:adding monthly income
loan_data$monthly_inc <- round(loan_data$annual_inc/12,2)
#Derived metrics: adding percentage of installment per monthly income
loan_data$emi_sal_percentage <- round(((loan_data$installment/loan_data$monthly_inc)*100),2)

#Another round of data cleaning to remove few outlier values in monthly income.
ggplot(loan_data,aes(emp_length,monthly_inc,col=loan_status))+geom_point(position='jitter',alpha=0.3)+theme(axis.text.x = element_text(angle = 90, hjust = 1))
#As the last plot suggests that there are some extreme outliers for emp_length 10+ years and 3 years.We exclude these two data for our analysis.
loan_data_wo_outlier <- loan_data %>% filter(!(monthly_inc >= 3.00E+05))

#Density plot to compare for charged off and fully paid status
ggplot(subset(loan_data_wo_outlier,(loan_status=="Charged Off"|loan_status=="Fully Paid")),aes(emi_sal_percentage,fill=loan_status))+geom_density(alpha=0.5)+labs(title="Installment per monthly income analysis",y="Density",x="Installment per monthly income percentage")

#Comparing average installment/monthly income percentage for different loan status
installment_sal_analysis <- loan_data_wo_outlier %>% group_by(loan_status) %>% summarise(avg_emi_sal_percentage = mean(emi_sal_percentage,na.rm=T))
ggplot(installment_sal_analysis,aes(loan_status,avg_emi_sal_percentage,fill='red'))+geom_col()+labs(title="Installment per monthly income \n analysis on Loan status",y="Average of installment monthly income percentage",x="Loan Status")+geom_text(aes(label = avg_emi_sal_percentage),position = position_dodge(0.9),vjust= 0)

#This  analysis indicates that Charged off rate i.e borrowers' being defaulted rises based on the borrowing capacity.
#When installment amount rises up than the monthly income, the risk of the borrower's getting defaulted increases.

##
#b.Multivariate analysis- To check correlation matrix for continuous  variable
mydata <- loan_data_wo_outlier[,c("funded_amnt","funded_amnt_inv","installment","annual_inc",
                                  "dti",
                                  "open_acc",
                                  "pub_rec",
                                  "revol_bal",
                                  "total_acc",
                                  "out_prncp",
                                  "out_prncp_inv",
                                  "total_pymnt",
                                  "total_pymnt_inv",
                                  "total_rec_prncp",
                                  "total_rec_int",
                                  "total_rec_late_fee",
                                  "recoveries",
                                  "collection_recovery_fee",
                                  "last_pymnt_amnt")]
head(mydata)
#create correlation matrix
cormat_loan_data <- round(cor(mydata),2)
head(cormat_loan_data)

#correlation heatmap
#melt the cormat_loan_data for heatmap plot
melt_cormat_loan_data <- melt(cormat_loan_data)
ggplot(melt_cormat_loan_data,aes(x=Var1,y=Var2,fill=value))+geom_tile()+theme(axis.text.x = element_text(angle = 90, hjust = 1))

#As correlation matrix  has redundant data, we will set half of the cormat loan matrix  as NA
get_upper_cormat_loan_data <- function(cormat_data){
  cormat_data[lower.tri(cormat_data)] <- NA
  return(cormat_data)
}
upper_cormat_loan_data_matrix <- get_upper_cormat_loan_data(cormat_loan_data)

#Plot melted lower cormat data in heatmap
melted_upper_cormat <- melt(upper_cormat_loan_data_matrix,na.rm=TRUE)

ggplot(data = melted_upper_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "red", high = "green", mid = "yellow",
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Correlation") +
  theme_minimal()+theme(axis.text.x = element_text(angle = 90, hjust = 1))

##
#c.Grade/subgrade analysis along with loan terms:

subgrade_plot <- ggplot(loan_data_wo_outlier,aes(sub_grade,fill=loan_status))+geom_bar(width=0.5,position = 'dodge')
subgrade_plot + labs(title="Sub-Gradewise Bar chart",y="Count",x="Sub-Grade")+theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
subgrade_plot2 <- ggplot(subset(loan_data_wo_outlier,loan_status=="Charged Off"),aes(sub_grade,fill=term))+geom_bar(width=0.5,position = 'dodge')
subgrade_plot2 + labs(title="Sub-Grade-Term analysis",y="Count",x="Sub-Grade")+theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Since sub-grade more granualar then grades we considered subgrade analysis. This analysis indicates that sub-grades B3 to C3 has a spike in 
# Charged Off rates and again at D2 there is  a spike in charged off rates which then subsides gradually.
# The analysis is more effective when we considered two different loan terms. For loan termof 36 months and sub grade B3 there is a increase of charged off rates,
# and for loan term 60 months and subgrade D2 there is a huge spike in charged off rates.

##
#d.Emp_length analysis:
emp_length_loan_stat <- loan_data_wo_outlier %>% group_by(emp_length,loan_status) %>% count(emp_length)
emp_length_plot <-ggplot(emp_length_loan_stat,aes(emp_length,n,fill=loan_status))+geom_col(position='dodge')
emp_length_plot + labs(title="Employment Length analysis",y="Loan request count",x="Employment Length")+theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Employment length analysis shows that for the borrowers' having 10+ years of experience charged off rate is high than the rest of the borrowers.
# Though it is to be noticed that Fully Paid rate is  also high for this segment borrowers. 

##
#e.Purpose analysis over loan status
write.csv(loan_data_wo_outlier,'loan_data_wo_outlier.csv')
loan_purpose_analysis <- loan_data_wo_outlier %>% group_by(purpose,loan_status) %>% count(loan_status)
ggplot(loan_data_wo_outlier,aes(purpose,fill=loan_status))+geom_bar(position='stack') + labs(title="Purpose Analysis",y="Loan request count",x="Borrower's Purpose")+theme(axis.text.x = element_text(angle = 90, hjust = 1))

## Analysis shows that purpose could a driving factor to predict defaulters. The above plot shows that for debt_consolidation,
# Charged off rate is high followed by charged off rate of others and credit card.

##
#f.State wise loan request analysis for loan status
#we segregate approved loan amnt funded_amnt in $0k-$5k, $5.1k- $10k, $10.1K-$15K,$15.1K-$20K,$20.1K Above based on our histogram plot
ggplot(loan_data_wo_outlier,aes(funded_amnt))+geom_histogram()
loan_data_wo_outlier$loan_amnt_range = ifelse(loan_data_wo_outlier$funded_amnt <= 5000, "Range: $0k-$5k", ifelse(loan_data_wo_outlier$funded_amnt <= 10000,"Range: $5.1k-$10k",ifelse(loan_data_wo_outlier$funded_amnt <= 15000, "Range: $10.1k-$15k",ifelse(loan_data_wo_outlier$funded_amnt <= 20000,"Range: $15.1k-$20k",ifelse(loan_data_wo_outlier$funded_amnt <= 30000,"Range: $20.1k-$30k","Range:$30k Above")))))
Statewise_loan_analysis <- loan_data_wo_outlier %>% group_by(loan_amnt_range,addr_state,loan_status) %>% count(loan_amnt_range)

#plot including all the loan status
ggplot(Statewise_loan_analysis,aes(addr_state,n,fill=loan_amnt_range))+geom_col()
#subplot for Charged off status
ggplot(subset(Statewise_loan_analysis,loan_status=="Charged Off"),aes(addr_state,n,fill=loan_amnt_range))+geom_col()+theme(axis.text.x = element_text(angle = 90, hjust = 1))+labs(title="State wise charged Off Loan request Analysis",y="Loan request count",x="State")

#Analysis is  showing for CA -California state charged off count is higher.Since, loan request is higher for this  state, charged off rate is  also higher.

##
#g. Considering earliest credit line as proxy for age

age_analysis <- loan_data_wo_outlier %>% group_by(years = year(earliest_cr_line),loan_status) %>% count(loan_status)
ggplot(age_analysis,aes(x=years))+geom_line(aes(y=n,col=loan_status))+labs(title="Earliest Credit Line Analysis",y="Loan request count",x="Earliest Credit Line Year")

## Analysisis  showing there is a spike in charged off rates for the borrowers' whose cedit line started around year 2000.

##
##End of Analysis------------##


