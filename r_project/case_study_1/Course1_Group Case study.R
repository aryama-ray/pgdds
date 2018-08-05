
#Load the companies and rounds data into two data frames and name them companies and rounds2 respectively.
#Set the location of input files
#setwd("H:/PG Diploma IN Data Science IIITB/Group Case Study/03_Data Preparation/Input")
#getwd()

#Including required libraries

library(gdata)
library(dplyr)
library(tidyr)

#Read from Companies text file
companies <-  read.delim("companies.txt",header=TRUE,stringsAsFactors = FALSE)

#Read from rounds2 csv file
rounds2 <- read.csv("rounds2.csv",header=TRUE,stringsAsFactors = FALSE)

#Convert permalink column of companies table in lowercase
companies_lower <- companies %>% mutate(permalink = tolower(companies[,"permalink"]))

#Convert company_permalink column of rounds2 table in lowercase
rounds2_lower <- rounds2 %>% mutate(company_permalink = tolower(rounds2[,"company_permalink"]))

##---------------Checkpoint 1: Data Cleaning 1----------------------------------------------------##
#Understanding the data set

##Table-1.1- Unique companies present in rounds2
rounds2_lower_distinct <-distinct(rounds2_lower,company_permalink)
no_of_unique_rounds2 <- nrow(rounds2_lower_distinct)

##Table-1.1- Unique companies present in companies
companies_lower_distinct <-distinct(companies_lower,permalink)
no_of_unique_companies <- nrow(companies_lower_distinct)

##Table-1.1-In the companies data frame, which column can be used as the unique key for each company? Write the name of the column.
##permalink. (as it is the Unique ID of company)

##Table-1.1-Are there any companies in the rounds2 file which are not present in companies ? Answer Y/N.
rounds2_lower_arrange <- rounds2_lower_distinct %>% arrange(desc(company_permalink))
companies_lower_arrange <- companies_lower_distinct %>% arrange(desc(permalink))
sum(rounds2_lower_arrange$company_permalink!=companies_lower_arrange$permalink)
#Sum of not matching permalink fields in both the table implies that there  are no companies in the rounds2 file which are not present in companies.


##Table-1.1-Merge the two data frames so that all variables (columns) 
#in the companies frame are added to the rounds2 data frame.
#Name the merged frame master_frame. How many oservations are present in master_frame ?

master_frame <- merge(rounds2_lower,companies_lower,by.x="company_permalink",by.y="permalink",all=T)
observation_master_frame <- nrow(master_frame)


##-------------------------------End of CheckPoint 1----------------------------------------------------##

##-------------------------Checkpoint 2: Funding Type Analysis------------------------------------------##

#Table 2.1-Calculate the average investment amount for each of the four funding types (venture, angel, seed, and private equity)
#From master frame here we are selecting two fields 'funding_round_type' and 'raised_amount_usd' and filetring data out based on given 4-funding round  types.
#We are aggregating investment amount grouping on funding round types.

avg_investment_selected <- master_frame %>% select(funding_round_type, raised_amount_usd) %>% filter(funding_round_type %in% c("venture", "angel", "seed","private_equity"))
grp_by_funding <- group_by(avg_investment_selected,funding_round_type)
avg_investment <- summarise(grp_by_funding,avg_val=  round(mean(raised_amount_usd,na.rm = T),digits = 0))
avg_investment_sorted <- arrange(avg_investment,desc(avg_val))

#Considering that Spark Funds wants to invest between 5 to 15 million USD per investment round, which investment type is the most suitable for them?
spark_fund_invest <- avg_investment_sorted %>% filter(avg_val> 5000000 & avg_val<= 15000000)

##-------------------------------End of CheckPoint 2----------------------------------------------------##

##--------------------------Checkpoint 3: Country Analysis----------------------------------------------##

# top nine countries which have received the highest total funding (across ALL sectors for the chosen investment type)

funding_filter <- master_frame %>% select(funding_round_type, country_code,raised_amount_usd) %>% filter(funding_round_type == "venture")
group_by_funding_filter <- group_by(funding_filter,country_code)
country_funding <- summarise(group_by_funding_filter,tot_funding=sum(raised_amount_usd,na.rm=T))
country_funding_arranged <- arrange(country_funding,desc(tot_funding))
#As there is a blank country code we are considering first 10 countries with highest funding amount to get top 9.
top9 <- head(country_funding_arranged,n=10)

#top3 english speaking countries
library(countrycode)
mycode<-c("USA","GBR","IND","CAN","CHN","JPN","FRA","ISR","DEU")
Eng_speaking_countries <- top9 %>% filter(country_code %in% c("USA","GBR","IND","CAN")) %>% mutate(countryname = countrycode(country_code,origin = "iso3c",destination = "country.name"))
top3_Eng_speaking_countries <-head(Eng_speaking_countries,n=3)


##-------------------------------End of CheckPoint 3----------------------------------------------------##


##-------------------------------Checkpoint 4: Sector Analysis 1----------------------------------------##

###Check Point 4
mapping <- read.csv("mapping.csv",stringsAsFactors = FALSE,header = TRUE,sep=",")

#separating primary sectors

#company_primary_sector <- separate(companies_lower,"category_list",into=c("primary_sector","secondary_sector"),sep="[\\|]")
master_primary_sector <- separate(master_frame,"category_list",into=c("primary_sector","secondary_sector"),sep="[\\|]")

#converting primary sector values from company table into lower case
master_primary_sector_lower <- master_primary_sector %>% mutate(primary_sector=tolower(master_primary_sector[,"primary_sector"]))

#converting primary sector values from mapping table into lower case and data cleaning on category name
mapping_clean <- mapping %>% mutate(category_list = tolower(gsub('0','na',mapping[,"category_list"])))

#merging of comapany  and mapping dataset based on primary sector
main_sector_merge <- merge(master_primary_sector_lower,mapping_clean,by.x="primary_sector",by.y="category_list")

##-------------------------------End of CheckPoint 4----------------------------------------------------##


##-------------------------------Checkpoint 5: Sector Analysis 2----------------------------------------##

#Clean up of main sector merge file

main_sector_gather <- gather(main_sector_merge,main_sector,sector_val,Automotive...Sports:Social..Finance..Analytics..Advertising)
main_sector_gather_clean <- main_sector_gather[!(main_sector_gather$sector_val==0),]
main_sector_clean <- main_sector_gather_clean[,-18]

#Exlusion of Blank Main sector 

main_sector_exclude_blank <- main_sector_clean[!(main_sector_clean$main_sector=="Blanks"),]

#Writing data for Tableau Plot
#write.csv(main_sector_exclude_blank,"Main_Sector_Cleaned.csv")

###----- Analysis for D1 dataframe-------------------------------###

#Dataframe D1 contains observations of funding type 'Venture' falling within the 5-15 million USD rangefalling within the 5-15 million USD range for Country USA.

D1_filter <- main_sector_exclude_blank %>% filter(country_code=="USA",funding_round_type=="venture",raised_amount_usd %in% 5000000:15000000)


# D1_inv_count dataframe counts the number of investment for each main sectors
D1_inv_count <- D1_filter %>% group_by(main_sector) %>% count(main_sector)

# D1_inv_usd dataframe sums up the investment amount in usd for each main sectors
D1_inv_usd <- D1_filter %>% group_by(main_sector) %>% summarise(total_amount_invested=sum(raised_amount_usd,na.rm = T))

# D1_investment merges both D1_inv_count and D1_inv_usd dataframes based  on main sector
D1_investment <- merge(D1_inv_usd,D1_inv_count,by="main_sector")
colnames(D1_investment) <- c("main_sector","total_amount_invested","total_number_investment")

#Merge D1 with investement count and amount for each main sector
D1 <- merge(D1_filter,D1_investment,by.x="main_sector",by.y="main_sector")
##

##Table 5.1 - Total number of Investments (count) for Country C1

C1_Total_number_of_Investment <- sum(D1_investment$total_number_investment)

##Table 5.1 - Total amount of investment (USD) for country C1

C1_Total_amount_of_investment <- sum(D1_investment$total_amount_invested)

##Table 5.1 - Top Sector name (no. of investment-wise) for country C1

C1_Top_Sector <- arrange(D1_investment,desc(total_amount_invested,total_number_investment))
C1_top <- C1_Top_Sector[1,"main_sector"]

##Table 5.1- Second Sector name (no. of investment-wise)
C1_top_second <- C1_Top_Sector[2,"main_sector"]

##Table 5.1 - Third Sector name (no. of investment-wise)
C1_top_third <- C1_Top_Sector[3,"main_sector"]

#Table 5.1 - Number of investments in top sector (3)
C1_top_inv_number <- C1_Top_Sector[1,"total_number_investment"]

#Table 5.1 -Number of investments in second sector (4)
C1_top_second_inv_number <- C1_Top_Sector[2,"total_number_investment"]

#Table 5.1- Number of investments in third sector (5)
C1_top_third_inv_number <- C1_Top_Sector[3,"total_number_investment"]

#For point 3 (top sector count-wise), which company received the highest investment?

#Assumption: Funding per round is restricted to 5-15 M USD
company_wise_investment <- D1 %>% filter(main_sector==C1_top) %>% group_by(name) %>% summarise(highest_val_inv=sum(raised_amount_usd,na.rm = T))

company_wise_investment_arranged <- arrange(company_wise_investment,desc(highest_val_inv))
company_top_investment <- company_wise_investment_arranged[1,"name"]

#For point 4 (second best sector count-wise), which company received the highest investment?

company_second_investment <- company_wise_investment_arranged[2,"name"]


###-----------------------------EOF D1 analysis-----------------------------------------####

###-----------------------Analysis for D2 dataframe-------------------------------------####

#Dataframe D2 contains observations of funding type 'Venture' falling within the 5-15 million USD range falling within the 5-15 million USD range for Country GBR.


D2_filter <- main_sector_exclude_blank %>% filter(country_code=="GBR",funding_round_type=="venture",raised_amount_usd %in% 5000000:15000000)

# D2_inv_count dataframe counts the number of investment for each main sectors
D2_inv_count <- D2_filter %>% group_by(main_sector) %>% count(main_sector)

# D2_inv_usd dataframe sums up the investment amount in usd for each main sectors
D2_inv_usd <- D2_filter %>% group_by(main_sector) %>% summarise(total_amount_invested=sum(raised_amount_usd,na.rm = T))

# D2_investment merges both D2_inv_count and D2_inv_usd dataframes basedon main sector
D2_investment <- merge(D2_inv_usd,D2_inv_count,by="main_sector")
colnames(D2_investment) <- c("main_sector","total_amount_invested","total_number_investment")

#Merge D2 with investement count and amount for each main sector
D2 <- merge(D2_filter,D2_investment,by.x="main_sector",by.y="main_sector")
##

##Table 5.1 - Total number of Investments (count) for Country C2

C2_Total_number_of_Investment <- sum(D2_investment$total_number_investment)

##Table 5.1 - Total amount of investment (USD) for country C2

C2_Total_amount_of_investment <- sum(D2_investment$total_amount_invested)

##Table 5.1 - Top Sector name (no. of investment-wise) for country C2

C2_Top_Sector <- arrange(D2_investment,desc(total_amount_invested,total_number_investment))
C2_top <- C2_Top_Sector[1,"main_sector"]

##Table 5.1- Second Sector name (no. of investment-wise)
C2_top_second <- C2_Top_Sector[2,"main_sector"]

##Table 5.1 - Third Sector name (no. of investment-wise)
C2_top_third <- C2_Top_Sector[3,"main_sector"]

#Table 5.1 - Number of investments in top sector (3)
C2_top_inv_number <- C2_Top_Sector[1,"total_number_investment"]

#Table 5.1 -Number of investments in second sector (4)
C2_top_second_inv_number <- C2_Top_Sector[2,"total_number_investment"]

#Table 5.1- Number of investments in third sector (5)
C2_top_third_inv_number <- C2_Top_Sector[3,"total_number_investment"]

#For point 3 (top sector count-wise), which company received the highest investment?

#Assumption: Funding per round is restricted to 5-15 M USD
D2_company_wise_investment <- D2 %>% filter(main_sector==C2_top) %>% group_by(name) %>% summarise(highest_val_inv=sum(raised_amount_usd,na.rm = T))

D2_company_wise_investment_arranged <- arrange(D2_company_wise_investment,desc(highest_val_inv))
D2_company_top_investment <- D2_company_wise_investment_arranged[1,"name"]

#For point 4 (second best sector count-wise), which company received the highest investment?

D2_company_second_investment <- D2_company_wise_investment_arranged[2,"name"]


###-----------------------------EOF D2 analysis---------------------------------------------------####

###-----------------------Analysis for D3 dataframe-----------------------------------------------####

#Dataframe D3 contains observations of funding type 'Venture' falling within the 5-15 million USD range falling within the 5-15 million USD range for Country IND.


D3_filter <- main_sector_exclude_blank %>% filter(country_code=="IND",funding_round_type=="venture",raised_amount_usd %in% 5000000:15000000)


# D3_inv_count dataframe counts the number of investment for each main sectors
D3_inv_count <- D3_filter %>% group_by(main_sector) %>% count(main_sector)

# D3_inv_usd dataframe sums up the investment amount in usd for each main sectors
D3_inv_usd <- D3_filter %>% group_by(main_sector) %>% summarise(total_amount_invested=sum(raised_amount_usd,na.rm = T))

# D3_investment merges both D3_inv_count and D3_inv_usd dataframes basedon main sector
D3_investment <- merge(D3_inv_usd,D3_inv_count,by="main_sector")
colnames(D3_investment) <- c("main_sector","total_amount_invested","total_number_investment")

#Merge D3 with investement count and amount for each main sector
D3 <- merge(D3_filter,D3_investment,by.x="main_sector",by.y="main_sector")
##
##Table 5.1 - Total number of Investments (count) for Country C3

C3_Total_number_of_Investment <- sum(D3_investment$total_number_investment)

##Table 5.1 - Total amount of investment (USD) for country C3

C3_Total_amount_of_investment <- sum(D3_investment$total_amount_invested)

##Table 5.1 - Top Sector name (no. of investment-wise) for country C3

C3_Top_Sector <- arrange(D3_investment,desc(total_amount_invested,total_number_investment))
C3_top <- C3_Top_Sector[1,"main_sector"]

##Table 5.1- Second Sector name (no. of investment-wise)
C3_top_second <- C3_Top_Sector[2,"main_sector"]

##Table 5.1 - Third Sector name (no. of investment-wise)
C3_top_third <- C3_Top_Sector[3,"main_sector"]

#Table 5.1 - Number of investments in top sector (3)
C3_top_inv_number <- C3_Top_Sector[1,"total_number_investment"]

#Table 5.1 -Number of investments in second sector (4)
C3_top_second_inv_number <- C3_Top_Sector[2,"total_number_investment"]

#Table 5.1- Number of investments in third sector (5)
C3_top_third_inv_number <- C3_Top_Sector[3,"total_number_investment"]

#For point 3 (top sector count-wise), which company received the highest investment?

#Assumption: Funding per round is restricted to 5-15 M USD
D3_company_wise_investment <- D3 %>% filter(main_sector==C3_top) %>% group_by(name) %>% summarise(highest_val_inv=sum(raised_amount_usd,na.rm = T))

D3_company_wise_investment_arranged <- arrange(D3_company_wise_investment,desc(highest_val_inv))
D3_company_top_investment <- D3_company_wise_investment_arranged[1,"name"]

#For point 4 (second best sector count-wise), which company received the highest investment?

D3_company_second_investment <- D3_company_wise_investment_arranged[2,"name"]

###-----------------------------EOF D3 analysis-----------------------------------------------------------####

##------------------------------EOF Checkpoint 5: Sector Analysis 2-----------------------------------------##






















