# load the library
remove (list=ls())

library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(stringr)

#Set Working directory
#setwd("H:/PG Diploma IN Data Science IIITB/Course 2/EDA Assignment")
#getwd()

#Read UBER Request Data csv file
Uber_request <- read.csv('Uber Request Data.csv')

#Check the structure of the input file
str(Uber_request)


#Data Clean up
#Request.timestamp and Drop.timestamp fields are having date and time in multiple formats
#Converting all date timestamp Factor variables into Date timestamp
Uber_request$Request.timestamp <- parse_date_time(Uber_request$Request.timestamp,c('%d/%m/%Y %H:%M','%d-%m-%Y %H:%M:%S'))
Uber_request$Drop.timestamp <- parse_date_time(Uber_request$Drop.timestamp,c('%d/%m/%Y %H:%M','%d-%m-%Y %H:%M:%S'))

#write.csv(Uber_request,"Uber_request_Derived.csv")
#Checking structure and summary of input file after cleaning up the Request and Drop Date format
str(Uber_request)
summary(Uber_request)

#EDA 
#Performed univariate analysis.Created new fields for Request data,Time, Drop Date, Time
Uber_request_Derived <- Uber_request %>% separate(Request.timestamp,into=c("Request.Date","Request.Time"),sep=" ") %>% separate(Drop.timestamp,into=c("Drop.Date","Drop.Time"),sep=" ")
#Converting Request date into Date format
Uber_request_Derived$Request.Date <- as.Date(Uber_request_Derived$Request.Date,format='%Y-%m-%d')

#Created Request.Hour field to bucket up time in Hours from Request timestamp field
Uber_request_Derived$Request.Hour <- hour(Uber_request$Request.timestamp)

#Adding Request number to aid request frequency calculation
Uber_request_Derived$Request.num <- 1

##Assmption: As we are analysing request date timestamp for all type of status, we are not taking account Drop Date timestamp.
##Hence NA values in Drop date timestamp were not handlled here.
#Drop Time analysis is taken out of scope since our analysis is based on Trip status only. 
#Route distance covered is not available, so analysis on trip completion time kept out of scope.


#Plot to visualise Airport to City and City to Airport request count through out the day.
ggplot(Uber_request_Derived,aes(x=Request.Hour,fill=Pickup.point))+geom_bar(position='dodge')

#Segmented univariate analysis.
#Segmenting time slots in to six segments based cab request frequency through out the day
Uber_request_Derived$Time.Slot <- ifelse((Uber_request_Derived$Request.Hour >=0 & Uber_request_Derived$Request.Hour < 5),'Mid_Night[12am_To_4:59am]',
                                         ifelse((Uber_request_Derived$Request.Hour >=5 & Uber_request_Derived$Request.Hour <10),'Early_Morning[5am_To_9:59am]',
                                                ifelse((Uber_request_Derived$Request.Hour >=10 & Uber_request_Derived$Request.Hour <13),'Late_Morning[10am_To_12:59pm]',
                                                       ifelse((Uber_request_Derived$Request.Hour >=13 & Uber_request_Derived$Request.Hour <17),'Afternoon[1pm_To_4:59pm]',
                                                              ifelse((Uber_request_Derived$Request.Hour >=17 & Uber_request_Derived$Request.Hour <22),'Evening[5pm_To_9:59pm]',
                                                                     ifelse((Uber_request_Derived$Request.Hour >=22 & Uber_request_Derived$Request.Hour <24),'Night[10pm_To_11:59pm]',NA))))))



#Plot to visualize cab request for city to airport / airport to city frequency in different time slots throught the day
ggplot(Uber_request_Derived,aes(Status,fill=Pickup.point))+geom_bar(stat='count')+theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Plot to visualize the frequency of requests in different time slots through out the day
ggplot(Uber_request_Derived,aes(x=Time.Slot,fill=Status))+geom_bar(position = 'stack')+theme(axis.text.x = element_text(angle = 90, hjust = 1))


#Boxplot to visualise and compare  Status of request throught the day for both Airport and City pick up 
#ggplot(Uber_request_Derived,aes(x=Status,y=Request.Hour,fill=Pickup.point))+geom_boxplot()+theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Plot to visualize the frequency of requests in different time slots through out the day for both Airport to city and vice versa commute
par(mfrow = c(1, 2))
ggplot(subset(Uber_request_Derived,Pickup.point=='Airport'),aes(x=Time.Slot,fill=Status))+geom_bar(position = 'stack')+theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggplot(subset(Uber_request_Derived,Pickup.point=='City'),aes(x=Time.Slot,fill=Status))+geom_bar(position = 'stack')+theme(axis.text.x = element_text(angle = 90, hjust = 1))

 
###-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------###
#Find out the gap between supply and demand :Total number of Request vs Completed trip through out the day
##
##Assumption : Demand is considered as Total number of requests with all status.
#Supply is considered as number of requests with Trip completed status.
#Supply Demand gap is computed by taking percentage of Trip completion.

#Created data frame to get request count for Pick up point and time slots of the day
Uber_request_Status_Freq <- Uber_request_Derived %>% group_by(Pickup.point,Time.Slot,Status) %>% summarise(Total_Request=sum(Request.num))


#Filtering dataframe for Airport pick up containing total request and status wise request
Uber_request_Status_Freq_Airport <- Uber_request_Status_Freq %>% filter(Pickup.point=='Airport')

#From Airport total number of request throught the different time slots of the day
Total_Request_from_Airport <- Uber_request_Derived %>% filter(Pickup.point=='Airport') %>% group_by(Time.Slot) %>% summarise(Total_Req_Airport=sum(Request.num))


#Combining both the data from for Airport pick up analyis
Combined_Uber_request_Airport <- merge(Uber_request_Status_Freq_Airport,Total_Request_from_Airport,x.by=Uber_request_Status_Freq_Airport$Time.Slot,y.by=Total_Request_from_Airport$Time.Slot)

#Derived metrics %of total request
Combined_Uber_request_Airport$Percentage_of_Request <- round(((Combined_Uber_request_Airport$Total_Request/Combined_Uber_request_Airport$Total_Req_Airport)*100),2)

#From City total number of request throught the different time slots of the day
Total_Request_from_City <- Uber_request_Derived %>% filter(Pickup.point=='City') %>% group_by(Time.Slot) %>% summarise(Total_Req_City=sum(Request.num))
#colnames(Total_Request_from_City) <- c("Time.Slot","Total_Req_City")

#Created dataframe for Airport pick up containing total request and status wise request
Uber_request_Status_Freq_City <- Uber_request_Status_Freq %>% filter(Pickup.point=='City')

#Combining both the data from for City pick up analyis
Combined_Uber_request_City <- merge(Uber_request_Status_Freq_City,Total_Request_from_City,x.by=Uber_request_Status_Freq_City$Time.Slot,y.by=Total_Request_from_City$Time.Slot)
#Derived metrics %of total request
Combined_Uber_request_City$Percentage_of_Request <- round(((Combined_Uber_request_City$Total_Request/Combined_Uber_request_City$Total_Req_City)*100),2)


#Plot to visualize Airport to City demand supply gap-
ggplot(Combined_Uber_request_Airport,aes(x=Time.Slot,y=Percentage_of_Request,fill=Status))+geom_col(position = 'fill')+theme(axis.text.x = element_text(angle = 90, hjust = 1))+ggtitle(label='Airport to City Demand Supply Gap')
ggplot(subset(Combined_Uber_request_Airport,Status=='Trip Completed'),aes(x=Time.Slot,y=Percentage_of_Request))+geom_col(fill='green')+theme(axis.text.x = element_text(angle = 90, hjust = 1))+ggtitle(label='Airport to City Demand Supply Gap')

#Plot to visualize City to Aiport demand supply gap-
ggplot(Combined_Uber_request_City,aes(x=Time.Slot,y=Percentage_of_Request,fill=Status))+geom_col(position = 'fill')+theme(axis.text.x = element_text(angle = 90, hjust = 1))+ggtitle(label='City to Airport Demand Supply Gap')
ggplot(subset(Combined_Uber_request_City,Status=='Trip Completed'),aes(x=Time.Slot,y=Percentage_of_Request))+geom_col(fill='green')+theme(axis.text.x = element_text(angle = 90, hjust = 1))+ggtitle(label='City to Airport Demand Supply Gap')

#Weekly Trend Analysis
#Request Date wise Hourly Request frequency with status plot.This is additional check to analyse the Request and status  per hour trend through out the week
#For Airport to City commute
Uber_request_derived_Airport_time <- Uber_request_Derived %>% filter(Pickup.point=='Airport') %>% group_by(Status,Date=day(Request.Date),Request.Hour)%>% summarise(Airport_Req_Freq=sum(Request.num))
par(mfrow=c(5,1))
ggplot(subset(Uber_request_derived_Airport_time,Date==11),aes(x=Request.Hour,y=Airport_Req_Freq))+geom_line(aes(color = Status), size = 1)
ggplot(subset(Uber_request_derived_Airport_time,Date==12),aes(x=Request.Hour,y=Airport_Req_Freq))+geom_line(aes(color = Status), size = 1)
ggplot(subset(Uber_request_derived_Airport_time,Date==13),aes(x=Request.Hour,y=Airport_Req_Freq))+geom_line(aes(color = Status), size = 1)
ggplot(subset(Uber_request_derived_Airport_time,Date==14),aes(x=Request.Hour,y=Airport_Req_Freq))+geom_line(aes(color = Status), size = 1)
ggplot(subset(Uber_request_derived_Airport_time,Date==15),aes(x=Request.Hour,y=Airport_Req_Freq))+geom_line(aes(color = Status), size = 1)
#

#For City to Airport commute
Uber_request_derived_City_time <- Uber_request_Derived %>% filter(Pickup.point=='City') %>% group_by(Status,Date=day(Request.Date),Request.Hour)%>% summarise(City_Req_Freq=sum(Request.num))
par(mfrow=c(5,1))
ggplot(subset(Uber_request_derived_City_time,Date==11),aes(x=Request.Hour,y=City_Req_Freq))+geom_line(aes(color = Status), size = 1)
ggplot(subset(Uber_request_derived_City_time,Date==12),aes(x=Request.Hour,y=City_Req_Freq))+geom_line(aes(color = Status), size = 1)
ggplot(subset(Uber_request_derived_City_time,Date==13),aes(x=Request.Hour,y=City_Req_Freq))+geom_line(aes(color = Status), size = 1)
ggplot(subset(Uber_request_derived_City_time,Date==14),aes(x=Request.Hour,y=City_Req_Freq))+geom_line(aes(color = Status), size = 1)
ggplot(subset(Uber_request_derived_City_time,Date==15),aes(x=Request.Hour,y=City_Req_Freq))+geom_line(aes(color = Status), size = 1)
#

