library(car)
library(MASS)
library(dplyr)
library(tidyr)

carPrice <- read.csv("CarPrice_Assignment.csv")
#####################################################################################################
#Basic Data cleaning
##Checking for NA values 
##Checking for duplicate data
#####################################################################################################
#Checking for NA values : count NA value for all the variables

carPrice%>%summarise_all(funs(sum(is.na(.))))
sum(is.na(carPrice))


#Checking duplicate car id in the dataset
sum(duplicated(carPrice$car_ID))

#####################################################################################################
#Data understanding
#
#Checking structure of the data
str(carPrice)
#checking summary of the data
summary(carPrice)
#
#####################################################################################################
#1.In the data set there are four types of data present
#2.car_ID is a  sequence number provided for each observation.
#3.symboling is insurance rating categorical variable and not a feature of a car.
#4.Variables for Car feature are as follows:
#carCompany		
#fueltype		
#aspiration		
#doornumber		
#carbody		
#drivewheel		
#enginelocation		
#wheelbase		
#carlength		
#carwidth		
#carheight		
#curbweight		
#enginetype		
#cylindernumber		
#enginesize		
#fuelsystem		
#stroke		
#compressionratio		
#horsepower		
#peakrpm		
#citympg		
#highwaympg
#5.price is a dependent variable. It is dependent on car feature.
#
#####################################################################################################
# Business Model
# How does car Pricing work in the American marketing?
#
# Business Objective 
# Chinese automobile company wants to understand the factors affecting the pricing of cars in the 
# American marketing, since those may be very different from the Chinese market. 
# As a part of this case study our aim is to find out the independent factors significant in predicting 
# car price.
# 
#####################################################################################################
# Data Preparation
#
# carName variable consists of two part in it. First part denotes the car Company and second part 
# denotes the car model name. We need toconsider only carCompany as independent variable for the 
# model building purpose.
# 
# carCompany variable is derived from carName by separating the strings. 
# Spelling is corrected for all the carCompany for the ease of the modeling.
# 
#Deriving car maker from the car name variable:
carPrice <- separate(carPrice,"CarName",into=c("carCompany","carModel"),sep="[\\-|[:space:]]")

#case correction and spelling correction in carCompany variable
carPrice <- carPrice%>%mutate(carCompany=tolower(carCompany))
#maxda to mazda
#porcshce to porsche
#vw to volkswagen
#vokswagen to volkswagen
#toyouta to toyota

carPrice$carCompany <- gsub("maxda","mazda",carPrice[,"carCompany"])
carPrice$carCompany <- gsub("porcshce","porsche",carPrice[,"carCompany"])
carPrice$carCompany <- gsub("vw","volkswagen",carPrice[,"carCompany"])
carPrice$carCompany <- gsub("vokswagen","volkswagen",carPrice[,"carCompany"])
carPrice$carCompany <- gsub("toyouta","toyota",carPrice[,"carCompany"])

str(carPrice$carCompany)
# carCompany variable is in character format. Hence we convert it to factor variable.
carPrice$carCompany <- as.factor(carPrice$carCompany)


# EDA has been performed to analyse the data set
# 
#Plot to check relation between carCompany and Price
ggplot(carPrice,aes(carCompany,price))+geom_col()
# It is observed that car models by Toyota are having highest prices followed by buick and bmw.

#Plot to check the Relation with carbody 
ggplot(carPrice,aes(carbody,price))+geom_col()
ggplot(carPrice,aes(carCompany,price,fill=carbody))+geom_col(position='dodge')+coord_flip()
# This plot is  showing price of cars with different body type from different car company.
# Car company buick has highest carprices for four type of carbodies, whereas bmw only has sedan type of cars
# with highest price range followed by jaguar.


#Summary of carPrice dataset
summary(carPrice)

#Relation of car price with carbody
ggplot(carPrice,aes(carbody,price))+geom_boxplot()
# Boxplot shows that hardtop car price median are highest in all  type of cars, whereas hatchback has 
# lowest median.

#Relationship of carprice and symboling
ggplot(carPrice,aes(as.factor(symboling),price))+geom_boxplot()

##Removing outliers################################################################################
#Boxplot is showing there are outliers for symboling 0,1,3
#For symboling 0
priceat_0 <- subset(carPrice,symboling == 0, select=c(price))
summary(priceat_0)
#Removing the outlier prices which are greater than 95% of the range
price_at0_wo <- subset(priceat_0,price < (max(price)-min(price))*.95,select=c(price))
summary(price_at0_wo)

#For symboling 1
priceat_1 <- subset(carPrice,symboling == 1, select=c(price))
summary(priceat_1)
#Removing the outlier prices which are greater than 95% of the range
price_at1_wo <- subset(priceat_1,price < (max(price)-min(price))*.95,select=c(price))
summary(price_at1_wo)

#For symboling 3
priceat_3 <- subset(carPrice,symboling == 3, select=c(price))
summary(priceat_3)
#Removing the outlier prices which are greater than 95% of the range
price_at3_wo <- subset(priceat_3,price < (max(price)-min(price))*.95,select=c(price))
summary(price_at3_wo)

price_wo_outier <- rbind(price_at0_wo,price_at1_wo,price_at3_wo)

#Combine price list for other symboling e.g -2,-1,2
price_at_minus2 <- subset(carPrice,symboling==-2,select=c(price))
price_at_minus1 <- subset(carPrice,symboling==-1,select=c(price))
price_at_2 <- subset(carPrice,symboling==2,select=c(price))

#combine with price_wo_outier

price_wo_outier_final <- rbind(price_wo_outier,price_at_minus2,price_at_minus1,price_at_2)

carPrice <- carPrice %>% filter(price %in% price_wo_outier_final$price)

#Relationship of carprice and symboling after removing outliers
ggplot(carPrice,aes(as.factor(symboling),price))+geom_boxplot()

# Symboling assigned insurance risk rating. Plot is  showing that for cars with symboling -1 and -2
# have high prices since -2 and -1 cars are maked safer than the +3 cars

####################################################################################################
## As a pat of data preparation , dummy variables were created for all the factor type variables.
#
# convert factors with 2 levels to numerical variables

#diesel:1,gas:0
levels(carPrice$fueltype)<-c(1,0)
carPrice$fueltype <- as.numeric(levels(carPrice$fueltype))[carPrice$fueltype]

#std:1, turbo:0
levels(carPrice$aspiration)<-c(1,0)
carPrice$aspiration <- as.numeric(levels(carPrice$aspiration))[carPrice$aspiration]

#four:1, two:0
levels(carPrice$doornumber)<-c(1,0)
carPrice$doornumber <- as.numeric(levels(carPrice$doornumber))[carPrice$doornumber]

#front:1,rear:0
levels(carPrice$enginelocation)<-c(1,0)
carPrice$enginelocation <- as.numeric(levels(carPrice$enginelocation))[carPrice$enginelocation]

str(carPrice)
#carbody 
# Create the dummy variable for carbody variable
dummy_1 <- data.frame(model.matrix( ~carbody, data = carPrice))
View(dummy_1)
dummy_1 <- dummy_1[,-1]

# Combine the dummy variables and the numeric columns of carPrice dataset, in a new dataset called carPrice_1
carPrice_1 <- cbind(carPrice[,-8], dummy_1)

#str(carPrice_1)
#enginetype
# Create the dummy variable for enginetype variable
dummy_2 <- data.frame(model.matrix( ~enginetype, data = carPrice_1))
View(dummy_2)
dummy_2 <- dummy_2[,-1]
str(carPrice_1)
# Combine the dummy variables and the numeric columns of carPrice_1 dataset, in a new dataset called carPrice_2
carPrice_2 <- cbind(carPrice_1[,-15], dummy_2)

str(carPrice_2)


#drivewheel
# Create the dummy variable for drivewheel variable
dummy_3 <- data.frame(model.matrix( ~drivewheel, data = carPrice_2))
View(dummy_3)
dummy_3 <- dummy_3[,-1]

# Combine the dummy variables and the numeric columns of carPrice_2 dataset, in a new dataset called carPrice_3
carPrice_3 <- cbind(carPrice_2[,-8], dummy_3)
str(carPrice_3)

#cylindernumber
# Create the dummy variable for cylindernumber variable
dummy_4 <- data.frame(model.matrix( ~cylindernumber, data = carPrice_3))
View(dummy_4)
dummy_4 <- dummy_4[,-1]

# Combine the dummy variables and the numeric columns of carPrice_3 dataset, in a new dataset called carPrice_4
carPrice_4 <- cbind(carPrice_3[,-14], dummy_4)
str(carPrice_4)


#fuelsystem
# Create the dummy variable for fuelsystem variable
dummy_5 <- data.frame(model.matrix( ~fuelsystem, data = carPrice_4))
View(dummy_5)
dummy_5 <- dummy_5[,-1]

# Combine the dummy variables and the numeric columns of carPrice_4 dataset, in a new dataset called carPrice_5
carPrice_5 <- cbind(carPrice_4[,-15], dummy_5)
str(carPrice_5)

#Removing car Model variable from carPrice_5 data set:
carPrice_5 <- carPrice_5[,!(colnames(carPrice_5) %in% c("carModel"))]


#carCompany
# Create the dummy variable for carCompany variable
dummy_6 <- data.frame(model.matrix( ~carCompany, data = carPrice_5))
View(dummy_6)
dummy_6 <- dummy_6[,-1]

# Combine the dummy variables and the numeric columns of carPrice_5 dataset, in a new dataset called carPrice_4
carPrice_6 <- cbind(carPrice_5[,-3], dummy_6)
str(carPrice_6)

#Symboling though integer variable it is in actual categorical
carPrice_6$symboling <- as.factor(carPrice_6$symboling)
dummy_7 <- data.frame(model.matrix(~symboling,data=carPrice_6))
View(dummy_7)
dummy_7 <- dummy_7[,-1]
colnames(dummy_7) <- c("symboling-1","symboling0","symboling1","symboling2","symboling3")

# Combine the dummy variables and the numeric columns of carPrice_5 dataset, in a new dataset called carPrice_4
carPrice_7 <- cbind(carPrice_6[,-2], dummy_7)
str(carPrice_7)


#Creating derived metrics
#Car volume = carlength*carwidth*carheight
carPrice_7$carVolume = carPrice_7$carlength * carPrice_7$carwidth * carPrice_7$carheight

#bore/stroke ratio (= boreratio/stroke)
carPrice_7$boreperstroke = carPrice_7$boreratio/carPrice_7$stroke

write.csv(carPrice_7,"carPrice_7.csv")

#Omit car_ID parameter as itis unique identiifier only
carPrice_7 <- carPrice_7[,!(colnames(carPrice_7) %in% c("car_ID"))]

#Price - it has two and one decimal places in the data set.So omitting decimal places from price.
carPrice_7$price <- round(carPrice_7$price,0)

####################################################################################################
# Model building :
# carPrice_7 is the prepared data set with which modeling will be performed.
#
# separate training and testing data in 7:3 ratio.

set.seed(100)
trainindices= sample(1:nrow(carPrice_7), 0.7*nrow(carPrice_7))
train = carPrice_7[trainindices,]
test = carPrice_7[-trainindices,]

# Build model 1 containing all variables
model_car_1 <-lm(price~.,data=train)
summary(model_car_1)
#Multiple R-squared:  0.9767,	Adjusted R-squared:  0.9575

# stepAIC is performed on the first built model to identify significant variables.
stepCar_1 <- stepAIC(model_car_1, direction="both")

#build model 2 with significant variables identified in previous step

model_car_2 <-lm(price ~ doornumber + wheelbase + carlength + carheight + curbweight + 
                   enginesize + compressionratio + horsepower + highwaympg + 
                   carbodyhatchback + carbodysedan + carbodywagon + enginetypedohcv + 
                   enginetypel + enginetypeohc + enginetypeohcv + enginetyperotor + 
                   drivewheelrwd + cylindernumberfive + cylindernumberfour + 
                   cylindernumbersix + fuelsystem2bbl + fuelsystemmpfi + fuelsystemspfi + 
                   carCompanyaudi + carCompanybmw + carCompanybuick + carCompanychevrolet + 
                   carCompanydodge + carCompanyjaguar + carCompanymercury + carCompanymitsubishi + 
                   carCompanyplymouth + carCompanyporsche + carCompanysaab + carCompanytoyota + 
                   carCompanyvolvo + symboling1 + symboling2,data=train)

summary(model_car_2)
#For Model_car_2: R-squared:  0.9747,	Adjusted R-squared:  0.9644
#We check VIF of the model

vif(model_car_2)

#carheight is having less significance and high vif - 6.021243. Hence we will remove it.

model_car_3 <-lm(price ~ doornumber + wheelbase + carlength + curbweight + 
                   enginesize + compressionratio + horsepower + highwaympg + 
                   carbodyhatchback + carbodysedan + carbodywagon + enginetypedohcv + 
                   enginetypel + enginetypeohc + enginetypeohcv + enginetyperotor + 
                   drivewheelrwd + cylindernumberfive + cylindernumberfour + 
                   cylindernumbersix + fuelsystem2bbl + fuelsystemmpfi + fuelsystemspfi + 
                   carCompanyaudi + carCompanybmw + carCompanybuick + carCompanychevrolet + 
                   carCompanydodge + carCompanyjaguar + carCompanymercury + carCompanymitsubishi + 
                   carCompanyplymouth + carCompanyporsche + carCompanysaab + carCompanytoyota + 
                   carCompanyvolvo + symboling1 + symboling2,data=train)

summary(model_car_3)
#Multiple R-squared:  0.9737,	Adjusted R-squared:  0.9634

vif(model_car_3)
#doornumber is having low significance and high vif= 3.732317. Hence removing it.

model_car_4 <-lm(price ~ wheelbase + carlength + curbweight + 
                   enginesize + compressionratio + horsepower + highwaympg + 
                   carbodyhatchback + carbodysedan + carbodywagon + enginetypedohcv + 
                   enginetypel + enginetypeohc + enginetypeohcv + enginetyperotor + 
                   drivewheelrwd + cylindernumberfive + cylindernumberfour + 
                   cylindernumbersix + fuelsystem2bbl + fuelsystemmpfi + fuelsystemspfi + 
                   carCompanyaudi + carCompanybmw + carCompanybuick + carCompanychevrolet + 
                   carCompanydodge + carCompanyjaguar + carCompanymercury + carCompanymitsubishi + 
                   carCompanyplymouth + carCompanyporsche + carCompanysaab + carCompanytoyota + 
                   carCompanyvolvo + symboling1 + symboling2,data=train)

summary(model_car_4)
#Multiple R-squared:  0.973,	Adjusted R-squared:  0.9628 
vif(model_car_4)


#compressionratio is less significant and has high vif =4.929809. Hence removing it.
model_car_5 <-lm(price ~ wheelbase + carlength + curbweight + 
                   enginesize + horsepower + highwaympg + 
                   carbodyhatchback + carbodysedan + carbodywagon + enginetypedohcv + 
                   enginetypel + enginetypeohc + enginetypeohcv + enginetyperotor + 
                   drivewheelrwd + cylindernumberfive + cylindernumberfour + 
                   cylindernumbersix + fuelsystem2bbl + fuelsystemmpfi + fuelsystemspfi + 
                   carCompanyaudi + carCompanybmw + carCompanybuick + carCompanychevrolet + 
                   carCompanydodge + carCompanyjaguar + carCompanymercury + carCompanymitsubishi + 
                   carCompanyplymouth + carCompanyporsche + carCompanysaab + carCompanytoyota + 
                   carCompanyvolvo + symboling1 + symboling2,data=train)

summary(model_car_5)
#Multiple R-squared:  0.972,	Adjusted R-squared:  0.9618

vif(model_car_5)
#carbodysedan is less significant and has high vif-10.064502. Hence removing it.

model_car_6 <-lm(price ~ wheelbase + carlength + curbweight + 
                   enginesize + horsepower + highwaympg + 
                   carbodyhatchback + carbodywagon + enginetypedohcv + 
                   enginetypel + enginetypeohc + enginetypeohcv + enginetyperotor + 
                   drivewheelrwd + cylindernumberfive + cylindernumberfour + 
                   cylindernumbersix + fuelsystem2bbl + fuelsystemmpfi + fuelsystemspfi + 
                   carCompanyaudi + carCompanybmw + carCompanybuick + carCompanychevrolet + 
                   carCompanydodge + carCompanyjaguar + carCompanymercury + carCompanymitsubishi + 
                   carCompanyplymouth + carCompanyporsche + carCompanysaab + carCompanytoyota + 
                   carCompanyvolvo + symboling1 + symboling2,data=train)

summary(model_car_6)
#Multiple R-squared:  0.9708,	Adjusted R-squared:  0.9605
vif(model_car_6)

#wheelbase is less significant and has high vif = 8.879381. Hence removing it. 

model_car_7 <-lm(price ~ carlength + curbweight + 
                   enginesize + horsepower + highwaympg + 
                   carbodyhatchback + carbodywagon + enginetypedohcv + 
                   enginetypel + enginetypeohc + enginetypeohcv + enginetyperotor + 
                   drivewheelrwd + cylindernumberfive + cylindernumberfour + 
                   cylindernumbersix + fuelsystem2bbl + fuelsystemmpfi + fuelsystemspfi + 
                   carCompanyaudi + carCompanybmw + carCompanybuick + carCompanychevrolet + 
                   carCompanydodge + carCompanyjaguar + carCompanymercury + carCompanymitsubishi + 
                   carCompanyplymouth + carCompanyporsche + carCompanysaab + carCompanytoyota + 
                   carCompanyvolvo + symboling1 + symboling2,data=train)

summary(model_car_7)
#Multiple R-squared:  0.9698,	Adjusted R-squared:  0.9597
vif(model_car_7)

#carbodyhatchback is insignificant now. Hence removing it.

model_car_8 <-lm(price ~ carlength + curbweight + 
                   enginesize + horsepower + highwaympg + 
                   carbodywagon + enginetypedohcv + 
                   enginetypel + enginetypeohc + enginetypeohcv + enginetyperotor + 
                   drivewheelrwd + cylindernumberfive + cylindernumberfour + 
                   cylindernumbersix + fuelsystem2bbl + fuelsystemmpfi + fuelsystemspfi + 
                   carCompanyaudi + carCompanybmw + carCompanybuick + carCompanychevrolet + 
                   carCompanydodge + carCompanyjaguar + carCompanymercury + carCompanymitsubishi + 
                   carCompanyplymouth + carCompanyporsche + carCompanysaab + carCompanytoyota + 
                   carCompanyvolvo + symboling1 + symboling2,data=train)

summary(model_car_8)
#Multiple R-squared:  0.9693,	Adjusted R-squared:  0.9593
vif(model_car_8)

#carCompanyporsche is  insignificant and has high vif=2.580242. Hence removing it.

model_car_9 <-lm(price ~ carlength + curbweight + 
                   enginesize + horsepower + highwaympg + 
                   carbodywagon + enginetypedohcv + 
                   enginetypel + enginetypeohc + enginetypeohcv + enginetyperotor + 
                   drivewheelrwd + cylindernumberfive + cylindernumberfour + 
                   cylindernumbersix + fuelsystem2bbl + fuelsystemmpfi + fuelsystemspfi + 
                   carCompanyaudi + carCompanybmw + carCompanybuick + carCompanychevrolet + 
                   carCompanydodge + carCompanyjaguar + carCompanymercury + carCompanymitsubishi + 
                   carCompanyplymouth + carCompanysaab + carCompanytoyota + 
                   carCompanyvolvo + symboling1 + symboling2,data=train)

summary(model_car_9)
#Multiple R-squared:  0.9682,	Adjusted R-squared:  0.9583
vif(model_car_9)


#symboling1 is  less significant and has high vif. Hence removing it.
model_car_10 <-lm(price ~ carlength + curbweight + 
                   enginesize + horsepower + highwaympg + 
                   carbodywagon + enginetypedohcv + 
                   enginetypel + enginetypeohc + enginetypeohcv + enginetyperotor + 
                   drivewheelrwd + cylindernumberfive + cylindernumberfour + 
                   cylindernumbersix + fuelsystem2bbl + fuelsystemmpfi + fuelsystemspfi + 
                   carCompanyaudi + carCompanybmw + carCompanybuick + carCompanychevrolet + 
                   carCompanydodge + carCompanyjaguar + carCompanymercury + carCompanymitsubishi + 
                   carCompanyplymouth + carCompanysaab + carCompanytoyota + 
                   carCompanyvolvo + symboling2,data=train)

summary(model_car_10)
#Multiple R-squared:  0.9666,	Adjusted R-squared:  0.9567 
vif(model_car_10)


#cylindernumberfour is  having very high vif=59.489503.Removing it
model_car_11 <-lm(price ~ carlength + curbweight + 
                    enginesize + horsepower + highwaympg + 
                    carbodywagon + enginetypedohcv + 
                    enginetypel + enginetypeohc + enginetypeohcv + enginetyperotor + 
                    drivewheelrwd + cylindernumberfive +  
                    cylindernumbersix + fuelsystem2bbl + fuelsystemmpfi + fuelsystemspfi + 
                    carCompanyaudi + carCompanybmw + carCompanybuick + carCompanychevrolet + 
                    carCompanydodge + carCompanyjaguar + carCompanymercury + carCompanymitsubishi + 
                    carCompanyplymouth + carCompanysaab + carCompanytoyota + 
                    carCompanyvolvo + symboling2,data=train)

summary(model_car_11)
#Multiple R-squared:  0.9587,	Adjusted R-squared:  0.9469 
vif(model_car_11)

#enginetypedohcv is less significant and has high vif.
model_car_12 <-lm(price ~ carlength + curbweight + 
                    enginesize + horsepower + highwaympg + 
                    carbodywagon +  
                    enginetypel + enginetypeohc + enginetypeohcv + enginetyperotor + 
                    drivewheelrwd + cylindernumberfive +  
                    cylindernumbersix + fuelsystem2bbl + fuelsystemmpfi + fuelsystemspfi + 
                    carCompanyaudi + carCompanybmw + carCompanybuick + carCompanychevrolet + 
                    carCompanydodge + carCompanyjaguar + carCompanymercury + carCompanymitsubishi + 
                    carCompanyplymouth + carCompanysaab + carCompanytoyota + 
                    carCompanyvolvo + symboling2,data=train)

summary(model_car_12)
#Multiple R-squared:  0.9586,	Adjusted R-squared:  0.9473 
vif(model_car_12)

#enginetypeohcv is less  significant.
model_car_13 <-lm(price ~ carlength + curbweight + 
                    enginesize + horsepower + highwaympg + 
                    carbodywagon +  
                    enginetypel + enginetypeohc + enginetyperotor + 
                    drivewheelrwd + cylindernumberfive +  
                    cylindernumbersix + fuelsystem2bbl + fuelsystemmpfi + fuelsystemspfi + 
                    carCompanyaudi + carCompanybmw + carCompanybuick + carCompanychevrolet + 
                    carCompanydodge + carCompanyjaguar + carCompanymercury + carCompanymitsubishi + 
                    carCompanyplymouth + carCompanysaab + carCompanytoyota + 
                    carCompanyvolvo + symboling2,data=train)

summary(model_car_13)
#Multiple R-squared:  0.9577,	Adjusted R-squared:  0.9467
vif(model_car_13)

#enginetyperotor is less significant.
model_car_14 <-lm(price ~ carlength + curbweight + 
                    enginesize + horsepower + highwaympg + 
                    carbodywagon +  
                    enginetypel + enginetypeohc +  
                    drivewheelrwd + cylindernumberfive +  
                    cylindernumbersix + fuelsystem2bbl + fuelsystemmpfi + fuelsystemspfi + 
                    carCompanyaudi + carCompanybmw + carCompanybuick + carCompanychevrolet + 
                    carCompanydodge + carCompanyjaguar + carCompanymercury + carCompanymitsubishi + 
                    carCompanyplymouth + carCompanysaab + carCompanytoyota + 
                    carCompanyvolvo + symboling2,data=train)

summary(model_car_14)
#Multiple R-squared:  0.9577,	Adjusted R-squared:  0.9471 
vif(model_car_14)

#carCompanysaab is less significant.Hence removing it.
model_car_15 <-lm(price ~ carlength + curbweight + 
                    enginesize + horsepower + highwaympg + 
                    carbodywagon +  
                    enginetypel + enginetypeohc +  
                    drivewheelrwd + cylindernumberfive +  
                    cylindernumbersix + fuelsystem2bbl + fuelsystemmpfi + fuelsystemspfi + 
                    carCompanyaudi + carCompanybmw + carCompanybuick + carCompanychevrolet + 
                    carCompanydodge + carCompanyjaguar + carCompanymercury + carCompanymitsubishi + 
                    carCompanyplymouth + carCompanytoyota + 
                    carCompanyvolvo + symboling2,data=train)

summary(model_car_15)
#Multiple R-squared:  0.9568,	Adjusted R-squared:  0.9465
vif(model_car_15)

#carCompanychevrolet is  less significant.Hence removing it.
model_car_16 <-lm(price ~ carlength + curbweight + 
                    enginesize + horsepower + highwaympg + 
                    carbodywagon +  
                    enginetypel + enginetypeohc +  
                    drivewheelrwd + cylindernumberfive +  
                    cylindernumbersix + fuelsystem2bbl + fuelsystemmpfi + fuelsystemspfi + 
                    carCompanyaudi + carCompanybmw + carCompanybuick +  
                    carCompanydodge + carCompanyjaguar + carCompanymercury + carCompanymitsubishi + 
                    carCompanyplymouth + carCompanytoyota + 
                    carCompanyvolvo + symboling2,data=train)

summary(model_car_16)
#Multiple R-squared:  0.9561,	Adjusted R-squared:  0.9461
vif(model_car_16)

#drivewheelrwd is having  high vif=3.568592 and less significant
model_car_17 <-lm(price ~ carlength + curbweight+
                    enginesize + horsepower + highwaympg + 
                    carbodywagon +  
                    enginetypel + enginetypeohc +  
                    cylindernumberfive +  
                    cylindernumbersix + fuelsystem2bbl + fuelsystemmpfi + fuelsystemspfi + 
                    carCompanyaudi + carCompanybmw + carCompanybuick +  
                    carCompanydodge + carCompanyjaguar + carCompanymercury + carCompanymitsubishi + 
                    carCompanyplymouth + carCompanytoyota + 
                    carCompanyvolvo + symboling2,data=train)

summary(model_car_17)
#Multiple R-squared:  0.9538,	Adjusted R-squared:  0.9439 
vif(model_car_17)


#fuelsystemspfi is  insignificant.
model_car_18 <-lm(price ~ carlength + curbweight+
                    enginesize + horsepower + highwaympg + 
                    carbodywagon +  
                    enginetypel + enginetypeohc +  
                    cylindernumberfive +  
                    cylindernumbersix + fuelsystem2bbl + fuelsystemmpfi +  
                    carCompanyaudi + carCompanybmw + carCompanybuick +  
                    carCompanydodge + carCompanyjaguar + carCompanymercury + carCompanymitsubishi + 
                    carCompanyplymouth + carCompanytoyota + 
                    carCompanyvolvo + symboling2,data=train)

summary(model_car_18)
#Multiple R-squared:  0.9529,	Adjusted R-squared:  0.9432 
vif(model_car_18)

#enginetypeohc is insignificant.
model_car_19 <-lm(price ~ carlength + curbweight+
                    enginesize + horsepower + highwaympg + 
                    carbodywagon +  
                    enginetypel +   
                    cylindernumberfive +  
                    cylindernumbersix + fuelsystem2bbl + fuelsystemmpfi +  
                    carCompanyaudi + carCompanybmw + carCompanybuick +  
                    carCompanydodge + carCompanyjaguar + carCompanymercury + carCompanymitsubishi + 
                    carCompanyplymouth + carCompanytoyota + 
                    carCompanyvolvo + symboling2,data=train)

summary(model_car_19)
#Multiple R-squared:  0.9518,	Adjusted R-squared:  0.9424 
vif(model_car_19)

#enginesize is less significant and  has  high vif.
model_car_20 <-lm(price ~ carlength + curbweight+
                    horsepower + highwaympg + 
                    carbodywagon +  
                    enginetypel +   
                    cylindernumberfive +  
                    cylindernumbersix + fuelsystem2bbl + fuelsystemmpfi +  
                    carCompanyaudi + carCompanybmw + carCompanybuick +  
                    carCompanydodge + carCompanyjaguar + carCompanymercury + carCompanymitsubishi + 
                    carCompanyplymouth + carCompanytoyota + 
                    carCompanyvolvo + symboling2,data=train)

summary(model_car_20)
#Multiple R-squared:  0.9495,	Adjusted R-squared:  0.9402 
vif(model_car_20)

#fuelsystem2bbl is  less  significant.
model_car_21 <-lm(price ~ carlength + curbweight+
                    horsepower + highwaympg + 
                    carbodywagon +  
                    enginetypel +   
                    cylindernumberfive +  
                    cylindernumbersix + fuelsystemmpfi +  
                    carCompanyaudi + carCompanybmw + carCompanybuick +  
                    carCompanydodge + carCompanyjaguar + carCompanymercury + carCompanymitsubishi + 
                    carCompanyplymouth + carCompanytoyota + 
                    carCompanyvolvo + symboling2,data=train)

summary(model_car_21)
#Multiple R-squared:  0.9482,	Adjusted R-squared:  0.9392
vif(model_car_21)

#carCompanyvolvo is  insignificant.
model_car_22 <-lm(price ~ carlength + curbweight+
                    horsepower + highwaympg + 
                    carbodywagon +  
                    enginetypel +   
                    cylindernumberfive +  
                    cylindernumbersix + fuelsystemmpfi +  
                    carCompanyaudi + carCompanybmw + carCompanybuick +  
                    carCompanydodge + carCompanyjaguar + carCompanymercury + carCompanymitsubishi + 
                    carCompanyplymouth + carCompanytoyota + 
                    symboling2,data=train)

summary(model_car_22)
#Multiple R-squared:  0.9471,	Adjusted R-squared:  0.9385 
vif(model_car_22)

#cylindernumberfive is less significant.
model_car_23 <-lm(price ~ carlength + curbweight+
                    horsepower + highwaympg + 
                    carbodywagon +  
                    enginetypel +   
                    cylindernumbersix + fuelsystemmpfi +  
                    carCompanyaudi + carCompanybmw + carCompanybuick +  
                    carCompanydodge + carCompanyjaguar + carCompanymercury + carCompanymitsubishi + 
                    carCompanyplymouth + carCompanytoyota + 
                    symboling2,data=train)

summary(model_car_23)
#Multiple R-squared:  0.9456,	Adjusted R-squared:  0.9372 
vif(model_car_23)

#fuelsystemmpfi less significant.
model_car_24 <-lm(price ~ carlength + curbweight+
                    horsepower + highwaympg + 
                    carbodywagon +  
                    enginetypel +   
                    cylindernumbersix +   
                    carCompanyaudi + carCompanybmw + carCompanybuick +  
                    carCompanydodge + carCompanyjaguar + carCompanymercury + carCompanymitsubishi + 
                    carCompanyplymouth + carCompanytoyota + 
                    symboling2,data=train)

summary(model_car_24)
#Multiple R-squared:  0.9424,	Adjusted R-squared:  0.9341 
vif(model_car_24)


#carCompanydodge is  insignificant
model_car_25 <-lm(price ~ carlength + curbweight+
                    horsepower + highwaympg + 
                    carbodywagon +  
                    enginetypel +   
                    cylindernumbersix +   
                    carCompanyaudi + carCompanybmw + carCompanybuick +  
                    carCompanyjaguar + carCompanymercury + carCompanymitsubishi + 
                    carCompanyplymouth + carCompanytoyota + 
                    symboling2,data=train)

summary(model_car_25)
#Multiple R-squared:  0.9406,	Adjusted R-squared:  0.9326
vif(model_car_25)


#carlength is less significant and has  high vif.
model_car_26 <-lm(price ~ curbweight+
                    horsepower + highwaympg + 
                    carbodywagon +  
                    enginetypel +   
                    cylindernumbersix +   
                    carCompanyaudi + carCompanybmw + carCompanybuick +  
                    carCompanyjaguar + carCompanymercury + carCompanymitsubishi + 
                    carCompanyplymouth + carCompanytoyota + 
                    symboling2,data=train)

summary(model_car_26)
#Multiple R-squared:  0.9391,	Adjusted R-squared:  0.9315 
vif(model_car_26)

#carCompanymercury has become insignificant.
model_car_27 <-lm(price ~ curbweight+
                    horsepower + highwaympg + 
                    carbodywagon +  
                    enginetypel +   
                    cylindernumbersix +   
                    carCompanyaudi + carCompanybmw + carCompanybuick +  
                    carCompanyjaguar + carCompanymitsubishi + 
                    carCompanyplymouth + carCompanytoyota + 
                    symboling2,data=train)

summary(model_car_27)
#Multiple R-squared:  0.9373,	Adjusted R-squared:   0.93 
vif(model_car_27)


#enginetypel is less significant
model_car_28 <-lm(price ~ curbweight+
                    horsepower + highwaympg + 
                    carbodywagon +  
                    cylindernumbersix +   
                    carCompanyaudi + carCompanybmw + carCompanybuick +  
                    carCompanyjaguar + carCompanymitsubishi + 
                    carCompanyplymouth + carCompanytoyota + 
                    symboling2,data=train)

summary(model_car_28)
#Multiple R-squared:  0.9353,	Adjusted R-squared:  0.9284 
vif(model_car_28)

#carCompanytoyota has less significance
model_car_29 <-lm(price ~ curbweight+
                    horsepower + highwaympg + 
                    carbodywagon +  
                    cylindernumbersix +   
                    carCompanyaudi + carCompanybmw + carCompanybuick +  
                    carCompanyjaguar + carCompanymitsubishi + 
                    carCompanyplymouth +  
                    symboling2,data=train)

summary(model_car_29)
#Multiple R-squared:  0.9331,	Adjusted R-squared:  0.9266 
vif(model_car_29)

#symboling2 is less significant
model_car_30 <-lm(price ~ curbweight+
                    horsepower + highwaympg + 
                    carbodywagon +  
                    cylindernumbersix +   
                    carCompanyaudi + carCompanybmw + carCompanybuick +  
                    carCompanyjaguar + carCompanymitsubishi+carCompanyplymouth,data=train)

summary(model_car_30)
#Multiple R-squared:  0.9307,	Adjusted R-squared:  0.9245 
vif(model_car_30)


# carCompanyplymouth is less significant now.
model_car_31 <-lm(price ~ curbweight+
                    horsepower + highwaympg + 
                    carbodywagon +  
                    cylindernumbersix +   
                    carCompanyaudi + carCompanybmw + carCompanybuick +  
                    carCompanyjaguar + carCompanymitsubishi,data=train)

summary(model_car_31)
#Multiple R-squared:  0.9287,	Adjusted R-squared:  0.923 
vif(model_car_31)


cor(carPrice_7$cylindernumbersix,carPrice_7$horsepower)
#highwaympg : We remove highwaympg as it is less significant now and has high vif.
model_car_32 <-lm(price ~ curbweight+
                    horsepower +  
                    carbodywagon +  
                    cylindernumbersix +   
                    carCompanyaudi + carCompanybmw + carCompanybuick +  
                    carCompanyjaguar + carCompanymitsubishi,data=train)

summary(model_car_32)
#Multiple R-squared:  0.9234,	Adjusted R-squared:  0.9179 
vif(model_car_32)


#cylindernumbersix : We remove it as it has become less significant
model_car_33 <-lm(price ~ curbweight+
                    horsepower +  
                    carbodywagon +  
                    carCompanyaudi + carCompanybmw + carCompanybuick +  
                    carCompanyjaguar + carCompanymitsubishi,data=train)

summary(model_car_33)
#Multiple R-squared:  0.9185,	Adjusted R-squared:  0.9134
vif(model_car_33)

#carCompanyaudi is  less significant.Hence removing it
model_car_34 <-lm(price ~ curbweight+
                    horsepower +  
                    carbodywagon +  
                    carCompanybmw + carCompanybuick +  
                    carCompanyjaguar + carCompanymitsubishi,data=train)

summary(model_car_34)
#Multiple R-squared:  0.9125,	Adjusted R-squared:  0.9077
vif(model_car_34)

#carCompanyjaguar is  now become less significant. Hence removing it.
model_car_35 <-lm(price ~ curbweight+
                    horsepower +  
                    carbodywagon +  
                    carCompanybmw + carCompanybuick +  
                    carCompanymitsubishi,data=train)

summary(model_car_35)
#Multiple R-squared:  0.9049,	Adjusted R-squared:  0.9004
vif(model_car_35)


# Now all the variables of this model are significant i.e having p value less than 0.05
# and vif is under 5 for all. Also, gap between r-squared(90.49%) and adjusted r-squared(90.04%) is 
# significantly low which suggests that variables added in this model are redundant.
# As per model_car_35 we have following variables as most significant variable for predicting car price
# curbweight
# horsepower
# carbodywagon
# carCompanybmw
# carCompanybuick
# carCompanymitsubishi
# 
####################################################################################################
# Model Evaluation
#
# predicting the results in test dataset
Predict_car_1 <- predict(model_car_35,test[,-18])
test$predicted_price <- Predict_car_1

# Now, we need to test the r square between actual and predicted price. 
r <- cor(test$price,test$predicted_price)
# We will find r squared value 
rsquared <- cor(test$price,test$predicted_price)^2
rsquared
# rsquared=0.8312919 for test dataset.
# For train dataset adjusted r-squared value is 0.9004


# To assess our model we further plot the predictor varables against the predicted values.
#Plot for continuous variables:
ggplot(test, aes(curbweight,price))+geom_line()+geom_line(aes(test$curbweight,test$predicted_price,col="red"))
ggplot(test, aes(horsepower,price))+geom_line()+geom_line(aes(test$horsepower,test$predicted_price,col="red"))

# Error plot : We plot the residual errors against the predictor variables.
ggplot(test,aes(curbweight,predicted_price-price))+geom_point()+geom_hline(yintercept=0)
ggplot(test,aes(horsepower,predicted_price-price))+geom_point()+geom_hline(yintercept=0)
#Overall residual plot
plot(model_car_35)
#
# Points in the plots  are random that means there is  no definite pattern in the error.

####################################################################################################
# Model Interpretation
# From the model we found there are six most significant factors affecting the price of the car.

# 1. Curbweight is a car feature which increases the car price.
# 2. Horsepower is another car feature which affects car price.
# 3. CarComany - buick,bmw and mitsubishi. When the car making companies are either of buick, bmw or 
#                mitsubishi, they affect the car price.

####################################################################################################

                                                              