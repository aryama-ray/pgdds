  ###########################################################################################################################
  #-----------------------Retail-Giant Sales Forecasting Group Case Study--------------------------------------------------##         
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
  #
  # "Global Mart" is an online store super giant having worldwide operations. It takes orders and delivers across the globe and 
  #   deals with all the major product categories - consumer, corporate & home office.
  # The store caters to 7 different market segments and in 3 major categories i.e company caters for 7*3=21 combinations of
  #   market+segment combinations.
  # Objective is to find out 2 most profitable(and consistent)combination of market+segment. Then using time series analysis
  #   forecast the sales and the demand for the next 6 months
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
  library(lubridate)
  library(forecast)
  library(tseries)
  require(graphics)
  
  #-------------------------------------------------------------------------------------------------------------------------#
  #**********************DATA SOURCING**************************************************************************************#
  #-------------------------------------------------------------------------------------------------------------------------#
  
  gl_sup_store <- read.csv("Global Superstore.csv",stringsAsFactors = F)
  
  #------------------------------------------------------------------------------------------------------------------------#
  # *********************DATA UNDERSTANDING AND PREPARATION****************************************************************#
  #------------------------------------------------------------------------------------------------------------------------#
  str(gl_sup_store)
  
  #Checking the uniue keys - Row.ID, Order.ID, Customer.ID, Product.ID
  
  length(unique(gl_sup_store$Row.ID)) #unique row_id- 51290 
  length(unique(gl_sup_store$Order.ID)) # Total 25035 unique order id 
  length(unique(gl_sup_store$Customer.ID)) #Total 1590 unique customer id
  length(unique(gl_sup_store$Product.ID)) #Total 10292 unique product id
  
  #-------------------------------------------------------------------------------------------------------------------------#
  #**********************Data Preparation **********************************************************************************#
  #-------------------------------------------------------------------------------------------------------------------------#
  
  # 1. Converting character variables in to factors
  
  fact_var <- c("Ship.Mode","Segment", "City", "State", "Country", "Market", "Region", "Category", "Sub.Category", "Order.Priority")
  
  gl_sup_store[,fact_var] <- as.data.frame(sapply(gl_sup_store[,fact_var],as.factor))
  
  # 2. Converting Dates into Date variables
  
  gl_sup_store$Order.Date <- as.Date(gl_sup_store$Order.Date,"%d-%m-%Y")
  gl_sup_store$Ship.Date <- as.Date(gl_sup_store$Ship.Date,"%d-%m-%Y")
  
  gl_sup_store$Order.Date <- paste(month(gl_sup_store$Order.Date),year(gl_sup_store$Order.Date),sep = "-")
  
  head(gl_sup_store)
  # 3.Market segments data
  nlevels(gl_sup_store$Market) # We have 7 market segments to analyse
  # We will store all 7 market segments in a vector
  market <- c(levels(gl_sup_store$Market))
  
  # 4. Major product segment data
  nlevels(gl_sup_store$Segment)
  
  # We will store all the 3 segments in a vector
  segment <- c(levels(gl_sup_store$Segment))
  
  # 4. Creating 7*3 subset dataframe for each market and segments
  
  create_df_subset <- function(attrib1,attrib2){
   df <- data.frame();
   subsets.market.segment <- list();
   
   for (x in attrib1) {
     for (y in attrib2)
     {
       VM <- gl_sup_store$Market  ==x
       VS <- gl_sup_store$Segment  ==y
       L <- VM&VS
       
       df <- gl_sup_store[L,]
       
       subsets.market.segment[[(length(subsets.market.segment)+1)]]  <- df
     }
   }
   return(subsets.market.segment)
  }
  
  #Creating the list of 21 dataframes
  market_segment_list <- create_df_subset(market,segment)
  # Checking a data frame from the list
  market_segment_list[[2]]
  
  # 5. We will aggregate the 3 attributes  - Sales, Quantity & Profit, 
  # over the Order Date to arrive at monthly values for these attributes
  
  get_aggr_sales_data <- function(in_list){
  aggr_sales_data <- list();
  for (i in 1:length(in_list)){
    aggr_sales_data[[i]] <- in_list[[i]] %>% group_by(Order.Date,Market,Segment) %>% summarise(monthly_sales = sum(Sales),monthly_tot_ordered_qty=sum(Quantity),monthly_profit=sum(Profit))
    }
  return(aggr_sales_data)
  }
  aggr_sales_data_list <- get_aggr_sales_data(market_segment_list)
  
  
  # 6. We will find 2 most profitable and consistently profitable segments by computing coefficient of variation on monthly_profit
  calculate_cv_market_segment <- function(in_list){
  segment_profit_data <- list();
  
  for(i in 1:length(in_list)){
    segment_profit_data[[i]] <- in_list[[i]] %>% group_by(Market,Segment) %>% summarise(coef_var_profit = sd(monthly_profit)/abs(mean(monthly_profit)))
    
    }
  return(segment_profit_data)
  }
  
  profit_segment_list <- calculate_cv_market_segment(aggr_sales_data_list)
  
  #We will store CVs of all the market segment in a vector
  min_coef_var_profit <- c();
  for(i in 1:length(profit_segment_list)){
    min_coef_var_profit[i] <- profit_segment_list[[i]][[3]]
  }
  
  #CV with low values will give us better and conistent series.Hence we will find out 2 minimum CV and their corresponding market and segment.
  #Minimum two cv values are:
  min_2_cv <- head(sort(min_coef_var_profit),2)
  temp <- c();
  #Segments with min cvs : 0.6243052, 0.6321323
  for(x in 1: length(profit_segment_list)){
    temp[x] <- profit_segment_list[[x]][[3]]==min_2_cv[1] 
    if(temp[x])
      profit_segment_with_min_cv1 <- profit_segment_list[[x]] #Market - EU, Segment - Consumer ,CV - 0.624
      temp[x] <- profit_segment_list[[x]][[3]]==min_2_cv[2] 
    if(temp[x])
      profit_segment_with_min_cv2 <- profit_segment_list[[x]] #Market - APAC, Segment - Consumer ,CV - 0.632
  }
  
  ##Satrt: New code
  ##top 4 
  ##0.6243052 0.6321323 0.6614828 0.6980869
  profit_segment_list
  sort(min_coef_var_profit)
  
  gl_sup_plot <- gl_sup_store%>%group_by(Order.Date, Segment,Market)%>%
    summarise( profit = sum(Profit,is.na=F)) %>%
    mutate( area = paste(Market,Segment,sep=" " ) ) %>%
    filter((Market=="LATAM" & Segment=="Consumer") |
             (Market=="EU" & Segment=="Consumer") |
             (Market=="APAC" & Segment=="Consumer") |
             (Market=="APAC" & Segment=="Corporate") ) %>%
    arrange(Order.Date)
  
  gl_sup_plot$Order.Date <- paste("1",gl_sup_plot$Order.Date,sep="-")
  gl_sup_plot$Order.Date <- as.Date(gl_sup_plot$Order.Date,format="%d-%m-%Y")
  
  ggplot(gl_sup_plot,aes(x=Order.Date, y=profit, col=area))+geom_line()
  
  ## This plot show the consistancy of top 4 sigment+Market
  ##END: New code
  
  #We will get the sales and quantity data frames for both the profitable market segements
  
  #Data set for two profitable segments: 
  # i.Market - EU, Segment - Consumer 
  #ii. Market - APAC, Segment - Consumer 
  
  get_consumer_segment <- function(market_name,segment_name){
  temp1 <- c();
  temp2 <- c();
  temp4 <- list();
  for(x in 1:length(aggr_sales_data_list)){
    temp1[x] <- aggr_sales_data_list[[x]][[2]] == market_name 
    temp2[x] <- aggr_sales_data_list[[x]][[3]] == segment_name
    temp3 <- temp1&temp2
    }
  pos <- which(temp3)
  consumer <- as.data.frame(matrix(unlist(aggr_sales_data_list[[pos]]),nrow=nrow(aggr_sales_data_list[[pos]]),byrow=FALSE))
  colnames(consumer) <- c('Order.Date','Market','Segment','monthly_sales','monthly_tot_ordered_qty','monthly_profit')
  consumer$Market <- market_name
  consumer$Segment <- segment_name
  return(consumer)
  
  }
  
  EU_Consumer_df <- get_consumer_segment('EU','Consumer')
  APAC_Consumer_df <- get_consumer_segment('APAC','Consumer')
  
  #-------------------------------------------------------------------------------------------------------------------------#
  #****************************************** Model Building ***************************************************************#
  #-------------------------------------------------------------------------------------------------------------------------#
  
  #----------------------------------EU-Consumer Market Segment Analysis and Forecasting------------------------------------#
  
  #1. We check the structure of EU market segment dataset
  str(EU_Consumer_df)
  
  #2.We convert Order.Date into date format
  EU_Consumer_df$Order.Date <- sapply(EU_Consumer_df$Order.Date, as.character)
  EU_Consumer_df$Order.Date <- paste("1",EU_Consumer_df$Order.Date,sep="-")
  EU_Consumer_df$Order.Date <- as.Date(EU_Consumer_df$Order.Date,format="%d-%m-%Y")
  
  #3. We convert monthly_sales into numeric format
  EU_Consumer_df$monthly_sales <- sapply(sapply(EU_Consumer_df$monthly_sales,as.character),as.numeric)
  
  #4. We convert total ordered Quantity(Demand) into numeric format
  EU_Consumer_df$monthly_tot_ordered_qty <- sapply(sapply(EU_Consumer_df$monthly_tot_ordered_qty,as.character),as.numeric)
  
  #We will perform Sales and Demand forecasting separately using different smoothing techniques and will compare MAPE values.
  #
  #----------------------------------EU-Consumer Sales Analysis-------------------------------------------------------------#
  #
  # 1.Classical Decomposition
  #a.We will take subset of EU_Consumer_df for Sales analysis
  EU_Consumer_df_Sales <- EU_Consumer_df %>% select(Order.Date,monthly_sales) %>% arrange(Order.Date)
  
  #We will split last 6 months data for accuracy test from the base data set
  EU_sales_indata <- EU_Consumer_df_Sales[1:42,]
  
  #We will convert this dataframe to time series
  
  timeseries_EU_Consumer_df_Sales <- ts(EU_sales_indata$monthly_sales)
  #We plot the series
  plot(timeseries_EU_Consumer_df_Sales)
  
  #-------------------We will perform smoothing using Simple Moving Average Smoothing technique----------------------------#
  #Set the window
  EU_sales_width <- 1
  EU_sales_smoothedseries <- stats::filter(timeseries_EU_Consumer_df_Sales,filter=rep(1/(2*EU_sales_width+1),(2*EU_sales_width+1)),method = 'convolution',sides=2)
  
  #Smoothing left end of the time series
  
  width_diff <- EU_sales_smoothedseries[EU_sales_width+2] - EU_sales_smoothedseries[EU_sales_width+1]
  for (i in seq(EU_sales_width,1,-1)) {
    EU_sales_smoothedseries[i] <- EU_sales_smoothedseries[i+1] - width_diff
  }
  
  #Smoothing right end of the time series
  n <- length(timeseries_EU_Consumer_df_Sales)
  width_diff <- EU_sales_smoothedseries[n-EU_sales_width] - EU_sales_smoothedseries[n-EU_sales_width-1]
  for (i in seq(n-EU_sales_width+1, n)) {
    EU_sales_smoothedseries[i] <- EU_sales_smoothedseries[i-1] + width_diff
  }
  #
  
  #Smoothed time series-EU_sales_smoothedseries plot
  # 
  EU_sales_time_val <- EU_sales_indata$Order.Date
  
  lines(EU_sales_smoothedseries, col="blue", lwd=2)
  
 #We will build classical decomposition model on the smoothed time series
 #Converting time series to a dataframe
  
 EU_sales_smoothed_df <- as.data.frame(cbind(EU_sales_time_val,as.vector(EU_sales_smoothedseries)))
 colnames(EU_sales_smoothed_df) <- c('Order_Month','Sales')
 
 #
 #We build the global model over the smoothed series
  
 EU_sales_linearfit <- lm(Sales ~ sin(0.5*Order_Month) * poly(Order_Month,3) + cos(0.5*Order_Month) * poly(Order_Month,3)+Order_Month, data=EU_sales_smoothed_df)
 
 #Summary of EU_sales_linearfit
 summary(EU_sales_linearfit)
 #Multiple R-squared:  0.7748,	Adjusted R-squared:  0.6923 
 #F-statistic: 9.385 on 11 and 30 DF,  p-value: 5.283e-07
 
 EU_sales_global_pred <- predict(EU_sales_linearfit, Month=EU_sales_time_val)
 summary(EU_sales_global_pred)
 lines(EU_sales_time_val, EU_sales_global_pred, col='red', lwd=2)
 #
 #We will subtract global pred to get local prediction
 EU_sales_local_pred <- timeseries_EU_Consumer_df_Sales - EU_sales_global_pred
 plot(EU_sales_local_pred, col='green', type = "l")
 #To check stationaity in local series we will plot ACF and PACF.
 #ACF plot 
 acf(EU_sales_local_pred) #ACF plot shows there is 2 more peaks after the peak at lag 0 , which exceed confidence level.
 #PACF plot
 acf(EU_sales_local_pred, type="partial") #PACF plot has 2 peaks outside confidence level.

 #We will perform auto arima on local series
 EU_sales_armafit <- auto.arima(EU_sales_local_pred)

 tsdiag(EU_sales_armafit)
 EU_sales_armafit
 #ARIMA(0,0,0) with zero mean 
 #sigma^2 estimated as 122153574:  log likelihood=-450.63
 #AIC=903.26   AICc=903.36   BIC=905
 
 #Formal tests to check if the residual series is white noise
 EU_sales_residual <- EU_sales_local_pred - fitted(EU_sales_armafit)

 #Dickey-Fuller test
 adf.test(EU_sales_residual,alternative = "stationary")
 #Result: Dickey-Fuller = -3.8924, Lag order = 3, p-value = 0.02348
 #Since p-value < 0.05 ,null hypothesis is rejected.That implies the residual series is stationary.

 #KPSS test
 kpss.test(EU_sales_residual)
 #Result: KPSS Level = 0.031263, Truncrow.namesation lag parameter = 1, p-value = 0.1
 #Since p-value > 0.05 ,null hypothesis is failed to be rejected.That implies the residual series is stationary.


 
 
 #--------------------------------Model evaluation with classical decomposition with simple moving average----------------#
 #We will perform forecasting on the rest of the six months data
 EU_sales_outdata <- EU_Consumer_df_Sales[43:48,]
 
  EU_sales_outdata$Order.Date #Outdata Order data starts from 2014-07-01 and ends 2014-12-01
  EU_sales_time_val_out <- as.numeric(EU_sales_outdata$Order.Date)
  #Forecasting for unseen 6 months data
  #We are taking forecasting window for 12 months starting from 2014-07-01 among which we have data to test till 2014-12-01
  forecast_window <- as.data.frame(seq(as.Date("2014-07-01"), by = "month", length.out = 12))
  colnames(forecast_window) <- c("Order.Date")
  
  EU_sales_time_val_out <- as.numeric(forecast_window$Order.Date)
  #We will predict 12months sales
  EU_sales_global_pred_out <- predict(EU_sales_linearfit,data.frame(Order_Month =EU_sales_time_val_out))
   
  #MAPE value fr the forecasting
  EU_sales_MAPE <-accuracy(EU_sales_global_pred_out,EU_sales_outdata[,2])[5]
  #MAPE : 25.50923
 
  #Forecast Visualizaation
  ##This will give sales for 12 month.
  EU_sales_global_pred_out_next <- as.data.frame(EU_sales_global_pred_out) 
 
 EU_sales_global_pred_out_forecast <- cbind(forecast_window,EU_sales_global_pred_out_next)
 
 colnames(EU_sales_global_pred_out_forecast) <- c('Order.Date','monthly_sales')
 combine_date <- rbind(EU_sales_indata,EU_sales_global_pred_out_forecast)


 plot(combine_date)
 lines(combine_date,col="darkgreen",lwd=1,lty=1)
 lines(EU_sales_global_pred_out_forecast,col='red', lwd=2,lty=2)

 
 #----------------------------End of Model evaluation for Classical decomposition for EU-Consumer-Sales-------------------#

 #We will perform smoothing using Holt Winters technique with various alpha  value----------------------------------------#
  
 #We plot the series again
 plot(timeseries_EU_Consumer_df_Sales)
  
 colors <- c("yellow", "orange", "green", "pink", "blue")
 alphas <- c(0.02, 0.3, 0.2, 0.1)
 labels <- c(paste("alpha =", alphas), "Original")
 for (i in seq(1,length(alphas))) {
   
    EU_sales_smoothedseries_HW <- HoltWinters(timeseries_EU_Consumer_df_Sales, alpha=alphas[i],beta=FALSE,gamma=FALSE)
    lines(fitted(EU_sales_smoothedseries_HW)[,1], col=colors[i], lwd=2)
  }
 legend('bottomright',labels, col=colors, lwd=2,xjust = 1)  
  
 #since the series has both seasonality and trend
 plot(timeseries_EU_Consumer_df_Sales)
 
 #Smoothing parameters:
 #alpha: 0.1
 
 EU_sales_smoothedseries_HW$fitted
 plot(EU_sales_smoothedseries_HW)
 
 
 
 #--------------Model evaluation with Holt Winter smoothing--------------------------------------------------------------------------#

 #MAPE value for the prediction
 EU_sales_pred_out_HW<-predict(EU_sales_smoothedseries_HW,n.ahead = 6,prediction.interval = TRUE)
 
 EU_sales_MAPE_HW <-accuracy(EU_sales_pred_out_HW, EU_sales_outdata[,2])[5]
 #MAPE : 32.52626
 #Holt-Winter's MAPE value is slightly less good than moving average smoothing in classical decomposition.
 
 #EU_sales_forecast_HW <- forecast:::forecast.HoltWinters(EU_sales_smoothedseries_HW,h=12)
 #forecast:::plot.forecast(EU_sales_forecast_HW)
 #ts.plot(timeseries_EU_Consumer_df_Sales,EU_sales_forecast_HW,EU_sales_outdata,lty=c(1,3,2), col=c('darkgreen','red','darkgreen'), lwd=c(2,2,1))
 
 EU_sales_forecast.HW <- forecast:::forecast.HoltWinters(EU_sales_smoothedseries_HW,h=12)
 forecast:::plot.forecast(EU_sales_forecast.HW,lty=1,col="red")
 
 EU_sales_forecast.HW_level95_upper <- cbind(as.data.frame(c(43:54)),as.data.frame(EU_sales_forecast.HW$upper[,2]))
 EU_sales_forecast.HW_level95_upper <- cbind(forecast_window,EU_sales_forecast.HW_level95_upper[,2])
 colnames(EU_sales_forecast.HW_level95_upper) <- c('Order.Date','monthly_sales')
 EU_sales_combine_HW_level95_upper <- rbind(EU_sales_indata,EU_sales_forecast.HW_level95_upper)
 plot(EU_sales_combine_HW_level95_upper)
 lines(EU_sales_combine_HW_level95_upper,col="darkgreen",lwd=1,lty=1)
 lines(EU_sales_forecast.HW_level95_upper,col='red', lwd=2,lty=2)
 
 EU_sales_forecast.HW_level95_lower <- cbind(as.data.frame(c(43:54)),as.data.frame(EU_sales_forecast.HW$lower[,2]))
 EU_sales_forecast.HW_level95_lower <- cbind(forecast_window,EU_sales_forecast.HW_level95_lower[,2])
 colnames(EU_sales_forecast.HW_level95_lower) <- c('Order.Date','monthly_sales')
 EU_sales_combine_HW_level95_lower <- rbind(EU_sales_indata,EU_sales_forecast.HW_level95_lower)
 lines(EU_sales_combine_HW_level95_lower)
 lines(EU_sales_combine_HW_level95_lower,col="blue",lwd=1,lty=1)
 lines(EU_sales_forecast.HW_level95_lower,col='red', lwd=2,lty=2)
 #------------------------------------------------------------------------------------------------------------------------#
 
 #2.Auto- arima model
 
 EU_sales_Autoarima <- auto.arima(timeseries_EU_Consumer_df_Sales)
 tsdiag(EU_sales_Autoarima)
 plot(EU_sales_Autoarima$x,col="black")
 lines(fitted(EU_sales_Autoarima), col="blue")

 #We will compute residual series to test white noise
 EU_sales_Autoarima_residual <- timeseries_EU_Consumer_df_Sales - fitted(EU_sales_Autoarima)

 #Dickey-Fuller test 
 adf.test(EU_sales_Autoarima_residual,alternative = "stationary")
 #Result: -4.3522, Lag order = 3, p-value = 0.01
 #Since p-value > 0.05 ,null hypothesis is failed to be rejected.That implies the residual series is not stationary.

 #KPSS test  
 kpss.test(EU_sales_Autoarima_residual)
 #Result: KPSS Level =  0.05314, Truncation lag parameter = 1, p-value = 0.1
 #Since p-value > 0.05 ,null hypothesis is failed to be rejected.That implies the residual series is stationary.

 #--------------Model evaluation with AUTO ARIMA --------------------------------------------------------------------------#

 EU_sales_global_pred_out_autoarima <- predict(EU_sales_Autoarima,n.ahead = 12) ## Change 6 to 12
 #MAPE value fr the forecasting
 EU_sales_MAPE_autoarima <-accuracy(EU_sales_global_pred_out_autoarima$pred, EU_sales_outdata[,2])[5]
 #MAPE : 28.9226
 # MAPE value for autoarima model is better than Holt-Winter forecast but not as good as 
 # classical decomposition moving average smoothing.
 
  ##This is will give number
  EU_sales_global_pred_out_autoarima$pred
 
  #Forecast Visulization 
  #We create a autoarima  window since 2011-1-1 for forecast plot.
  forecast_window_autoarima <- as.data.frame(seq(as.Date("2011-01-01"), by = "month", length.out = 54))
  
  #We plot original data set
 plot(EU_Consumer_df_Sales, col = "black",lwd=1,lty=1)
 lines(EU_Consumer_df_Sales, col = "black",lty=1)
 
 #Now we willplot auto arima prediction since 2011-1-1 to 2015-6-1
 auto_arima_pred <- c(fitted(EU_sales_Autoarima),ts(EU_sales_global_pred_out_autoarima$pred))
 EU_sales_combine_date_autoarima_full <- cbind(forecast_window_autoarima,as.data.frame(auto_arima_pred))
 colnames(EU_sales_combine_date_autoarima_full) <- c("Order.Date","Monthly.Sales")
 lines(EU_sales_combine_date_autoarima_full,col = "green",lty=1)
 
 #Following plot shows the forecasted data for last 12 monthe including last 6 month's unseen data
 EU_sales_combine_date_autoarima <- cbind(forecast_window,as.data.frame(ts(EU_sales_global_pred_out_autoarima$pred)))
 colnames(EU_sales_combine_date_autoarima) <- c("Order.Date","Monthly.Sales")
 lines(EU_sales_combine_date_autoarima,col='red', lwd=3,lty=2)
 
 

 #------------------------End of Model evaluation for Auto-arima for EU-Consumer-Sales-------------------------------------#
 
 #-------------------------------------------------------------------------------------------------------------------------#
 #----------------------------------EU-Consumer Quantity [Demand] Analysis-------------------------------------------------#

 # 1.Classical Decomposition
 #a.We will take subset of monthly_tot_ordered_qty for total ordered Quantity(Demand)  analysis
 EU_Consumer_df_Qty <- EU_Consumer_df %>% select(Order.Date,monthly_tot_ordered_qty) %>% arrange(Order.Date)

 #We will split last 6 months data for accuracy test from the base data set
 EU_qty_indata <- EU_Consumer_df_Qty[1:42,]

 #We will convert this dataframe to time series
  timeseries_EU_Consumer_df_Qty <- ts(EU_qty_indata$monthly_tot_ordered_qty)
 #We plot the series
 plot(timeseries_EU_Consumer_df_Qty)

 #We will perform smoothing using Simple Moving Average Smoothing technique------------------------------------------------#
 EU_qty_width <- 1
 EU_qty_smoothedseries <- stats::filter(timeseries_EU_Consumer_df_Qty,filter=rep(1/(2*EU_qty_width+1),(2*EU_qty_width+1)),method = 'convolution',sides=2)

 #Smoothing left end of the time series

 width_diff <- EU_qty_smoothedseries[EU_qty_width+2] - EU_qty_smoothedseries[EU_qty_width+1]
 for (i in seq(EU_qty_width,1,-1)) {
  EU_qty_smoothedseries[i] <- EU_qty_smoothedseries[i+1] - width_diff
 }

 #Smoothing right end of the time series

 n <- length(timeseries_EU_Consumer_df_Qty)
 width_diff <- EU_qty_smoothedseries[n-EU_qty_width] - EU_qty_smoothedseries[n-EU_qty_width-1]
 for (i in seq(n-EU_qty_width+1, n)) {
  EU_qty_smoothedseries[i] <- EU_qty_smoothedseries[i-1] + width_diff
 }

 #Smoothed time series-EU_qty_smoothedseries plot
 EU_qty_time_val <- EU_qty_indata$Order.Date
 lines(EU_qty_smoothedseries, col="blue", lwd=2)

 #We will build classical decomposition model on the smoothed time series
 #Converting time series to a dataframe

 EU_qty_smoothed_df <- as.data.frame(cbind(EU_qty_time_val,as.vector(EU_qty_smoothedseries)))
 colnames(EU_qty_smoothed_df) <- c('Order_Month','monthly_tot_ordered_qty')

 #We build the global model
 EU_qty_linearfit <- lm(monthly_tot_ordered_qty ~ sin(0.6*Order_Month) * poly(Order_Month,4) + cos(0.4*Order_Month) * poly(Order_Month,4) + Order_Month, data=EU_qty_smoothed_df)
 
 #Summary of EU_qty_linearfit
 summary(EU_qty_linearfit)
 
 #Multiple R-squared:  0.8893,	Adjusted R-squared:  0.8319 
 #F-statistic: 15.49 on 14 and 27 DF,  p-value: 2.114e-09
 
 EU_qty_global_pred <- predict(EU_qty_linearfit, Month=EU_qty_time_val)
 summary(EU_qty_global_pred)
 lines(EU_qty_time_val, EU_qty_global_pred, col='red', lwd=2)
 #
 #We will subtract global pred to get local prediction
 EU_qty_local_pred <- timeseries_EU_Consumer_df_Qty - EU_qty_global_pred
 plot(EU_qty_local_pred, col='green', type = "l")

 #ACF plot 
 acf(EU_qty_local_pred) #ACF plot shows there is 4 more peak after the peak at lag 0 , which exceed confidence level.
 acf(EU_qty_local_pred, type="partial") #PACF plot has 3 peaks outside confidence level.

 #We will perform auto arima on local prediction
 EU_qty_armafit <- auto.arima(EU_qty_local_pred)

 tsdiag(EU_qty_armafit)
 EU_qty_armafit
 #Result:
 #sigma^2 estimated as 8302:  log likelihood=-248.57
 #AIC=503.14   AICc=503.77   BIC=508.35
 
 #Formal testing to check if the residual series in white noise
 EU_qty_residual <- EU_qty_local_pred - fitted(EU_qty_armafit)

 #Dickey-Fuller test
 adf.test(EU_qty_residual,alternative = "stationary")
 #Result: Dickey-Fuller = -3.8644, Lag order = 3, p-value = 0.02459
 #Since p-value > 0.05 ,null hypothesis is failed to be rejected.That implies the residual series is not stationary.

 #KPSS test
 kpss.test(EU_qty_residual)
 #Result: KPSS Level = 0.035466, Truncation lag parameter = 1, p-value = 0.1
 #Since p-value > 0.05 ,null hypothesis is failed to be rejected.That implies the residual series is stationary.

 #--------------Model evaluation with classical decomposition with simple moving average----------------------------------#

 #We will perform forecasting on the rest of the six months data
 EU_qty_outdata <- EU_Consumer_df_Qty[43:48,]
 #EU_qty_time_val_out <- as.numeric(EU_qty_outdata$Order.Date)
 EU_qty_time_val_out <- as.numeric(forecast_window$Order.Date)
 
 EU_qty_global_pred_out <- predict(EU_qty_linearfit,data.frame(Order_Month =EU_qty_time_val_out))

 #MAPE value for the forecasting
 EU_qty_MAPE <-accuracy(EU_qty_global_pred_out,EU_qty_outdata[,2])[5]
 #MAPE : 47.62624

 #Forecast Visualizaation
 ##This will give sales for 12 month.
 EU_qty_global_pred_out_next <- as.data.frame(EU_qty_global_pred_out)
 
 EU_qty_global_pred_out_forecast <- cbind(forecast_window,EU_qty_global_pred_out_next)
 
 colnames(EU_qty_global_pred_out_forecast) <- c('Order.Date','monthly_tot_ordered_qty')
 combine_date <- rbind(EU_qty_indata,EU_qty_global_pred_out_forecast)
 
 
 plot(combine_date)
 lines(combine_date,col="darkgreen",lwd=1,lty=1)
 lines(EU_qty_global_pred_out_forecast,col='red', lwd=2,lty=2)
 
 
 #----------End of Model evaluation for Classical decomposition for EU-Consumer-monthly_tot_ordered_qty-------------------#

 #We will perform smoothing using Holt Winters technique with various alpha  value----------------------------------------#

 #We plot the series again
 plot(timeseries_EU_Consumer_df_Qty)

 colors <- c("yellow", "orange", "green", "pink", "blue")
 alphas <- c(0.02, 0.3, 0.2, 0.1)
 labels <- c(paste("alpha =", alphas), "Original")
 for (i in seq(1,length(alphas))) {
  EU_qty_smoothedseries_HW <- HoltWinters(timeseries_EU_Consumer_df_Qty, alpha=alphas[i],beta=FALSE, gamma=FALSE)
  lines(fitted(EU_qty_smoothedseries_HW)[,1], col=colors[i], lwd=2)
 }
 legend("bottomright", labels, col=colors, lwd=1)  

 #since the series has both seasonality and trend
 plot(timeseries_EU_Consumer_df_Qty)
 
 EU_qty_smoothedseries_HW$fitted
 plot(EU_qty_smoothedseries_HW)
 #Smoothing parameters:
 #alpha: 0.1
 #beta : False
 #gamma: False

 #--------------Model evaluation with Holt Winter smoothing--------------------------------------------------------------------------#
 
 #MAPE value for the prediction
 EU_qty_pred_out_HW<-predict(EU_qty_smoothedseries_HW,n.ahead = 6,prediction.interval = TRUE)
 
 EU_qty_MAPE_HW <-accuracy(EU_qty_pred_out_HW, EU_qty_outdata[,2])[5]
 #MAPE : 33.94594
 #Holt-Winter's MAPE value is better than moving average smoothing in classical decomposition.
 
 EU_qty_forecast.HW <- forecast:::forecast.HoltWinters(EU_qty_smoothedseries_HW,h=12)
 forecast:::plot.forecast(EU_qty_forecast.HW,lty=1,col="red")
 
 EU_qty_forecast.HW_level95_upper <- cbind(as.data.frame(c(43:54)),as.data.frame(EU_qty_forecast.HW$upper[,2]))
 EU_qty_forecast.HW_level95_upper <- cbind(forecast_window,EU_qty_forecast.HW_level95_upper[,2])
 colnames(EU_qty_forecast.HW_level95_upper) <- c('Order.Date','monthly_tot_ordered_qty')
 EU_qty_combine_HW_level95_upper <- rbind(EU_qty_indata,EU_qty_forecast.HW_level95_upper)
 plot(EU_qty_combine_HW_level95_upper)
 lines(EU_qty_combine_HW_level95_upper,col="darkgreen",lwd=1,lty=1)
 lines(EU_qty_forecast.HW_level95_upper,col='red', lwd=2,lty=2)
 
 EU_qty_forecast.HW_level95_lower <- cbind(as.data.frame(c(43:54)),as.data.frame(EU_qty_forecast.HW$lower[,2]))
 EU_qty_forecast.HW_level95_lower <- cbind(forecast_window,EU_qty_forecast.HW_level95_lower[,2])
 colnames(EU_qty_forecast.HW_level95_lower) <- c('Order.Date','monthly_tot_ordered_qty')
 EU_qty_combine_HW_level95_lower <- rbind(EU_qty_indata,EU_qty_forecast.HW_level95_lower)
 lines(EU_qty_combine_HW_level95_lower)
 lines(EU_qty_combine_HW_level95_lower,col="blue",lwd=1,lty=1)
 lines(EU_qty_forecast.HW_level95_lower,col='red', lwd=2,lty=2)
 #------------------------------------------------------------------------------------------------------------------------#
 #2.Auto- arima model

 EU_qty_Autoarima <- auto.arima(timeseries_EU_Consumer_df_Qty)
 tsdiag(EU_qty_Autoarima)
 plot(EU_qty_Autoarima$x,col="black")
 lines(fitted(EU_qty_Autoarima), col="blue")

 #We will compute residual series to test white noise
 EU_qty_Autoarima_residual <- timeseries_EU_Consumer_df_Qty - fitted(EU_qty_Autoarima)

 #Dickey-Fuller test
 adf.test(EU_qty_Autoarima_residual,alternative = "stationary")
 #Result: Dickey-Fuller = -3.5969, Lag order = 3, p-value = 0.04521
 #Since p-value < 0.05 ,null hypothesis is rejected.That implies the residual series is stationary.

 #KPSS test
 kpss.test(EU_qty_Autoarima_residual)
 #Result: KPSS Level = 0.047939, Truncation lag parameter = 1, p-value = 0.1
 #Since p-value > 0.05 ,null hypothesis is failed to be rejected.That implies the residual series is stationary.

 #--------------Model evaluation with AUTO ARIMA -------------------------------------------------------------------------#

 EU_qty_global_pred_out_autoarima <- predict(EU_qty_Autoarima,n.ahead = 12)
 #MAPE value fr the forecasting
 EU_qty_MAPE_autoarima <-accuracy(EU_qty_global_pred_out_autoarima$pred, EU_qty_outdata[,2])[5]
 #MAPE : 30.13319
 # MAPE value for autoarima model is better than Holt-Winter forecast as well as better than
 # classical decomposition moving average smoothing.
 
 ##This is will give number
 EU_qty_global_pred_out_autoarima$pred
 
 #Forecast Visulization 
 #We create a autoarima  window since 2011-1-1 for forecast plot.
 forecast_window_autoarima <- as.data.frame(seq(as.Date("2011-01-01"), by = "month", length.out = 54))
 
 #We plot original data set
 plot(EU_Consumer_df_Qty, col = "black",lwd=1,lty=1)
 lines(EU_Consumer_df_Qty, col = "black",lty=1)
 
 #Now we willplot auto arima prediction since 2011-1-1 to 2015-6-1
 auto_arima_pred <- c(fitted(EU_qty_Autoarima),ts(EU_qty_global_pred_out_autoarima$pred))
 EU_qty_combine_date_autoarima_full <- cbind(forecast_window_autoarima,as.data.frame(auto_arima_pred))
 colnames(EU_qty_combine_date_autoarima_full) <- c("Order.Date","monthly_tot_ordered_qty")
 lines(EU_qty_combine_date_autoarima_full,col = "green",lty=1)
 
 #Following plot shows the forecasted data for last 12 monthe including last 6 month's unseen data
 EU_qty_combine_date_autoarima <- cbind(forecast_window,as.data.frame(ts(EU_qty_global_pred_out_autoarima$pred)))
 colnames(EU_qty_combine_date_autoarima) <- c("Order.Date","monthly_tot_ordered_qty")
 lines(EU_qty_combine_date_autoarima,col='red', lwd=3,lty=2)
 
 
 #----------End of Model evaluation for Auto-arima for EU-Consumer-monthly_tot_ordered_qty---------------------------------#
 #----------End of EU-Consumer Market segment analysis --------------------------------------------------------------------#
 
 #-------------------------------------------------------------------------------------------------------------------------#
 #****************************************** Model Building ***************************************************************#
 #-------------------------------------------------------------------------------------------------------------------------#
 
 #----------------------------------EU-Consumer Market Segment Analysis and Forecasting------------------------------------#
 
 #1. We check the structure of EU market segment dataset
 str(APAC_Consumer_df)
 
 #2.We convert Order.Date into date format
 APAC_Consumer_df$Order.Date <- sapply(APAC_Consumer_df$Order.Date, as.character)
 APAC_Consumer_df$Order.Date <- paste("1",APAC_Consumer_df$Order.Date,sep="-")
 APAC_Consumer_df$Order.Date <- as.Date(APAC_Consumer_df$Order.Date,format="%d-%m-%Y")
 
 #3. We convert monthly_sales into numeric format
 APAC_Consumer_df$monthly_sales <- sapply(sapply(APAC_Consumer_df$monthly_sales,as.character),as.numeric)
 
 #4. We convert total ordered Quantity(Demand) into numeric format
 APAC_Consumer_df$monthly_tot_ordered_qty <- sapply(sapply(APAC_Consumer_df$monthly_tot_ordered_qty,as.character),as.numeric)
 
 #We will perform Sales and Demand forecasting separately using different smoothing techniques and will compare MAPE values.
 #
 #----------------------------------APAC-Consumer Sales Analysis-------------------------------------------------------------#
 #
 # 1.Classical Decomposition
 #a.We will take subset of APAC_Consumer_df for Sales analysis
 APAC_Consumer_df_Sales <- APAC_Consumer_df %>% select(Order.Date,monthly_sales) %>% arrange(Order.Date)
 #We will split last 6 months data for accuracy test from the base data set
 APAC_sales_indata <- APAC_Consumer_df_Sales[1:42,]
 
 #We will convert this dataframe to time series
 
 timeseries_APAC_Consumer_df_Sales <- ts(APAC_sales_indata$monthly_sales)
 #We plot the series
 plot(timeseries_APAC_Consumer_df_Sales)
 
 #-------------------We will perform smoothing using Simple Moving Average Smoothing technique----------------------------#
 #Set the window
 APAC_sales_width <- 1
 APAC_sales_smoothedseries <- stats::filter(timeseries_APAC_Consumer_df_Sales,filter=rep(1/(2*APAC_sales_width+1),(2*APAC_sales_width+1)),method ='convolution',sides=2)
 
 #Smoothing left end of the time series
  
 width_diff <- APAC_sales_smoothedseries[APAC_sales_width+2] - APAC_sales_smoothedseries[APAC_sales_width+1]
  for (i in seq(APAC_sales_width,1,-1)) {
    APAC_sales_smoothedseries[i] <- APAC_sales_smoothedseries[i+1] - width_diff
  }
  #
  #Smoothing right end of the time series
  n <- length(timeseries_APAC_Consumer_df_Sales)
  width_diff <- APAC_sales_smoothedseries[n-APAC_sales_width] - APAC_sales_smoothedseries[n-APAC_sales_width-1]
  for (i in seq(n-APAC_sales_width+1, n)) {
    APAC_sales_smoothedseries[i] <- APAC_sales_smoothedseries[i-1] + width_diff
  }
  #
  
  #Smoothed time series-APAC_sales_smoothedseries plot
  # 
  APAC_sales_time_val <- APAC_sales_indata$Order.Date
  
  lines(APAC_sales_smoothedseries, col="blue", lwd=2)
  
  #We will build classical decomposition model on the smoothed time series
  #Converting time series to a dataframe
  
  APAC_sales_smoothed_df <- as.data.frame(cbind(APAC_sales_time_val,as.vector(APAC_sales_smoothedseries)))
  colnames(APAC_sales_smoothed_df) <- c('Order_Month','Sales')
  #
  #We build the global model over the smoothed series
  
  APAC_sales_linearfit <- lm(Sales ~ sin(0.4*Order_Month) * poly(Order_Month,2) + cos(0.6*Order_Month) * poly(Order_Month,2)+Order_Month, 
                             data=APAC_sales_smoothed_df)
  
  #Summary of APAC_sales_linearfit
  summary(APAC_sales_linearfit)
  #Multiple R-squared:  0.8402,	Adjusted R-squared:  0.8014 
  #F-statistic: 21.68 on 8 and 30 DF,  p-value: 4.68e-11
  
  APAC_sales_global_pred <- predict(APAC_sales_linearfit, Month=APAC_sales_time_val)
  summary(APAC_sales_global_pred)
  lines(APAC_sales_time_val, APAC_sales_global_pred, col='red', lwd=2)
  #
  #We will subtract global pred to get local prediction
  APAC_sales_local_pred <- timeseries_APAC_Consumer_df_Sales - APAC_sales_global_pred
  plot(APAC_sales_local_pred, col='green', type = "l")
  #To check stationaity in local series we will plot ACF and PACF.
  #ACF plot 
  acf(APAC_sales_local_pred) #ACF plot shows there is 1 more peaks after the peak at lag 0 , which exceed confidence level.
  #PACF plot
  acf(APAC_sales_local_pred, type="partial") #PACF plot has 2 peaks outside confidence level.
  
  #We will perform auto arima on local series
  APAC_sales_armafit <- auto.arima(APAC_sales_local_pred)
  
  tsdiag(APAC_sales_armafit)
  APAC_sales_armafit
  #ARIMA(0,0,1) with zero mean 
  #sigma^2 estimated as 83058300:  log likelihood=-442.52
  #AIC=889.04   AICc=889.35   BIC=892.52
  
  #Formal tests to check if the residual series is white noise
  APAC_sales_residual <- APAC_sales_local_pred - fitted(APAC_sales_armafit)
  
  #Dickey-Fuller test
  adf.test(APAC_sales_residual,alternative = "stationary")
  #Result: Dickey-Fuller = -4.1227, Lag order = 3, p-value =  0.01436
  #Since p-value < 0.05 ,null hypothesis is rejected.That implies the residual series is stationary.
  
  #KPSS test
  kpss.test(APAC_sales_residual)
  #Result: KPSS Level = 0.0829, Truncrow.namesation lag parameter = 1, p-value = 0.1
  #Since p-value > 0.05 ,null hypothesis is failed to be rejected.That implies the residual series is stationary.
  
  #--------------------------------Model evaluation with classical decomposition with simple moving average----------------#
  #We will perform forecasting on the rest of the six months data
  APAC_sales_outdata <- APAC_Consumer_df_Sales[43:48,]
  #APAC_sales_time_val_out <- as.numeric(APAC_sales_outdata$Order.Date)
  APAC_sales_time_val_out <- as.numeric(forecast_window$Order.Date)
  APAC_sales_global_pred_out <- predict(APAC_sales_linearfit,data.frame(Order_Month =APAC_sales_time_val_out))
  
  #MAPE value fr the forecasting
  APAC_sales_MAPE <-accuracy(APAC_sales_global_pred_out,APAC_sales_outdata[,2])[5]
  #MAPE : 30.17101
  
  #Forecast Visualizaation
  ##This will give sales for 12 month.
  APAC_sales_global_pred_out_next <- as.data.frame(APAC_sales_global_pred_out) 
  
  APAC_sales_global_pred_out_forecast <- cbind(forecast_window,APAC_sales_global_pred_out_next)
  
  colnames(APAC_sales_global_pred_out_forecast) <- c('Order.Date','monthly_sales')
  combine_date <- rbind(APAC_sales_indata,APAC_sales_global_pred_out_forecast)
  
  
  plot(combine_date)
  lines(combine_date,col="darkgreen",lwd=1,lty=1)
  lines(APAC_sales_global_pred_out_forecast,col='red', lwd=2,lty=2)
  
  
  #----------------------------End of Model evaluation for Classical decomposition for EU-Consumer-Sales-------------------#
  
  #We will perform smoothing using Holt Winters technique with various alpha  value----------------------------------------#
  
  #We plot the series again
  plot(timeseries_APAC_Consumer_df_Sales)
  
  colors <- c("yellow", "orange", "green", "pink", "blue")
  alphas <- c(0.02, 0.3, 0.2, 0.1)
  labels <- c(paste("alpha =", alphas), "Original")
  for (i in seq(1,length(alphas))) {
    APAC_sales_smoothedseries_HW <- HoltWinters(timeseries_APAC_Consumer_df_Sales, alpha=alphas[i],beta=FALSE,gamma=FALSE)
    lines(fitted(APAC_sales_smoothedseries_HW)[,1], col=colors[i], lwd=2)
  }
  legend('bottomright',labels, col=colors, lwd=2,xjust = 1)  
  
  #since the series has both seasonality and trend
  plot(timeseries_APAC_Consumer_df_Sales)
  
  #Smoothing parameters:
  #alpha: 0.1
  
  APAC_sales_smoothedseries_HW$fitted
  plot(APAC_sales_smoothedseries_HW)

  #--------------Model evaluation with Holt Winter smoothing--------------------------------------------------------------------------#
  
  #MAPE value for the prediction
  APAC_sales_pred_out_HW<-predict(APAC_sales_smoothedseries_HW,n.ahead = 6,prediction.interval = TRUE)
  
  APAC_sales_MAPE_HW <-accuracy(APAC_sales_pred_out_HW, APAC_sales_outdata[,2])[5]
  #MAPE : 30.01394
  #Holt-Winter's MAPE value is slightly less good than moving average smoothing in classical decomposition.
  APAC_sales_forecast.HW <- forecast:::forecast.HoltWinters(APAC_sales_smoothedseries_HW,h=12)
  forecast:::plot.forecast(APAC_sales_forecast.HW,lty=1,col="red")
  
  APAC_sales_forecast.HW_level95_upper <- cbind(as.data.frame(c(43:54)),as.data.frame(APAC_sales_forecast.HW$upper[,2]))
  APAC_sales_forecast.HW_level95_upper <- cbind(forecast_window,APAC_sales_forecast.HW_level95_upper[,2])
  colnames(APAC_sales_forecast.HW_level95_upper) <- c('Order.Date','monthly_sales')
  APAC_sales_combine_HW_level95_upper <- rbind(APAC_sales_indata,APAC_sales_forecast.HW_level95_upper)
  plot(APAC_sales_combine_HW_level95_upper)
  lines(APAC_sales_combine_HW_level95_upper,col="darkgreen",lwd=1,lty=1)
  lines(APAC_sales_forecast.HW_level95_upper,col='red', lwd=2,lty=2)
  
  APAC_sales_forecast.HW_level95_lower <- cbind(as.data.frame(c(43:54)),as.data.frame(APAC_sales_forecast.HW$lower[,2]))
  APAC_sales_forecast.HW_level95_lower <- cbind(forecast_window,APAC_sales_combine_HW_level95_lower[,2])
  colnames(APAC_sales_forecast.HW_level95_lower) <- c('Order.Date','monthly_sales')
  APAC_sales_combine_HW_level95_lower <- rbind(APAC_sales_indata,APAC_sales_forecast.HW_level95_lower)
  lines(APAC_sales_combine_HW_level95_lower)
  lines(APAC_sales_combine_HW_level95_lower,col="blue",lwd=1,lty=1)
  lines(APAC_sales_forecast.HW_level95_lower,col='red', lwd=2,lty=2)
  
  
  
  #------------------------------------------------------------------------------------------------------------------------#
  
  #2.Auto- arima model
  APAC_sales_Autoarima <- auto.arima(timeseries_APAC_Consumer_df_Sales)
  tsdiag(APAC_sales_Autoarima)
  plot(APAC_sales_Autoarima$x,col="black")
  lines(fitted(APAC_sales_Autoarima), col="blue")
  
  #We will compute residual series to test white noise
  APAC_sales_Autoarima_residual <- timeseries_APAC_Consumer_df_Sales - fitted(APAC_sales_Autoarima)
  
  #Dickey-Fuller test 
  adf.test(APAC_sales_Autoarima_residual,alternative = "stationary")
  #Result: -4.2563, Lag order = 3, p-value = 0.01
  #Since p-value < 0.05 ,null hypothesis is rejected.That implies the residual series is stationary.
  
  #KPSS test  
  kpss.test(APAC_sales_Autoarima_residual)
  #Result: KPSS Level =  0.042734, Truncation lag parameter = 1, p-value = 0.1
  #Since p-value > 0.05 ,null hypothesis is failed to be rejected.That implies the residual series is stationary.
  
  #--------------Model evaluation with AUTO ARIMA --------------------------------------------------------------------------#
  
  APAC_sales_global_pred_out_autoarima <- predict(APAC_sales_Autoarima,n.ahead = 6)
  #MAPE value fr the forecasting
  APAC_sales_MAPE_autoarima <-accuracy(APAC_sales_global_pred_out_autoarima$pred, APAC_sales_outdata[,2])[5]
  #MAPE : 27.68952
  
  # MAPE value for autoarima model is better than Holt-Winter forecast but not as good as 
  # classical decomposition moving average smoothing.
  
  #------------------------End of Model evaluation for Auto-arima for EU-Consumer-Sales-------------------------------------#
  
  #-------------------------------------------------------------------------------------------------------------------------#
  #----------------------------------EU-Consumer Quantity [Demand] Analysis-------------------------------------------------#
  
  # 1.Classical Decomposition
  #a.We will take subset of monthly_tot_ordered_qty for total ordered Quantity(Demand)  analysis
  APAC_Consumer_df_Qty <- APAC_Consumer_df %>% select(Order.Date,monthly_tot_ordered_qty) %>% arrange(Order.Date)
  
  #We will split last 6 months data for accuracy test from the base data set
  APAC_qty_indata <- APAC_Consumer_df_Qty[1:42,]
  
  #We will convert this dataframe to time series
  timeseries_APAC_Consumer_df_Qty <- ts(APAC_qty_indata$monthly_tot_ordered_qty)
  #We plot the series
  plot(timeseries_APAC_Consumer_df_Qty)
  
  #We will perform smoothing using Simple Moving Average Smoothing technique------------------------------------------------#
  APAC_qty_width <- 1
  APAC_qty_smoothedseries <- stats::filter(timeseries_APAC_Consumer_df_Qty,filter=rep(1/(2*APAC_qty_width+1),(2*APAC_qty_width+1)),method = 
                                             'convolution',sides=2)
  
  #Smoothing left end of the time series
  
  width_diff <- APAC_qty_smoothedseries[APAC_qty_width+2] - APAC_qty_smoothedseries[APAC_qty_width+1]
  for (i in seq(APAC_qty_width,1,-1)) {
    APAC_qty_smoothedseries[i] <- APAC_qty_smoothedseries[i+1] - width_diff
  }
  
  #Smoothing right end of the time series
  
  n <- length(timeseries_APAC_Consumer_df_Qty)
  width_diff <- APAC_qty_smoothedseries[n-APAC_qty_width] - APAC_qty_smoothedseries[n-APAC_qty_width-1]
  for (i in seq(n-APAC_qty_width+1, n)) {
    APAC_qty_smoothedseries[i] <- APAC_qty_smoothedseries[i-1] + width_diff
  }
  
  #Smoothed time series-APAC_qty_smoothedseries plot
  APAC_qty_time_val <- APAC_qty_indata$Order.Date
  lines(APAC_qty_smoothedseries, col="blue", lwd=2)
  
  #We will build classical decomposition model on the smoothed time series
  #Converting time series to a dataframe
  
  APAC_qty_smoothed_df <- as.data.frame(cbind(APAC_qty_time_val,as.vector(APAC_qty_smoothedseries)))
  colnames(APAC_qty_smoothed_df) <- c('Order_Month','monthly_tot_ordered_qty')
  
  #We build the global model
  APAC_qty_linearfit <- lm(monthly_tot_ordered_qty ~ sin(0.4*Order_Month) * poly(Order_Month,3) + cos(0.6*Order_Month) * poly(Order_Month,3) + 
                             Order_Month, data=APAC_qty_smoothed_df)
  
  #Summary of APAC_qty_linearfit
  summary(APAC_qty_linearfit)
  
  #Multiple R-squared:  0.8591,	Adjusted R-squared: 0.8074
  #F-statistic: 16.63 on 11 and 30 DF,  p-value: 7.174e-10
  
  APAC_qty_global_pred <- predict(APAC_qty_linearfit, Month=APAC_qty_time_val)
  summary(APAC_qty_global_pred)
  lines(APAC_qty_time_val, APAC_qty_global_pred, col='red', lwd=2)
  #
  #We will subtract global pred to get local prediction
  APAC_qty_local_pred <- timeseries_APAC_Consumer_df_Qty - APAC_qty_global_pred
  plot(APAC_qty_local_pred, col='green', type = "l")
  
  #ACF plot 
  acf(APAC_qty_local_pred) #ACF plot shows there is 4 more peak after the peak at lag 0 , which exceed confidence level.
  acf(APAC_qty_local_pred, type="partial") #PACF plot has 3 peaks outside confidence level.
  
  #We will perform auto arima on local prediction
  APAC_qty_armafit <- auto.arima(APAC_qty_local_pred)
  
  tsdiag(APAC_qty_armafit)
  APAC_qty_armafit
  #Result:
  #sigma^2 estimated as 10461:  log likelihood=-253.96
  #AIC=509.92   AICc=510.02   BIC=511.65
  
  #Formal testing to check if the residual series in white noise
  APAC_qty_residual <- APAC_qty_local_pred - fitted(APAC_qty_armafit)
  
  #Dickey-Fuller test
  adf.test(APAC_qty_residual,alternative = "stationary")
  #Result: Dickey-Fuller = -7.1823, Lag order = 3, p-value = 0.01
  #Since p-value < 0.05 ,null hypothesis is rejected.That implies the residual series is stationary.
  
  #KPSS test
  kpss.test(APAC_qty_residual)
  #Result: KPSS Level = 0.020222, Truncation lag parameter = 1, p-value = 0.1
  #Since p-value > 0.05 ,null hypothesis is failed to be rejected.That implies the residual series is stationary.
  
  #--------------Model evaluation with classical decomposition with simple moving average----------------------------------#
  
  #We will perform forecasting on the rest of the six months data
  APAC_qty_outdata <- APAC_Consumer_df_Qty[43:48,]
  #APAC_qty_time_val_out <- as.numeric(APAC_qty_outdata$Order.Date)
  APAC_qty_time_val_out <- as.numeric(forecast_window$Order.Date)
  
  APAC_qty_global_pred_out <- predict(APAC_qty_linearfit,data.frame(Order_Month =APAC_qty_time_val_out))
  
  #MAPE value for the forecasting
  APAC_qty_MAPE <-accuracy(APAC_qty_global_pred_out,APAC_qty_outdata[,2])[5]
  #MAPE : 81.44083
  
  #Forecast Visualizaation
  ##This will give sales for 12 month.
  APAC_qty_global_pred_out_next <- as.data.frame(APAC_qty_global_pred_out)
  
  APAC_qty_global_pred_out_forecast <- cbind(forecast_window,APAC_qty_global_pred_out_next)
  
  colnames(APAC_qty_global_pred_out_forecast) <- c('Order.Date','monthly_tot_ordered_qty')
  combine_date <- rbind(APAC_qty_indata,APAC_qty_global_pred_out_forecast)
  
  
  plot(combine_date)
  lines(combine_date,col="darkgreen",lwd=1,lty=1)
  lines(APAC_qty_global_pred_out_forecast,col='red', lwd=2,lty=2)
  
  #----------End of Model evaluation for Classical decomposition for EU-Consumer-monthly_tot_ordered_qty-------------------#
  
  #We will perform smoothing using Holt Winters technique with various alpha  value----------------------------------------#
  
  #We plot the series again
  plot(timeseries_APAC_Consumer_df_Qty)
  
  colors <- c("yellow", "orange", "green", "pink", "blue")
  alphas <- c(0.02, 0.3, 0.2, 0.1)
  labels <- c(paste("alpha =", alphas), "Original")
  for (i in seq(1,length(alphas))) {
    APAC_qty_smoothedseries_HW <- HoltWinters(timeseries_APAC_Consumer_df_Qty, alpha=alphas[i],beta=FALSE, gamma=FALSE)
    lines(fitted(APAC_qty_smoothedseries_HW)[,1], col=colors[i], lwd=2)
  }
  legend("bottomright", labels, col=colors, lwd=1)  
  
  #since the series has both seasonality and trend
  plot(timeseries_APAC_Consumer_df_Qty)
  
  APAC_qty_smoothedseries_HW$fitted
  plot(APAC_qty_smoothedseries_HW)
  #Smoothing parameters:
  #alpha: 0.1
  #beta : False
  #gamma: False
  
  APAC_qty_forecast.HW <- forecast:::forecast.HoltWinters(APAC_qty_smoothedseries_HW,h=12)
  forecast:::plot.forecast(APAC_qty_forecast.HW,lty=1,col="red")
  
  APAC_qty_forecast.HW_level95_upper <- cbind(as.data.frame(c(43:54)),as.data.frame(APAC_qty_forecast.HW$upper[,2]))
  APAC_qty_forecast.HW_level95_upper <- cbind(forecast_window,APAC_qty_forecast.HW_level95_upper[,2])
  colnames(APAC_qty_forecast.HW_level95_upper) <- c('Order.Date','monthly_tot_ordered_qty')
  APAC_qty_combine_HW_level95_upper <- rbind(APAC_qty_indata,APAC_qty_forecast.HW_level95_upper)
  plot(APAC_qty_combine_HW_level95_upper)
  lines(APAC_qty_combine_HW_level95_upper,col="darkgreen",lwd=1,lty=1)
  lines(APAC_qty_forecast.HW_level95_upper,col='red', lwd=2,lty=2)
  
  APAC_qty_forecast.HW_level95_lower <- cbind(as.data.frame(c(43:54)),as.data.frame(APAC_qty_forecast.HW$lower[,2]))
  APAC_qty_forecast.HW_level95_lower <- cbind(forecast_window,APAC_qty_forecast.HW_level95_lower[,2])
  colnames(APAC_qty_forecast.HW_level95_lower) <- c('Order.Date','monthly_tot_ordered_qty')
  APAC_qty_combine_HW_level95_lower <- rbind(APAC_qty_indata,APAC_qty_forecast.HW_level95_lower)
  lines(APAC_qty_combine_HW_level95_lower)
  lines(APAC_qty_combine_HW_level95_lower,col="blue",lwd=1,lty=1)
  lines(APAC_qty_forecast.HW_level95_lower,col='red', lwd=2,lty=2)
  
  
  #--------------Model evaluation with Holt Winter smoothing--------------------------------------------------------------------------#
  
  #MAPE value for the prediction
  APAC_qty_pred_out_HW<-predict(APAC_qty_smoothedseries_HW,n.ahead = 6,prediction.interval = TRUE)
  
  APAC_qty_MAPE_HW <-accuracy(APAC_qty_pred_out_HW, APAC_qty_outdata[,2])[5]
  #MAPE : 36.88358
  #Holt-Winter's MAPE value is better than moving average smoothing in classical decomposition.
  
  #----------------------------------------------------------------------------------------------------------------------------------#
  #2.Auto- arima model
  
  APAC_qty_Autoarima <- auto.arima(timeseries_APAC_Consumer_df_Qty)
  tsdiag(APAC_qty_Autoarima)
  plot(APAC_qty_Autoarima$x,col="black")
  lines(fitted(APAC_qty_Autoarima), col="blue")
  
  #We will compute residual series to test white noise
  APAC_qty_Autoarima_residual <- timeseries_APAC_Consumer_df_Qty - fitted(APAC_qty_Autoarima)
  
  #Dickey-Fuller test
  adf.test(APAC_qty_Autoarima_residual,alternative = "stationary")
  #Result: Dickey-Fuller = -4.3326, Lag order = 3, p-value = 0.01
  #Since p-value < 0.05 ,null hypothesis is rejected.That implies the residual series is stationary.
  
  #KPSS test
  kpss.test(APAC_qty_Autoarima_residual)
  #Result: KPSS Level = 0.0479390.031535, Truncation lag parameter = 1, p-value = 0.1
  #Since p-value > 0.05 ,null hypothesis is failed to be rejected.That implies the residual series is stationary.
  
  #--------------Model evaluation with AUTO ARIMA -------------------------------------------------------------------------#
  
  APAC_qty_global_pred_out_autoarima <- predict(APAC_qty_Autoarima,n.ahead = 12)
  #MAPE value fr the forecasting
  APAC_qty_MAPE_autoarima <-accuracy(APAC_qty_global_pred_out_autoarima$pred, APAC_qty_outdata[,2])[5]
  #MAPE : 26.24458
  # MAPE value for autoarima model is better than Holt-Winter forecast as well as better than
  # classical decomposition moving average smoothing.
  
  
  ##This is will give number
  APAC_qty_global_pred_out_autoarima$pred
  
  #Forecast Visulization 
  #We create a autoarima  window since 2011-1-1 for forecast plot.
  forecast_window_autoarima <- as.data.frame(seq(as.Date("2011-01-01"), by = "month", length.out = 54))
  
  #We plot original data set
  plot(APAC_Consumer_df_Qty, col = "black",lwd=1,lty=1)
  lines(APAC_Consumer_df_Qty, col = "black",lty=1)
  
  #Now we willplot auto arima prediction since 2011-1-1 to 2015-6-1
  auto_arima_pred <- c(fitted(APAC_qty_Autoarima),ts(APAC_qty_global_pred_out_autoarima$pred))
  APAC_qty_combine_date_autoarima_full <- cbind(forecast_window_autoarima,as.data.frame(auto_arima_pred))
  colnames(APAC_qty_combine_date_autoarima_full) <- c("Order.Date","monthly_tot_ordered_qty")
  lines(APAC_qty_combine_date_autoarima_full,col = "green",lty=1)
  
  #Following plot shows the forecasted data for last 12 monthe including last 6 month's unseen data
  APAC_qty_combine_date_autoarima <- cbind(forecast_window,as.data.frame(ts(APAC_qty_global_pred_out_autoarima$pred)))
  colnames(APAC_qty_combine_date_autoarima) <- c("Order.Date","monthly_tot_ordered_qty")
  lines(APAC_qty_combine_date_autoarima,col='red', lwd=3,lty=2)
  
  #----------End of Model evaluation for Auto-arima for EU-Consumer-monthly_tot_ordered_qty---------------------------------#
  