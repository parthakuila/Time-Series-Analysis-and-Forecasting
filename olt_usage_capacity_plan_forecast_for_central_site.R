################### Install require packages and Import packages ###################
rm(list=ls()) #Removes all items in Environment!
{sink("./ranalytics.log")
  starting_time <- Sys.time()
  print(paste("Starting process at :",Sys.time()))
  print(paste("Starting processing from date :",Sys.Date()))
  
  # install.packages(c('data.table','dplyr','tidyverse','bit64','chron','xts','forecast','TSA','gtools','mongolite'))
  ## Load require Library
  library(data.table)  ## data import and manipulation
  library(dplyr)       ## data import and manipulation
  library(tidyverse)   ## data import and manipulation
  library(bit64)       ## long number
  library(chron)       ## for handle date and time
  library(xts)         ## for time series object and analysis
  library(forecast)    ## for time series forecasting
  library(TSA)         ## for time wise data handaling
  library(ggplot2)     ## for graphical presentation
  library(gtools)
  ## Using Mongolite
  library(mongolite)  ## mongodb database connection 
  mongoUrl <- "mongodb://spectauser:spectaDb#011@10.0.0.14:27017/spectaData" #<-admin here is the mongodb database that stores the authentication info
  ### Collection of Input Data ####
  ## Site wise change the collection name ##
  col_site <- "AAA_data_forecast_olt_ctg"
  ## create connection (con) ##
  con_site <- mongo(collection = col_site, url = mongoUrl, db="spectaData")
  ### Collection of Output Data ####
  ## create Output Collection ##
  ## Site wise change the Output collection name ##
  col_site_test <- "weekly_olt_forecast"
  #create connection (con)
  con_site_test <- mongo(collection = col_site_test, url = mongoUrl, db="spectaData")
  ## Check Data and convert Dataframe
  # con_site$count('{}')
  # con_site$find(limit = 5) ## Check data
  site <- con_site$find()  ## store the input data as dataframe
  ## Create sub-groups of site dataframe and select necessary column for building model ##
  data <- site[,c('recorddate','olt','usage','uniques')]  
  #################### Business Logic #######################
  ## Business Logic of User Growth ##
  ## one Month growth function ##
  user_growth_1M <- function(stPt,perc,shape = "Linear")
  {
    if(shape == "Linear"){
      users <- seq(from = stPt*(1+perc/30), by = (stPt*perc)/(30), length.out = 30 )
    }
    return(users)
  }
  ## three months growth function ##
  user_growth_3M <- function(stPt,perc,shape = "Linear") {
    if(shape == "Linear"){
      users1 <- seq(from = stPt*(1+perc/30), by = (stPt*perc)/(30), length.out = 30 )
      users2 <- seq(from = users1[30]*(1+perc/30), by = (users1[30]*perc/30), length.out = 30)        
      users3 <- seq(from = users2[30]*(1+perc/30), by = (users2[30]*perc/30), length.out = 30)
      users <- c( users1, users2,users3)
    }
    return (users)
  }
  ## User growth ##
  growth <- c(0.05,-0.05,0.1,-0.1,0)
  perms <- permutations(n=5,r=1,v=growth,repeats.allowed=T)
  ################### Data cleaning Part ##########################
  ## Check the data set ##
  ## Missing data handaling ##
  head(data)
  dim(data)
  colnames(data)
  lapply(data, class)
  str(data)
  colSums(is.na(data))
  data <- na.omit(data) ## na remove
  ## converted date time ##
  data$recorddate <- as.Date(data$recorddate, format = "%A %b %d %Y")
  # head(data$recorddate)
  max(data$recorddate)
  min(data$recorddate)
  ## converted usage in TB ##
  data$usage <- data$usage/(1024*1024*1024*1024) #in tb
  # head(data$usage)
  data$olt <- as.factor(data$olt) 
  ## Remove UNKNOWN and SELECT olt
  # data <- data[data$olt!='UNKNOWN' & data$olt!='SELECT', ]
  data <- data[data$olt!='UNKNOWN',]
  head(data, 5)
  colnames(data)
  ## Cumulative sum for overall forecast of each site ##
  df <- data %>% 
    group_by(recorddate) %>% 
    mutate(csum_usage = cumsum(usage)) %>%
    mutate(csum_user = cumsum(uniques))
  ## check and remove duplicate data over olt and keep highest value by recorddate ##
  df<- df %>% 
    group_by(recorddate) %>%
    top_n(1, abs(csum_usage)) %>%
    top_n(1,abs(csum_user))
  print(length(df$recorddate))
  ## Create new dataframe ##
  dfnew <- df[,c('recorddate','csum_usage','csum_user')]
  ## Change column name ##
  colnames(dfnew)[2:3] <- c("usage", "uniques")
  ## Check the missing sequence of Date each Olt ##
  date_range <- seq(min(dfnew$recorddate), max(dfnew$recorddate), by = "day")
  date_range[!date_range %in% dfnew$recorddate]
  ## Create new dataframe correct time sequence wise ##
  OLTts <- dfnew %>%
    ungroup(recorddate) %>%
    mutate(recorddate = as.Date(recorddate)) %>%
    complete(recorddate = seq.Date(min(recorddate), max(recorddate), by="day"))
  # colSums(is.na(OLTts))
  print(length(OLTts$recorddate))
  ## Replacing by median for Usage & Uniques ##
  OLTts$usage=ifelse(is.na(OLTts$usage),median(OLTts$usage,na.rm=T),OLTts$usage) # By median
  OLTts$uniques=ifelse(is.na(OLTts$uniques),median(OLTts$uniques,na.rm=T),OLTts$uniques) # By median
  colSums(is.na(OLTts))
  ################### OUTLIER manipulation ########################
  ## Univariate Outliers 
  ## Check for Usage
  # outlier_values_usage <- boxplot.stats(OLTts$usage)$out  # outlier values.
  # boxplot(OLTts$usage, main="Usage(TB)", boxwex=0.1)
  # mtext(paste("Outliers: ", paste(outlier_values_usage, collapse=", ")), cex=0.6)
  # print(outlier_values_usage)
  ## check for Uniques
  # outlier_values_uniques <- boxplot.stats(OLTts$uniques)$out  # outlier values.
  # boxplot(OLTts$uniques, main="Users", boxwex=0.1)
  # mtext(paste("Outliers: ", paste(outlier_values_uniques, collapse=", ")), cex=0.6)
  # # print(outlier_values_uniques)
  ## Outliers Replace by Median ##
  outlier <- function(x) {
    x[x < quantile(x,0.25) - 1.5 * IQR(x) | x > quantile(x,0.75) + 1.5 * IQR(x)] <- median(x)
    x
  }
  OLTts$usage = outlier(OLTts$usage)
  OLTts$uniques = outlier(OLTts$uniques)
  min(OLTts$recorddate)
  max(OLTts$recorddate)
  ################### Modelling #######################################
  ## create TS objective ##
  usageTs <- xts(OLTts$usage, order.by = OLTts$recorddate, deltat=1/7)
  usageTs1 <- ts(coredata(usageTs))
  length(usageTs1)
  # train_len <- (length(usageTs1)-90)
  y <- ts(usageTs1, frequency = 7, start = (as.numeric(format((OLTts$recorddate)[1], "%j")))/7)
  len_uniques <- length(OLTts$uniques)
  x.reg <- cbind(OLTts$uniques[1:len_uniques])
  ## TimeSeries Modelling
  # fit.arima <- auto.arima(y, xreg = x.reg)
  fit.arima <- auto.arima(y, xreg=x.reg, d = NA, D = NA, max.p = 10, max.P = 5,
                          max.q = 10,max.Q = 5, max.d = 2,max.D = 1,max.order = 10,
                          start.p = 1,start.q = 1, start.P = 1,start.Q = 1,
                          stationary = FALSE,seasonal = TRUE, stepwise = TRUE)
  
  ################ Predictive Dataframe and dump ########################
  #### 1 month forecast ###################
  for (i in 1:nrow(perms)) {
    x.frc <- cbind(user_growth_1M(OLTts$uniques[len_uniques],perms[i,1]))
    x.frc <- ceiling(x.frc)  ## convert Integr with upper case
    x.frc <- as.integer(x.frc)  ## convert Integer with lower case
    arima.frc <- forecast(fit.arima,xreg=x.frc,h=30,level = 95) ## Forecasting 
    maxday <- max(OLTts$recorddate)
    frcstdate <- seq(maxday+1, by = "day", length.out = 30)
    frcstdate <- as.Date(frcstdate)
    frcstdate <- as.POSIXct(frcstdate)
    arima.frc$mean <- arima.frc$mean*(1024*1024*1024*1024)    ## convert to Byte
    arima.frc$mean <- as.numeric(arima.frc$mean)
    arima.frc$mean <- as.integer64(arima.frc$mean)
    arima.frc$lower <- arima.frc$lower*(1024*1024*1024*1024)
    arima.frc$lower <- as.numeric(arima.frc$lower)
    arima.frc$lower <- as.integer64(arima.frc$lower)
    arima.frc$upper <- arima.frc$upper*(1024*1024*1024*1024)
    arima.frc$upper <- as.numeric(arima.frc$upper)
    arima.frc$upper <- as.integer64(arima.frc$upper)
    df_pred <- data.frame(frcstdate,data.frame(rep(perms[i,1],30),data.frame(x.frc),arima.frc$mean,arima.frc$lower,arima.frc$upper,'1M',as.POSIXct(Sys.Date())))
    colnames(df_pred)<-c("frcstdate","users_growth","users","usage","usage_low","usage_high","period","recorddate")
    lapply(df_pred, class)
    ## Insert Output tino DataBase
    con_site_test$insert(df_pred)
  }
  ###### 3 month forecast ###############
  for (j in 1:nrow(perms)) {
    x.frc <- cbind(user_growth_3M(OLTts$uniques[len_uniques],perms[j,1]))
    x.frc <- ceiling(x.frc)  ## convert Integr with upper case
    x.frc <- as.integer(x.frc)  ## convert Integer with lower case
    arima.frc <- forecast(fit.arima,xreg=x.frc,h=90,level = 95) ## forecast
    maxday <- max(OLTts$recorddate)
    frcstdate <- seq(maxday+1, by = "day", length.out = 90)
    frcstdate <- as.POSIXct(frcstdate)
    class(frcstdate)
    arima.frc$mean <- arima.frc$mean*(1024*1024*1024*1024)    ## convert to Byte
    arima.frc$mean <- as.numeric(arima.frc$mean)
    arima.frc$mean <- as.integer64(arima.frc$mean)
    arima.frc$lower <- arima.frc$lower*(1024*1024*1024*1024)
    arima.frc$lower <- as.numeric(arima.frc$lower)
    arima.frc$lower <- as.integer64(arima.frc$lower)
    arima.frc$upper <- arima.frc$upper*(1024*1024*1024*1024)
    arima.frc$upper <- as.numeric(arima.frc$upper)
    arima.frc$upper <- as.integer64(arima.frc$upper)
    df_pred <- data.frame(frcstdate,data.frame(rep(perms[j,1],90),data.frame(x.frc),arima.frc$mean,arima.frc$lower,arima.frc$upper,'3M',as.POSIXct(Sys.Date())))
    colnames(df_pred)<-c("frcstdate","users_growth","users","usage","usage_low","usage_high","period","recorddate")
    lapply(df_pred, class)
    ## Insert output into DataBase
    con_site_test$insert(df_pred)
  }
  cat("\nAccuracy of model :",accuracy(fit.arima))
  # print(con_site_test$count('{}'))  # Count all records
  cat("\nNumber of record count :",con_site_test$count('{}'),"\n")  # Count all records
  cat(paste0("Ending process at :",Sys.time()),"\n")
  cat(paste0("Ending processing from date :",Sys.Date()),"\n")
  ending_time <- Sys.time()
  processing_time <- (ending_time - starting_time)
  cat("\nModel run time :", processing_time)
  sink() ## sink() for checking log of script
}