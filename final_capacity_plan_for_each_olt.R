rm(list=ls()) #Removes all items in Environment!
starting_time <- Sys.time()
######### Install require Library and Import #############
install.packages(c('data.table','dplyr','tidyverse','bit64','chron','xts','forecast','TSA','gtools','mongolite'))
## Load require Library
library(data.table)
library(dplyr)
library(tidyverse)
library(bit64)
library(chron)
library(xts)
library(forecast)
library(TSA)
## Using Mongolite
library(mongolite)
mongoUrl <- "mongodb://spectauser:spectaDb#011@10.0.0.14:27017/spectaData" ## Connect with Database
#### Create Input Collection ##
# change col to your collection
col_site <- "AAA_data_forecast_olt_sylhet"
#create connection (con)
con_site <- mongo(collection = col_site, url = mongoUrl, db="spectaData")
#### create Output Collection
col_site_test <- "test_forecast_sylhet"
## create connection (con)
con_site_test <- mongo(collection = col_site_test, url = mongoUrl, db="spectaData")
# count how many records (fyi this is just a test)
# con$count('{}')
## Check Data and convert Dataframe
con_site$count('{}')
con_site$find(limit = 5) ## Check data
site <- con_site$find()
# head(site)
# colnames(site)
# dim(site)
# length(site)
data <- site[,c('recorddate','olt','usage','uniques')]
# head(data)
# tail(data)
# dim(data)
# length(data)
# colnames(data)
# lapply(data, class)
#str(data)
colSums(is.na(data))
data <- na.omit(data)
## converted date time
data$recorddate <- as.Date(data$recorddate, format = "%A %b %d %Y")
# head(data$recorddate)
max(data$recorddate)
min(data$recorddate)
## converted usage in GB
data$usage <- data$usage/(1024*1024*1024) #in gb
# head(data$usage)
data$olt <- as.factor(data$olt) 
## Business Logic of User Growth
user_growth_1M <- function(stPt,perc,shape = "Linear")
{
  if(shape == "Linear"){
    users <- seq(from = stPt*(1+perc/30), by = (stPt*perc)/(30), length.out = 30 )
  }
  return(users)
}
user_growth_3M <- function(stPt,perc,shape = "Linear") {
  if(shape == "Linear"){
    users1 <- seq(from = stPt*(1+perc/30), by = (stPt*perc)/(30), length.out = 30 )
    users2 <- seq(from = users1[30]*(1+perc/30), by = (users1[30]*perc/30), length.out = 30)        
    users3 <- seq(from = users2[30]*(1+perc/30), by = (users2[30]*perc/30), length.out = 30)
    users <- c( users1, users2,users3)
  }
  return (users)
}
## User growth
library(gtools)
growth <- c(0.05,-0.05,0.1,-0.1,0)
perms <- permutations(n=5,r=1,v=growth,repeats.allowed=T)
## Remove UNKNOWN and SELECT olt
# data <- data[data$olt!='UNKNOWN' & data$olt!='SELECT', ]
data <- data[data$olt!='UNKNOWN',]
## check unique olt
OLT <- as.character(unique(data$olt))
length(OLT)
df <- data$usage[data$olt==OLT[11]]
length(df)
########### Ecah OLT data manipulation, missing check, outlier check and build model and forecast ################
for(olt_name in 1:length(OLT)){
  start_time <- Sys.time()
  # olt <- function(olt_name) {
  OLTts <- data[data$olt==OLT[olt_name],]
  ## check and remove duplicate data over olt
  OLTts = distinct(OLTts, recorddate, .keep_all= T)
  len <- length(OLTts$recorddate)
  count = 0
  if(len >= 365){
    count = count + olt_name
    cat("\n\n#### Output of OLT :",OLT[olt_name])
    cat("\nolt name :", OLT[olt_name])
    cat("\nActual length of olt :", length(OLTts$recorddate))
    ## Check the missing sequence of Date each Olt
    date_range <- seq(min(OLTts$recorddate), max(OLTts$recorddate), by = "day")
    # print(OLT[2])
    date_range[!date_range %in% OLTts$recorddate]
    # print(date_range[!date_range %in% OLTts$recorddate])
    OLTts <- OLTts %>%
      mutate(recorddate = as.Date(recorddate)) %>%
      complete(recorddate = seq.Date(min(recorddate), max(recorddate), by="day")) 
    # fill('uniques','usage', 'olt')
    colSums(is.na(OLTts))
    cat("\nAfter manipulating olt, length of olt :",length(OLTts$recorddate))
    ## Replacing NA by mean, median 
    ## For Usage
    # OLTts$usage=ifelse(is.na(OLTts$usage),mean(OLTts$usage,na.rm=T),OLTts$usage)  # By mean 
    OLTts$usage=ifelse(is.na(OLTts$usage),median(OLTts$usage,na.rm=T),OLTts$usage) # By median
    ## For Uniques
    # OLTts$uniques=ifelse(is.na(OLTts$uniques),mean(OLTts$uniques,na.rm=T),OLTts$uniques)  # By mean
    OLTts$uniques=ifelse(is.na(OLTts$uniques),median(OLTts$uniques,na.rm=T),OLTts$uniques) # By median
    ## check missing data and fill by mean, median and quantile
    colSums(is.na(OLTts))
    ###### Check Outliers and substitute by quantile ########
    capOutlier <- function(x){
      qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
      caps <- quantile(x, probs=c(.05, .95), na.rm = T)
      H <- 1.5 * IQR(x, na.rm = T)
      x[x < (qnt[1] - H)] <- caps[1]
      x[x > (qnt[2] + H)] <- caps[2]
      return(x)
    }
    OLTts$usage=capOutlier(OLTts$usage)
    OLTts$uniques=capOutlier(OLTts$uniques)
    ###### Univariate Outliers and visulize outliers ############# 
    ## Check for Usage
    # outlier_values_usage <- boxplot.stats(OLTts$usage)$out  # outlier values.
    # boxplot(OLTts$usage, main="Usage(GB)", boxwex=0.1)
    # mtext(paste("Outliers: ", paste(outlier_values_usage, collapse=", ")), cex=0.6)
    # # print(outlier_values_usage)
    ## check for Uniques
    # outlier_values_uniques <- boxplot.stats(OLTts$uniques)$out  # outlier values.
    # boxplot(OLTts$uniques, main="Users", boxwex=0.1)
    # mtext(paste("Outliers: ", paste(outlier_values_uniques, collapse=", ")), cex=0.6)
    # print(outlier_values_uniques)
    ###### Outliers Replace by Median ##########
    outlier <- function(x) {
      x[x < quantile(x,0.25) - 1.5 * IQR(x) | x > quantile(x,0.75) + 1.5 * IQR(x)] <- median(x)
      x
    }
    ## Outliers replace by median
    OLTts$usage = outlier(OLTts$usage)
    OLTts$uniques = outlier(OLTts$uniques)
    ## create TS objective
    usageTs <- xts(OLTts$usage, order.by = OLTts$recorddate, deltat=1/7)
    length(usageTs)
    usageTs1 <- ts(coredata(usageTs))
    length(usageTs1)
    y <- ts(usageTs1, frequency = 7, start = (as.numeric(format((OLTts$recorddate)[1], "%j")))/7)
    len_uniques <- length(OLTts$uniques)
    x.reg <- cbind(OLTts$uniques[1:len_uniques])
    length(x.reg)
    ##### Modelling Part ###################
    # fit.arima <- auto.arima(y, xreg = x.reg)
    fit.arima <- auto.arima(y, xreg=x.reg, d = NA, D = NA, max.p = 10, max.P = 5,
                            max.q = 10,max.Q = 5, max.d = 2,max.D = 1,max.order = 10,
                            start.p = 1,start.q = 1, start.P = 1,start.Q = 1,
                            stationary = FALSE,seasonal = TRUE, stepwise = TRUE)
    ###### Forecast for one month ############
    for (i in 1:nrow(perms)) {
      x.frc <- cbind(user_growth_1M(OLTts$uniques[(len_uniques)],perms[i,1]))
      x.frc <- ceiling(x.frc)  ## convert Integr with upper case
      x.frc <- as.integer(x.frc)  ## convert Integer with lower case
      arima.frc <- forecast(fit.arima,xreg=x.frc,h=30,level = 95)
      maxday <- max(OLTts$recorddate)
      frcstdate <- seq(maxday+1, by = "day", length.out = 30)
      frcstdate <- as.Date(frcstdate)
      frcstdate <- as.POSIXct(frcstdate)
      arima.frc$mean <- arima.frc$mean*(1024*1024*1024)    ## convert to Byte
      arima.frc$mean <- as.numeric(arima.frc$mean)
      arima.frc$mean <- as.integer64(arima.frc$mean)
      arima.frc$lower <- arima.frc$lower*(1024*1024*1024)
      arima.frc$lower <- as.numeric(arima.frc$lower)
      arima.frc$lower <- as.integer64(arima.frc$lower)
      arima.frc$upper <- arima.frc$upper*(1024*1024*1024)
      arima.frc$upper <- as.numeric(arima.frc$upper)
      arima.frc$upper <- as.integer64(arima.frc$upper)
      df <- data.frame(frcstdate,data.frame(rep(perms[i,1],30),data.frame(x.frc),arima.frc$mean,arima.frc$lower,arima.frc$upper,'1M',OLT[olt_name],as.POSIXct(Sys.Date())))
      colnames(df)<-c("frcstdate","users_growth","users","usage","usage_low","usage_high","period","olt","recorddate")
      lapply(df, class)
      ## Insert output into DataBase
      con_site_test$insert(df)
    }
    ########### Forecast for three months ############
    for (j in 1:nrow(perms)) {
      x.frc <- cbind(user_growth_3M(OLTts$uniques[len_uniques],perms[j,1]))
      x.frc <- ceiling(x.frc)  ## convert Integr with upper case
      x.frc <- as.integer(x.frc)  ## convert Integer with lower case
      arima.frc <- forecast(fit.arima,xreg=x.frc,h=90,level = 95) ## forecast 
      maxday <- max(OLTts$recorddate)
      frcstdate <- seq(maxday+1, by = "day", length.out = 90)
      frcstdate <- as.POSIXct(frcstdate)
      arima.frc$mean <- arima.frc$mean*(1024*1024*1024)    ## convert to Byte
      arima.frc$mean <- as.numeric(arima.frc$mean)
      arima.frc$mean <- as.integer64(arima.frc$mean)
      arima.frc$lower <- arima.frc$lower*(1024*1024*1024)
      arima.frc$lower <- as.numeric(arima.frc$lower)
      arima.frc$lower <- as.integer64(arima.frc$lower)
      arima.frc$upper <- arima.frc$upper*(1024*1024*1024)
      arima.frc$upper <- as.numeric(arima.frc$upper)
      arima.frc$upper <- as.integer64(arima.frc$upper)
      df <- data.frame(frcstdate,data.frame(rep(perms[j,1],90),data.frame(x.frc),arima.frc$mean,arima.frc$lower,arima.frc$upper,'3M',OLT[olt_name],as.POSIXct(Sys.Date())))
      colnames(df)<-c("frcstdate","users_growth","users","usage","usage_low","usage_high","period","olt","recorddate")
      lapply(df, class)
      ## Insert output into DataBase
      con_site_test$insert(df)
    }
    cat("\nTotal number of olt more than 365 days :", count)
    cat("\nAccuracy of model :",accuracy(fit.arima))
    end_time <- Sys.time()
    processing_time <- end_time - start_time
    cat("\nOLT run time :", processing_time)
  }
}
cat("\nNumber of record count :",con_site_test$count('{}'))  # Count all records
ending_time <- Sys.time()
run_time <- ending_time - starting_time
cat("\n\n\noverall prosessing time is :",run_time)


