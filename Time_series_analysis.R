require(nnfor)
require(TStools)
require(devtools)
library(thief)
library(forecast)
library(xts)
library(tseries)
library(ggplot2)
library(data.table)
library(lubridate)
library(MAPA)#http://kourentzes.com/forecasting/2014/04/19/multiple-aggregation-prediction-algorithm-mapa/

deb_loc <- "/home/partha/Documents/JKT-CPK-OLT1-HW5600"
setwd(deb_loc)
Sys.getenv('TZ')
Sys.setenv(TZ="UTC")

# tz = 'GMT+7'

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

user_growth_6M <- function(stPt,perc,shape = "Linear") {
  if(shape == "Linear"){
    users1 <- seq(from = stPt*(1+perc/30), by = (stPt*perc)/(30), length.out = 30 )
    users2 <- seq(from = users1[30]*(1+perc/30), by = (users1[30]*perc/30), length.out = 30)        
    users3 <- seq(from = users2[30]*(1+perc/30), by = (users2[30]*perc/30), length.out = 30)
    users4 <- seq(from = users3[30]*(1+perc/30), by = (users3[30]*perc/30), length.out = 30)
    users5 <- seq(from = users4[30]*(1+perc/30), by = (users4[30]*perc/30), length.out = 30)
    users6 <- seq(from = users5[30]*(1+perc/30), by = (users5[30]*perc/30), length.out = 30)
    users <- c( users1, users2,users3,users4, users5, users6)
  }
  return (users)
  
}

#install.packages('gtools')

library(gtools)

dates <- seq(as.Date("2018-06-15"), as.Date("2018-07-14"), "days")


growth <- c(0.05,-0.05,0.1,-0.1,0)
perms <- permutations(n=5,r=4,v=growth,repeats.allowed=T)

df <- read.csv("/home/partha/Documents/Model_Docs/new data.csv")
head(df)
na.omit(df)
lapply(df, class)
df$DT <- paste(substr(as.character(df$recorddate),5,17))
df$DT <- strptime(df$DT, format='%b %d %Y')
df$usage <- df$usage/(1024*1024*1024) #in gb
df$olt <- as.factor(df$olt) 
df$tp <- df$tp/1000
plot(df$usage,type="l")

OLT<- as.character(unique(df$olt))
#length(df$usage[df$olt==OLT[1]])
OLTts <- df[df$olt==OLT[4],]

x.reg<-  cbind(OLTts$GIG.HSI.30.Mbps_subscribers[1:218],OLTts$GIG.HSI.5.Mbps_subscribers[1:218],OLTts$GIG.HSI.15.Mbps_subscribers[1:218],OLTts$GIG.HSI.100.Mbps_subscribers[1:218])

usageTs <- xts(OLTts$usage, order.by = OLTts$DT, deltat=1/7)
plot(usageTs)

usageTs1 <- ts(coredata(usageTs), frequency =7)

y <- ts(usageTs1, frequency = 7, start=week(index(usageTs)[1]))
plot(y)

fit <- auto.arima(y,xreg=x.reg)

for (i in 1:nrow(perms)) {
  x.frc <- cbind(user_growth_1M(OLTts$GIG.HSI.30.Mbps_subscribers[218],perms[i,1]),user_growth_1M(OLTts$GIG.HSI.5.Mbps_subscribers[218],perms[i,2]),user_growth_1M(OLTts$GIG.HSI.15.Mbps_subscribers[218],perms[i,3]),user_growth_1M(OLTts$GIG.HSI.100.Mbps_subscribers[218],perms[i,4]))
  frc <- forecast(fit,xreg=x.frc,h=30,level=95)
  df <- data.frame(dates,data.frame(rep(perms[i,1],30),rep(perms[i,2],30),rep(perms[i,3],30),rep(perms[i,4],30)),data.frame(x.frc),frc$mean,frc$lower,frc$upper)
  colnames(df)<-c("dates","BP30_growth","BP05_growth","BP15_growth","BP100_growth","BP30_future","BP05_future","BP15_future","BP100_future","forecastUsage","forecast_low","forecast_high")
  name <- paste("1M","-BP30_",perms[i,1],"-BP05_",perms[i,2],"-BP15_",perms[i,3],"-BP100_",perms[i,4],".csv",sep="")
  write.csv(df,file=name)
}

arima.frc <- forecast(fit.arima,xreg=x.frc,h=30)
autoplot(arima.frc)

fit.tbats <- tbats(y,xreg=x.reg)
tbats.frc <- forecast(fit.tbats, h = 30,xreg=x.frc)
autoplot(tbats.frc)

fit.nnetar <- nnetar(y,xreg=x.reg)
nnetar.frc <- forecast(fit.nnetar, h = 30,xreg=x.frc)
autoplot(nnetar.frc)

accuracy(arima.frc$mean,y_test)
accuracy(tbats.frc$mean,y_test)
accuracy(nnetar.frc$mean,y_test)

plot(y, xlim = c(index(y)[1],1+index(y_test)[length(y_test)]), lwd =1)#plot original
lines(y_test, col ='black', lwd = 2, lty=2) #orinigal for backtesting
lines(arima.frc$mean, col = 'green', lwd = 2, lty=2)#auto.arima
lines(tbats.frc$mean, col = 'blue', lwd = 2, lty=1)#TBATS
lines(nnetar.frc$mean, col = 'purple', lwd = 2, lty=1)# NeuralNet

avg.frc <- rowMeans(cbind(as.numeric(arima.frc$mean),as.numeric(tbats.frc$mean),as.numeric(nnetar.frc$mean)))
accuracy(y_test,avg.frc)

modelAccuracy<- rbind(as.data.frame(accuracy(y_test, arima.frc$mean), row.names = 'arima.frc'),as.data.frame(accuracy(y_test, tbats.frc$mean), row.names = 'tbats.frc'),as.data.frame(accuracy(y_test, nnetar.frc$mean), row.names = 'nnetar.frc'))
row.names(modelAccuracy[which(modelAccuracy$MAPE == min(modelAccuracy$MAPE)),])
(modelAccuracy[which(modelAccuracy$MAPE == min(modelAccuracy$MAPE)),])
