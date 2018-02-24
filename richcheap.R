library(XML)
library("xml2", lib.loc="/Library/Frameworks/R.framework/Versions/3.3/Resources/library")
library(lubridate)
library(RMySQL)
library(dbConnect)
library(httr)
library(RSelenium)
library(stringr)
library(dplyr)
library(ggplot2)
library(ggrepel)


db1 <- dbConnect(MySQL(), user="root", host = "localhost", db = "dbRates", password = "Newyork@1996")
iRates <- dbReadTable(db1, name = "rates")

iRates$date <- as.POSIXlt(iRates$date)

complete <- complete.cases(iRates$muniYield10Y, iRates$treasuryYield10Y)
iRatesComplete <- iRates[complete,]

ggplot(iRatesComplete, aes(x=iRatesComplete$treasuryYield10Y, y= iRatesComplete$muniYield10Y)) + 
  geom_point(aes(color=iRatesComplete$date)) + 
  stat_smooth(method="lm", col="red")+
  theme_minimal()+
  ggtitle(paste("MUNI/TREAS 10Y -- as of ", Sys.Date() ))+
  geom_hline(yintercept=iRatesComplete[nrow(iRatesComplete),"muniYield10Y"])+
  geom_vline(xintercept = iRatesComplete[nrow(iRatesComplete),"treasuryYield10Y"])


ggplot(iRatesComplete, aes(x=iRatesComplete$treasuryYield30Y, y= iRatesComplete$muniYield30Y)) + 
  geom_point(aes(color=iRatesComplete$date)) + 
  stat_smooth(method="lm", col="red")+
  theme_minimal()+
  ggtitle(paste("MUNI/TREAS 30Y -- as of ", Sys.Date() ))+
  geom_hline(yintercept=iRatesComplete[nrow(iRatesComplete),"muniYield30Y"])+
  geom_vline(xintercept = iRatesComplete[nrow(iRatesComplete),"treasuryYield30Y"])

dbDisconnect(db1)
