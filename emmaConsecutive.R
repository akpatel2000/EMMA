## Graphs how many of the 100 EMMA most active trade and then trade again 
## the next day
library(XML)
library(xml2)
library(lubridate)
library(RMySQL)
library(dbConnect)
library(httr)
library(RSelenium)
library(stringr)
library(dplyr)
library(tibble)

# Some house keeping
Sys.setenv(TZ='EST')
setwd("~/Applications/EMMA")

# Open/Close database and read emmaMostActive
db1 <- dbConnect(MySQL(), user = "root", host = "localhost", db = "dbRates", password = "Newyork@1996")
emma100 <- dbReadTable(db1, name = "emmaMostActive")
dbDisconnect(db1)

# Need to convert to date type
emma100$asof <- as.Date(emma100$asof)
emma100$emmaMaturity <- as.Date(emma100$emmaMaturity)

# Let's do this for all the dates on file
# Loop thru for each date
# find all the unique EMMAIDs for that date and the next day
# graph the number EMMAIDs that match both days
x <- unique(emma100$asof)
z <- integer()
nDF <- data_frame(asof = as.Date(character()), tradeNext = integer())
for (i in 1:(length(x)-1)) {
    y1 <- subset(emma100, asof == x[i])
    y1 <- unique(y1$emmaID)
    y1 <- data_frame(emmaID = y1)
    y2 <- subset(emma100, asof == x[i+1])
    y2 <- unique(y2$emmaID)
    y2 <- data_frame(emmaID = y2)
    z <- nrow(inner_join(y1, y2, by = "emmaID"))
    print(i)
    nDF <- rbind(nDF, data_frame(asof = x[i], consecTrade = z))
}

ggplot(data = nDF, aes(x= asof, y= consecTrade)) + 
    geom_point() + 
    geom_hline(aes(yintercept = mean(nDF$consecTrade)), color = "red") +
    geom_hline(aes(yintercept = (mean(nDF$consecTrade) + sd(nDF$consecTrade))), color = "blue") +
    geom_hline(aes(yintercept = (mean(nDF$consecTrade) - sd(nDF$consecTrade))), color = "blue") 