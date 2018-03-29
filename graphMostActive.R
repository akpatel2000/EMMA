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

Sys.setenv(TZ='EST')

setwd("~/Applications/EMMA")
system("docker run -d -p 4445:4444 selenium/standalone-chrome")
Sys.sleep(3)

# Start Selenium standalone server
remDr <- remoteDriver(remoteServerAddr = "localhost" 
                      , port = 4445L
                      , browserName = "chrome")

source("emmaTradeDetails.R")
source("emmaTradeHist.R")

emmaID <- c("ADFC0EC12B488FE0B0944581D3C5E164F", 
            "A73BBE55EFFE578D64B33BA855D90A681", 
            "A7590D68F04965D6E23EE37833A892B20",
            "A09034B52328596DC5E8A3C12947F2779",
            "A24EE32BDBDD6FADFF385A5D9FDBE5D41",
            "A06490F47926C1352C6BBC7896FA6283A")

remDr$open()
Sys.sleep(3)

u <- "https://emma.msrb.org/TradeData/MostActivelyTraded"
remDr$navigate(u)
Sys.sleep(2)
remDr$findElement("css", "#ctl00_mainContentArea_disclaimerContent_yesButton")$clickElement()
Sys.sleep(2)

for (i in 1:length(emmaID)) {
  # # Retrieve Page and extract html
  linkTag <- "http://emma.msrb.org/SecurityDetails/TradeActivity/"
  u <- paste(linkTag, emmaID[i], sep="")
  remDr$navigate(u)
  Sys.sleep(4)
  
  # Retrieve Page and extract html
  txt = remDr$getPageSource()
  
  emmaTradeRecord <- emmaTradeHist(txt)
  detailRecord <- emmaTradeDetails(txt)
  
  
  q1 <- sapply(split(emmaTradeRecord, emmaTradeRecord$emmaReportedDateTimeStamp), nrow)
  q1 <- log10(q1)
  
  par(mar=c(1,1,1,1))
  par(plt=c(0.1,1,0.1,.95))
  par(fig=c(0.05,0.95,0.35,0.95))
  
  boxplot(emmaTradeYield ~ emmaReportedDateTimeStamp, data = emmaTradeRecord, col = "grey", ylab = "Yield", 
          main=paste(substr(detailRecord$emmaDesc,1,20)," ", detailRecord$emmaInterestRate, " ", detailRecord$emmaMaturityDate))
  abline(h=detailRecord$emmaInitialOfferYield, col = "blue")
  
  f1 <- filter(emmaTradeRecord, emmaReportedDateTimeStamp == emmaTradeRecord$emmaReportedDateTimeStamp[1])
  m1 <- median(f1$emmaTradeYield)
  abline(h=m1, col = "green")
  
  par(new=TRUE)
  par(plt=c(0.1,1,0.1,1))
  par(fig=c(0.05,0.95,0.05,0.35))
  
  barplot(q1, col = "red", ylab = "log10(trades)")
  
}


# Close Selenium Server -- will close web browsing session
remDr$closeServer()
remDr$close()