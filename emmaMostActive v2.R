library(XML)
library(RMySQL)
library(dbConnect)
library(RSelenium)
library(lubridate)
library(stringr)

Sys.setenv(TZ='EST')

setwd("~/Applications/EMMA")
system("docker run -d -p 4445:4444 selenium/standalone-chrome")
Sys.sleep(3)

# Start Selenium standalone server
remDr <- remoteDriver(remoteServerAddr = "localhost" 
                      , port = 4445L
                      , browserName = "chrome")
remDr$open()
remDr$navigate("http://emma.msrb.org/TradeData/MostActivelyTraded")
remDr$findElement("css", "#ctl00_mainContentArea_disclaimerContent_yesButton")$clickElement()
Sys.sleep(3)
remDr$findElement("css", "#lvTradeData_length > label > select")$clickElement()
remDr$findElement("css", "#lvTradeData_length > label > select > option:nth-child(4)")$clickElement()
txt = remDr$getPageSource()



# Parse document using htmlParse and readHTMLTable
doc = htmlParse(txt, asText = TRUE)

tt <- readHTMLTable(doc, stringasFactors = FALSE) %>% as.data.frame()

# Extract data fields
emmaDesc <- str_trim(tt$lvTradeData.Security.Description.., side = "both") %>% as.character()
emmaMaturity <- str_trim(tt$lvTradeData.Maturity.Date, side = "both") %>% mdy()
emmaInterestRate <- gsub("-","", tt$lvTradeData.Coupon...) %>% as.numeric()

price <- str_trim(tt$lvTradeData.High.Low.Price...., side = "both")
dash <- str_locate(price, "/")
end <- nchar(price)
emmaPriceHigh <- substr(price, 1, dash-1) %>% as.numeric()
emmaPriceLow <- substr(price, dash+1, end) %>% as.numeric()

yield <- str_trim(tt$lvTradeData.High.Low.Yield...., "both")
yield <- gsub("-","", yield)
dash <- str_locate(yield, "/")
end <- nchar(yield)
emmaYieldHigh <- substr(yield, 1, dash-1) %>% as.numeric()
emmaYieldLow <- substr(yield, dash+1, end) %>% as.numeric()

emmaTradeCount <- str_trim(tt$lvTradeData.Trade.Count, side = "both") %>% as.numeric()
emmaTradeAmount <- str_trim(tt$lvTradeData.Total.Trade.Amount...., side = "both")
emmaTradeAmount <- gsub(",", "", emmaTradeAmount)
emmaTradeBlockSize <- grepl("\\+", emmaTradeAmount) %>% as.numeric()
emmaTradeAmount <- gsub("\\+", "", emmaTradeAmount)
emmaTradeAmount <- str_trim(emmaTradeAmount, side="both") %>% as.numeric()

# Get links that have emmaID
attrCUSIP <- xpathApply(doc, "//tbody//a[@href]", xmlAttrs)
links <- sapply(attrCUSIP, function(x) x[[3]])

# Extract emmaID from link
emmaID <- ""
x = 1
for (i in 1:(length(links)/3)) {
    emmaID[i] <- substr(links[x], (str_locate(links[1], "="))+1, nchar(links[x]))
    x = x + 3
}

# Construct Dataframe
activelyTradedRecord <- NULL
activelyTradedRecord <- data.frame(asof = Sys.Date(),
                                   emmaID,
                                   emmaDesc,
                                   emmaInterestRate,
                                   emmaMaturity,
                                   emmaPriceHigh,
                                   emmaPriceLow,
                                   emmaYieldHigh,
                                   emmaYieldLow,
                                   emmaTradeCount,
                                   emmaTradeAmount,
                                   emmaTradeBlockSize)


# Write record to database
if (nrow(activelyTradedRecord) == 100) {
    db1 <- dbConnect(MySQL(), user = "root", host = "localhost", db = "dbRates", password = "Newyork@1996")
    dbWriteTable(db1, activelyTradedRecord, name = "emmaMostActive", append = TRUE, row.names = FALSE)
    print("All Good -- Record written to database")
    dbDisconnect(db1)    
} else {
    print("Error -- Did not write record")
}


# Close connection
remDr$close()
system("docker stop $(docker ps -q)")