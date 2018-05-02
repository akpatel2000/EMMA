############# Set run time environment
library(XML)
library(RMySQL)
library(dbConnect)
library(RSelenium)
library(lubridate)
library(stringr)
library(dplyr)

Sys.setenv(TZ='EST')
setwd("~/Applications/EMMA")

############# Define functions to be used in this function
scrapeCompPage <- function(tt) {
    x <- as.character(tt$tblCompetitiveUpcomingIssues$State) 
    x2 <- which(nchar(x) > 2)
    y <- mdy(as.character(tt$tblCompetitiveUpcomingIssues$State[x2]))
    x3 <- tt$tblCompetitiveUpcomingIssues$`Principal Amount ($)`
    x3 <- str_replace_all(x3, ",", "")
    x <- data.frame(date = as.Date(y[1]),
                    state = as.character(x), 
                    issuer = as.character(tt$tblCompetitiveUpcomingIssues$`Issuer Name/Issue Description`),
                    par = as.numeric(x3),
                    time = as.character(tt$tblCompetitiveUpcomingIssues$`Time of Sale (ET)`),
                    bank = as.character(tt$tblCompetitiveUpcomingIssues$`Bank Qualified`),
                    tax = as.character(tt$tblCompetitiveUpcomingIssues$`Tax Status`),
                    stringsAsFactors = FALSE)
    
    # x is dataframe containing rows and dates
    # x2 contains the dates that need to be replicated in their own columns
    # y contains the dates that need to be replicated
    for (i in 1:(length(x2)-1)) {
        x[(x2[i]):(x2[i+1]),1] <- y[i]
    }
    x[(x2[i+1]:nrow(x)),1] <- y[i+1]
    
    # delete the unnecessary date field as identified as rows in variable x2
    x <- x[-x2,]
    row.names(x) <- 1:nrow(x)
    x$bank[grep("[^(Yes|No)]", x$bank)] <- NA
    x <- x %>% mutate(asof = date())
    return(x)
}

scrapeNegoPage <- function(tt) {
    x <- as.character(tt$tblNegotiatedUpcomingIssues$State)
    x2 <- which(nchar(x) > 2)
    x4 <- grep("([Dd][Aa][Yy].[Tt][Oo])+", x)
    y <- mdy(as.character(tt$tblNegotiatedUpcomingIssues$State[x2]), quiet = TRUE)
    x3 <- tt$tblNegotiatedUpcomingIssues$`Principal Amount ($)`
    x3 <- str_replace_all(x3, ",", "")
    x <- data.frame(date = as.Date(y[1]),
                    state = as.character(x), 
                    issuer = as.character(tt$tblNegotiatedUpcomingIssues$`Issuer Name/Issue Description`),
                    par = as.numeric(x3),
                    bank = as.character(tt$tblNegotiatedUpcomingIssues$`Bank Qualified`),
                    tax = as.character(tt$tblNegotiatedUpcomingIssues$`Tax Status`),
                    dayToDay = as.logical(FALSE),
                    stringsAsFactors = FALSE)
    
    # the data is not tidy.  Some rows contain dates only and not other data
    # other rows contain data and now dates.  We want to collapse this so that
    # each data row contains the date for which it pertains.
    for (i in 1:length(x2)) {
        x[(x2[i]):(nrow(x)),1] <- y[i]
    }
    
    # Some deals are classified as day to day.  They have not date = NA
    # for those day to day deals we want to turn on a flag to true to further identify
    if (x4 > 0) {
        x$dayToDay[which(is.na(x$date))] <- TRUE
    }
    
    # delete the unnecessary date field as identified as rows in variable x2
    x <- x[-x2,]
    row.names(x) <- 1:nrow(x)
    x$bank[grep("[^(Yes|No)]", x$bank)] <- NA
    x <- x %>% mutate(asof = date())
    return(x)
}

###################################################
###################################################
############# Begining of Program #################
###################################################
###################################################

# Start Selenium standalone server
system("docker run -d -p 4445:4444 selenium/standalone-chrome")
Sys.sleep(3)
remDr <- remoteDriver(remoteServerAddr = "localhost" 
                      , port = 4445L
                      , browserName = "chrome")
remDr$open()
# Navigate to New Issue page -- handle cookie request
remDr$navigate("https://emma.msrb.org/ToolsAndResources/NewIssueCalendar")
remDr$findElement("css", "#ctl00_mainContentArea_disclaimerContent_yesButton")$clickElement()
Sys.sleep(5)

# It defaults to the competitive calendar, so let's scrape that first --
# There are multiple pages so let's start with the first
# we will click on next to move along
txt = remDr$getPageSource()
doc = htmlParse(txt, asText = TRUE)
tt <- readHTMLTable(doc, header = TRUE)
emmaCompCal <- scrapeCompPage(tt)
ttPrev <- as.character(tt$tblCompetitiveUpcomingIssues$State[1])

# repeat this for multiple pages -- break when no more pages
repeat {
    remDr$findElement("css", "#tblCompetitiveUpcomingIssues_next")$clickElement()
    txt = remDr$getPageSource()
    doc = htmlParse(txt, asText = TRUE)
    tt <- readHTMLTable(doc, header = TRUE)
    ## You keep repeating this loop until clicking Next returns same page
    if (as.character(tt$tblCompetitiveUpcomingIssues$State[1]) == ttPrev) {
        break
    } 
    emmaCompCal <- rbind(emmaCompCal, scrapeCompPage(tt))
    ttPrev <- as.character(tt$tblCompetitiveUpcomingIssues$State[1])
}



# Now scrape the negotiated calendar
remDr$findElement("css", "#negotiatedSalesLink > a")$clickElement()
Sys.sleep(5)
txt = remDr$getPageSource()
doc = htmlParse(txt, asText = TRUE)
tt <- readHTMLTable(doc, header = TRUE)
emmaNegoCal <- scrapeNegoPage(tt)
ttPrev <- as.character(tt$tblNegotiatedUpcomingIssues$State[1])

# repeat this for multiple pages -- break when no more pages
repeat {
    remDr$findElement("css", "#tblNegotiatedUpcomingIssues_next")$clickElement()
    txt = remDr$getPageSource()
    doc = htmlParse(txt, asText = TRUE)
    tt <- readHTMLTable(doc, header = TRUE)
    ## You keep repeating this loop until clicking Next returns same page
    if (as.character(tt$tblNegotiatedUpcomingIssues$State[1]) == ttPrev) {
        break
    } 
    emmaNegoCal <- rbind(emmaNegoCal, scrapeNegoPage(tt))
    ttPrev <- as.character(tt$tblNegotiatedUpcomingIssues$State[1])
}


# Write record to database
if ((nrow(emmaCompCal) > 0) & (nrow(emmaNegoCal) > 0)) {
    db1 <- dbConnect(MySQL(), user = "root", host = "localhost", db = "dbRates", password = "Newyork@1996")
    dbWriteTable(db1, emmaCompCal, name = "emmaCompCal", append = TRUE, row.names = FALSE)
    dbWriteTable(db1, emmaNegoCal, name = "emmaNegoCal", append = TRUE, row.names = FALSE)
    print("All Good -- Calendar Written")
    dbDisconnect(db1)    
} else {
    print("Error -- Did not write Calendar")
}

# Shutdown selenium
remDr$close()
system("docker stop $(docker ps -q)")