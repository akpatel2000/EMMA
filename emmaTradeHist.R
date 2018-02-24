emmaTradeHist <- function (txt) {
    #############################################################################################
    #   This function requires Selenium stand-alone server initiated prior to calling this function.
    #   This function is called using emmaID (character string)
    #   which is used to navigate to the page and extract all market data on the emmaID, which
    #   is returned to the calling function as a dataframe.
    #############################################################################################

    doc <- htmlParse(txt, asText = TRUE)
    
    # # # Extract descriptive data elements on page
    # emmaDesc <- xpathApply(doc, "//a[@class='issueDataLink']", xmlValue)
    # securityDetails1 <- xpathApply(doc,
    #                                "/html/body//div[@class='value']/span[1]",
    #                                xmlValue)
    # securityDetails2 <- xpathApply(doc,
    #                                "/html/body//div[@class='value']/span[2]",
    #                                xmlValue)
    # if (grepl("Dated", securityDetails1[[1]], ignore.case = TRUE)) {
    #     emmaDatedDate <- mdy(securityDetails2[[1]])
    # } else {
    #     return ("error Dated Date not found")
    # }
    # 
    # if (grepl("Maturity", securityDetails1[[2]], ignore.case = TRUE)) {
    #     emmaMaturityDate <- mdy(securityDetails2[[2]])
    # } else {
    #     return ("error Maturity Date not found")
    # }
    # 
    # if (grepl("Coupon", securityDetails1[[3]], ignore.case = TRUE)) {
    #     emmaInterestRate <- gsub("%","", securityDetails2[[3]])
    #     emmaInterestRate <- as.double(emmaInterestRate)
    # } else {
    #     return ("error Interest Rate not found")
    # }
    # 
    # if (grepl("Principal", securityDetails1[[4]], ignore.case = TRUE)) {
    #     emmaPrincipalAmount <- gsub(",", "", securityDetails2[[4]])
    #     emmaPrincipalAmount <- str_sub(emmaPrincipalAmount, start = 2, end = -1L)
    #     emmaPrincipalAmount <- as.integer(emmaPrincipalAmount)
    # } else {
    #     return ("error Principal Amount not found")
    # }
    # 
    # if (grepl("Offering Price", securityDetails1[[5]], ignore.case = TRUE)) {
    #     emmaInitialOfferPrice <- as.double(securityDetails2[[5]])
    # } else {
    #     return ("error Offering Price not found")
    # }
    # 
    # ## There are instances where Initial Offering Yield is not available so don't flag as error
    # if (length(securityDetails1) > 5) {
    #     if (grepl("Offering Yield", securityDetails1[[6]], ignore.case = TRUE)) {
    #         emmaInitialOfferYield <- as.double(securityDetails2[[6]])
    #     }
    # } else {
    #     emmaInitialOfferYield <- NA
    # }
    # 
    # emmaDescriptiveRecord <- data.frame(emmaID, emmaDatedDate, emmaMaturityDate, emmaInterestRate,
    #                                     emmaPrincipalAmount, emmaInitialOfferPrice, emmaInitialOfferYield)
    # 
    # # Write record to database
    # db1 <- dbConnect(MySQL(), user = "root", host = "localhost", db = "dbRates", password = "Newyork@1996")
    # dbWriteTable(db1, emmaDescriptiveRecord, name = "emmaDescriptive", 
    #              row.names = FALSE, append = TRUE)
    # dbDisconnect(db1)
    
    # Exract trade elements on page by finding data within the square brackets
    loc <- str_locate(txt, "var tradeData =")
    rawTradeData <- str_sub(txt, loc[2], end = -1L)
    loc <- str_locate(rawTradeData, "\\[")
    rawTradeData <- str_sub(rawTradeData, loc[2]+2, end = -2L)
    loc <- str_locate(rawTradeData, "]")
    rawTradeData <- str_sub(rawTradeData, 2, loc[2]-2)
    
    # Remove unused elements
    rawTradeData <- gsub("\"", "", rawTradeData)
    rawTradeData <- gsub("\\{", "", rawTradeData)
    rawTradeData <- gsub("TDT:", "", rawTradeData)
    rawTradeData <- gsub("TD:", "", rawTradeData)
    rawTradeData <- gsub("PX:", "", rawTradeData)
    rawTradeData <- gsub("YX:", "", rawTradeData)
    rawTradeData <- gsub("TA:", "", rawTradeData)
    rawTradeData <- gsub("TT:", "", rawTradeData)
    rawTradeData <- gsub("WI:", "", rawTradeData)
    rawTradeData <- gsub("BI:", "", rawTradeData)
    rawTradeData <- gsub("PI:", "", rawTradeData)
    rawTradeData <- gsub("LI:", "", rawTradeData)
    rawTradeData <- gsub("AT:", "", rawTradeData)
    rawTradeData <- gsub( "NT:", "", rawTradeData)
    rawTradeData <- gsub( "UI:", "", rawTradeData)
    rawTradeData <- gsub( "STL:", "", rawTradeData)
    rawTradeData <- gsub( "AI:", "", rawTradeData)
    rawTradeData <- gsub( "CT:", "", rawTradeData)
    rawTradeData <- gsub( "CDP:", "", rawTradeData)
    rawTradeData <- gsub( "null", "NA", rawTradeData)
    # Split into individual trades by splitting on },
    r <- str_split(rawTradeData, "\\},")
    # Split into individual fields by splitting on comma
    r <- lapply(r, function(x) str_split(x, ","))
    # get length of list r
    x <- sapply(r, length)
    
    # Build and tidy individual fields into dataframe emmaTradeRecord
    emmaTradeRecord <- data.frame()
    
    ## Define the individual fields of the record as type list
    f0 <- list()
    f1 <- list()
    f2 <- list()
    f3 <- list()
    f4 <- list()
    f5 <- list()
    f6 <- list()
    f7 <- list()
    f8 <- list()
    f9 <- list()
    f10 <- list()
    f11 <- list()
    f12 <- list()
    f13 <- list()
    f14 <- list()
    f15 <- list()
    f16 <- list()
    f17 <- list()
    f18 <- list()
    
    ## Extract data and place into defined field
    y <- 1
    while (y<=x) {
        f0[y] <- emmaID
        f1[y] <- r[[1]][[y]][1]
        f2[y] <- r[[1]][[y]][2]
        f3[y] <- r[[1]][[y]][3]
        f4[y] <- r[[1]][[y]][4]
        #   need to check if trade amount is MM+ (short for block)
        locate <- str_locate(r[[1]][[y]][5], "MM+") 
        if (is.na(locate[1])) {
            f5[y] <- r[[1]][[y]][5]
        } else {
            f5[y] <- "5000000.1"
        }
        f6[y] <- (r[[1]][[y]][6])
        f7[y] <- (r[[1]][[y]][7])
        f8[y] <- (r[[1]][[y]][8])
        f9[y] <- (r[[1]][[y]][9])
        f10[y] <- (r[[1]][[y]][10])
        f11[y] <- (r[[1]][[y]][11])
        f12[y] <- (r[[1]][[y]][12])
        f13[y] <- (r[[1]][[y]][13])
        f14[y] <- (r[[1]][[y]][14])
        f15[y] <- (r[[1]][[y]][15])
        f16[y] <- (r[[1]][[y]][16])
        
        locate <- str_locate(r[[1]][[y]][17], "<BR")
        if (is.na(locate[1])) {
            f17[y] <- NA
            f18[y] <- NA
        } else {
            f17[y] <- str_trim(str_sub(r[[1]][[y]][17], start = 1L, end = locate[1]-1), side = "both")
            locate <- str_locate(r[[1]][[y]][17], "@")
            f18[y] <- str_trim(str_sub(r[[1]][[y]][17], start = locate[2]+1, end = -1L), side = "both")
        }
        
        y = y + 1
    }
    
    f0 <- as.character(f0)
    f1 <- ymd_hms(f1, tz="America/New_York")
    f2 <- ymd_hms(f2, tz="America/New_York")
    f3 <- as.double(f3)
    f4 <- as.double(f4)
    f5 <- as.double(f5)
    f6 <- as.character(f6)
    f7 <- as.character(f7)
    f8 <- as.character(f8)
    f9 <- as.character(f9)
    f10 <- as.character(f10)
    f11 <- as.character(f11)
    f12 <- as.character(f12)
    f13 <- as.character(f13)
    f14 <- str_sub(f14, start = 1L, end = 10)
    f14 <- as.Date(f14)
    f15 <- as.character(f15)
    f16 <- as.character(f16)
    f17 <- mdy(f17, tz = "America/New_York")
    f18 <- as.double(f18)
    
    
    emmaTradeRecord <- data.frame(f0,f1,f2,f3,f4,f5,f6,f7,f8,f9,f10,f11,f12,f13,f14,f15,f16,f17,f18)
    
    headers <- c("emmaID", "emmaTradeDateTimeStamp",
                 "emmaReportedDateTimeStamp", "emmaTradePrice", "emmaTradeYield",
                 "emmaTradeAmount", "emmaTradeType", "r7", "r8", "r9",
                 "r10", "r11", "r12", "r13", "emmaSettleDate",
                 "r15", "r16", "emmaWorkoutDate",
                 "emmaWorkoutPrice")
    
    names(emmaTradeRecord) <- headers
    
    
    
    return(emmaTradeRecord)
    
    # db1 <- dbConnect(MySQL(), user = "root", host = "localhost", db = "dbRates", password = "Newyork@1996")    # t1 <- dbGetQuery(db1, "SELECT emmaTradeDateTimeStamp from emmaTrades limit 1;")
    # # t1 <- as.POSIXct(t1$emmaTradeDateTimeStamp)
    # # s1 <- subset(emmaTradeRecord, emmaTradeRecord$emmaTradeDateTimeStamp > t1)
    # 
    # # Write record to database
    # dbWriteTable(db1, emmaTradeRecord, name = "emmaTrades", 
    #              row.names = FALSE, append = TRUE)
    # dbDisconnect(db1)
    
}