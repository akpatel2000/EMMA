emmaTradeDetails <- function (txt) {
  #############################################################################################
  #   This function requires Selenium stand-alone server initiated prior to calling this function.
  #   This function is called using emmaID (character string)
  #   which is used to navigate to the page and extract all market data on the emmaID, which
  #   is returned to the calling function as a dataframe.
  #############################################################################################

    

    doc <- htmlParse(txt, asText = TRUE)
    
    # # Extract descriptive data elements on page
    emmaDesc <- xpathApply(doc, "//a[@class='issueDataLink']", xmlValue)
    emmaDesc <- as.character(emmaDesc)
    
    securityDetails1 <- xpathApply(doc,
                                  "/html/body//div[@class='value']/span[1]",
                                  xmlValue)
    securityDetails2 <- xpathApply(doc,
                                  "/html/body//div[@class='value']/span[2]",
                                  xmlValue)
    
    if (grepl("Dated", securityDetails1[[1]], ignore.case = TRUE)) {
      emmaDatedDate <- mdy(securityDetails2[[1]])
    } else {
      return ("error Dated Date not found")
    }

    if (grepl("Maturity", securityDetails1[[2]], ignore.case = TRUE)) {
      emmaMaturityDate <- mdy(securityDetails2[[2]])
    } else {
      return ("error Maturity Date not found")
    }

    if (grepl("Coupon", securityDetails1[[3]], ignore.case = TRUE)) {
      emmaInterestRate <- gsub("%","", securityDetails2[[3]])
      emmaInterestRate <- as.double(emmaInterestRate)
    } else {
      return ("error Interest Rate not found")
    }

    if (grepl("Principal", securityDetails1[[4]], ignore.case = TRUE)) {
      emmaPrincipalAmount <- gsub(",", "", securityDetails2[[4]])
      emmaPrincipalAmount <- str_sub(emmaPrincipalAmount, start = 2, end = -1L)
      emmaPrincipalAmount <- as.integer(emmaPrincipalAmount)
    } else {
      return ("error Principal Amount not found")
    }

    if (grepl("Offering Price", securityDetails1[[5]], ignore.case = TRUE)) {
      emmaInitialOfferPrice <- as.double(securityDetails2[[5]])
    } else {
      return ("error Offering Price not found")
    }

    ## There are instances where Initial Offering Yield is not available so don't flag as error
    if (length(securityDetails1) > 5) {
      if (grepl("Offering Yield", securityDetails1[[6]], ignore.case = TRUE)) {
        emmaInitialOfferYield <- as.double(securityDetails2[[6]])
      }
    } else {
        emmaInitialOfferYield <- NA
    }

    emmaDescriptiveRecord <- data.frame(emmaDesc, emmaDatedDate, emmaMaturityDate, emmaInterestRate,
                      emmaPrincipalAmount, emmaInitialOfferPrice, emmaInitialOfferYield)

    return(emmaDescriptiveRecord)
   
}