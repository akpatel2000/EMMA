emmaDesc <- function (emmaID) {
#   This function given emmaID (character string) will navigate to the website and 
#   get the description associated with that emmaID
  
    linkTag <- "http://emma.msrb.org/SecurityView/SecurityDetailsRatings.aspx?cusip="
    u <- paste(linkTag, emmaID, sep="")
 
    remDr$navigate(u)
    Sys.sleep(1)
    
    # Retrieve Page and extract html
    txt = remDr$getPageSource()
    doc <- htmlParse(txt, asText = TRUE)
    
    # Extract descriptive data elements on page
    desc <- xpathApply(doc, "//*[@id='ctl00_mainContentArea_securityHeaderLabelsUserControl1_topLevelIssueDataLink']/text()", xmlValue)
    
    return(desc)
  }