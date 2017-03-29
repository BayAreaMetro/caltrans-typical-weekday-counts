library(readr)
setwd("C:/projects/caltrans-typical-weekday-counts")
typical_weekday_counts <- read_csv("C:/projects/caltrans-typical-weekday-counts/data/typical-weekday-counts.csv")

#format the request

library(RCurl)

validate.postmile.parameters <- function(countyCode,
                                         postmileSuffixCode,
                                         postmileValue,
                                         routeNumber){

    body_start = '<?xml version="1.0" encoding="utf-8"?>
                  <soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema">
                  <soap:Body>
                  <validatePostmileParameters xmlns="urn:webservice.postmile.lrs.gis.dot.ca.gov">
                  <options>
                  <effectiveDate xsi:nil="true"/>
                  <tolerance>0.1</tolerance>
                  </options>
                  <postmileEvent>'

    body_middle <- paste("<q0:alignmentCode>",postmileSuffixCode,"</q0:alignmentCode>",
                         "<q0:countyCode>",countyCode,"</q0:countyCode>",
                        "<q0:postmileValue>",postmileValue,"</q0:postmileValue>",
                        "<q0:routeNumber>",routeNumber,"</q0:routeNumber>",
                        sep = "")  
    
    body_end <- '</postmileEvent>
                <postmileSegmentEvent xsi:nil="true"/>
                </validatePostmileParameters>
                </soap:Body>
                </soap:Envelope>'
    
    body <- paste(body_start,body_middle,body_end, sep="")

    headerFields =
      c(Accept = "text/xml",
        Accept = "multipart/*",
        'Content-Type' = "text/xml; charset=utf-8",
        SOAPAction = "http://geo2.dot.ca.gov/pmws/services/PostmileWebService")
    
    h = basicTextGatherer()
    
    h$reset()
    
    curlPerform(url = "http://geo2.dot.ca.gov/pmws/services/PostmileWebService",
                httpheader = headerFields,
                writefunction = h$update,
                postfields = body)
    
    h$value()
}

library("XML")

parse.caltrans.xml.for.valid.postmile <- function(some.caltrans.xml) {
    isXMLString(some.caltrans.xml)
    
    #parse
    x <- xmlInternalTreeParse(some.caltrans.xml) 
    
    l1 <- xmlToList(x)
    
    cnd <- l1$Body$validatePostmileReturn$candidates[1]
    candidateCount <- length(cnd)
    if (candidateCount>1){
      print("more than 1 candidate")
    }
    countyCode <- cnd$item$candidatePostmile$countyCode
    postmilePrefixCode <- cnd$item$candidatePostmile$postmilePrefixCode
    postmileValue <- cnd$item$candidatePostmile$postmileValue
    routeNumber <- cnd$item$candidatePostmile$routeNumber

    c(countyCode,postmilePrefixCode,postmileValue,routeNumber)
  }


get.coordinates.for.postmile <- function(countyCode,
                                      postmileSuffixCode,
                                      postmileValue,
                                      routeNumber) {
    #build request
    body_start <- '<?xml version="1.0" encoding="utf-8"?>
    <soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" xmlns:q0="urn:webservice.postmile.lrs.gis.dot.ca.gov" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
    <soapenv:Body>
    <q0:getCoordinatesForPostmileParameters>
    <q0:options>
    <q0:alignmentType>0</q0:alignmentType>
    <q0:offsetDistance>0</q0:offsetDistance>
    </q0:options>
    <q0:postmileEvent><q0:alignmentCode xsi:nil="true"/>'

    body_middle <- paste("<q0:countyCode>",countyCode,"</q0:countyCode>",
                "<q0:alignmentCode>",postmileSuffixCode,"</q0:alignmentCode>",
                "<q0:postmileValue>",postmileValue,"</q0:postmileValue>",
                "<q0:routeNumber>",routeNumber,"</q0:routeNumber>",
                sep = "")  
  
    body_end <- '</q0:postmileEvent>
    <q0:postmileSegmentEvent xsi:nil="true"/>
    </q0:getCoordinatesForPostmileParameters>
    </soapenv:Body></soapenv:Envelope>'

    body <- paste(body_start,body_middle,body_end, sep="")
   
    #make the request
    headerFields =
      c(Accept = "text/xml",
        Accept = "multipart/*",
        'Content-Type' = "text/xml; charset=utf-8",
        SOAPAction = "http://geo2.dot.ca.gov/pmws/services/PostmileWebService")
    
    h = basicTextGatherer()
    
    h$reset()
    
    curlPerform(url = "http://geo2.dot.ca.gov/pmws/services/PostmileWebService",
                httpheader = headerFields,
                writefunction = h$update,
                postfields = body)
    
    h$value()
}

library("XML")

parse.caltrans.xml.for.xy <- function(some.caltrans.xml) {
  # check the string
  isXMLString(some.caltrans.xml)
  
  #parse
  x <- xmlInternalTreeParse(some.caltrans.xml) 
  
  #turn it into a list because thats easier to work with
  l1 <- xmlToList(x)

  #get x and y  
    if (is.null(l1$Body$Fault)) {
      x1 <- l1$Body$getCoordinatesForPostmileReturn$pointGeometry$y
      y1 <- l1$Body$getCoordinatesForPostmileReturn$pointGeometry$x
    } else {
      x1 <- -99
      y1 <- -99
    }

  c(as.double(x1),as.double(y1))
}


get.coordinates.for.df <- function(county,suffix,post_mile,route){
  
  valid.postmile.xml <- validate.postmile.parameters()
  x <- xmlInternalTreeParse(valid.postmile.xml) 
  l1 <- xmlToList(x)
  
  if (l1$Body$validatePostmileReturn$isValid == "false") {
    v1 <- parse.caltrans.xml.for.valid.postmile() 
    ct.xml <- do.call(get.coordinates.for.postmile, as.list(v1))
  }
  else {
    ct.xml <- get.coordinates.for.postmile(countyCode = county,
                                           postmileSuffixCode = suffix,
                                           postmileValue = post_mile,
                                           routeNumber = route)
  }
  if (ct.xml != "") {
    xy <- parse.caltrans.xml.for.xy(ct.xml)
  } else {
    xy = c('nil','nil')
  }
  Sys.sleep(1)
  xy
}

twc_unq <- unique(typical_weekday_counts[,c("county","post_mile","route","direction")])

#get the suffix caltrans web service expects
twc_unq$suffix <- replace(twc_unq$direction,twc_unq$direction=="N","R")
twc_unq$suffix <- replace(twc_unq$suffix,twc_unq$suffix=="E","R")
twc_unq$suffix <- replace(twc_unq$suffix,twc_unq$suffix=="S","L")
twc_unq$suffix <- replace(twc_unq$suffix,twc_unq$suffix=="W","L")

twc_unq$ID <- 1:nrow(twc_unq)


twc_xy <- apply(twc_unq[,1:5], 1, function(x) get.coordinates.for.df(x[['county']],
                               x[['suffix']],
                               x[['post_mile']], 
                               as.integer(x[['route']])))

twc_xy_t <- t(twc_xy)
twc_unq_xy <- cbind(twc_xy_t,twc_unq)
write.csv(twc_unq_xy, file = "data/twc_xy.csv")

typical_weekday_counts <- merge(typical_weekday_counts, twc_unq)


