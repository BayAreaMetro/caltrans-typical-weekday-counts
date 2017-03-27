library(readr)
setwd("C:/projects/caltrans-typical-weekday-counts")
typical_weekday_counts <- read_csv("C:/projects/caltrans-typical-weekday-counts/data/typical-weekday-counts.csv")

#format the request

library(RCurl)

get.coordinates.for.postmile <- function(countyCode,
                                      postmilePrefixCode,
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
                "<q0:postmilePrefixCode>",postmilePrefixCode,"</q0:postmilePrefixCode>",
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


get.coordinates.for.df <- function(county,post_mile,route){
  ct.xml <- get.coordinates.for.postmile(countyCode = county,
                                         postmilePrefixCode = "R",
                                         postmileValue = post_mile,
                                         routeNumber = route)
  if (ct.xml != "") {
    xy <- parse.caltrans.xml.for.xy(ct.xml)
  } else {
    xy = c('nil','nil')
  }
  Sys.sleep(5)
  xy
}

twc_unq <- unique(typical_weekday_counts[,c("county","post_mile","route")])

twc_xy <- apply(twc_unq, 1, function(x) get.coordinates.for.df(x[['county']],
                               x[['post_mile']], 
                               x[['route']]))

twc_xy_t <- t(twc_xy)


