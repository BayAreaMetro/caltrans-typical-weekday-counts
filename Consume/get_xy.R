##
# This script does the following:
# 1) Read the counts data file from Box
# 2) Lookup the locations of the counts using the Postmile Webservice
# 3) Merge in the manual locations for those that have failed in the past
# 4) Save the lookup file to Box
##

library(dplyr)
library(readr)
library(RCurl)
library(XML)

now                 <- Sys.time()
BOX_DATA_DIR        <- file.path(Sys.getenv("HOME"),"..","Box","Modeling and Surveys","Share Data","caltrans-typical-weekday")
GIT_REPO_DIR        <- "X:/caltrans-typical-weekday-counts"

# input files
COUNT_FILE          <- file.path(BOX_DATA_DIR,"typical-weekday-counts.csv")           # counts with locations
MANUAL_LOC_FILE     <- "data/typical-weekday-counts-locations-manual-web-lookup.csv"  # locations manually looked up

# this is the output file
COUNT_LOCATION_FILE <- file.path(BOX_DATA_DIR, "typical-weekday-counts-xy-lookup.csv")

soap_head    <- '<?xml version="1.0" encoding="utf-8"?><soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" xmlns:q0="urn:webservice.postmile.lrs.gis.dot.ca.gov" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"><soapenv:Body>'
soap_foot    <- '</soapenv:Body></soapenv:Envelope>'

validate.postmile.parameters <- function(countyCode,
                                         postmileValue,
                                         routeNumber,
                                         postmileSuffixCode){


    validate_head = '<validatePostmileParameters xmlns="urn:webservice.postmile.lrs.gis.dot.ca.gov">
                      <options>
                        <effectiveDate xsi:nil="true"/>
                        <tolerance>0.1</tolerance>
                      </options>
                      <postmileEvent>'

    postmile_xml <- paste("<q0:alignmentCode>",postmileSuffixCode,"</q0:alignmentCode>",
                             "<q0:countyCode>",countyCode,"</q0:countyCode>",
                             "<q0:postmileValue>",postmileValue,"</q0:postmileValue>",
                             "<q0:routeNumber>",routeNumber,"</q0:routeNumber>",
                       sep = "")  
    
    validate_end <-   '</postmileEvent>
                      <postmileSegmentEvent xsi:nil="true"/>
                    </validatePostmileParameters>'
    
    body <- paste(soap_head,
                  validate_head,
                  postmile_xml,
                  validate_end,
                  soap_foot, 
            sep="")

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
                                      routeNumber,
                                      postmilePrefixCode='') {
    #build request
    get_coords_head <- '<q0:getCoordinatesForPostmileParameters>
                                <q0:options>
                                  <q0:alignmentType>0</q0:alignmentType>
                                  <q0:offsetDistance>0</q0:offsetDistance>
                                </q0:options>
                             <q0:postmileEvent>
                             <q0:alignmentCode xsi:nil="true"/>'

    postmile_xml <- paste("<q0:countyCode>",countyCode,"</q0:countyCode>",
                         "<q0:alignmentCode>",postmileSuffixCode,"</q0:alignmentCode>",
                         "<q0:postmileValue>",postmileValue,"</q0:postmileValue>",
                         "<q0:routeNumber>",routeNumber,"</q0:routeNumber>",
                         "<q0:postmilePrefixCode>",postmilePrefixCode,"</q0:postmilePrefixCode>",
                   sep = "")  
  
    get_coords_foot <- '</q0:postmileEvent>
                       <q0:postmileSegmentEvent xsi:nil="true"/>
                       </q0:getCoordinatesForPostmileParameters>'

    body <- paste(soap_head,
                  get_coords_head,
                  postmile_xml,
                  get_coords_foot,
                  soap_foot, 
            sep="")
   
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


#this function is passed to an apply function
#it accepts a single set of postmile parameters, 
#checks their validity, accepting candidate params if necessary,
#and then requests the x/y coordinates for it
get.coordinates.for.df <- function(l2){
  
  coords.postmile.xml <- do.call(get.coordinates.for.postmile,l2)
  
  xy <- parse.caltrans.xml.for.xy(coords.postmile.xml)
  
  #weirdly, the validation service will return a "not valid", even
  #though the get.coordinates service will return an x/y for the same parameters
  #so, as a kind of hack, we will just do a validation check on 
  #postmiles that don't get an x/y back
  
  if (xy[1]==-99 ) {
    #validate the postmile-sometimes its missing a prefix
    #the service will return a best guess for the prefix 
    validate.postmile.xml <- do.call(validate.postmile.parameters,l2)
    v1 <- parse.caltrans.xml.for.valid.postmile(validate.postmile.xml)
    if (length(names(v1[2]))>0 && names(v1[2])=="nil"){
      xy <- c(-99,-99)
    }
    else
    {
      coords.postmile.xml <- do.call(get.coordinates.for.postmile, as.list(c(v1,l2$postmileSuffixCode)))
      xy <- parse.caltrans.xml.for.xy(coords.postmile.xml)
    }
    xy
  }
  
  Sys.sleep(1)
  xy
}

######
#Example requests
######
test_coord <- get.coordinates.for.df(as.list(c(countyCode="SON", postmileValue=0.19, routeNumber=1, postmileSuffixCode="R")))

####
# fetch the location for the existing count set
setwd(GIT_REPO_DIR)

typical_weekday_counts <- read_csv(COUNT_FILE)

twc_unq <- unique(typical_weekday_counts[,c("county","post_mile","route","direction")])

#put the suffix caltrans web service expects
twc_unq <- mutate(twc_unq,
                  postmileSuffixCode=case_when(direction=="N" ~ "R",
                                               direction=="E" ~ "R",
                                               direction=="S" ~ "L",
                                               direction=="W" ~ "L"))

#put an id on the unique location table for merging back into main table later
twc_unq$ID <- 1:nrow(twc_unq)

#change column names to CalTrans webservice names
twc_unq <- rename(twc_unq, countyCode=county, postmileValue=post_mile, routeNumber=route)

#make the route column an int
twc_unq$routeNumber <- as.numeric(as.character(twc_unq$routeNumber))

#iterate over the df
twc_xy <- apply(twc_unq[,c('countyCode','postmileValue','routeNumber','postmileSuffixCode')], 1, 
                function(x) get.coordinates.for.df(as.list(x)))

#format xy's and merge back with df, note about source
twc_xy_t <- t(twc_xy)
twc_unq_xy <- cbind(twc_xy_t,twc_unq)
twc_unq_xy <- rename(twc_unq_xy, latitude=1, longitude=2) %>% 
  select(-ID) %>%
  mutate(loc_source=paste("fetched from PostmileWebService via get_xy.R on",now))
print(paste("Initial fetch: Out of",nrow(twc_unq_xy),"unique locations,",
      nrow(filter(twc_unq_xy, latitude==-99|longitude==-99)),"have missing locations"))

# direction is not part of the lookup -- add reversed version
twc_reverse_xy <- data.frame(twc_unq_xy) %>% mutate(direction=case_when(direction == "E" ~ "W",
                                                                        direction == "W" ~ "E",
                                                                        direction == "S" ~ "N",
                                                                        direction == "N" ~ "S"))
twc_unq_xy <- left_join(twc_unq_xy, twc_reverse_xy,
                        by=c('countyCode','postmileValue','routeNumber','direction'),
                        suffix=c("","_rev"))
twc_unq_xy <- mutate(twc_unq_xy, 
                     latitude =ifelse( (latitude ==-99) & !is.na(latitude_rev),  latitude_rev,  latitude),
                     longitude=ifelse( (longitude==-99) & !is.na(longitude_rev), longitude_rev, longitude)) %>%
  select(-loc_source_rev, -latitude_rev, -longitude_rev, -postmileSuffixCode_rev)

print(paste("After merging reverse: Out of",nrow(twc_unq_xy),"unique locations,",
            nrow(filter(twc_unq_xy, latitude==-99|longitude==-99)),"have missing locations"))

# merge in manual lookup
manual_xy  <- read_csv(MANUAL_LOC_FILE)
twc_unq_xy <- left_join(twc_unq_xy, manual_xy,
                        by=c('countyCode','postmileValue','routeNumber','direction'),
                        suffix=c("","_man"))
twc_unq_xy <- mutate(twc_unq_xy, 
                     loc_source=ifelse((latitude ==-99) & !is.na(latitude_man),  loc_source_man, loc_source),
                     latitude  =ifelse((latitude ==-99) & !is.na(latitude_man),  latitude_man,   latitude),
                     longitude =ifelse((longitude==-99) & !is.na(longitude_man), longitude_man,  longitude)) %>%
  select(-loc_source_man, -latitude_man, -longitude_man, -postmileSuffixCode_man)

print(paste("After merging manual: Out of",nrow(twc_unq_xy),"unique locations,",
            nrow(filter(twc_unq_xy, latitude==-99|longitude==-99)),"have missing locations"))

# sort it
twc_unq_xy <- arrange(twc_unq_xy, countyCode, routeNumber, postmileValue, direction)
write.csv(twc_unq_xy, file = COUNT_LOCATION_FILE, row.names = FALSE)
print(paste("Wrote",COUNT_LOCATION_FILE))
