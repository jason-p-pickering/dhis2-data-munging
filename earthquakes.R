require(jsonlite)
require(httr)
require(rlist)
require(assertthat)

#Utility function which will generate UIDs
generateUID<-function(codeSize=11){
  #Generate a random seed
  runif(1)
  allowedLetters<-c(LETTERS,letters)
  allowedChars<-c(LETTERS,letters,0:9)
  #First character must be a letter according to the DHIS2 spec
  firstChar<-sample(allowedLetters,1)
  otherChars<-sample(allowedChars,codeSize-1)
  uid<-paste(c(firstChar,paste(otherChars,sep="",collapse="")),sep="",collapse="")
  return(uid)}

#A helper function to transform 
#milliseconds since the epoch to a timestamp
epochToTimestamp<-function(x) {
  rv<-NA
  if ( !is.na(x) ) { 
    x<-as.double(x)
    rv <- format(as.POSIXct(x/1000, origin='1970-01-01', tz='GMT'),"%Y-%m-%dT%H:%m:%OS")}
  return(rv)
}

#Get the program metadata from the server
username<-"admin"
password<-"district"
url<-"http://localhost:8080/dhis/api/dataElements.json?fields=id,name,code"
r<-httr::GET(url, 
             httr::authenticate(username,password),
             httr::timeout(60))
assert_that(r$status == 200L)
r<- httr::content(r, "text")
#Change the JSON response to a data frame
des<-jsonlite::fromJSON(r,flatten=TRUE)$dataElements
#We need to massage the codes a bit in order to map them properly later
des$map_code<-paste0("properties.",des$code)

#We will get some data from the USGS web service which provides earthquakes
url<-"http://earthquake.usgs.gov/fdsnws/event/1/query.geojson?starttime=2006-01-01%2000:00:00&minmagnitude=6&endtime=2016-08-09%2023:59:59&orderby=time"
r<-httr::GET(url)
#We should get a HTTP 200 here
assert_that(r$status == 200L)
r<- httr::content(r, "text")
#Stores the earthquakes in an object called eqs
eqs<-jsonlite::fromJSON(r,flatten=TRUE)

#Start processing the data. First, we deal with the 
# event date and location
d<-eqs$features
d$longitude<-rapply(d$geometry.coordinates, function(x) x[1])
d$latitude<-rapply(d$geometry.coordinates, function(x) x[2])
d$eventDate<-sapply(d$properties.time, epochToTimestamp)

#Remap the names
names.dest<-paste0("properties.",des$code)
#Get the columns of interest
d<-d[,names(d) %in% c( names.dest,"id","eventDate","longitude","latitude")]
#We will factorize the events using their ID
d$id<-as.factor(d$id)
#Reshape the data from wide to long format.
d<-reshape2::melt(d,id.vars=c("id"))
names(d)<-c("id","dataElement","value")

#We will consider each event by their intrinsic ID
events<-unique(d$id)
events.list=list(events=list())
#Loop through the events and assemble them into JSON payloads for
#Import into DHIS2

for (i in 1:length(events)) {
#Get one event
  foo<-d[d$id==events[i],]
  #Assign it a UID,program, programStage
  #Assign to the global orgunit
  #And prepare an empty list for the actual data values
  bar<-list(event=generateUID(),
            status="ACTIVE",
            program= "dQNFgL2wFzZ",
            programStage= "JGDuHCiuGZT",
            orgUnit= "u6yjgvIMzKn",
            orgUnitName= "Global",
            eventDate=  foo[foo$dataElement=="eventDate","value"],
            coordinate= list(latitude=foo[foo$dataElement=="latitude","value"],
                             longitude=foo[foo$dataElement=="longitude","value"]),
            dataValues=list())
            #Extract the actual data values into a list
            tmp<-foo[grepl("properties.",foo$dataElement),c("dataElement","value")]
            #Remap the data element codes to actual UIDs (should do this sooner)
            tmp$dataElement<-plyr::mapvalues(tmp$dataElement,des$map_code,des$id,warn_missing=FALSE)
            #Transform a value to boolean
            tmp[tmp$dataElement=="x4KYTdszd53","value"]<-ifelse(tmp[tmp$dataElement=="x4KYTdszd53","value"] == "0","false","true")
            #Transform milliseconds to time stamps
            tmp[tmp$dataElement=="W65BRpRj3zn","value"]<-epochToTimestamp(tmp[tmp$dataElement=="W65BRpRj3zn","value"])
            tmp[tmp$dataElement=="ZDYmT9fwteK","value"]<-epochToTimestamp(as.double(tmp[tmp$dataElement=="ZDYmT9fwteK","value"]))
            #Remove any values which are null
            tmp<-tmp[complete.cases(tmp),]
            #Reassemble to a list
            for ( j in 1:nrow(tmp) ) {
              bar$dataValues<-list.append(bar$dataValue,list(dataElement=tmp[j,"dataElement"],
                                   value=tmp[j,"value"]))
            
            }
  #Append this object to the full list of events
  events.list$events<-list.append(events.list$events,bar)
}

#Save this to a JSON file for upload to DHIS2
#TODO: Post directly to the API
cat(toJSON(events.list,auto_unbox=TRUE),file="eqs.json")

