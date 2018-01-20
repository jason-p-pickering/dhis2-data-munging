generateUID <- function(codeSize = 11) {
  #Generate a random seed
  runif(1)
  allowedLetters <- c(LETTERS, letters)
  allowedChars <- c(LETTERS, letters, 0:9)
  #First character must be a letter according to the DHIS2 spec
  firstChar <- sample(allowedLetters, 1)
  otherChars <- sample(allowedChars, codeSize - 1)
  uid <- paste(c(firstChar,paste(
    otherChars, sep = "", collapse = "" )), sep = "", collapse = "")
  return(uid)
}



d_all<-read.csv("/home/jason/development/dhis2-data-munging/unaids_HIV/hiv_estimates.csv",stringsAsFactors = FALSE)

#Lets only get the middle estimates, ignoring the lower,upper and Footnote columns

d<-grepl("^X[0-9]{4}$",names(d_all))

d<-cbind(d_all[,"Country"],d_all[,my_cols])

#Get the column name for the first column straight
names(d)[1]<-"Country"

#Lets melt the data
d$Country<-as.factor(d$Country)
d<-melt(d,id.vars="Country")

#Get rid of the X in front of the year and make it a number

d$variable<-as.numeric(gsub("X","",d$variable))

#Get rid of the < and spaces and be sure its numeric in value

d$value<-as.numeric(gsub("[< .]","",d$value))

#Remove any NA values

d<-d[!is.na(d$value),]


require(httr)
require(jsonlite)

setwd("/home/jason/development/dhis2-data-munging/")
baseurl<-"http://localhost:8080/dhis/"
username<-"admin"
password<-"district"
#This will give us a cookie we can use for later use. 

loginDHIS2<-function(baseurl,username,password) {
  url<-paste0(baseurl,"api/me")
  r<-GET(url,authenticate(username,password))
  assert_that(r$status_code == 200L) }

loginDHIS2(baseurl,username,password)
r <- GET(paste0(baseurl,"api/27/organisationUnits?paging=false&filter=level:eq:3&fields=id,name"))
r<- httr::content(r, "text")
ous<-jsonlite::fromJSON(r,flatten=TRUE)$organisationUnits
names(ous)<-c("ou_name","ou_id")


#
d$Country<-as.character(d$Country)
#Now, we need to try and merge the OUs from DHIS2 with the OUs from the CSV file. 
#This will be similar to a "left join" as we will keep all of the 

d_merged<-merge(d,ous,by.x="Country",by.y="ou_name",all.x=TRUE)

unique(d_merged[is.na(d_merged$ou_id),"Country"])
       
from<-c("Bolivia (Plurinational State of)",
"Cabo Verde","Côte d'Ivoire",
"Global","Myanmar",
"South Sudan",
"Venezuela (Bolivarian Republic of)")
to<-c("Bolivia","Cape Verde","Cote d'Ivoire","Global","Burma","South Sudan","Venezuela")

d$Country<-mapvalues(d$Country,from,to)

unique(d_merged[is.na(d_merged$ou_id),"Country"])

d_merged<-merge(d,ous,by.x="Country",by.y="ou_name")


de_name<-"NUMBER OF PEOPLE LIVING WITH HIV"
this_id<-generateUID()
des_import<-data.frame(name=de_name
                       ,shortName=de_name,
                       aggregationType="SUM",
                       valueType="NUMBER",
                       domainType="AGGREGATE",
                       id=this_id)

url<-paste0(baseurl,"api/27/metadata?importStrategy=CREATE")
#Post to the metadata API as JSON
r<-POST(url,body=toJSON(list(dataElements = des_import),
                        auto_unbox = TRUE),
        content_type_json())

d_import<-d_merged[,c("ou_id","variable","value")]
d_import$dataElement=this_id
d_import<-d_import[,c("dataElement","variable","ou_id","value")]
names(d_import)<-c("dataElement","period","orgUnit","value")

#Import the data, skipping checks for existing values
url<-paste0(baseurl,"api/27/dataValueSets?preheatCache=true")
r<-POST(url,body=toJSON(list(dataValues = d_import),auto_unbox = TRUE),content_type_json())
#Lets trigger analytics
url<-paste0(baseurl,"api/27/resourceTables/analytics")
r<-POST(url)
