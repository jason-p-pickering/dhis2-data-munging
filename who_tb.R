require(httr)


set.seed(6657753)

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


setwd("/home/jason/consultancy/who_tb/")
des<-read.csv("TB_data_dictionary_2016-01-15.csv",stringsAsFactors=F)
#Data elements
des_import<-data.frame(name=des$definition,uid=""
                       ,code=des$variable_name
                       ,short_name=des$variable_name)
write.table(des_import,file="who_tb_des.csv",row.names=FALSE,col.names=TRUE,sep=",",quote=TRUE)
#Data element groups
de_groups<-data.frame(name=unique(des$dataset))
write.table(de_groups,file="who_tb_degs.csv",row.names=FALSE,col.names=TRUE,sep=",",quote=TRUE)


#install.packages(httr)
#install.packages(jsonlite)
require(httr)
require(jsonlite)
#Lets put the data elements into groups
base.url<-"http://localhost:8080/dhis/"
username<-"admin"
password<-"district"
r <- GET(paste0(base.url,"api/dataElements?paging=false&fields=id,code"),authenticate(username, password))
r<- httr::content(r, "text")
des_dhis2<-jsonlite::fromJSON(r,flatten=TRUE)$dataElements

#Get the groups
r <- GET(paste0(base.url,"api/dataElementGroups?paging=false"),authenticate(username, password))
r<- httr::content(r, "text")
degs<-jsonlite::fromJSON(r,flatten=TRUE)$dataElementGroups
names(degs)<-c("deg_uid","de_group")

#Merge the Data elements from the CSV with those from DHIS2
des_with_groups<-merge(des[,c("variable_name","dataset")],des_dhis2,by.x="variable_name",by.y="code")
#Get the data element group UIDs
des_with_groups<-merge(des_with_groups,degs,by.x="dataset",by.y="de_group")


pb <- txtProgressBar(min=1,max=nrow(des_with_groups),style=3)
for (i in 1:nrow(des_with_groups)) {
  url<-paste0(base.url,"api/dataElementGroups/",
              des_with_groups$deg_uid[i],
              "/dataElements/",
              des_with_groups$id[i])
  r<-POST(url,authenticate(username,password),verbose=TRUE)
  assert_that(r$status==204L)
  setTxtProgressBar(pb, i)
}
close(pb)


#Load the organisation units
#Fix the organisation unit levels
#Fix the user assignment

#Load up the TB data
tb<-read.csv("TB_burden_countries_2016-08-10.csv",stringsAsFactors=FALSE)

tb<-reshape2::melt(tb,id.vars=c("country","iso2","iso3","iso_numeric","g_whoregion","year"))
tb<-merge(tb,des_dhis2,by.x="variable",by.y="code")

#We need to get the Country codes
r <- GET(paste0(base.url,"api/organisationUnits?paging=false&filter=level:eq:3&fields=id,code"),authenticate(username, password))
r<- httr::content(r, "text")
ous<-jsonlite::fromJSON(r,flatten=TRUE)$organisationUnits
names(ous)<-c("ou_code","ou_id")

#Get the default category option code

r <- GET(paste0(base.url,"api/categoryOptionCombos?paging=false&filter=name:eq:default&fields=id"),authenticate(username, password))
default_uid<- httr::content(r, "parsed")$categoryOptionCombos[[1]]$id

#Merge the OUs with the data
tb<-merge(tb,ous,by.x="iso3",by.y="ou_code")


#Start to parse out the columns we actually need
tb_out<-tb[,c("id","year","ou_id","value")]
#Add the Category option combo
tb_out$coc<-default_uid
#Add a blank for the attribute option combo
tb_out$acoc<-""
#Arrange the columns
tb_out<-tb_out[,c("id","year","ou_id","coc","acoc","value")]
#Ignore anything which has a NULL value
tb_out<-tb_out[!is.na(tb_out$value),]

write.table(tb_out,file="tb_values.csv"
            ,row.names=F
            ,col.names=T
            ,sep=","
            ,quote=T)
