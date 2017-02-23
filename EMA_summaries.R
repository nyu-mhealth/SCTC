# Combine all three SCTC dataset to one
setwd("Y:/Kirchner/GIS Projects/SCTC/DATA/EMA DATA/EMA")

# read in three files individually
washu<- read.csv("purchase_EmaSurvey_washu.csv", header=TRUE)
unc<- read.csv("purchase_EmaSurvey_unc.csv", header=TRUE)
stanford<- read.csv("purchase_EmaSurvey_stanford.csv", header=TRUE)
# before putting three dataset together, delete persons that are not in the study group
washu<- washu[grep("washu0", washu$MemberCode, ignore.case=T),]
unc<- unc[grep("UNC0", unc$MemberCode, ignore.case=T),]
stanford<- stanford[grep("su3", stanford$MemberCode, ignore.case=T),]
# exclude admin triggered survey
washu<- subset(washu, TriggeredBy=="geo" | TriggeredBy=="self")
unc<- subset(unc, TriggeredBy=="geo" | TriggeredBy=="self")
stanford<- subset(stanford, TriggeredBy=="geo" | TriggeredBy=="self")

# change time zone
library(reshape)
unc<- rename(unc, c(SurveyTime="send_time"))
washu<- rename(washu, c(SurveyTime="send_time"))
stanford<- rename(stanford, c(SurveyTime="send_time"))
su<- stanford
unc$send_time<- strptime(unc$send_time, "%Y-%m-%dT%H:%M:%SZ",tz="GMT")
unc$send_time<- as.POSIXct(unc$send_time)
unc$localtime<- as.POSIXct(unc$send_time, "America/New_York")
attr(unc$localtime, "tzone") <- "America/New_York"
su$send_time<- as.POSIXct(strptime(su$send_time, "%Y-%m-%dT%H:%M:%SZ",tz="GMT"))
su$localtime<- as.POSIXct(su$send_time)
attr(su$localtime, "tzone") <- "America/Los_Angeles"
washu$send_time<- as.POSIXct(strptime(washu$send_time, "%Y-%m-%dT%H:%M:%SZ",tz="GMT"))
washu$localtime<- as.POSIXct(washu$send_time)
attr(washu$localtime, "tzone") <- "America/Chicago"
# extract date from datetime seperately
su$date<- as.Date(su$send_time, tz="America/Los_Angeles")
unc$date<- as.Date(unc$send_time, tz="America/New_York")
washu$date<- as.Date(washu$send_time, tz="America/Chicago")
# generate university code
unc$place<- "unc"
washu$place<- "washu"
su$place<- "stanford"
# change localtime to string, otherwise the timezone will be messed up
unc$localtime<- as.character(unc$localtime)
washu$localtime<- as.character(washu$localtime)
su$localtime<- as.character(su$localtime)

# append three datasets together
sctc<- rbind(washu, unc, su)

# clean dataset
# delete records that are sent by admin
sctc<- subset(sctc, TriggeredBy=='geo' | TriggeredBy=='self')
# delete StudyDay variable
sctc<- subset(sctc, select=c(MemberCode, job_code, send_time, localtime, date, place, 
                             Latitude, Longitude, Status, ListeningFor, 
                             TriggeredBy, recent_purchase, plan_purchase, last_text_purchase))

# convert SurveyTime to R date as POSIXlt format
sctc$localtime<- as.POSIXct(sctc$localtime, format="%Y-%m-%d %H:%M:%S")
# extract time information from the time variable
sctc$month<- months(sctc$localtime)
sctc$dow<- weekdays(sctc$localtime)
library(lubridate)
sctc$year<- year(sctc$localtime)
sctc$hour<- hour(sctc$localtime)

# basic statistics
sctc <- transform(sctc, uidn_d = as.numeric(interaction(MemberCode, date, drop=TRUE)))
# get observation number each person each day
library(plyr)
obs<- ddply(sctc, .(uidn_d), function(z){
  data.frame(obsnum = seq_along(z$uidn_d))
})
obsn<- aggregate(obsnum~uidn_d, obs, max)
sctc<- merge(sctc, obsn, by=c("uidn_d"))
# recode empty survey answers to missing
levels(sctc$recent_purchase)[1]<-NA
levels(sctc$plan_purchase)[1]<-NA
levels(sctc$last_text_purchase)[1]<-NA
# recode recent purchase answers
sctc$recent_purchase[sctc$recent_purchase==" 1"] <- "1"
sctc$recent_purchase[sctc$recent_purchase=="\nSignature"] <- NA
sctc$recent_purchase[sctc$recent_purchase=="N"] <- "1"
sctc$recent_purchase[sctc$recent_purchase=="Yes"] <- NA
sctc$recent_purchase[sctc$recent_purchase=="Yes 2"] <- "2"
sctc$recent_purchase[sctc$recent_purchase=="Yes.. at the full price\nSignature"] <- "2"
sctc$recent_purchase[sctc$recent_purchase=="Yes... one pack at regular price.\n..\nSignature"] <- "2"
sctc$recent_purchase[sctc$recent_purchase=="yes..ome pack at full price.\nSignature"] <- "2"
sctc$recent_purchase[sctc$recent_purchase=="No"] <- "1"
sctc$recent_purchase[sctc$recent_purchase=="1 "] <- "1"
sctc$recent_purchase[sctc$recent_purchase=="2 "] <- "1"
sctc$recent_purchase[sctc$recent_purchase=="Please quit texting to this number.  
                     I am not making any if the purchases you are talking about!!!!"] <- NA
sctc$recent_purchase[sctc$recent_purchase=="zJames saw some zxcarests"] <- NA
sctc$recent_purchase<- as.factor(as.character(sctc$recent_purchase))
# recode plan purchase
sctc$plan_purchase[sctc$plan_purchase=="No"] <- "N"
sctc$plan_purchase[sctc$plan_purchase=="y"] <- "Y"
sctc$plan_purchase<- as.factor(as.character(sctc$plan_purchase))
# recode last text purchase
sctc$last_text_purchase[sctc$last_text_purchase=="1 "] <- "1"
sctc$last_text_purchase[sctc$last_text_purchase==" 1"] <- "1"
sctc$last_text_purchase[sctc$last_text_purchase=="1=No"] <- "1"
sctc$last_text_purchase[sctc$last_text_purchase=="N"] <- "1"
sctc$last_text_purchase<- as.factor(as.character(sctc$last_text_purchase))
library(Hmisc)
describe(sctc)
# summary by group
library(psych)
describe.by(stanford, stanford$recent_purchase, na.rm=TRUE)
