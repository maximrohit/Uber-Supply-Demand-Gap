library(ggplot2)
#read the file with the orginal columns names 
Uber_base<-read.csv("Uber Request Data.csv",stringsAsFactors = F,check.names = F)
str(Uber_base)
summary(Uber_base)
#additional column with Status as factor
Uber_base$Status_fact<-factor(Uber_base$Status)
summary(Uber_base$Status_fact)
# Cancelled        :1264
#No Cars Available :2650
# Trip Completed   :2831 
#additional column with Pickup point as factor
Uber_base$Pickup_point_fact <-factor(Uber_base$`Pickup point` )
summary(Uber_base$Pickup_point_fact)
#Airport:3238
#City   :3507
summary(Uber_base)

#analysing the relation ship between status and pickup point  
ggplot(Uber_base,aes(x=`Pickup point`,fill=Status_fact))+geom_bar()+geom_text(aes(label=..count..),stat="count",position=position_stack(0.5))
# Observation Set 1
#airport seems to have a lot of 'No Cars Available', greater than trip completed
#city seems to have good mix cancelled and no cars available.



#`Request timestamp` with format '%d-%m-%Y %H:%M:%S' into another column
Uber_base$REQ_TS_Processed<-strptime(Uber_base$`Request timestamp`,format='%d-%m-%Y %H:%M:%S')
#`Request timestamp` with format %d/%m/%Y %H:%M' into the temp columns
Uber_base$REQ_TS_Processed_temp<-
  strptime(paste(Uber_base$`Request timestamp`,':00',sep=''),format='%d/%m/%Y %H:%M:%S')
#merging the columns
Uber_base$REQ_TS_Processed[which(is.na(Uber_base$REQ_TS_Processed))]<-Uber_base[which(is.na(Uber_base$REQ_TS_Processed)),c('REQ_TS_Processed_temp')]

#`Drop timestamp` with format '%d-%m-%Y %H:%M:%S' into another column
Uber_base$DROP_TS_Processed<-strptime(Uber_base$`Drop timestamp`,format='%d-%m-%Y %H:%M:%S')
#`Drop timestamp` with format %d/%m/%Y %H:%M' into the temp columns
Uber_base$DROP_TS_Processed_temp<-
  strptime(paste(Uber_base$`Drop timestamp`,':00',sep=''),format='%d/%m/%Y %H:%M:%S')
#merging the columns, lhp and rhp are intention accessing the
Uber_base$DROP_TS_Processed[which(is.na(Uber_base$DROP_TS_Processed))]<-Uber_base[which(is.na(Uber_base$DROP_TS_Processed)),c('DROP_TS_Processed_temp')]

#Dropping the temp cols
Uber_base$DROP_TS_Processed_temp<-NULL
Uber_base$REQ_TS_Processed_temp<-NULL


#Extracting dates for request timestamps
Uber_base$REQ_Date<-format(Uber_base$REQ_TS_Processed,'%d-%m-%Y')
unique(Uber_base$REQ_Date) # 5 unique days data
Uber_base$REQ_Date<-factor(Uber_base$REQ_Date)
summary(Uber_base) # 1300+ plus trip daily seems consistent

#checing the relation between requested date and status
ggplot(Uber_base,aes(x=REQ_Date,fill=Status_fact))+geom_bar()+geom_text(aes(label=..count..),stat="count",position=position_stack(0.5))
#seems to be  fairly consistent 14(Tues)-15(Wed) seems to have for 'no cars avaliable'

#checing the relation between requested date and Pickup point
ggplot(Uber_base,aes(x=REQ_Date,fill=`Pickup point`))+geom_bar()+geom_text(aes(label=..count..),stat="count",position=position_stack(0.5))
#seems to be  fairly consistent , 12-13 are on a lower side for city

#Since data is consistant dropping dates from probable affecticing attributes
#Analysis on days like monday, tuesday ect are also conuted out

#Extracting dates for drop timestamps
Uber_base$DROP_Date<-format(Uber_base$DROP_TS_Processed,'%d-%m-%Y')

unique(Uber_base$DROP_Date) 
# 6 unique days data and Na's for no cars and driver cancelled
# this implies that trips are across midnight , if not for all atleast for 15 july.

Uber_base$DROP_Date<-factor(Uber_base$DROP_Date)
summary(Uber_base) 
# 500+ plus trip daily seems consistent, 
# 16th are the 15th trips that crossed over the midnight mark
#drop date is only applicable for completed trips , hence not comparing it to statuses

#not required as only complete trip has drop time
#ggplot(Uber_base[which(is.na(Uber_base$DROP_Date)==0),],aes(x=DROP_Date,fill=Status_fact))+geom_bar()+geom_text(aes(label=..count..),stat="count",position=position_stack(0.5))
#redundant check as drop time is directly related to pick up time hence observation are same
#small change w.r.t few trips tsrating late night and ending next day
ggplot(Uber_base[which(is.na(Uber_base$DROP_Date)==0),],aes(x=DROP_Date,fill=`Pickup point`))+geom_bar()+geom_text(aes(label=..count..),stat="count",position=position_stack(0.5))



#duration of trips
Uber_base$duration<-round(Uber_base$DROP_TS_Processed-Uber_base$REQ_TS_Processed,2)
summary(Uber_base$duration)
summary(unclass(Uber_base$duration))
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 20.78   41.00   52.08   52.41   64.00   83.00    3914
#na are for driver canclled and no cars found
#Median and mean are very close represnting and equal distribution of data with no outiner

#duration relation with other dimensions
#pickup location seesm to have very little impact on duration
ggplot(Uber_base,aes(y=duration,x=`Pickup point`))+geom_boxplot()
#requested dates also seems to be similar in nature
ggplot(Uber_base,aes(y=duration,x=REQ_Date))+geom_boxplot()

#is duration impacted by the hours of the day the trip is taken?
# extracting the Hour of request
Uber_base$REQ_Hour<-factor(format(Uber_base$REQ_TS_Processed,'%H'))
unique(Uber_base$REQ_Hour) # 24 observation mapping to 24 hrs a day
summary(Uber_base)
str(Uber_base)

#calation drop hours
Uber_base$Drop_Hour<-factor(format(Uber_base$DROP_TS_Processed,'%H'))
unique(Uber_base$Drop_Hour) # 24 observation mapping to 24 hrs a day
summary(Uber_base)
str(Uber_base)

#duration vs request hours 
ggplot(Uber_base,aes(y=duration,x=REQ_Hour))+geom_boxplot() 
#midnight and 1 am seems to have an unusal high duration(median), with the wideset span
#9,10,11 are higher duration , morning rush ours


#pickpoint versus request hour
ggplot(Uber_base,aes(fill=`Pickup point`,x=REQ_Hour))+geom_bar() +geom_text(aes(label=..count..),stat="count",position=position_stack(0.5))
#obervation 2
#17-22 demand is very high in airport,maybe lots of flights coming inbound
#5-9 demand is very high in city, maybe lots of flights going outbound

#Status_fact versus request hour
ggplot(Uber_base,aes(fill=Status_fact,x=REQ_Hour))+geom_bar() +geom_text(aes(label=..count..),stat="count",position=position_stack(0.5))
#observation 3
#17-22 no cars available happend often
#5-9 cancelled trips seems to shot up

#We can see 17-22 airport request are high and no cars available are high, we need to check if this is a coincidence or something more.
#Also 5-9 city has a lot or request and cancelled trips are high too, again mere co- incidence or more.


#durtion of wait time untill next trip for drivers from airport
?lag
library(dplyr)

Uber_base$REQ_TS_Processed<-as.POSIXct(Uber_base$REQ_TS_Processed)
Uber_base$DROP_TS_Processed<-as.POSIXct(Uber_base$DROP_TS_Processed)
#########################################
#lets caluate the wait_time for the drivers
Uber_base<- Uber_base %>%
  group_by(`Driver id`) %>%
  mutate(lead_REQ_TS_Processed = dplyr::lead(REQ_TS_Processed, n = 1, default = NA,order_by=REQ_TS_Processed))

Uber_base<- Uber_base %>%
  group_by(`Driver id`) %>%
  mutate(lead_Pickup_point = dplyr::lead(`Pickup point`, n = 1, default = NA,order_by=`REQ_TS_Processed`))


# subtracting the drop time with next  request time to calculate the waiting time after thsi trip
Uber_base$wait_time<- round(difftime(Uber_base$lead_REQ_TS_Processed, Uber_base$DROP_TS_Processed, units = "mins"))

# since data provided is already filtered 
# their might be missing  trips in between city to airport and back 
# to minimise their impact consdering only trip that originated from city
# and are follwered by an trip from airport for calculation purpose
#Basically waiting time at the airport
Uber_base[which(!(Uber_base$`Pickup point`=='City' & Uber_base$lead_Pickup_point=='Airport')),c('wait_time')]<-0

#setting all the last trips  wait time to zero as we have no reffernce to calcute that value
Uber_base$wait_time[which(is.na(Uber_base$wait_time))]<-0
summary(Uber_base)
str(Uber_base)


ggplot(Uber_base[which(Uber_base$wait_time!=0),],aes(x=unclass(wait_time)))+geom_histogram()
ggplot(Uber_base[which(Uber_base$wait_time!=0),],aes(x=unclass(wait_time),y=`Driver id`))+geom_point()
ggplot(Uber_base[which(Uber_base$wait_time!=0),],aes(x=unclass(wait_time),y=REQ_Hour))+geom_point()
#their is still some really long wait time, mostly due to missing/filtered(in between trips may be missing ) data set
#we will use the median  to ignore the outliner.


#write.csv(Uber_base, "Uber_base.csv")
# agregating based on drop hour

#################################################################################################################
                    # The wait time for cabs w.r.t. after having drooped at Airport has the drop hour is used
                    
                    ?aggregate
                    medianbyhour_wait_time<-aggregate(wait_time~Drop_Hour,Uber_base[which(Uber_base$wait_time>0),],FUN=median)
                    Countbyhour_wait_time<-aggregate(wait_time~Drop_Hour,Uber_base[which(Uber_base$wait_time>0),],FUN=function(x) sum(x != 0))
                    medianbyhour_wait_time$medianbyhour_wait_time<-medianbyhour_wait_time$wait_time
                    medianbyhour_wait_time$wait_time<-NULL
                    Countbyhour_wait_time$Countbyhour_wait_time<-Countbyhour_wait_time$wait_time
                    Countbyhour_wait_time$wait_time<-NULL
                    
                    
                    # Countbyhour_wait_time
                    # count(Uber_base)
                    # head(Uber_base)
                    # Uber_base<-merge(x=Uber_base,y=medianbyhour_wait_time,by="Drop_Hour",all.x = TRUE)
                    # Uber_base<-merge(x=Uber_base,y=Countbyhour_wait_time,by="Drop_Hour",all.x = TRUE)
                    # head(Uber_base)
                    # 
                    Drop_Hour_analysis<-merge(medianbyhour_wait_time,Countbyhour_wait_time,by="Drop_Hour")
                    Drop_Hour_analysis$label <- paste("median_wait_time", round(Drop_Hour_analysis$medianbyhour_wait_time,2), sep = ":")
                    Drop_Hour_analysis$label<-paste(Drop_Hour_analysis$label,paste("Count_wait_time", Drop_Hour_analysis$Countbyhour_wait_time, sep = ":"),"\n ")
                    Drop_Hour_analysis$label<-paste(Drop_Hour_analysis$label,paste("Drop_Hour", Drop_Hour_analysis$Drop_Hour, sep = ":"),"\n ")
                    
                    #install.packages("ggfittext")
                    install.packages("treemapify")
                    #if the packge is in use then the installation will fail
                    # keeping it un comments as i am not sure whether it would be availabe in enviroment where the code will be verified
                    #library(tidyverse)
                    #library("ggfittext")
                    library("treemapify")
                    
                    ggplot(Drop_Hour_analysis, aes(area = unclass(medianbyhour_wait_time), fill = unclass(Countbyhour_wait_time), label = label)) +
                      geom_treemap()+geom_treemap_text(min.size=5)
                    #20,21,22,23,00,04,07 are a set of wrong timmings for reaching airport
                    #the median waiting time from their seems to be high
                    ggplot(Uber_base[which(Uber_base$wait_time!=0),],aes(x=unclass(wait_time)))+geom_histogram()
                    # it is difficult to say how correct this data is hence we will use that as reffernce only
                    # not in the actual presentation

####################################################################################################
                   # The wait time for cabs w.r.t. pickup at city has the requetsed hour is used
                     ?aggregate
                    medianbyhour_wait_time<-aggregate(wait_time~REQ_Hour,Uber_base[which(Uber_base$wait_time>0),],FUN=median)
                    Countbyhour_wait_time<-aggregate(wait_time~REQ_Hour,Uber_base[which(Uber_base$wait_time>0),],FUN=function(x) sum(x != 0))
                    medianbyhour_wait_time$medianbyhour_wait_time<-medianbyhour_wait_time$wait_time
                    medianbyhour_wait_time$wait_time<-NULL
                    Countbyhour_wait_time$Countbyhour_wait_time<-Countbyhour_wait_time$wait_time
                    Countbyhour_wait_time$wait_time<-NULL


                    # Countbyhour_wait_time
                    # head(Uber_base)
                    # Uber_base<-merge(Uber_base,medianbyhour_wait_time,by="REQ_Hour")
                    # Uber_base<-merge(Uber_base,Countbyhour_wait_time,by="REQ_Hour")
                    # head(Uber_base)

                    Req_hour_analysis<-merge(medianbyhour_wait_time,Countbyhour_wait_time,,by="REQ_Hour")
                    Req_hour_analysis$label <- paste("mean_wait_time", round(Req_hour_analysis$medianbyhour_wait_time,2), sep = ":")
                    Req_hour_analysis$label<-paste(Req_hour_analysis$label,paste("Count_wait_time", Req_hour_analysis$Countbyhour_wait_time, sep = ":"),"\n ")
                    Req_hour_analysis$label<-paste(Req_hour_analysis$label,paste("Req_hour", Req_hour_analysis$REQ_Hour, sep = ":"),"\n ")


                    ggplot(Req_hour_analysis, aes(area = unclass(medianbyhour_wait_time), fill = unclass(Countbyhour_wait_time), label = label)) +
                      geom_treemap()+geom_treemap_text(min.size=5)
                    #21,22,23,6,4 are a set of wrong timmings for reaching airport
                    #the avg waiting time from their seems to be high
                    ggplot(Uber_base[which(Uber_base$wait_time!=0),],aes(x=unclass(wait_time)))+geom_histogram()

##########################################################################################



# based on analysis so far importanct obervation makred as 1,2,3
# requested_hr,pickup point and status seems to related 
ggplot(Uber_base,aes(x=REQ_Hour,fill=Status_fact))+geom_bar(position='fill') + geom_abline(slope=0, intercept=0.45,  col = "red",lty=2)
#Except for 10-16 & 22-23 hrs ranges all other hrs range have less than 45% success ratio
#It implies that for remaing 15 hrs(70%) of the time we just make 45 out of the possible 100
# Thats 38% of total revenue more than 1/3
#Ignoreing other routes present but missing in the data and repeated requets that might be made in the data 
#The data seems to have clear partitions 
# 10-16       where things are good atleast decent
# 17-21-(22-23 partially)-00-04 where no cars seems to dominate
# 04-09       where cancellation rule

#lets break up based on pickup point the third variant

#airport
ggplot(Uber_base[which(Uber_base$`Pickup point`=='Airport'),],aes(x=REQ_Hour,fill=Status_fact))+geom_bar()+geom_text(aes(label=..count..),stat="count",position=position_stack(0.5))
ggplot(Uber_base[which(Uber_base$`Pickup point`=='Airport'),],aes(x=REQ_Hour,fill=Status_fact))+geom_bar(position = 'fill')
# 17-22 have really high % of no cars available with number of requests being high too
# Evidently demand is very high as compared to supply 
# Demand being high signify that flights/passengers are inbound at large
# We don't have enough cabs at the airport for met this surge in demand
# 
# 00(Midnight)-04 have very few requests but around 50% end up as no cars available 
# Few request result in higher wait time for the driver which is another deterrent in generating enough supply.
# 
# We need to make sure we close this gap


#city
ggplot(Uber_base[which(Uber_base$`Pickup point`=='City'),],aes(x=REQ_Hour,fill=Status_fact))+geom_bar()+geom_text(aes(label=..count..),stat="count",position=position_stack(0.5))
ggplot(Uber_base[which(Uber_base$`Pickup point`=='City'),],aes(x=REQ_Hour,fill=Status_fact))+geom_bar(position='fill') 
# Hours(5-9 ) their seems to be a lot of cancellation during this time
# 6,8,9 hrs there are a lot of cars waiting at airport although the wait time is not high
# Drivers don't want to go to airport during these hours as they know they will have to wait their for while for sure
# High demand at city and waiting at airport would mean that their less inbound flights and more outbound flights
# We need to motivates driver to still take these trips while managing their wait time.
# Alternatively allowing them to come back from airport without incurring gas cost.

