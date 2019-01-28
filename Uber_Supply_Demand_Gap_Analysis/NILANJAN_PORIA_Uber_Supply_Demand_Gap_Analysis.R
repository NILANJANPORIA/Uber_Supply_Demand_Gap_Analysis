#load the required library 
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(scales)


## Loading input files into datafame
UBER_DATA <- read.csv("Uber Request Data.csv",na.strings=c(""," ","NA",NA), stringsAsFactors = F)

##############################################################################
###
### Data cleaning & Data Preparation
###

## Data Observation:

#   Date is available in different string format ("%d/%m/%Y %H:%M" and "%d-%m-%Y %H:%M:%S").
#   All the Date needs to be same date format for further analysis

## This function is used to clean columns containing date in different format
Date_Cleaning <- function(date_column){
  date_column_frmt1<-strptime(date_column, "%d/%m/%Y %H:%M")
  date_column_frmt2<-strptime(date_column, "%d-%m-%Y %H:%M:%S")
  date_column_new<-date_column_frmt1
  NA_position<-is.na(date_column_frmt1)
  date_column_new[NA_position]<-date_column_frmt2[NA_position] 
  return(date_column_new)
}

UBER_DATA$Request.timestamp<-Date_Cleaning(UBER_DATA$Request.timestamp)
UBER_DATA$Drop.timestamp<-Date_Cleaning(UBER_DATA$Drop.timestamp)


###
###Derived columns
###
UBER_DATA$trip_weekday=weekdays(UBER_DATA$Request.timestamp)
UBER_DATA$request_hour=UBER_DATA$Request.timestamp$hour
UBER_DATA$trip_duration=round(difftime(UBER_DATA$Drop.timestamp, UBER_DATA$Request.timestamp, units = "mins"),digits = 0)

## Converted String as Factor where applicable 
UBER_DATA$Pickup.point<-as.factor(UBER_DATA$Pickup.point)
UBER_DATA$Status<-as.factor(UBER_DATA$Status)
UBER_DATA$trip_weekday<-as.factor(UBER_DATA$trip_weekday)
UBER_DATA$trip_weekday <- ordered(UBER_DATA$trip_weekday, levels = c('Sunday','Monday','Tuesday','Wednesday','Thursday','Friday','Saturday'))



##############################################################################

###
### Data Visualization for identifing problems on different hours
###

## This function is used to visualize the plot of various Request Status for each hour
Hourly_Analysis <- function(UBER_DATASET,req_hour,pickup,Data_contains){
  y_label=str_c("Number of ",Data_contains)
  Title=str_c(Data_contains ," Vs Hour relation for Uber")
  ggplot(UBER_DATASET,aes(x=factor(req_hour),fill=factor(pickup)))+
    geom_bar(stat='count',position = "dodge",width=0.8)+labs(x="Time in Hours", y=y_label,fill="Pickup Point")+
    ggtitle(Title)+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
}

# visualize 'Requested Cabs' Vs Hour Plot
hourly_request_plot <- Hourly_Analysis(UBER_DATA,UBER_DATA$request_hour,UBER_DATA$Pickup.point,"Requested Cabs")
hourly_request_plot

# visualize 'Completed Trip' Vs Hour Plot
Completed_Trip <- subset(UBER_DATA,Status=="Trip Completed")
hourly_Completed_Trip_plot <- Hourly_Analysis(Completed_Trip,Completed_Trip$request_hour,Completed_Trip$Pickup.point,"Completed Trip")
hourly_Completed_Trip_plot

# visualize 'Cancelled Trip' Vs Hour Plot
Cancelled_Trip <- subset(UBER_DATA,Status=="Cancelled")
hourly_Cancelled_Trip_plot <- Hourly_Analysis(Cancelled_Trip,Cancelled_Trip$request_hour,Cancelled_Trip$Pickup.point,"Cancelled Trip")
hourly_Cancelled_Trip_plot

# visualize 'Cars Unavailability' Vs Hour Plot
Unavailable_Car<-subset(UBER_DATA,Status=="No Cars Available")
hourly_Unavailable_Car_plot <- Hourly_Analysis(Unavailable_Car,Unavailable_Car$request_hour,Unavailable_Car$Pickup.point,"Unavailable Car")
hourly_Unavailable_Car_plot

##############################################################################

###
### Clustering Time based on above visualiztion
###

##  NIGHT TIME: 11 PM to 4 AM
##  MORNING RUSH: 4 AM to 11 AM
##  DAY TIME: 11 AM to 5 PM
##  EVENING RUSH: 5 PM to 11 PM

UBER_DATA[UBER_DATA$request_hour>4 | UBER_DATA$request_hour<=23 , "Time_Slot"] <- "NIGHT TIME"
UBER_DATA[UBER_DATA$request_hour>=4 & UBER_DATA$request_hour<11, "Time_Slot"] <- "MORNING RUSH"
UBER_DATA[UBER_DATA$request_hour>=11 & UBER_DATA$request_hour<17, "Time_Slot"] <- "DAY TIME"
UBER_DATA[UBER_DATA$request_hour>=17 & UBER_DATA$request_hour<23, "Time_Slot"] <- "EVENING RUSH"

UBER_DATA$Time_Slot<-as.factor(UBER_DATA$Time_Slot)
UBER_DATA$Time_Slot <- ordered(UBER_DATA$Time_Slot, levels = c('NIGHT TIME','MORNING RUSH','DAY TIME','EVENING RUSH'))
# Leveling are done for observing Timeslot in correct order in graph

##############################################################################

###
###Analysis based on Time Slot from different 'pickup point'
###Key poinis to check: Which time slot maximum Requested Trips are not completed
###

## This function is used to visualize the plot of various Request Status for each Time Slot
TripStatus_TimeSlot_Analysis<-function(UBER_DATASET,TimeSlot,TripStatus,PickupPoint){
  Crt_title=str_c("Cab Request on diff Time Slot from ",PickupPoint)
  TripStatus_TimeSlot_Analysis_Plot<-ggplot(UBER_DATASET,aes(x=TimeSlot,fill=TripStatus))+
    geom_bar(stat='count',position = "stack",width=0.5)+
    scale_fill_discrete(limits=c("No Cars Available","Trip Completed","Cancelled"))+
    ggtitle(Crt_title)+
    labs(x="Time Slot", y="Number of Cabs Requested",fill="Status")+ 
    theme(axis.text.x = element_text(angle = 30, hjust = 0.6))
}

# Visualize Request Status vs Time Slot plot when Pickup Point is City
UBER_CITY_PICKUP<-subset(UBER_DATA,Pickup.point=="City")
TripStatus_TimeSlot_Analysis_CITY_Plot=TripStatus_TimeSlot_Analysis(UBER_CITY_PICKUP,UBER_CITY_PICKUP$Time_Slot,UBER_CITY_PICKUP$Status,"City")
TripStatus_TimeSlot_Analysis_CITY_Plot

# Visualize Request Status vs Time Slot plot when Pickup Point is Airport
UBER_AIRPORT_PICKUP<-subset(UBER_DATA,Pickup.point=="Airport")
TripStatus_TimeSlot_Analysis_AIRPORT_Plot=TripStatus_TimeSlot_Analysis(UBER_AIRPORT_PICKUP,UBER_AIRPORT_PICKUP$Time_Slot,UBER_AIRPORT_PICKUP$Status,"Airport")
TripStatus_TimeSlot_Analysis_AIRPORT_Plot

##############################################################################
###
###Identify the gap between Demand (Total Requested Trip) and Supply (Total Completed Trip)
###
### **ASSUMPTION** :  Total 'Requested Trip' is not always represent the actual demand as 
###                   for one trip one customer may request multiple time if previous requests
###                   are not successful. As 'User Id' is not present, this analysis can't identify
###                   that properly. But assuming that a customer trying almost same number of time
###                   for each trip, 'Total Requested Trip' proportionate to actual demand
###

## This function is used to visualize plot of the count of various Request Status for a given Time Slot
Trip_Status_Analysis<-function(UBER_DATASET,Trip_Status,Pickup_point,TimeSlot){
  Crt_title=str_c("Requested Trip Status on ",TimeSlot)
  Trip_Status_Analysis_Plot<-ggplot(UBER_DATASET,aes(x=Trip_Status,fill=Pickup_point))+
    geom_bar(stat='count',position = "dodge")+
    ggtitle(Crt_title)+
    labs(x="Status", y="Number of Trip",fill="PICKUP POINT")+
    geom_text(stat='count',aes(label = ..count.., y = ..count..),position=position_dodge(width=0.9), vjust=-0.25)+
    theme(axis.text.x = element_text(angle = 30, hjust = 0.6))
}

#visualize the plot of the count of various Request Status for MORNING RUSH time
UBER_Morning_Rush_Trip <- subset(UBER_DATA,Time_Slot=="MORNING RUSH")
UBER_Morning_Rush_Trip_Plot <- Trip_Status_Analysis(UBER_Morning_Rush_Trip,UBER_Morning_Rush_Trip$Status,UBER_Morning_Rush_Trip$Pickup.point,"Morning Rush Time")
UBER_Morning_Rush_Trip_Plot

#visualize the plot of the count of various Request Status for EVENING RUSH time
UBER_Evening_Rush_Trip <- subset(UBER_DATA,Time_Slot=="EVENING RUSH")
UBER_Evening_Rush_Trip_Plot <- Trip_Status_Analysis(UBER_Evening_Rush_Trip,UBER_Evening_Rush_Trip$Status,UBER_Evening_Rush_Trip$Pickup.point,"Evening Rush Time")
UBER_Evening_Rush_Trip_Plot




#No of service requests from airport to city during evening rush
UBER_Evening_Rush_Airport_trip <- length(which(UBER_Evening_Rush_Trip$Pickup.point=="Airport"))

#No of completed service from airport to city during evening rush
UBER_Evening_Rush_Airport_trip_Completed <- length(which(UBER_Evening_Rush_Trip$Pickup.point=="Airport" & UBER_Evening_Rush_Trip$Status=="Trip Completed"))

#No of service requests from city to airport during morning rush
UBER_Morning_Rush_City_Trip <- length(which(UBER_Morning_Rush_Trip$Pickup.point=="City"))

#No of completed service from city to airport during morning rush
UBER_Morning_Rush_City_Trip_Completed <- length(which(UBER_Morning_Rush_Trip$Pickup.point=="City" & UBER_Morning_Rush_Trip$Status=="Trip Completed"))


##############################################################################
###
### Data Understanding if Trip duration is any reason (like: one way roads/ traffic etc)
###
UBER_Completed_Trip <- subset(UBER_DATA,Status=="Trip Completed")
Duration_Analysis_plot<-ggplot(UBER_Completed_Trip, aes(x=trip_weekday, y=as.integer(trip_duration),color=Time_Slot)) +
  geom_boxplot()

Duration_Analysis_plot
###
### ***Findings*** :  Duration is not changed significantly over Weekdays or Timeslots.
###                   So issues are independent  on Trip Duration
###



##############################################################################
###
### Visualize how data changed on various day of the week
### ***Limitaion*** : Only weekdays data available, no records on requested trip during weekends
###       

## Completed Trip for each weekday from City to Airport and Vice Versa
## If there is not much difference on count, then chances are less for a driver travel one side empty
UBER_Completed_Trip <- subset(UBER_DATA,Status=="Trip Completed")
UBER_Cmplt_Trip_PickUp<- ggplot(UBER_Completed_Trip,aes(x=trip_weekday,fill=Pickup.point))+geom_bar(stat='count',position = "dodge")+
  ggtitle("Completed Trip on diff Time Slot ")+
  labs(x="Weekdays", y="Number of Completed Trip",fill="PICKUP POINT")+
  geom_text(stat='count',aes(label = ..count.., y = ..count..),position=position_dodge(width=0.8), vjust=-0.25)

UBER_Cmplt_Trip_PickUp

### 
### ***Findings*** :  huge difference identified on Thursday. More cabs travel from City to Airport
###                   but less cabs return with passenger
### 

## This function is used to visualize the plot of various Request Status for each Weekday
TripStatus_TimeSlot_Analysis<-function(UBER_DATASET,Weekday,TripStatus,PickupPoint){
  Crt_title=str_c("Cab Request on diff Time Slot from ",PickupPoint)
  TripStatus_TimeSlot_Analysis_Plot<-ggplot(UBER_DATASET,aes(x=Weekday,fill=TripStatus))+
    geom_bar(stat='count',position = "dodge",width=0.5)+
    scale_fill_discrete(limits=c("No Cars Available","Trip Completed","Cancelled"))+
    ggtitle(Crt_title)+
    labs(x="Weekdays", y="Number of Cabs Requested",fill="Status")+ 
    theme(axis.text.x = element_text(angle = 30, hjust = 0.6))
}

# Visualize Request Status vs Time Slot plot when Pickup Point is City
UBER_CITY_PICKUP<-subset(UBER_DATA,Pickup.point=="City")
TripStatus_Weekdays_Analysis_CITY_Plot=TripStatus_TimeSlot_Analysis(UBER_CITY_PICKUP,UBER_CITY_PICKUP$trip_weekday,UBER_CITY_PICKUP$Status,"City")
TripStatus_Weekdays_Analysis_CITY_Plot

# Visualize Request Status vs Time Slot plot when Pickup Point is Airport
UBER_AIRPORT_PICKUP<-subset(UBER_DATA,Pickup.point=="Airport")
TripStatus_Weekdays_Analysis_AIRPORT_Plot=TripStatus_TimeSlot_Analysis(UBER_AIRPORT_PICKUP,UBER_AIRPORT_PICKUP$trip_weekday,UBER_AIRPORT_PICKUP$Status,"Airport")
TripStatus_Weekdays_Analysis_AIRPORT_Plot

