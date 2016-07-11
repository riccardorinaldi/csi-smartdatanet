library(leaflet)
library(plyr)
library(ggplot2)
library(lubridate)
library(data.table)

library(scales)

setwd("C:\\projects\\torino")
####Parameter Variables
window_size <- 20 #20 minutes -- important for window to be under 30 mins and more recent periods have greater 
station_threshold <- 152
#######################

do_toBike <- function()
{


print("Getting and cleaning weather data")
weather <- read.csv("https://www.wunderground.com/history/airport/LIMF/2015/7/1/CustomHistory.html?dayend=1&monthend=7&yearend=2016&req_city=&req_state=&req_statename=&reqdb.zip=&reqdb.magic=&reqdb.wmo=&format=1",header=T,sep=",",stringsAsFactors=F)
weather_events <- tolower(weather$Events)
weather$fog <- grepl("fog",weather_events)
weather$precip <- grepl("rain|snow|hail",weather_events)
weather$tstorm <- grepl("thunderstorm",weather_events)
weather$tornado <- grepl("tornado",weather_events)
weather$date <- as.Date(ymd(weather$CEST))
weather$CloudCover[is.na(weather$CloudCover)] <- 0
weather <<- weather[,c("date","Mean.TemperatureC","MeanDew.PointC","Mean.Humidity","Mean.Sea.Level.PressurehPa","Mean.VisibilityKm","Mean.Wind.SpeedKm.h","Precipitationmm","CloudCover","fog","precip","tstorm","tornado")]
#population <- read.csv(file="./resident_population/2014zonestat_popolazione_residente.csv",header=T,sep=";",stringsAsFactors=F)
#air_quality <- read.csv(file="./bussola-ds_Air_quality1_959-5.csv",header=T,sep=";",stringsAsFactors=F)
#noise <- read.csv(file="./quies-ds_Quies_212-4.csv",header=T,sep=";",stringsAsFactors=F)

print("Getting toBike Data")
toBike <- read.csv(file="limpid-ds_Bike_sharing_913-1-new.csv",header=T,sep=";",stringsAsFactors=F)
toBike <- toBike[,c("time","bici_disponibili","slot_vuoti","provider","indirizzo","nome","citta","network","id_stazione","longitudine","latitudine")]

##id_stazione appears to be the UID of the GPS device, an assumption given that the id_stazione changes over time!
##Notably, the id_stazione when the lat/long of the station also changes!!  
##Assumption is that a new GPS unit has been installed, or other physical modification that causes both a new ID value and a new location value
##Either way, these changes make for the data very unreliable and leads to some cleaning below
names(toBike) <- c("time","bikes_in","spaces_free","provider","address","name","city","network","gps_id","long","lat")

print("Filtering for Turin")
##Data cleaning/filtering
toBike <- toBike[tolower(toBike$city)=="torino",]
toBike <- toBike[toBike$name != "stazione test - non operativa",]
toBike <- unique(toBike) # There are lots of duplicated data


print("Aligning nuances in GPS positions for stations")
#f$total_bikes <- f$bikes_in + f$spaces_free
##See on accuracy of latitude and longitude decimal places.  4 decimals is accurate to 11m.
##http://gis.stackexchange.com/questions/8650/how-to-measure-the-accuracy-of-latitude-and-longitude/8674#8674
##4 decimal places = 11m
##5 decimal places = 1.1m
toBike$long[toBike$gps_id=="f0b65ebf7ca36e5375d469f44c880e04"] <- 7.676168 ##Data error
toBike$lat <- trunc(toBike$lat*10^5)/10^5   ##The latitude and longitude have different numbers of descimals
toBike$long <- trunc(toBike$long*10^5)/10^5 ##But they may effectively represent the same spot at 6 places (less, even)

print("Creating unique IDs for each station (based on name and aligned GPS)")
IDs <- unique(data.frame(name=toBike$name,lat=toBike$lat,long=toBike$long))
IDs <- ddply(IDs,.(name),mutate,maxlat=max(lat),minlat=min(lat),maxlong=max(long),minlong=min(long))
#Location duplication has a tolerance of approximately 100m before deciding it's a different station by the same name
duplicate_station_names <- !((abs(IDs$lat-IDs$minlat) < .001 & abs(IDs$lat - IDs$maxlat) < .001) | (abs(IDs$long-IDs$minlong) < .001 & abs(IDs$long - IDs$maxlong) < .001))

finalIDs <- IDs[duplicate_station_names,]
finalIDs[,c("final_lat","final_long")] <- finalIDs[,c("lat","long")]
finalIDs <- rbind(finalIDs,
              ddply(IDs[!duplicate_station_names,],.(name),mutate,final_lat=mean(lat),final_long=mean(long))) #Because measurements move arond, take average as actual location
index <<- 0
add_index <<- function() { index <<- index+1 }
finalIDs <- ddply(finalIDs,.(name,final_lat,final_long),mutate,id = add_index())
print(paste(length(unique(finalIDs$id)),"stations identified"))
toBike <- merge(x=toBike,y=finalIDs[,c("name","lat","long","final_lat","final_long","id")],by=c("name","lat","long"))
toBike$lat <- toBike$final_lat
toBike$long <- toBike$final_long
toBike$final_lat <- NULL
toBike$final_long <- NULL

##limpid-ds_Bike_sharing_913-1.csv
##In "time" variable, does ...000Z mean anything in time format?
##Is the time sampled at regular intervals or only when the bike availability status changes?
##What does it mean to for station to have zero bikes available; and zero spaces free?  Example: id_stazione=="0904053b19a69a368ca1b25448ac6aa6" and date of 2016-04-26?  Does it mean station is offline?
##Is total number of bikes equal to bici_disponibile + slot_vuoti?  Example, of Vittorio Emm II: total ranges between 18, 0 and 14 over time.
##Note: id_stazione bd152fea884f549f0c936e5e71a21952 : Citta chages from Bra to Alba (and back again)
##Note: Station location has error in data entry: f$longitudine[f$id_stazione=="f0b65ebf7ca36e5375d469f44c880e04"] <- 7.676168

print("Formatting time and setting season, period variables")
toBike$time <- as.POSIXct(strptime(toBike$time,format="%Y-%m-%dT%H:%M:%S.000Z",tz="GMT"))
toBike$period <- NA
toBike$period[hour(toBike$time) %in% c(4,5,6,7)] <- "early"
toBike$period[hour(toBike$time) %in% c(8,9,10,11)] <- "morning"
toBike$period[hour(toBike$time) %in% c(12,13,14,15)] <- "afternoon"
toBike$period[hour(toBike$time) %in% c(16,17,18,19)] <- "evening"
toBike$period[hour(toBike$time) %in% c(20,21,22,23)] <- "night"
toBike$period[hour(toBike$time) %in% c(0,1,2,3)] <- "late"
toBike$period <- as.factor(toBike$period)

toBike$season <- NA
toBike$season[month(toBike$time) %in% c(12,1,2)] <- "winter"
toBike$season[month(toBike$time) %in% c(3,4,5)] <- "spring"
toBike$season[month(toBike$time) %in% c(6,7,8)] <- "summer"
toBike$season[month(toBike$time) %in% c(9,10,11)] <- "autumn"
toBike$season <- as.factor(toBike$season)

##Data Correction for badly reporting GPS coordinate
print("Identifying non-operative stations")
toBike$station_operative <- 1
toBike$station_operative[(toBike$bikes_in + toBike$spaces_free) == 0] <- 0

print("Estimating total station side and number of broken bikes")
#Decided for total observations, not quarterly
#Let's use quarterly size for total station size, if station size changes during the quarter, could over-count malfunctioning bikes
#It's possible a bike  could remain broken for >1 month, so use quarterly data to determine size, while also allowing for station size to change over time
bikes_by_station <- ddply(toBike[,c("id","bikes_in","spaces_free")],.(id),summarize,total_station_size=max(bikes_in+spaces_free))
toBike <- merge(x=toBike,y=bikes_by_station,by=c("id"),all.x=T)
toBike$broken_bikes <- toBike$total_station_size - (toBike$bikes_in + toBike$spaces_free)
toBike$bikes_in_use <- toBike$spaces_free
toBike_stations <- unique(toBike[,c("id","name","lat","long","total_station_size")])
toBike <<- toBike
write.csv(toBike_stations,file="toBike_stations_only.csv",row.names=F)


#############################
### ALIGN REPORTING INTERVALS
#############################
print("**Aligning time-reporting intervals within a range period")


times <- as.numeric(floor_date(unique(toBike$time),"min")) #Time in seconds, floor to nearest minute
#Create a window of -20 minutes to +1 minutes and add it to each reported timestamp to get all possible window start periods
time_windows <- (as.matrix(rep(1,length(times))) %*% t(as.matrix((60*(c(-(window_size):1)))))) + as.numeric(times)
#From matrix to a vector of window starts and sort by order of time
time_windows <- unique(as.vector(time_windows))
time_windows <- sort(time_windows)
#Create a data.table of start/end window periods
time_window_table <- data.table(window_id=1:length(time_windows),window_start=time_windows, window_end=time_windows+(window_size*60))
setkey(time_window_table,window_start,window_end)


##Note: total station capacity is volatile over time: why? Because not all stations report at all time window intervals.  This increases volatility
station_observation_times <- toBike[toBike$total_station_size > 0 & toBike$station_operative==1,c("id","lat","long","bikes_in","spaces_free","broken_bikes","total_station_size","time","period","season")]
station_observation_times$time_num <- as.numeric(station_observation_times$time)
station_observation_times <- data.table(station_observation_times)
station_observation_times[,time_num2:=time_num]
setkey(station_observation_times,time_num,time_num2)

print(paste0("Applying ",window_size," minute window to each reporting time stamp in the data"))
station_observation_windows <- foverlaps(x=time_window_table,y=station_observation_times,type="any",by.x=c("window_start","window_end"),by.y=c("time_num","time_num2"))
setkey(station_observation_windows,window_id,id)
print(paste0("Identified ",dim(station_observation_windows)[1]," window overlaps"))
station_observation_windows <- station_observation_windows[!is.na(id),] ##Remove windows that don't report within any time windows (to be expected per how windows defined above)
station_observation_windows[,time_num2:=NULL]

print(paste0("For stations that report multiple times in the ",window_size," period, take only the reports closest to the window middle"))
station_observation_windows[,difference_from_window_mean:=time_num-(window_start+((window_end-window_start)/2))]
station_observation_windows[,min_distance_observation:=min(abs(difference_from_window_mean)),by=list(window_id,id)]
station_observation_windows <- station_observation_windows[station_observation_windows$min_distance_observation==abs(station_observation_windows$difference_from_window_mean),]  
station_observation_windows[,station_observations_in_window:=unique(length(id)),by=(window_id)]

print(paste0("Objective is to get a complete view of all bikes in the system, so look only at window periods where the majority of stations report data"))
station_observation_windows[,meets_threshold:=station_observations_in_window >= station_threshold]
station_observation_windows <- station_observation_windows[meets_threshold==T]
station_observation_windows[,`:=`(system_bikes_in=sum(bikes_in),system_spaces_free=sum(spaces_free),system_broken_bikes=sum(broken_bikes),system_capacity=sum(total_station_size)),by=(window_id)]
station_observation_windows[,system_status_str:=paste0(system_capacity,system_spaces_free,system_bikes_in,system_broken_bikes)]
station_observation_windows[,window_mean_time:=(window_start+((window_end-window_start)/2))]

setkey(station_observation_windows,window_mean_time,system_status_str)
setorder(station_observation_windows,window_mean_time,system_status_str)
print("Because windows overlap and thus duplicate observations, segment into discrete reporting windows that removes duplicated observations")
station_observation_windows[,new_status_segment:=(system_status_str != shift(x=system_status_str,n=1,type="lag",fill=T))]
station_observation_windows[,new_time_segment:=(window_mean_time-shift(x=window_mean_time,n=1,type="lag",fill=0))>60] #>60 seconds = 1 minute window shift
station_observation_windows[,new_segment:=(new_status_segment | new_time_segment)]
print(paste0("Identified ",sum(station_observation_windows$new_segment)," reporting segments"))

hit <- which(as.vector(station_observation_windows$new_segment == T))
starts <- hit
ends <- c(hit[-1]-1,dim(station_observation_windows)[1])

segments <- data.frame(starts=starts,ends=ends,segment=c(1:length(starts)))
segments <-apply(segments,1,FUN=function(x) { indexes <- seq(from=x["starts"],to=x["ends"],by=1); return(data.frame(index=indexes,segment=rep(x["segment"],length(indexes)))); })
segments <- ldply(segments,rbind)

station_observation_windows$segment<-0
station_observation_windows$segment[segments$index] <- segments$segment

print("Select only reporting segments where the average station reporting time is closest to the window center")
#station_observation_windows[,median_segment_time:=floor(median(as.numeric(window_mean_time))),by=(segment)]
station_observation_windows[,median_segment_time:=window_mean_time[floor(length(window_mean_time)/2)],by=(segment)]
print(paste("Filtering from",dim(station_observation_windows)[1]," to ",dim(station_observation_windows[window_mean_time==median_segment_time])[1]," rows"))
station_observation_windows <- station_observation_windows[window_mean_time==median_segment_time]

setkey(station_observation_windows,id,time_num)
setorder(station_observation_windows,id,time_num)

print(paste0("Window filtering and segmentation complete, generating ",length(unique(station_observation_windows$segment))," observations of the bike system where ", station_threshold," or more stations report in a ",window_size," minute peirod"))

print("Compute incremental changes for each station's reporting period")
station_observation_windows[,`:=`(change_bikes_in=bikes_in-shift(x=bikes_in,n=1,type="lag",fill=bikes_in[1]),
                                  change_spaces_free=spaces_free-shift(x=spaces_free,n=1,type="lag",fill=spaces_free[1])),by=list(id)]
station_observation_windows[,changes_observed:=(abs(change_bikes_in)+abs(change_spaces_free))/2]

station_observation_windows[,`:=`(total_changes_observd_in_window=sum(changes_observed),space_free_range_in_window=max(system_spaces_free)-min(system_spaces_free)),by=(segment)]
station_observation_windows[,window_mean_time:=.POSIXct(floor(window_mean_time),tz="GMT")]

station_observation_windows[,date:=as.Date(window_mean_time)]

station_observation_windows <<- station_observation_windows
print(paste0("Completed: ",dim(station_observation_windows)[1]," rows of observations across ",length(unique(station_observation_windows$segment))," window segments with an average of ",floor(mean(station_observation_windows$station_observations_in_window)),"/",max(station_observation_windows$id)," station observations per window"))
by_station <<- station_observation_windows[,list(id,date,bikes_in,spaces_free,broken_bikes,total_station_size,time,window_start,window_end,station_observations_in_window,system_bikes_in,system_spaces_free,system_broken_bikes,system_capacity,window_mean_time,segment,period,season)]

by_system <<- unique(station_observation_windows[,list(date,window_mean_time,window_start,window_end,station_observations_in_window,system_bikes_in,system_spaces_free,system_broken_bikes,system_capacity,segment,period,season)])

print("DONE!")
}

do_toBike()

by_system <- merge(x=by_system,y=weather,by=c("date"),all.x=T)
print(paste0("NA values following weather merge: ",sum(is.na(by_system))))
by_system$isWeekend <- weekdays(by_system$date) %in% c("Saturday","Sunday")


sys_model <- lm(system_spaces_free/system_capacity ~ 
                  Mean.TemperatureC  + CloudCover + MeanDew.PointC +
                  tstorm + isWeekend + season + precip, data=by_system)
                  #Mean.Humidity + free_range + Precipitationmm + period + as.factor(weekdays(date)) + fog 
summary(sys_model)

by_system$predict_day_spaces_free <- predict(sys_model,by_system)
setkey(by_system,window_mean_time)
predictions <- unique(by_system[,list(window_mean_time,predict_day_spaces_free)])
setkey(predictions,window_mean_time)
by_station <- by_station[predictions,on="window_mean_time"]
write.csv(x=by_station,file="toBike_by_station.csv")
write.csv(x=by_system,file="toBike_by_system.csv")

#ggplot(data=by_system) +
#  geom_point(aes(x=window_mean_time,y=system_spaces_free/system_capacity),alpha=.5,size=2) +
#  geom_point(aes(x=window_mean_time,y=predict_spaces_free),color="orange",alpha=.25,size=2) +
#  scale_x_datetime(breaks = date_breaks("1 month"), minor_breaks=date_breaks("1 month"), labels=date_format("%b")) +
#  ggtitle("ToBike Use Over Time") +
#  ylab("Percent of spaces free (% in-use)") +
#  xlab("Time") +
#  theme_bw()

by_day <- ddply(by_system,.(date),summarise,max_pct=max(system_spaces_free/system_capacity),min_pct=min(system_spaces_free/system_capacity),avg_pct=mean(system_spaces_free/system_capacity),avg_p_pct=mean(predict_day_spaces_free))
by_day <- melt(by_day,id.vars=names(by_day)[1],measure.vars=names(by_day)[2:5])
img <- ggplot(data=by_day) +
  geom_point(aes(x=date,y=value,color=variable),alpha=.5,size=3,shape=19) +
  scale_x_date(breaks = date_breaks("1 month"), minor_breaks=date_breaks("1 month"), labels=date_format("%b")) +
  scale_colour_manual(name="%Use",values=c("green","red","white","black"),labels=c("Max","Min","Avg","Predicted")) +
  ggtitle("ToBike City-Level Use Over Time") +
  ylab("Percent of spaces free (% in-use)") +
  xlab("Time") +
  theme_bw() 

png(filename="toBike_use_plot.png",width=700,height=400)
img
dev.off()

summary(by_system$system_spaces_free/by_system$system_capacity)
summary(by_system$predict_day_spaces_free)