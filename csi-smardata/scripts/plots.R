##Soren Heitmann
##July 7, 2016
##sorenh@jhu.edu

##File is random scripts to generate plots that were used in the presentation
##Plots are written to plot objects, not files
##Execute main.R to ensure variables are present

air_quality <- read.csv(file="./bussola-ds_Air_quality1_959-5.csv",header=T,sep=";",stringsAsFactors=F)

noise <- read.csv(file="./quies-ds_Quies_212-4.csv",header=T,sep=";",stringsAsFactors=F)
noise <- noise[,c("time","livello_rumore","source","latitudine","longitudine")]
names(noise) <- c("time","level","source","lat","long")
noise$lat <- trunc(noise$lat*10^5)/10^5   ##The latitude and longitude have different numbers of descimals
noise$long <- trunc(noise$long*10^5)/10^5 ##But they may effectively represent the same spot at 6 places (less, even)
noise$time <- as.POSIXct(strptime(noise$time,format="%Y-%m-%dT%H:%M:%OSZ",tz="GMT"))

bike_areas <- data.table(max_lat=max(toBike$lat),min_lat=min(toBike$lat),max_long=max(toBike$long),min_long=min(toBike$long))
noise <- noise[(noise$lat >= bike_areas$min_lat & noise$lat <= bike_areas$max_lat & noise$long >= bike_areas$min_long & noise$long <= bike_areas$max_long),]

##Sensors move around, guesstimate which ones by approximate standard deviation of distances moved
moving_sensors <- ddply(noise,.(source),summarise,longs=length(unique(long)),lats=length(unique(lat)),sd_long=round(111000*sd(long)*2),sd_lat=round(111000*sd(lat)*2))
moving_sensors$moving <- (moving_sensors$sd_lat + moving_sensors$sd_long)>200 | (moving_sensors$lats + moving_sensors$longs)>14
noise <- merge(x=noise,y=moving_sensors,by=c("source"),all.x=T)

noise$moving[is.na(noise$moving)] <- F
noise <- noise[noise$moving==F,]

sensor_sources <- data.table(unique(noise[,c("source","lat","long")]))
sensor_sources$sensor_id <- 1:dim(sensor_sources)[1]
setkey(sensor_sources,sensor_id)
names(sensor_sources) <- c("source","n_lat","n_long","sensor_id")


stations <- data.table(toBike_stations)
setkey(stations,id)
#http://hyperphysics.phy-astr.gsu.edu/hbase/acoustic/isprob2.html
#Initially, we thought noise would correlate nicely with city activity and be a proxy for time and use-demand
#We looked at distance calculations using the above link to find distance between sensors and bike stations
#But the data quickly revealed it would not be useful due to poor distribution and unreliable sampling time

leaflet() %>% 
  addTiles() %>% 
  addProviderTiles("Esri.WorldGrayCanvas",options = providerTileOptions(noWrap = TRUE)) %>%
  addCircleMarkers(data=sensor_sources,~n_long, ~n_lat, 
                   fillColor="black",fillOpacity=.8,radius=2,
                   stroke=T,color="black",weight=5,opacity=0.7) %>%
  addCircleMarkers(data=stations,~long, ~lat, 
                   fillColor="blue",fillOpacity=.5,radius=2,
                   stroke=T,color="blue",weight=2,opacity=0.7)

leaflet() %>% 
  addTiles() %>% 
  addProviderTiles("Esri.WorldGrayCanvas",options = providerTileOptions(noWrap = TRUE)) %>%
  addCircleMarkers(data=air_quality,~longitudine, ~latitudine, 
                   fillColor="black",fillOpacity=.8,radius=2,
                   stroke=T,color="black",weight=5,opacity=0.7) %>%
  addCircleMarkers(data=stations,~long, ~lat, 
                   fillColor="blue",fillOpacity=.5,radius=2,
                   stroke=T,color="blue",weight=2,opacity=0.7)


##Looking at bike data reporting.  Exploratory plots to understand the frequency of reporting periods and availability
##of the data.  Below are a small portion of what was examined, but these were useful in identifying the analysis strategy

toBike$date <- as.Date(toBike$time)
by_day_toBike <- ddply(toBike,.(date,season),summarise,spaces_free=sum(spaces_free),capacity=sum(total_station_size),observations=length(date))
by_day_toBike <- merge(x=by_day_toBike,y=weather,by=c("date"),all.x=T)

by_time_toBike <- ddply(toBike,.(time),summarise,spaces_free=sum(spaces_free),capacity=sum(total_station_size),observations=length(time))


ggplot(data=by_time_toBike) + geom_point(aes(x=time,y=spaces_free),alpha=.1)
ggplot(data=by_day_toBike) + geom_point(aes(x=date,y=spaces_free),alpha=.5)
ggplot(data=by_day_toBike) + geom_point(aes(x=date,y=spaces_free/observations),alpha=.5)


ggplot(data=by_day_toBike) + 
  geom_point(aes(x=date,y=spaces_free/capacity),color="black",alpha=.25,size=3,stroke=1)

summary(by_day_toBike$spaces_free/by_day_toBike$capacity)

