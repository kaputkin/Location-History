library(jsonlite)
library(plyr)
library(lubridate)
library(geosphere)
library(dplyr)

lh <- fromJSON("C:/Users/Ari/Desktop/Waywiser/Locatdion History/takeout-20201206T123330Z-001/Takeout/Location History/Location History.json")


locs <- lh$locations

names(locs)
ldf <- data.frame(t=rep(0,nrow(locs)))


# Time is in POSIX * 1000 (milliseconds) format, convert it to useful scale...
ldf$t <- as.numeric(locs$timestampMs)/1000
class(ldf$t) <- 'POSIXct'

# Convert longitude and lattitude
ldf$lat <- as.numeric(locs$latitudeE7/1E7)
ldf$lon <- as.numeric(locs$longitudeE7/1E7)

ldf$date <- as.Date(ldf$t)

ldf$time <- format(ldf$t,"%H:%M:%S")


ldf$year <- year(ymd(ldf$date))
ldf$month <- month(ymd(ldf$date)) 
ldf$day <- day(ymd(ldf$date))


ldf <- mutate(ldf, 
       Distance = distHaversine(cbind(lon, lat),
                                cbind(lag(lon), lag(lat))))

ldf_s <- ldf[which(ldf$Distance < 100),]

ldf_s$next_lat <- as.numeric(c(tail(ldf_s$lat, -1), ""))
ldf_s$next_lon <- as.numeric(c(tail(ldf_s$lon, -1), ""))

ldf_s <- subset(ldf_s,date>"2016-09-01")
ldf_movement <- ldf_s[which(ldf_s$Distance > 1),]

write.csv(ldf_s, "points NYC.csv")
write.csv(ldf_movement, "movement NYC.csv")


#covid subset
ldf_covid <- subset(ldf_s,date>"2020-03-15")
ldf_covid_compare <- subset(ldf_s,date >"2019-03-15")
ldf_covid_compare <- subset(ldf_covid_compare,date <"2020-03-15")

write.csv(ldf_covid, "ldf_covid.csv")
write.csv(ldf_covid_compare, "ldf_covid_compare.csv")

ldf2016 <- ldf[which(ldf$year==2016),]
ldf2017 <- ldf[which(ldf$year==2017),]
ldf2018 <- ldf[which(ldf$year==2018),]
ldf2019 <- ldf[which(ldf$year==2019),]
ldf2020 <- ldf[which(ldf$year==2020),]

write.csv(ldf2016, "ldf2016.csv")
write.csv(ldf2017, "ldf2017.csv")
write.csv(ldf2018, "ldf2018.csv")
write.csv(ldf2019, "ldf2019.csv")
write.csv(ldf2020, "ldf2020.csv")

getwd()

gc()
