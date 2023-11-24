# this is a R script to do data merging and mapping for the case study of Cyclictic


# list all tables
dbListTables(mySQLconnection)
# try a query
result <- dbSendQuery(mySQLconnection,"SELECT * FROM trip")
data.frame <- fetch(result,n=5)
print(data.frame)





library(tidyverse)
library(tidyr)
library(dplyr)

# read CSV files of format 2
dir2 <- r"(D:\School\Reading\Google Data Analytic Cert\Case Study\03 BikeSharing\Data\format2)"
files_format2 <- list.files(path=dir2, pattern=NULL, all.files=FALSE,
                            full.names=FALSE)

files_format2
data2 = data.frame()
cnt <- 0
maxCnt <- 20
for (file in files_format2){
  data2 <- rbind(data2,data.frame(read_csv(paste(dir2,"\\",file,sep=""))))
  cnt <- cnt +1
  if(cnt>maxCnt) break
  
  #head(data_raw,5)
}

nrow(data2)

# remove duplicates
data2 <- data2[!duplicated(data2),]

# first of all check for missing values
for(col in colnames(data2)){
  nas <- sum(is.na(data2[col]))
  print(paste(col,nas))
}
# quite a lot of missing values in station ids and station names
# a little(2853) missing values on ending latitude and longitude, which is around 0.13% of the data
# remove entries with missing values on ending latitude and longitude
data2 <- data2[!is.na(data2$end_lat),]
data2 <- data2[!is.na(data2$end_lng),]


# outliers
# use IQR to find outliers
detect_outliers <- function(x){
  q1 <- quantile(x,0.25)
  q3 <- quantile(x,0.75)
  IQR <- q3-q1
  x > q3 + (IQR*1.5) | x < q1 - (IQR*1.5)
}

print(nrow(data2))

cleanStationName <- function(name){
  print(i)
  
  ## clean names with format "Public Rack - XXXX"
  name <- sub('Public Rack - ','',name)
  
  ## clean names with asterisk
  name  <- sub('\\*','',name)
  
  ## clean names with brackets
  name  <- sub('\\(','',name)
  name  <- sub('\\)','',name)
  
  ## trim station names
  name <- trimws(name)
  
  return(name)
}

# create mapping of station names to station id
h1 <- c("-1"="xx")
h2 <- c("xx"="-1")

for(i in 1:nrow(data2)){
  start_id <- data2$start_station_id[i]
  end_id <- data2$end_station_id[i]

  # if the start id is not in mapping, and not null
  if(!is.na(start_id) && is.na(h1[start_id])){
    start_station <- cleanStationName(data2$start_station_name[i])
    
    if(!is.na(start_station) && is.na(h2[start_station])){
        # 00 station name not null, both name and id not in mapping
        h1[start_id] <- start_station
        h2[start_station] <- start_id
    # 01 station name not null, id not in mapping but name is in
    }else if(!is.na(start_station)){
        if(h2[start_station]!=start_id){
          s <- paste("name already in mapping",start_station,start_id,h2[start_station],sep=" , ")
          write(s,file=r"(D:\mappings.txt)",append=TRUE)
        }
        
    }
  # id already in mapping
  }else if(!is.na(start_id)){
    # 10 station name not null, id in mapping, name not in
    if(!is.na(start_station) && is.na(h1[start_id])){
      if(h1[start_id]!=start_station){
        s <- paste("id already in mapping",start_id,start_station,h1[start_id],sep=" , ")
        write(s,file=r"(D:\mappings.txt)",append=TRUE)
      }
    }else if(!is.na(start_station)){
      # 11 both not null, both in mapping
      next
    }
  }
  
  # if the end id is not in mapping, and not null
  if(!is.na(end_id) && is.na(h1[end_id])){
    end_station <- cleanStationName(data2$end_station_name[i])
    # station name is not in mapping, the name is not null
    if(!is.na(end_station) && is.na(h2[end_station])){
      h1[end_id] <- end_station
      h2[end_station] <- end_id
    }
  }
  print(paste(i," loop ends"))
}
# from above, we can see that there are many inconsistencies in the mapping from station ids to station name
# there will be 2 ways to resolve this inconsistencies : 
# 1. Discard the station name column, and use solely the station id column
# 2. Unite the station names, change rows of different station names corresponding to one station id into one representative name.
# 3. Ignore the inconsistencies. As long as I do not incorporate station(geographical) aspect into the analysis, these inconsistencies will do no harm to the analysis
# For now, I chose option 3, for if I encounter necessity to deal with it during the analysis, then I make choice from option 1&2.

# change some columns names 

# compute values(new columns)
colnames(data2)
## compute ride duration (in seconds)
ride_duration_sec <- as.POSIXct(data2$ended_at, format="%Y-%m-%d %H:%M:%S", tz="UTC")-as.POSIXct(data2$started_at, format="%Y-%m-%d %H:%M:%S", tz="UTC")
ride_duration_sec
data2$ride_duration_sec <- ride_duration_sec
data2$ride_duration_hour <- data2$ride_duration_sec/3600
data2$ride_duration_hour
# look for outliers
# also a hell lot of outliers
hist(as.numeric(data2$ride_duration_hour))
tt <- data2$ride_duration_hour[detect_outliers(data2$ride_duration_hour)]
tt <- as.numeric(tt)
outliers_duration_day <- rev(sort(tt))/24
outliers_duration_day


# the longest ride lasted for about 8 days
# there are not many rides that lasted longer than 24 hours(about 138 entries)
nrow(data2[data2$ride_duration_hour>24,])
# given the statistical insignificance, it is reasonable to remove them all
data2 <- data2[data2$ride_duration_hour<=24,]
nrow(data2)

colnames(data2)

## compute start and end station distance using Haversine formula
# judging from the values, the longitudes and latitudes are in degrees
library(pracma)
library(units)
# convert to radians
deg2rad <- function(deg) {(deg * pi) / (180)}
# (longitude, latitude)
data2$start_lat_rad <- deg2rad(data2$start_lat)
data2$start_lng_rad  <- deg2rad(data2$start_lng)
data2$end_lng_rad <- deg2rad(data2$end_lng)
data2$end_lat_rad <- deg2rad(data2$end_lat)


data2$dist_km <- vector("numeric",nrow(data2))

for(i in 1:nrow(data2)){
  print(i)
  if(is.na(data2$start_lat_rad[i])||is.na(data2$start_lng_rad[i])||is.na(data2$end_lat_rad[i])||is.na(data2$end_lng_rad[i])){
    d <- -1
  }else{
    start_loc <- c(data2$start_lat_rad[i],data2$start_lng_rad[i])
    end_loc <- c(data2$end_lat_rad[i],data2$end_lng_rad[i])
    d <- haversine(start_loc,end_loc)
  }
  data2$dist_km[i] <- d
}

# there are 10 rows where the staring and ending position are clearly too far away (188km)
# when Chicago's max length is about 40 KM from north to south
rev(sort(data2$dist_km))



colnames(data2)

## compute day of week
data2$day <- weekdays(as_datetime(data2$started_at))
data2$day

## separate started_at to start_year,start_month,start_day,start_time
data2$start_year <- year(as_datetime(data2$started_at))
data2$start_month <- month(as_datetime(data2$started_at))
data2$start_day <- day(as_datetime(data2$started_at))
data2$start_time <- format(as.POSIXct(data2$started_at,format="%Y-%m-%d %H:%M:%S", tz="UTC"), format = "%H:%M")

data2$end_year <- year(as_datetime(data2$ended_at))
data2$end_month <- month(as_datetime(data2$ended_at))
data2$end_day <- day(as_datetime(data2$ended_at))
data2$end_time <-format(as.POSIXct(data2$ended_at,format="%Y-%m-%d %H:%M:%S", tz="UTC"), format = "%H:%M")


# check for outliers : some data entries contain ride info that is way too long
# remove outlier in duration

# not a lot outliers found, only one 0
data2$dist_km[detect_outliers(data2$dist_km)]
boxplot(data2$dist_km)

# check for invalid values : negative duration 
data2$ride_duration_sec[data2$ride_duration_sec<0]
# found 1, remove it
data2 <- data2[data2$ride_duration_sec>0,]

# check for consistencies
unique(data2$member_casual)

write.csv(data2,r"(D:\School\Reading\Google Data Analytic Cert\Case Study\03 BikeSharing\Data\data2022_10_11_12_cleaned.csv)",row.names=FALSE)


# now we can start analysis
data1 <- read.csv(r"(D:\School\Reading\Google Data Analytic Cert\Case Study\03 BikeSharing\Data\data2022_789_cleaned.csv)")
data2 <- read.csv(r"(D:\School\Reading\Google Data Analytic Cert\Case Study\03 BikeSharing\Data\data2022_10_11_12_cleaned.csv)")
data3 <- read.csv(r"(D:\School\Reading\Google Data Analytic Cert\Case Study\03 BikeSharing\Data\data2023_123_cleaned.csv)")
data4 <- read.csv(r"(D:\School\Reading\Google Data Analytic Cert\Case Study\03 BikeSharing\Data\data2023_456_cleaned.csv)")

data5 <- rbind(data1,data2)
data5 <- rbind(data5,data3)
data5 <- rbind(data5,data4)

library(ggplot2)

monthSeq <- c("7","8","9","10","11","12","1","2","3","4","5","6")


# data viz for Trend 1 
ggplot(data5,aes(x=factor(start_month,level=monthSeq),fill=member_casual))+
  geom_bar(stat = "count",position="stack") +
  scale_fill_manual(values=c("#2f78dc","#eb9928"))+
  xlab("Month(July 2022 to June 2023)")+
  ylab("Total Monthly Usage")

# data viz for trend 2
data5$season <- ceiling(data5$start_month/3)
unique(data5$season)

data5$season <- paste("Q",data5$season,sep="")
unique(data5$season)

ggplot(data5,aes(x=as.numeric(substr(start_time,0,2)),color=season,group=season),) +
  geom_freqpoly(stat="count",size=1) +
  xlab("Ride Starting Time") + 
  ylab("Frequency")

ggplot(data5[data5$member_casual=="member",],aes(x=as.numeric(substr(start_time,0,2)),color=season,group=season),) +
  geom_freqpoly(stat="count",size=1) +
  xlab("Member Ride Starting Time") + 
  ylab("Frequency")

ggplot(data5[data5$member_casual=="casual",],aes(x=as.numeric(substr(start_time,0,2)),color=season,group=season),) +
  geom_freqpoly(stat="count",size=1) +
  xlab("Casual Ride Starting Time") + 
  ylab("Frequency")

data5

ggplot(data5,)

colnames(data5)
colnames(data3)
data_weather <- read.csv(r"(D:\School\Reading\Google Data Analytic Cert\Case Study\03 BikeSharing\Data\Chicago,United States 2022-07-01 to 2023-06-30.csv)")
nrow(data_weather)
colnames(data_weather)
data_weather$datetime


data5$start_day <- str_pad(data5$start_day, 2, pad = "0")
data5$start_day

data5$start_month <- str_pad(data5$start_month, 2, pad = "0")
data5$start_month

data5$start_date

colnames(data_weather)
data_weather$temp

t2 <- data_weather[c("datetime","temp")]
t2


dayCount <- data5 %>%
  group_by(start_date,member_casual) %>%
  tally()

dayCount <- data.frame(dayCount)

dayCount <- dayCount %>% 
  rename("datetime" = "start_date")

colnames(dayCount)
colnames(t2)

tt <- merge(dayCount,t2)
colnames(tt)

ggplot(tt,aes(x=temp,y=n,color=member_casual)) +
  geom_point() +
  geom_smooth(method="lm")

p1 <- ggplot(tt[tt$member_casual=="member",],aes(x=temp,y=n)) +
  geom_point(color='#ab3443') +
  geom_smooth(method="lm") +
  title("member")

p2 <- ggplot(tt[tt$member_casual=="casual",],aes(x=temp,y=n)) +
  geom_point(color='#3443ba') +
  geom_smooth(method="lm") +
  title("casual")

tt_member <- tt[tt$member_casual=="member",]
tt_casual <- tt[tt$member_casual=="casual",]
cor(tt_member$temp,tt_member$n)
cor(tt_casual$temp,tt_casual$n)

lm_member <- lm(n~temp, data = tt_member)
lm_casual <- lm(n~temp, data = tt_casual)

coef(lm_member)
coef(lm_casual)

lm_member1 <- lm(temp~n, data = tt_member)
lm_casual1 <- lm(temp~n, data = tt_casual)
coef(lm_member1)
coef(lm_casual1)

ggplot(tt,aes(x=temp,y=n,color=member_casual)) +
  geom_smooth(method="lm") 

grid.arrange(p1,p2)


data_member <- data4[data2$member_casual=="member",]
data_casual <- data4[data2$member_casual=="casual",]


ggplot(data=rbind(data2,data1,data3,data4)) +
  geom_bar(mapping=aes(x=start_month,fill=member_casual,label=),position="dodge",stat="count") +
  scale_x_discrete(name="month",limits=c(1,2,3,4,5,6,7,8,9,10,11,12)) +
  ylab("usage count")

# insight : seasonal factors affect usage alot
# insight : seasonal factors affect casual users much more than member users, in cold seasons casual users ride much less
# at peak season, casual usage is near member usage

data2$member_casual


# the goal is to find difference between subscribers and casual users
# separate subscribers and casual users
data_member <- data2[data2$member_casual=="member",]
data_casual <- data2[data2$member_casual=="casual",]

colnames(data2)
factor(day,level=days_seq)
ggplot(data=data2) +
  geom_line(mapping=aes(x=day,y=as.numeric(substr(start_time,0,2)),group=member_casual))


nrow(data_casual)/(nrow(data_casual)+nrow(data_member))
nrow(data_member)/(nrow(data_casual)+nrow(data_member))


data3 <-  rbind(data_member[sample(1:nrow(data_member), 120000),], data_casual[sample(1:nrow(data_casual), 120000),])

ggplot(data3,aes(x=factor(day,level=days_seq),group=member_casual,color=member_casual),) +
  geom_freqpoly(stat="count",size=1) +
  xlab("Ride distribution in a week(normalized)") + 
  ylab("Frequency") 


library(gridExtra)
library(ggplot2)


# insight 1 : there are more member users (58.634%) than casual users(41.366%)
# viz - ggplot2 do not have built-in pie chart function
# create a data to describe genre data
data_pie <- data.frame(
  group=c("member","casual"),
  value=c(nrow(data_member),nrow(data_casual))
)

# draw a piechart using stacked bar char and polar coordinates
ggplot(data_pie, aes(x="", y=value, fill=group)) + 
  geom_bar(stat="identity", width=1,linewidth=1, color="white") + # stacked bar chart
  coord_polar("y", start=0) + # transform coordinate into polar coordinate system
  theme_void() # remove background, grid, numeric labels cause they affect appearance

 # To display 2 charts together
colnames(data2)
substr(data_member$start_time,0,2)
as.numeric(substr(data_member$start_time,0,2))

p_start_time <- ggplot(data2,aes(x=substr(start_time,0,2),color=member_casual,group=member_casual),) +
  geom_freqpoly(stat="count",size=1) +
  xlab("Ride Starting Time") + 
  ylab("Frequency")
  

p_end_time <- ggplot(data2,aes(x=substr(end_time,0,2),color=member_casual,group=member_casual),) +
  geom_freqpoly(stat="count",size=1)  +
  xlab("Ride Ending Time") + 
  ylab("Frequency") 

grid.arrange(p_start_time,p_end_time, ncol=2)
# insight 2 : member users have a small peak ride usage at around 8 am while casual users don't.
#           both member and casual users have a highest peak ride usage at around 5pm (17:00).
#           casual members have increasing ride usage from 4am to 5pm(the peak)





p_ride_dur <- ggplot(data2,aes(x=ride_duration_hour,color=member_casual,group=member_casual),) +
  geom_freqpoly(stat="count",size=1) +
  xlab("Ride Duration") + 
  ylab("Frequency") +
  xlim(0,0.5) 
# same distribution
  
p_ride_day_in_month <- ggplot(data2,aes(x=start_day,color=member_casual,group=member_casual),) +
  geom_freqpoly(stat="count",size=1) +
  xlab("Ride distribution in a month") + 
  ylab("Frequency") +
  xlim(1,30)
# same distribution ? 



days_seq = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
member_average_rides_per_day <- nrow(data_member)/7
casual_average_rides_per_day <- nrow(data_casual)/7

p_ride_day_in_week_stacked <- ggplot(data2,aes(x=factor(day,level=days_seq),fill=member_casual),) +
  geom_bar(position="stack") +
  xlab("No. of rides")


p_ride_day_in_week_member <- ggplot(data_member,aes(x=factor(day,level=days_seq)),) +
  geom_histogram(stat="count",bins=1,fill='#dd3311') +
  xlab("No. of rides - member user") + 
  ylab("Frequency") +
  geom_hline(yintercept = member_average_rides_per_day,size=1)

p_ride_day_in_week_casual <- ggplot(data_casual,aes(x=factor(day,level=days_seq)),) +
  geom_histogram(stat="count",fill='#6688dd',bins=1) +
  xlab("Ride distribution in a week - casual user") + 
  ylab("Frequency")  +
  geom_hline(yintercept=casual_average_rides_per_day,size=1)

grid.arrange(p_ride_day_in_week_member,p_ride_day_in_week_casual,ncol=1)

data3 <-  rbind(data_member[sample(1:nrow(data_member), 120000),], data_casual[sample(1:nrow(data_casual), 120000),])


p_ride_day_in_week <- ggplot(data3,aes(x=factor(day,level=days_seq),group=member_casual,color=member_casual),) +
  geom_freqpoly(stat="count",size=1) +
  xlab("Ride distribution in a week(normalized)") + 
  ylab("Frequency") 
# insight 3 : huge discrepancy on rides distribution throughout the week
# member users take rides more often on weekdays, and less on weekends
# member users peak at Thursday and has a decreasing trend until Monday
# casual users take rides more often on weekends, and less on weekdays
# casual users peak at Saturday, then has a decreasing trend until Thursday

unique(data2$rideable_type)
unique(data_member$rideable_type)
unique(data_casual$rideable_type)
# insight 4: only casual users take use docked bikes


p1 <- ggplot(data_member) +
  geom_bar(aes(x=rideable_type),stat="count",fill='#dd3311') +
  xlab("bike type - member user")
# remove docked bike to make better comparison
p2 <- ggplot(data_casual[data_casual$rideable_type!="docked_bike",]) +
  geom_bar(aes(x=rideable_type),stat="count",fill='#6688dd') +
  xlab("bike type - casual user")
grid.arrange(p1,p2)
# both types of user use more of electric bikes(available bike type statistics?)


colnames(data2)
p1 <- ggplot(data_member) +
  geom_histogram(aes(x=dist_km)) +
  xlim(0,0.35)+
  xlab("")

p2 <- ggplot(data_casual) +
  geom_histogram(aes(x=dist_km)) +
  xlim(0,0.35)

grid.arrange(p1,p2)
# almost the same distribution shape



p1 <- ggplot(data_member) +
  geom_histogram(aes(x=ride_duration_hour)) +
  xlim(0,4) +
  xlab("ride duration(hour) - member")

p2 <- ggplot(data_casual) +
  geom_histogram(aes(x=ride_duration_hour)) +
  xlim(0,4) +
  xlab("ride duration(hour) - casual")

grid.arrange(p1,p2)

ggplot(data2,aes(x=ride_duration_hour,group=member_casual,color=member_casual))+
  geom_freqpoly(stat="count",size=1) +
  xlim(0,3)

# almost same distribution
  
colnames(data2)
ggplot((head(data_member,1000))) +
  geom_point(aes(x=substr(end_time,0,2),y=ride_duration_hour))


ggplot((head(data_member,5000))) +
  geom_point(aes(x=factor(day,level=days_seq),y=substr(start_time,0,2)),shape="0")


colnames(data2)
mean(data_member$ride_duration_hour)*60
mean(data_casual$ride_duration_hour)*60
# member user average ride duration : 12.9 minutes, median : 9.24 minutes
# casual user average ride duration : 22.1 minutes
summarise(data_member)
summarize(data_casual)

colnames(data2)

# insight 5: from pivot table, we see that casual user has a higher average ride duration as well as a higher standard deviation for it
#         casual          member
# mean    0.371809773	    0.210811816	
# stdv    0.74284461      0.334185139
ggplot(data2,aes(x=ride_duration_hour)) + 
  geom_boxplot(outlier.colour="black", outlier.shape=16,
               outlier.size=2, notch=FALSE)

data3 <- data2[data2$ride_duration_hour<0.75,] 
ggplot(data3)+
  geom_boxplot(aes(x=ride_duration_hour,y=member_casual),outlier.color = 'red')

# member has slightly shorter & more variable ride duration on average



library(sf)
library(ggmap)
library(leaflet)
library(tidyverse)



library("ggplot2")
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")
library("ggspatial")
library("maps")
library("ggthemes")


comm.file <- "https://data.cityofchicago.org/resource/igwz-8jzy.geojson"
chicago.comm <- read_sf(comm.file)
class(chicago.comm)
st_crs(chicago.comm)
plot(chicago.comm["area"])
chicago.comm[""]


chicago <- st_as_sf(maps::map("county", fill=TRUE, plot =FALSE))
chicago_points<- st_centroid(chicago)
chicago_points <- cbind(chicago, st_coordinates(st_centroid(chicago$geom)))

colnames(data_member)

p_member_start <- ggplot(chicago.comm)+
  geom_sf(color="#2b2b2b", fill="white",size=0.125)+
  coord_sf(xlim = c(-88, -87.5), ylim = c(41.5,42.1), expand = FALSE)+
  geom_point(data=data_member[sample(1:nrow(data_member), 1000), ] ,aes(x= start_lng,y=start_lat),size=2,shape=1,color='#ee7777')+
  ggthemes::theme_map() +
  ggtitle("member start")


p_member_end <- ggplot(chicago.comm)+
  geom_sf(color="#2b2b2b", fill="white",size=0.125)+
  coord_sf(xlim = c(-88, -87.5), ylim = c(41.5,42.1), expand = FALSE)+
  geom_point(data=data_member[sample(1:nrow(data_member), 1000), ] ,aes(x= end_lng,y=end_lat),size=2,shape=1,color='#ee7777')+
  ggthemes::theme_map() +
  ggtitle("member end")

p_casual_start <- ggplot(chicago.comm)+
  geom_sf(color="#2b2b2b", fill="white",size=0.125)+
  coord_sf(xlim = c(-88, -87.5), ylim = c(41.5,42.1), expand = FALSE)+
  geom_point(data=data_casual[sample(1:nrow(data_casual), 1000), ] ,aes(x= start_lng,y=start_lat),size=2,shape=1,color='#ee7777')+
  ggthemes::theme_map() +
  ggtitle("casual start")


p_casual_end <- ggplot(chicago.comm)+
  geom_sf(color="#2b2b2b", fill="white",size=0.125)+
  coord_sf(xlim = c(-88, -87.5), ylim = c(41.5,42.1), expand = FALSE)+
  geom_point(data=data_casual[sample(1:nrow(data_casual), 1000), ] ,aes(x= end_lng,y=end_lat),size=2,shape=1,color='#ee7777')+
  ggthemes::theme_map() +
  ggtitle("casual end")


grid.arrange(p_member_start,p_member_end,p_casual_start,p_casual_end,ncol=2)
# potential insight: (very weak!)casual member starting position is a little bit more spread-out? 


p_member <- ggplot(chicago.comm)+
  geom_sf(color="#2b2b2b", fill="white",size=0.125)+
  coord_sf(xlim = c(-88, -87.5), ylim = c(41.5,42.1), expand = FALSE)+
  geom_point(data=data_member[sample(1:nrow(data_member), 5000), ] ,aes(x= end_lng,y=end_lat),size=2,shape=1,color='#ee7777')+
  ggthemes::theme_map() +
  ggtitle("member")


p_casual <- ggplot(chicago.comm)+
  geom_sf(color="#2b2b2b", fill="white",size=0.125)+
  coord_sf(xlim = c(-88, -87.5), ylim = c(41.5,42.1), expand = FALSE)+
  geom_point(data=data_casual[sample(1:nrow(data_casual), 5000), ],aes(x= end_lng,y=end_lat),size=2,shape=1,color='#7777ee')+
  ggthemes::theme_map() +
  ggtitle("casual")

grid.arrange(p_member,p_casual,ncol=2)
# potential insight: ending position same distribution


colnames(data2)

colnames(data3)
member_casual
data3 <- data2[data2$dist_km<0.1,] 
ggplot(data3)+
  geom_boxplot(aes(x=dist_km,y=member_casual),outlier.color = 'red')
# same distribution on distance


# change user type values
# concat data
# further cleaning
# write rides info to database
# remove duplicates




data2




# read CSV files of another format


# change names of columns for merging
data1_t <- data1
data1_t <- data1_t %>% rename("ride_id"="trip_id","start_station_id"="from_station_id","start_station_name"="from_station_name","end_station_id"="to_station_id","end_station_name"="to_station_name","member_casual"="usertype","duration"="tripduration")

# columns to add for data1
col_add_1 <- c("end_lat","end_lng","start_lat","start_lng","rideable_type")
# columns to remove for data1
col_rmv_1 <- c("bikeid")
# columns to add for data2
col_add_2 <- c("birthyear","gender","duration")

data2<- data2 %>% rename("end_time"="ended_at","start_time"="started_at")


for(col in col_add_1){
  data1_t[col] <- NA
}

data1_t <- select(data1_t,-c("bikeid"))

for(col in col_add_2){
  data2[col] <- NA
}

sort(colnames(data1_t))
sort(colnames(data2))

data3 <- rbind(data1_t,data2)
glimpse(data3)


# now clean the data for database storage
# clean station_name first
unique(data2$start_station_name)
unique(data2$end_station_name)

station1 <- unique(data2$start_station_name)

station1 <- relist(sort(unlist(station1)), station1)
# remove rows with name format "Public Rack - XXXX"
for(i in 2:length(station1)){
  s <- station1[i][1][[1]]
  if(length(s)==0) next
  if(grepl( 'Public Rack',s , fixed = TRUE)){
    print(paste(i,s))
    print(substr(s,15,nchar(s)))
    station1[i][1][[1]] <- substr(s,15,nchar(s))
  }
}


station1 <- relist(sort(unlist(station1)), station1)
length(station1)

station1 <- unique(station1)
length(station1)


# now find rows whose name contain asterisk and remove those with actually same name in the list
for(i in 2:length(station1)){
  s <- station1[i][1][[1]]
  if(length(s)==0) next
  if(grepl( '*',s , fixed = TRUE)){
    print(i)
    print(paste(s))
    print(paste(station1[i-1][1][[1]]))
    print(paste(station1[i+1][1][[1]]))
    print("------------------")
  }
}


