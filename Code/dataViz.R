data1 <- read.csv(r"(D:\School\Reading\Google Data Analytic Cert\Case Study\03 BikeSharing\Data\data2022_789_cleaned.csv)")
data2 <- read.csv(r"(D:\School\Reading\Google Data Analytic Cert\Case Study\03 BikeSharing\Data\data2022_10_11_12_cleaned.csv)")
data3 <- read.csv(r"(D:\School\Reading\Google Data Analytic Cert\Case Study\03 BikeSharing\Data\data2023_123_cleaned.csv)")
data4 <- read.csv(r"(D:\School\Reading\Google Data Analytic Cert\Case Study\03 BikeSharing\Data\data2023_456_cleaned.csv)")

data5 <- rbind(data1,data2)
data5 <- rbind(data5,data3)
data5 <- rbind(data5,data4)

library(ggplot2)
library(dplyr)
library(patchwork) # To display 2 charts together
library(hrbrthemes)
library(stringr)
library(gridExtra)


monthSeq <- c("07","08","09","10","11","12","01","02","03","04","05","06")

data5$start_month <- str_pad(data5$start_month, 2, pad = "0")
data5$start_month
# data viz for Relationship 1 
ggplot(data5,aes(x=factor(start_month,level=monthSeq),fill=member_casual))+
  geom_bar(stat = "count",position="stack") +
  scale_fill_manual(values=c("#2f78dc","#eb9928"))+
  xlab("Month(July 2022 to June 2023)")+
  ylab("Total Monthly Usage")- 

# data viz for Trend 1
r1 <- ggplot(data5,aes(x=factor(start_month,level=monthSeq),fill=member_casual))+
  geom_bar(stat = "count",position="stack") +
  scale_fill_manual(values=c("#2f78dc","#eb9928"))+
  xlab("Month(July 2022 to June 2023)")+
  ylab("Total Monthly Usage")+
  theme(legend.position="top")

data_weather <- read.csv(r"(D:\School\Reading\Google Data Analytic Cert\Case Study\03 BikeSharing\Data\Chicago,United States 2022-07-01 to 2023-06-30.csv)")
nrow(data_weather)
colnames(data_weather)
data_weather$month <- data_weather$datetime %>% substr(6,7)

monthlyTemp <- data_weather %>% 
                group_by(month) %>%
                summarise_at(vars(temp), list(avgTemp = mean))

monthlyTemp <- data.frame(monthlyTemp)

monthSeq <- c("07","08","09","10","11","12","01","02","03","04","05","06")
t1t <- ggplot(data_weather, aes(x=factor(month,level=monthSeq), y=temp, group=1)) +
  geom_point(stat='summary', fun=mean,color='#c44601',size=3) +
  stat_summary(fun=mean, geom="line",color='#c44601',size=1) +
  ylim(0,100)+
  xlab("Month(July 2022 to June 2023)")+
  ylab("Average Temperature(F)")


grid.arrange(r1,t1t,ncol=1)


# data viz for trend 2
data5$start_month <- as.numeric(data5$start_month)
data5$season <- ceiling(data5$start_month/3)
unique(data5$season)

data5$season <- paste("Q",data5$season,sep="")
unique(data5$season)

data5$start_time <- str_pad(data5$start_time,5,pad="0")
as.numeric(substr(data5$start_time,0,2))
data5$start_time

ggplot(data5[data5$season=="Q1",],aes(x=as.numeric(substr(start_time,0,2)),color=season,group=season),) +
  geom_freqpoly(stat="count",size=1) +
  xlab("Ride Starting Time in Q1") + 
  ylab("Frequency")

ggplot(data5,aes(x=as.numeric(substr(start_time,0,2)),color=season,group=season),) +
  geom_freqpoly(stat="count",size=1) +
  xlab("Ride Starting Time") + 
  ylab("Frequency")

p_member <- ggplot(data5[data5$member_casual=="member",],aes(x=as.numeric(substr(start_time,0,2)),color=season,group=season),) +
  geom_freqpoly(stat="count",size=1.25) +
  xlab("Member Ride Starting Time") + 
  ylab("Frequency")

p_casual <- ggplot(data5[data5$member_casual=="casual",],aes(x=as.numeric(substr(start_time,0,2)),color=season,group=season),) +
  geom_freqpoly(stat="count",size=1.25) +
  xlab("Casual Ride Starting Time") + 
  ylab("Frequency")

grid.arrange(p_member,p_casual)
# "#2f78dc","#eb9928"))+
colnames(data5)

p_mon <- ggplot(data5[data5$day=="Monday",],aes(x=as.numeric(substr(start_time,0,2)),group=member_casual,color=member_casual),) +
  geom_freqpoly(stat="count",size=1.5) +
  xlab("Ride Starting Time on Monday") + 
  ylab("Frequency")

p_tue <- ggplot(data5[data5$day=="Tuesday",],aes(x=as.numeric(substr(start_time,0,2)),group=member_casual,color=member_casual),) +
  geom_freqpoly(stat="count",size=1.5) +
  xlab("Ride Starting Time on Tuesday") + 
  ylab("Frequency")

p_wed <- ggplot(data5[data5$day=="Wednesday",],aes(x=as.numeric(substr(start_time,0,2)),group=member_casual,color=member_casual),) +
  geom_freqpoly(stat="count",size=1.5) +
  xlab("Ride Starting Time on Wednesday") + 
  ylab("Frequency")

p_thu <- ggplot(data5[data5$day=="Thursday",],aes(x=as.numeric(substr(start_time,0,2)),group=member_casual,color=member_casual),) +
  geom_freqpoly(stat="count",size=1.5) +
  xlab("Ride Starting Time on Thursday") + 
  ylab("Frequency")

p_fri <- ggplot(data5[data5$day=="Friday",],aes(x=as.numeric(substr(start_time,0,2)),group=member_casual,color=member_casual),) +
  geom_freqpoly(stat="count",size=1.5) +
  xlab("Ride Starting Time on Friday") + 
  ylab("Frequency")

p_sat <- ggplot(data5[data5$day=="Saturday",],aes(x=as.numeric(substr(start_time,0,2)),group=member_casual,color=member_casual),) +
  geom_freqpoly(stat="count",size=1.5) +
  xlab("Ride Starting Time on Saturday") + 
  ylab("Frequency")

p_sun <- ggplot(data5[data5$day=="Sunday",],aes(x=as.numeric(substr(start_time,0,2)),group=member_casual,color=member_casual),) +
  geom_freqpoly(stat="count",size=1.5) +
  xlab("Ride Starting Time on Sunday") + 
  ylab("Frequency")

grid.arrange(p_mon,p_tue,p_wed,p_thu,p_fri,p_sat,p_sun,ncol=1)

# data viz for Trend 3
days_seq = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")

p_mem_q1 <- ggplot(data5[data5$season==paste("Q",1,sep="") & data5$member_casual=="member",],aes(x=factor(day,level=days_seq)),)+
  geom_bar(stat="count",size=1.25,fill="#eb9928") +
  geom_hline(aes(yintercept=nrow(data5[data5$season==paste("Q",1,sep="") & data5$member_casual=="member",])/7),color="#c44601",size=1)+
  xlab(paste("Q",1," - Total Member Usage across Week"))+
  ylab("Frequency")

p_mem_q2 <- ggplot(data5[data5$season==paste("Q",2,sep="") & data5$member_casual=="member",],aes(x=factor(day,level=days_seq)),)+
  geom_bar(stat="count",size=1.25,fill="#eb9928") +
  geom_hline(aes(yintercept=nrow(data5[data5$season==paste("Q",2,sep="") & data5$member_casual=="member",])/7),color="#c44601",size=1)+
  xlab(paste("Q",2," - Total Member Usage across Week"))+
  ylab("Frequency")

p_mem_q3 <- ggplot(data5[data5$season==paste("Q",3,sep="") & data5$member_casual=="member",],aes(x=factor(day,level=days_seq)),)+
  geom_bar(stat="count",size=1.25,fill="#eb9928") +
  geom_hline(aes(yintercept=nrow(data5[data5$season==paste("Q",3,sep="") & data5$member_casual=="member",])/7),color="#c44601",size=1)+
  xlab(paste("Q",3," - Total Member Usage across Week"))+
  ylab("Frequency")

p_mem_q4 <- ggplot(data5[data5$season==paste("Q",4,sep="") & data5$member_casual=="member",],aes(x=factor(day,level=days_seq)),)+
  geom_bar(stat="count",size=1.25,fill="#eb9928") +
  geom_hline(aes(yintercept=nrow(data5[data5$season==paste("Q",4,sep="") & data5$member_casual=="member",])/7),color="#c44601",size=1)+
  xlab(paste("Q",4," - Total Member Usage across Week"))+
  ylab("Frequency")

grid.arrange(p_mem_q1,p_mem_q2,p_mem_q3,p_mem_q4,ncol=1)


p_cas_q1 <- ggplot(data5[data5$season==paste("Q",1,sep="") & data5$member_casual=="casual",],aes(x=factor(day,level=days_seq)),)+
  geom_bar(stat="count",size=1.25,fill="#2f78dc") +
  geom_hline(aes(yintercept=nrow(data5[data5$season==paste("Q",1,sep="") & data5$member_casual=="casual",])/7),color="#054fb9",size=1)+
  xlab(paste("Q",1," - Total Casual Usage across Week"))+
  ylab("Frequency")

p_cas_q2 <- ggplot(data5[data5$season==paste("Q",2,sep="") & data5$member_casual=="casual",],aes(x=factor(day,level=days_seq)),)+
  geom_bar(stat="count",size=1.25,fill="#2f78dc") +
  geom_hline(aes(yintercept=nrow(data5[data5$season==paste("Q",2,sep="") & data5$member_casual=="casual",])/7),color="#054fb9",size=1)+
  xlab(paste("Q",2," - Total Casual Usage across Week"))+
  ylab("Frequency")

p_cas_q3 <- ggplot(data5[data5$season==paste("Q",3,sep="") & data5$member_casual=="casual",],aes(x=factor(day,level=days_seq)),)+
  geom_bar(stat="count",size=1.25,fill="#2f78dc") +
  geom_hline(aes(yintercept=nrow(data5[data5$season==paste("Q",3,sep="") & data5$member_casual=="casual",])/7),color="#054fb9",size=1)+
  xlab(paste("Q",3," - Total Casual Usage across Week"))+
  ylab("Frequency")

p_cas_q4 <- ggplot(data5[data5$season==paste("Q",4,sep="") & data5$member_casual=="casual",],aes(x=factor(day,level=days_seq)),)+
  geom_bar(stat="count",size=1.25,fill="#2f78dc") +
  geom_hline(aes(yintercept=nrow(data5[data5$season==paste("Q",4,sep="") & data5$member_casual=="casual",])/7),color="#054fb9",size=1)+
  xlab(paste("Q",4," - Total Casual Usage across Week"))+
  ylab("Frequency")

grid.arrange(p_cas_q1,p_cas_q2,p_cas_q3,p_cas_q4,ncol=1)


# data viz for Relationship 2
box1 <- ggplot(data5[data5$ride_duration_hour<0.5 & data5$season=="Q1",])+
  geom_boxplot(aes(x=ride_duration_hour,group=member_casual,fill=member_casual))+
  scale_fill_manual(values=c("#2f78dc","#eb9928"))+
  xlab("Ride Duration(hour) in Season 1")

box2 <- ggplot(data5[data5$ride_duration_hour<0.5 & data5$season=="Q2",])+
  geom_boxplot(aes(x=ride_duration_hour,group=member_casual,fill=member_casual))+
  scale_fill_manual(values=c("#2f78dc","#eb9928"))+
  xlab("Ride Duration(hour) in Season 2")

box3 <- ggplot(data5[data5$ride_duration_hour<0.5 & data5$season=="Q3",])+
  geom_boxplot(aes(x=ride_duration_hour,group=member_casual,fill=member_casual))+
  scale_fill_manual(values=c("#2f78dc","#eb9928"))+
  xlab("Ride Duration(hour) in Season 3")

box4 <- ggplot(data5[data5$ride_duration_hour<0.5 & data5$season=="Q4",])+
  geom_boxplot(aes(x=ride_duration_hour,group=member_casual,fill=member_casual))+
  scale_fill_manual(values=c("#2f78dc","#eb9928"))+
  xlab("Ride Duration(hour) in Season 4")

grid.arrange(box1,box2,box3,box4,ncol=1)

####### 
data_weather <- read.csv(r"(D:\School\Reading\Google Data Analytic Cert\Case Study\03 BikeSharing\Data\Chicago,United States 2022-07-01 to 2023-06-30.csv)")

data5$start_day <- str_pad(data5$start_day, 2, pad = "0")
data5$start_day

data5$start_month <- str_pad(data5$start_month, 2, pad = "0")
data5$start_month

data5$start_date <- paste(data5$start_year,"-",data5$start_month,"-",data5$start_day,sep="" )
data5$start_date

dayCount <- data5 %>%
  group_by(start_date,member_casual) %>%
  tally()

dayCount <- data.frame(dayCount)

dayCount <- dayCount %>% 
  rename("datetime" = "start_date")

t2 <- data_weather[c("datetime","temp")]
t2

tt <- merge(dayCount,t2)
colnames(tt)

ggplot(tt,aes(x=temp,y=n,color=member_casual)) +
  geom_point() +
  geom_smooth(method="lm") +
  scale_color_manual(values=c("#2f78dc","#eb9928"))+
  ylab("Daily Usage") + 
  xlab("Daily Average Temperature")

p1 <- ggplot(tt[tt$member_casual=="member",],aes(x=temp,y=n)) +
  geom_point(color='#eb9928') +
  geom_smooth(method="lm",color='#c44601')+
  ylab("Daily Usage") + 
  xlab("Daily Average Temperature") +
  ggtitle("Member - Temperature vs Usage")

p2 <- ggplot(tt[tt$member_casual=="casual",],aes(x=temp,y=n)) +
  geom_point(color='#2f78dc') +
  geom_smooth(method="lm",color='#054fb9')+
  ylab("Daily Usage") + 
  xlab("Daily Average Temperature")+
  ggtitle("Casual - Temperature vs Usage")
grid.arrange(p1,p2)

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
