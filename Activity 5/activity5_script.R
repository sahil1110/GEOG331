#load in lubridate

install.packages("lubridate")

library(lubridate)

#read in streamflow data
datH <- read.csv("y:\\Data\\activities\\a05\\stream_flow_data.csv",
                 na.strings = c("Eqp"))
head(datH)

#read in precipitation data
#hourly precipitation is in mm
datP <- read.csv("y:\\Data\\activities\\a05\\2049867.csv")
head(datP)

#only use most reliable measurements
datD <- datH[datH$discharge.flag == "A",]
head(datD)

#### define time for streamflow #####
#convert date and time
datesD <- as.Date(datD$date, "%m/%d/%Y")
#get day of year
datD$doy <- yday(datesD)
#calculate year
datD$year <- year(datesD)
#define time
timesD <- hm(datD$time)
#### define time for precipitation #####
dateP <- ymd_hm(datP$DATE)
#get day of year
datP$doy <- yday(dateP)
#get year
datP$year <- year(dateP)
#### get decimal formats #####
#convert time from a string to a more usable format
#with a decimal hour
datD$hour <- hour(timesD ) + (minute(timesD )/60)
#get full decimal time
datD$decDay <- datD$doy + (datD$hour/24)
#calculate a decimal year, but account for leap year
datD$decYear <- ifelse(leap_year(datD$year),datD$year + ((datD$decDay-1)/366),
                       datD$year + ((datD$decDay-1)/365))
#calculate times for datP
datP$hour <- hour(dateP ) + (minute(dateP )/60)
#get full decimal time
datP$decDay <- datP$doy + (datP$hour/24)
#calculate a decimal year, but account for leap year
datP$decYear <- ifelse(leap_year(datP$year),datP$year + ((datP$decDay-1)/366),
                       datP$year + ((datP$decDay-1)/365))

#plot discharge
plot(datD$decYear, datD$discharge, type="l", xlab="Year", ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))

#basic formatting
aveF <- aggregate(datD$discharge, by=list(datD$doy), FUN="mean")
colnames(aveF) <- c("doy","dailyAve")
sdF <- aggregate(datD$discharge, by=list(datD$doy), FUN="sd")
colnames(sdF) <- c("doy","dailySD")

#start new plot
dev.new(width=8,height=8)

#bigger margins
par(mai=c(1,1,1,1))
#make plot
plot(aveF$doy,aveF$dailyAve, 
     type="l", 
     xlab="Year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2)

#bigger margins
par(mai=c(1,1,1,1))
#make plot
plot(aveF$doy,aveF$dailyAve, 
     type="l", 
     xlab="Year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2,
     ylim=c(0,90),
     xaxs="i", yaxs ="i")#remove gaps from axes  
#show standard deviation around the mean
polygon(c(aveF$doy, rev(aveF$doy)),#x coordinates
        c(aveF$dailyAve-sdF$dailySD,rev(aveF$dailyAve+sdF$dailySD)),#ycoord
        col=rgb(0.392, 0.584, 0.929,.2), #color that is semi-transparent
        border=NA#no border
)

#bigger margins
par(mai=c(1,1,1,1))
#make plot
plot(aveF$doy,aveF$dailyAve, 
     type="l", 
     xlab="Year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2,
     ylim=c(0,90),
     xaxs="i", yaxs ="i",#remove gaps from axes
     axes=FALSE)#no axes
polygon(c(aveF$doy, rev(aveF$doy)),#x coordinates
        c(aveF$dailyAve-sdF$dailySD,rev(aveF$dailyAve+sdF$dailySD)),#ycoord
        col=rgb(0.392, 0.584, 0.929,.2), #color that is semi-transparent
        border=NA#no border
)       
axis(1, seq(0,360, by=40), #tick intervals
     lab=seq(0,360, by=40)) #tick labels
axis(2, seq(0,80, by=20),
     seq(0,80, by=20),
     las = 2)#show ticks at 90 degree angle

#bigger margins
par(mai=c(1,1,1,1))
#make plot
plot(aveF$doy,aveF$dailyAve, 
     type="l", 
     xlab="Year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2,
     ylim=c(0,90),
     xaxs="i", yaxs ="i",#remove gaps from axes
     axes=FALSE)#no axes
polygon(c(aveF$doy, rev(aveF$doy)),#x coordinates
        c(aveF$dailyAve-sdF$dailySD,rev(aveF$dailyAve+sdF$dailySD)),#ycoord
        col=rgb(0.392, 0.584, 0.929,.2), #color that is semi-transparent
        border=NA#no border
)       
axis(1, seq(0,360, by=40), #tick intervals
     lab=seq(0,360, by=40)) #tick labels
axis(2, seq(0,80, by=20),
     seq(0,80, by=20),
     las = 2)#show ticks at 90 degree angle
legend("topright", c("mean","1 standard deviation"), #legend items
       lwd=c(2,NA),#lines
       fill=c(NA,rgb(0.392, 0.584, 0.929,.2)),#fill boxes
       border=NA,#no border for both fill boxes (don't need a vector here since both are the same)
       bty="n")#no legend border

#bigger margins
par(mai=c(1,1,1,1))
#make plot
plot(aveF$doy,aveF$dailyAve, 
     type="l", 
     xlab="Year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2,
     ylim=c(0,160),
     xaxs="i", yaxs ="i",#remove gaps from axes
     axes=FALSE)#no axes
par(new=T)
plot(datD$doy[datD$year==2017], datD$discharge[datD$year==2017], col="green", type="l"
     ,xaxs="i", yaxs ="i",#remove gaps from axes
     axes=FALSE, lwd=2,
     xlab="Year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),ylim=c(0,160))
polygon(c(aveF$doy, rev(aveF$doy)),#x coordinates
        c(aveF$dailyAve-sdF$dailySD,rev(aveF$dailyAve+sdF$dailySD)),#ycoord
        col=rgb(0.392, 0.584, 0.929,.2), #color that is semi-transparent
        border=NA#no border
)       
months_days<- c(31, 31+29, 31+29+31, 31+29+31+30, 31+29+31+30+31, 31+29+31+30+31+30, 
                31+29+31+30+31+30+31, 31+29+31+30+31+30+31+31, 31+29+31+30+31+30+31+31+30, 
                31+29+31+30+31+30+31+31+30+31, 31+29+31+30+31+30+31+31+30+31+30, 31+29+31+30+31+30+31+31+30+31+30+31,
                31+29+31+30+31+30+31+31+30+31+30+31+30)
months_days<- months_days-30
months<- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "")
axis(1, months_days, #tick intervals
     lab=months) #tick labels
axis(2, seq(0,160, by=20),
     seq(0,160, by=20),
     las = 2)#show ticks at 90 degree angle
legend("topright", c("mean","1 standard deviation", "2017 observations"), #legend items
       lwd=c(2,NA, 2),#lines
       col=c("black",rgb(0.392, 0.584, 0.929,.2),"green"),#colors
       pch=c(NA,15, NA),#symbols
       bty="n")#no legend border

mean(datD$discharge[datD$year==2017])
sd(datD$discharge[datD$year==2017])

len_obs<- aggregate(datP$doy, by=list(datP$doy, datP$year), FUN= length)
colnames(len_obs)<- c("doy", "Year", "Number of observations")
len_obs<- subset(len_obs, len_obs$`Number of observations`==24)

# Alternative approach:

datD$full24<- ifelse(datD$year %in% len_obs$Year & datD$doy %in% len_obs$doy, 1, 0)


#bigger margins
par(mai=c(1,1,1,1))
#make plot
plot(aveF$doy,aveF$dailyAve, 
     xlab="Year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     ylim=c(0,90),
     xaxs="i", yaxs ="i",#remove gaps from axes
     axes=FALSE,
     pch= ifelse(aveF$doy %in% len_obs$doy, 6, 1),
     cex= ifelse(aveF$doy %in% len_obs$doy, 1, 0.8),
     col= ifelse(aveF$doy %in% len_obs$doy, "red", "black"))#no axes
axis(1, seq(0,360, by=40), #tick intervals
     lab=seq(0,360, by=40)) #tick labels
axis(2, seq(0,60, by=20),
     seq(0,60, by=20),
     las = 2)#show ticks at 90 degree angle
legend("topright", c("mean","mean (days with full 24 hour precipitation measurements)"), #legend items
       lwd=c(2,NA),#lines
       border=NA,#no border for both fill boxes (don't need a vector here since both are the same)
       bty="n",
       pch= c(NA, 6),
       col= c("black", "red"))

## Question 8 ##
par(mai=c(1,1,1,1))

hydroD <- datD[datD$doy >= 356 & datD$doy < 358 & datD$year == 2012,]
hydroP <- datP[datP$doy >= 356 & datP$doy < 358 & datP$year == 2012,]

#get minimum and maximum range of discharge to plot
#go outside of the range so that it's easy to see high/low values
#floor rounds down the integer
yl <- floor(min(hydroD$discharge))-1
#celing rounds up to the integer
yh <- ceiling(max(hydroD$discharge))+1
#minimum and maximum range of precipitation to plot
pl <- 0
pm <-  ceiling(max(hydroP$HPCP))+.5
#scale precipitation to fit on the 
hydroP$pscale <- (((yh-yl)/(pm-pl)) * hydroP$HPCP) + yl

par(mai=c(1,1,1,1))
#make plot of discharge
plot(hydroD$decDay,
     hydroD$discharge, 
     type="l", 
     ylim=c(yl,yh), 
     lwd=2,
     xlab="Day of year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))
#add bars to indicate precipitation 
for(i in 1:nrow(hydroP)){
  polygon(c(hydroP$decDay[i]-0.017,hydroP$decDay[i]-0.017,
            hydroP$decDay[i]+0.017,hydroP$decDay[i]+0.017),
          c(yl,hydroP$pscale[i],hydroP$pscale[i],yl),
          col=rgb(0.392, 0.584, 0.929,.2), border=NA)
}

# Spring: March 1- May 31
# Summer: June 1- August 31
# Fall: September 1- November 30
# Winter: December 1- February 28

Seasons<- c("Spring", "Summer", "Fall", "Winter")

datD$seasons<- ifelse(datD$doy>=60 & datD$doy<152, Seasons[1], ifelse(datD$doy<244, Seasons[2], 
                ifelse(datD$doy<335, Seasons[3], Seasons[4])))

# install.packages("ggplot2")
library(ggplot2)
datD$seasons<- as.factor(datD$seasons)
#make a violin plot
ggplot(data= datD[datD$year==2016,], aes(seasons,discharge)) + 
  geom_violin(fill="lightblue", trim = FALSE,
              alpha= 0.5,
              show.legend= FALSE)+
  xlab("Seasons")+
  ylab(expression(paste("Discharge ft"^"3 ","sec"^"-1", " (Year 2016)")))+
  ggtitle("Violin plot of discharge by season for the year 2016")+
  theme_bw()

## Violin plot 2017 ##

ggplot(data= datD[datD$year==2017,], aes(seasons,discharge)) + 
  geom_violin(fill="lightblue", trim = FALSE,
              alpha= 0.5,
              show.legend= FALSE)+
  xlab("Seasons")+
  ylab(expression(paste("Discharge ft"^"3 ","sec"^"-1", " (Year 2016)")))+
  ggtitle("Violin plot of discharge by season for the year 2017")+
  theme_bw()