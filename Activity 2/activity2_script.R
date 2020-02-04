heights<- c(30, 41, 20, 22)
heights_cm<- heights*100
heights_cm

heights[1]
heights[2:3]

Mat<- matrix(c(1,2,3,4,5,6), ncol=2, byrow=T)
Mat

Mat.bycol<-matrix(c(1,2,3,4,5,6), ncol=2)
Mat.bycol

Mat.bycol[1,2]
Mat.bycol[1,]
Mat.bycol[,1]

datW<- read.csv("Y:\\Students\\slalwani\\a02\\2011124.csv")
nrow(datW)
ncol(datW)

#specify a column with a proper date format
#note the format here dataframe$column
datW$dateF <- as.Date(datW$DATE, "%Y-%m-%d")
#google date formatting in r to find more options and learn more

#create a date column by reformatting the date to only include years
#and indicating that it should be treated as numeric data
datW$year <- as.numeric(format(datW$dateF,"%Y"))

#find out all unique site names
levels(datW$NAME)

#look at the mean maximum temperature for Aberdeen
mean(datW$TMAX[datW$NAME == "ABERDEEN, WA US"]) #gives NA because there is missing data in dataset
mean(datW$TMAX[datW$NAME == "ABERDEEN, WA US"], na.rm = T)

#calculate the average daily temperature
#This temperature will be halfway between the minimum and maximum temperature
datW$TAVE <- datW$TMIN + ((datW$TMAX-datW$TMIN)/2)

#get the mean across all sites
#the by function is a list of one or more variables to index over.
#FUN indicates the function we want to use
#if you want to specify any function specific arguments use a comma and add them after the function
#here we want to use the na.rm arguments specific to 
averageTemp <- aggregate(datW$TAVE, by=list(datW$NAME), FUN="mean",na.rm=TRUE)
averageTemp

#change the automatic output of column names to be more meaningful
#note that MAAT is a common abbreviation for Mean Annual Air Temperature
colnames(averageTemp) <- c("NAME","MAAT")
averageTemp

#convert level to number for factor data type
#you will have to reference the level output or look at the row of data to see the character designation.
datW$siteN <- as.numeric(datW$NAME)

## Question 4

par(mfrow= c(2,2))
#make a histogram for the first site in our levels, Aberdeen
#main= is the title name argument.
#Here you want to paste the actual name of the factor not the numeric index
#since that will be more meaningful. 
h1<-hist(datW$TAVE[datW$siteN == 1],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[1]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="grey50",
     border="white")
#add mean line with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE), 
       col = "tomato3",
       lwd = 3)
#add standard deviation line below the mean with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE) - sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)
#add standard deviation line above the mean with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE) + sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)

## Site 2

#make a histogram for the second site in our levels, Livermore
#main= is the title name argument.
#Here you want to paste the actual name of the factor not the numeric index
#since that will be more meaningful. 
h2<-hist(datW$TAVE[datW$siteN == 2],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[2]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="black",
     border="white")
#add mean line with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 2],na.rm=TRUE), 
       col = "tomato3",
       lwd = 3)
#add standard deviation line below the mean with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 2],na.rm=TRUE) - sd(datW$TAVE[datW$siteN == 2],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)
#add standard deviation line above the mean with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 2],na.rm=TRUE) + sd(datW$TAVE[datW$siteN == 2],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)

## Site 3

#make a histogram for the third site in our levels, Mandan Experiment Station
#main= is the title name argument.
#Here you want to paste the actual name of the factor not the numeric index
#since that will be more meaningful. 
h3<-hist(datW$TAVE[datW$siteN == 3],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[3]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="blue",
     border="white")
#add mean line with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 3],na.rm=TRUE), 
       col = "tomato3",
       lwd = 3)
#add standard deviation line below the mean with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 3],na.rm=TRUE) - sd(datW$TAVE[datW$siteN == 2],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)
#add standard deviation line above the mean with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 3],na.rm=TRUE) + sd(datW$TAVE[datW$siteN == 2],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)

## Site 4

#make a histogram for the second site in our levels, Mormon Flat
#main= is the title name argument.
#Here you want to paste the actual name of the factor not the numeric index
#since that will be more meaningful. 
h4<-hist(datW$TAVE[datW$siteN == 4],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[4]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="lightgray",
     border="white")
#add mean line with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 4],na.rm=TRUE), 
       col = "tomato3",
       lwd = 3)
#add standard deviation line below the mean with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 4],na.rm=TRUE) - sd(datW$TAVE[datW$siteN == 2],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)
#add standard deviation line above the mean with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 4],na.rm=TRUE) + sd(datW$TAVE[datW$siteN == 2],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)


## Question 5
# the seq function generates a sequence of numbers that we can use to plot the normal across the range of temperature
# values

dev.off()
par(mfrow=c(2,2))

h1<-hist(datW$TAVE[datW$siteN == 1],
         freq=FALSE, 
         main = paste(levels(datW$NAME)[1]),
         xlab = "Average daily temperature (degrees C)", 
         ylab="Relative frequency",
         col="grey50",
         border="white")

x.plot <- seq(-10,30, length.out = 100)
#the dnorm function will produce the probability density based on a mean and standard deviation.

y.plot <-  dnorm(seq(-10,30, length.out = 100),
                 mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
                 sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))

#create a density that is scaled to fit in the plot since the density has a different range from the data density.
#!!! this is helpful for putting multiple things on the same plot
#!!! It might seem confusing at first. It means the maximum value of the plot is always the same between the two
# datasets on the plot. Here both plots share zero as a minimum.

y.scaled <- (max(h1$density)/max(y.plot)) * y.plot

#points function adds points or lines to a graph
#the first two arguments are the x coordinates and the y coordinates.

points(x.plot,
       y.scaled, 
       type = "l", 
       col = "royalblue3",
       lwd = 4, 
       lty = 2)

## Site 2

h2<-hist(datW$TAVE[datW$siteN == 2],
         freq=FALSE, 
         main = paste(levels(datW$NAME)[2]),
         xlab = "Average daily temperature (degrees C)", 
         ylab="Relative frequency",
         col="grey50",
         border="white")

x.plot <- seq(-10,40, length.out = 100)
#the dnorm function will produce the probability density based on a mean and standard deviation.

y.plot <-  dnorm(seq(-10,40, length.out = 100),
                 mean(datW$TAVE[datW$siteN == 2],na.rm=TRUE),
                 sd(datW$TAVE[datW$siteN == 2],na.rm=TRUE))

#create a density that is scaled to fit in the plot since the density has a different range from the data density.
#!!! this is helpful for putting multiple things on the same plot
#!!! It might seem confusing at first. It means the maximum value of the plot is always the same between the two
# datasets on the plot. Here both plots share zero as a minimum.

y.scaled <- (max(h2$density)/max(y.plot)) * y.plot

#points function adds points or lines to a graph
#the first two arguments are the x coordinates and the y coordinates.

points(x.plot,
       y.scaled, 
       type = "l", 
       col = "royalblue3",
       lwd = 4, 
       lty = 2)

## Site 3

h3<-hist(datW$TAVE[datW$siteN == 3],
         freq=FALSE, 
         main = paste(levels(datW$NAME)[3]),
         xlab = "Average daily temperature (degrees C)", 
         ylab="Relative frequency",
         col="grey50",
         border="white")

x.plot <- seq(-40,40, length.out = 100)
#the dnorm function will produce the probability density based on a mean and standard deviation.

y.plot <-  dnorm(seq(-40,40, length.out = 100),
                 mean(datW$TAVE[datW$siteN == 3],na.rm=TRUE),
                 sd(datW$TAVE[datW$siteN == 3],na.rm=TRUE))

#create a density that is scaled to fit in the plot since the density has a different range from the data density.
#!!! this is helpful for putting multiple things on the same plot
#!!! It might seem confusing at first. It means the maximum value of the plot is always the same between the two
# datasets on the plot. Here both plots share zero as a minimum.

y.scaled <- (max(h3$density)/max(y.plot)) * y.plot

#points function adds points or lines to a graph
#the first two arguments are the x coordinates and the y coordinates.

points(x.plot,
       y.scaled, 
       type = "l", 
       col = "royalblue3",
       lwd = 4, 
       lty = 2)

## Site 4

h4<-hist(datW$TAVE[datW$siteN == 4],
         freq=FALSE, 
         main = paste(levels(datW$NAME)[4]),
         xlab = "Average daily temperature (degrees C)", 
         ylab="Relative frequency",
         col="grey50",
         border="white")

x.plot <- seq(-40,40, length.out = 100)
#the dnorm function will produce the probability density based on a mean and standard deviation.

y.plot <-  dnorm(seq(-40,40, length.out = 100),
                 mean(datW$TAVE[datW$siteN == 4],na.rm=TRUE),
                 sd(datW$TAVE[datW$siteN == 4],na.rm=TRUE))

#create a density that is scaled to fit in the plot since the density has a different range from the data density.
#!!! this is helpful for putting multiple things on the same plot
#!!! It might seem confusing at first. It means the maximum value of the plot is always the same between the two
# datasets on the plot. Here both plots share zero as a minimum.

y.scaled <- (max(h4$density)/max(y.plot)) * y.plot

#points function adds points or lines to a graph
#the first two arguments are the x coordinates and the y coordinates.

points(x.plot,
       y.scaled, 
       type = "l", 
       col = "royalblue3",
       lwd = 4, 
       lty = 2)

## Question 6
extreme<- qnorm(0.95, mean(datW$TAVE[datW$siteN==1], na.rm=T), sd(datW$TAVE[datW$siteN==1], na.rm=T))
1-pnorm(extreme, mean(datW$TAVE[datW$siteN==1], na.rm=T)+4, sd(datW$TAVE[datW$siteN==1], na.rm=T))
# We will expect to observe temperatures greater than the current threshold for extreme high temperatures (18.51026)
# 20.31%

## Question 7
dev.off()
plot.new()
hist(datW$PRCP[datW$siteN == 1],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[1]),
     xlab = "Daily Precipitation (Inches)", 
     ylab="Relative frequency",
     col="grey50",
     border="white") # Exponential distribution?
                     # Shorten x-axis range

## Question 8

prcp_year<- aggregate(datW$PRCP, by=list(as.factor(datW$year)), FUN="sum", na.rm=T)
prcp_site<- aggregate(datW$PRCP, by=list(datW$NAME), FUN="sum", na.rm=T)

prcp_site1<- aggregate(datW$PRCP[datW$siteN==1], by=list(subset(datW, datW$siteN==1)$year), 
                       FUN="sum", na.rm=T)
prcp_site2<- aggregate(datW$PRCP[datW$siteN==2], by=list(subset(datW, datW$siteN==2)$year), 
                       FUN="sum", na.rm=T)
prcp_site3<- aggregate(datW$PRCP[datW$siteN==3], by=list(subset(datW, datW$siteN==3)$year), 
                       FUN="sum", na.rm=T)
prcp_site4<- aggregate(datW$PRCP[datW$siteN==4], by=list(subset(datW, datW$siteN==4)$year), 
                       FUN="sum", na.rm=T)
prcp_site5<- aggregate(datW$PRCP[datW$siteN==5], by=list(subset(datW, datW$siteN==5)$year), 
                       FUN="sum", na.rm=T)

#Histogram for ABERDEEN, WA US

h11<-hist(prcp_site1$x,
     freq=FALSE, 
     main = paste(levels(datW$NAME)[1]),
     xlab = "Annual precipitation (in inches)", 
     ylab="Relative frequency",
     col="grey50",
     border="white") # roughly normally distributed

#add mean line with red (tomato3) color
#and thickness of 3
abline(v = mean(prcp_site1$x,na.rm=TRUE), 
       col = "tomato3",
       lwd = 3)
#add standard deviation line below the mean with red (tomato3) color
#and thickness of 3
abline(v = mean(prcp_site1$x,na.rm=TRUE) - sd(prcp_site1$x,na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)
#add standard deviation line above the mean with red (tomato3) color
#and thickness of 3
abline(v = mean(prcp_site1$x,na.rm=TRUE) + sd(prcp_site1$x,na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)

x.plot <- seq(1000,3000, length.out = 500)
#the dnorm function will produce the probability density based on a mean and standard deviation.

y.plot <-  dnorm(seq(1000,3000, length.out = 500),
                 mean(prcp_site1$x,na.rm=TRUE),
                 sd(prcp_site1$x,na.rm=TRUE))

#create a density that is scaled to fit in the plot since the density has a different range from the data density.
#!!! this is helpful for putting multiple things on the same plot
#!!! It might seem confusing at first. It means the maximum value of the plot is always the same between the two
# datasets on the plot. Here both plots share zero as a minimum.

y.scaled <- (max(h11$density)/max(y.plot)) * y.plot

#points function adds points or lines to a graph
#the first two arguments are the x coordinates and the y coordinates.

points(x.plot,
       y.scaled, 
       type = "l", 
       col = "royalblue3",
       lwd = 4, 
       lty = 2)

# Anything more for Q8?

## Question 9

meanprcp_site1<- mean(prcp_site1$x)
meanprcp_site2<- mean(prcp_site2$x)
meanprcp_site3<- mean(prcp_site3$x)
meanprcp_site4<- mean(prcp_site4$x)
meanprcp_site5<- mean(prcp_site5$x)

mean_annual_prcp<- data.frame()
mean_annual_prcp<- cbind("Number"= 1:5, "Name"= levels(datW$NAME),
                "Annual precipitation mean"= c(meanprcp_site1, meanprcp_site2, meanprcp_site3,
                                               meanprcp_site4, meanprcp_site5))

mean_annual_prcp<- as.data.frame(mean_annual_prcp)
mean_annual_prcp$`Annual precipitation mean`<- as.numeric(as.character
                                (mean_annual_prcp$`Annual precipitation mean`))

# 