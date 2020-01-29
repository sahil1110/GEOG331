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