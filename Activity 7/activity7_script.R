#### Load relevevant packages ####
library(raster)
library(sp)
library(rgdal)
library(rgeos)
library(mapview)
library(caret)
library(randomForest)
library(nnet)

#set up directory for oneida data folder
dirR <- "~/Documents/GitHub/GEOG331/Activity 7/Data/oneida"

#read in Sentinel data

rdatB2 <- raster(paste0(dirR,"/sentinel/T18TVN_20190814T154911_B02_20m.tif"))
rdatB3 <- raster(paste0(dirR,"/sentinel/T18TVN_20190814T154911_B03_20m.tif"))
rdatB4 <- raster(paste0(dirR,"/sentinel/T18TVN_20190814T154911_B04_20m.tif"))
rdatB5 <- raster(paste0(dirR,"/sentinel/T18TVN_20190814T154911_B05_20m.tif"))
rdatB6 <- raster(paste0(dirR,"/sentinel/T18TVN_20190814T154911_B06_20m.tif"))
rdatB7 <- raster(paste0(dirR,"/sentinel/T18TVN_20190814T154911_B07_20m.tif"))
rdatB8 <- raster(paste0(dirR,"/sentinel/T18TVN_20190814T154911_B08_20m.tif"))
rdatB11 <- raster(paste0(dirR,"/sentinel/T18TVN_20190814T154911_B11_20m.tif"))
rdatB12 <- raster(paste0(dirR,"/sentinel/T18TVN_20190814T154911_B12_20m.tif"))
clouds <- raster(paste0(dirR,"/sentinel/MSK_CLDPRB_20m.tif"))

#read in validation data
#here verbose=FALSE hiddes
algae <- readOGR(paste0(dirR,"/Oneida/algae.shp"), verbose=FALSE)
agri <- readOGR(paste0(dirR,"/Oneida/agriculture.shp"), verbose=FALSE)
built <- readOGR(paste0(dirR,"/Oneida/built.shp"), verbose=FALSE)
forest <- readOGR(paste0(dirR,"/Oneida/forest.shp"), verbose=FALSE)
water <- readOGR(paste0(dirR,"/Oneida/water.shp"), verbose=FALSE)
wetlands <- readOGR(paste0(dirR,"/Oneida/wetlands.shp"), verbose=FALSE)

#stack red green blue
rgbS <- stack(rdatB4,rdatB3,rdatB2)
#stack all raster data
allbands <- stack(rdatB2,rdatB3,rdatB4,rdatB5,rdatB6,rdatB7, rdatB8,rdatB11, rdatB12,clouds)


#set clouds to NA
allbandsCloud <- list()
for(i in 1:9){
  allbandsCloud[[i]] <- setValues(allbands[[i]],
                                  ifelse(getValues(allbands[[10]])>60,NA,getValues(allbands[[i]])))
  
}
allbandsCloudf <- stack(allbandsCloud[[1]],allbandsCloud[[2]],allbandsCloud[[3]],
            allbandsCloud[[4]],allbandsCloud[[5]],allbandsCloud[[6]],
            allbandsCloud[[7]],allbandsCloud[[8]],allbandsCloud[[9]])


##### Answer 1 #####

plotRGB(allbandsCloudf,r=4, g=3, b=2,  
        scale=10000, 
        stretch="lin", 
        margins=TRUE,
        colNA="grey50")

#### #####

#### Machine learning: Random Forest and Neural Network Landcover classification ####

#set seed so samples always the same
set.seed(12153)

#randomly select 
algSamp <- sort(sample(seq(1,120),60))
#set up vector for data type
algData <- rep("train",120)
#randomly replace half of the data to be validating data
algData[algSamp] <- "valid"


waterSamp <- sort(sample(seq(1,120),60))
#set up vector for data type
waterData <- rep("train",120)
#randomly replace half of the data to be validating data
waterData[waterSamp] <- "valid"


agriSamp  <- sort(sample(seq(1,120),60))
#set up vector for data type
agriData <- rep("train",120)
#randomly replace half of the data to be validating data
agriData[agriSamp] <- "valid"


builtSamp  <- sort(sample(seq(1,120),60))
#set up vector for data type
builtData <- rep("train",120)
#randomly replace half of the data to be validating data
builtData[builtSamp] <- "valid"

forestSamp  <- sort(sample(seq(1,120),60))
#set up vector for data type
forestData <- rep("train",120)
#randomly replace half of the data to be validating data
forestData[forestSamp] <- "valid"


wetlandsSamp  <- sort(sample(seq(1,120),60))
#set up vector for data type
wetlandsData <- rep("train",120)
#randomly replace half of the data to be validating data
wetlandsData[wetlandsSamp] <- "valid"

#create id table that gives each landcover an ID
landclass <- data.frame(landcID= seq(1,6),
                        landcover = c("algal bloom", "open water","agriculture","built","forest","wetlands"))

#set up table with coordinates and data type (validate or train) for each point
landExtract <-  data.frame(landcID = rep(seq(1,6),each=120),
  sampleType=c(algData,waterData,agriData,builtData,forestData, wetlandsData),
   x=c(algae@coords[,1],water@coords[,1],agri@coords[,1],built@coords[,1],
      forest@coords[,1],wetlands@coords[,1] ),
  y=c(algae@coords[,2],water@coords[,2],agri@coords[,2],built@coords[,2],
      forest@coords[,2],wetlands@coords[,2] ))


#extract raster data at each point
#using point coordinates
rasterEx <- data.frame(extract(allbandsCloudf,landExtract[,3:4]))
#give names of bands
colnames(rasterEx) <- c("B2","B3","B4","B5","B6","B7","B8","B11","B12")

#combine point information with raster information
dataAll <- cbind(landExtract,rasterEx)

trainD <- dataAll[dataAll$sampleType == "train",]
validD <- dataAll[dataAll$sampleType == "valid",]

#Kfold cross validation
tc <- trainControl(method = "repeatedcv", # repeated cross-validation of the training data
                   number = 10, # number 10 fold
                   repeats = 10) # number of repeats
###random forests
#Typically square root of number of variables
rf.grid <- expand.grid(mtry=1:sqrt(9)) # number of variables available for splitting at each tree node

# Train the random forest model to the Sentinel-2 data
#note that caret:: will make sure we use train from the caret package
rf_model <- caret::train(x = trainD[,c(5:13)], #digital number data
                         y = as.factor(trainD$landcID), #land class we want to predict
                         method = "rf", #use random forest
                         metric="Accuracy", #assess by accuracy
                         trainControl = tc, #use parameter tuning method
                         tuneGrid = rf.grid) #parameter tuning grid

# Change name in raster stack to match training data
names(allbandsCloudf) <- c("B2","B3","B4","B5","B6","B7","B8","B11","B12")
# Apply the random forest model to the Sentinel-2 data
rf_prediction <- raster::predict(allbandsCloudf, model=rf_model)
#view predictions
#plot(rf_prediction)


par(mai= c(0.2,1,0.5,0.2))
#set up categorical colors
landclass$cols <-c("#a6d854","#8da0cb","#66c2a5",
                   "#fc8d62","#ffffb3","#ffd92f")
#make plot and hide legend

#get validation data from raster by extracting 
#cell values at the cell coordinates
rf_Eval <- extract(rf_prediction, validD[,3:4])

#make the confusion matrix
rf_errorM <- confusionMatrix(as.factor(rf_Eval),as.factor(validD$landcID))
#add landcover names
colnames(rf_errorM$table) <- landclass$landcover
rownames(rf_errorM$table) <- landclass$landcover
#view the matrix
# rf_errorM$table

#look at the overall accuracy
# rf_errorM$overall


#set up grid
nnet.grid <- expand.grid(size = seq(from = 16, to = 28, by = 2), 
                  # number of neurons units in the hidden layer 
decay = seq(from = 0.1, to = 0.6, by = 0.1)) # regularization parameter to avoid over-fitting 

nnet_model <- caret::train(x = trainD[,c(5:13)], y = as.factor(trainD$landcID),
                           method = "nnet", metric="Accuracy", trainControl = tc, tuneGrid = nnet.grid,
                           trace=FALSE)

# Apply the neural network model to the Sentinel-2 data
nnet_prediction <- raster::predict(allbandsCloudf, model=nnet_model)

#make plot and hide legend

#extract predictions
nn_Eval = extract(nnet_prediction, validD[,3:4])
#confusion matrix
nn_errorM = confusionMatrix(as.factor(nn_Eval),as.factor(validD$landcID))
colnames(nn_errorM$table) <- landclass$landcover
rownames(nn_errorM$table) <- landclass$landcover
#nn_errorM$table


#### Answer 3 ####

# Make Random Forest classification outcome map

par(mai=c(0.15,0.15,0.15,0.15))
plot(rf_prediction,
     breaks=seq(0,6), 
     col=landclass$cols ,
     legend=FALSE, axes=FALSE)
legend("bottomleft", paste(landclass$landcover[1:2]),
       fill=landclass$cols[1:2] ,bty="n", cex=0.75)
legend("bottom", paste(landclass$landcover[3:4]),
       fill=landclass$cols[3:4] ,bty="n", cex=0.75)
legend("bottomright", paste(landclass$landcover[5:6]),
       fill=landclass$cols[5:6] ,bty="n", cex=0.75)
mtext("Random Forest", side=3,cex=2, line=-5)

# Make Neural Network classification outcome map

par(mai=c(0.2,0.2,0.2,0.2))
plot(nnet_prediction,
     breaks=seq(0,6), 
     col=landclass$cols ,
     legend=FALSE, axes=F)
legend("bottomleft", paste(landclass$landcover[1:2]),
       fill=landclass$cols[1:2] ,bty="n", cex=0.7)
legend("bottom", paste(landclass$landcover[3:4]),
       fill=landclass$cols[3:4] ,bty="n",  cex=0.7)
legend("bottomright", paste(landclass$landcover[5:6]),
       fill=landclass$cols[5:6] ,bty="n",  cex=0.7)
#add title
mtext("Neural Network", side=3,cex=2, line=-5)

#### Answer 4 ####

cell_area= 20*20 # area of each cell or pixel

#cell count random forest
cell_algal_count_rf<- freq(rf_prediction)[1,2] # Number of cells classified under algal bloom
area_algal_rf<- cell_algal_count_rf*cell_area # Area predicted under algal bloom by RF
area_algal_rf

#cell count neural net
cell_algal_count_nnet<- freq(nnet_prediction)[1,2] # Number of cells classified under algal bloom
area_algal_nnet<- cell_algal_count_nnet*cell_area # Area predicted under algal bloom by Nnet
area_algal_nnet

diff_algal<- area_algal_rf-area_algal_nnet # difference in area predictions
diff_algal
abs(diff_algal) #23063600

#### Answer 5 ####

rf_nnet_compare<- abs(rf_prediction-nnet_prediction) # create a raster that shows whether RF and nnet
                                                     # predictions agree or disagree
par(mfrow=c(1,1), mai=c(0.2,0.2,0.2,0.2)) # set relevant plot margins
plot(rf_nnet_compare,
     breaks= c(-0.5, 0.5, 8), # Set break to plot all similarly classified area white and
                              # areas with differences in classifications green
     col= c("white","#a6d854"),
     legend=FALSE, axes=FALSE)
#add legend
legend("bottomleft", c("Predictions agree\n(0)", "Predictions disagree\n(>0)"),
       fill=c("white","#a6d854"), bty="n", horiz=T, cex=0.75)
mtext("Comparing Random Forest and Neural Network Predictions", side=3,cex=2, line=-5) # Set title


#### Answer 6 ####

# Random Forest Method

# Producer's accuracy for algal blooms
correct_algal<- rf_errorM$table[1,1]
total_algal<- sum(rf_errorM$table[,1])

# producer's accuracy= (correct predictions/ total references)*100
pa_algal<- (correct_algal/total_algal)*100 

# User's accuracy for algal blooms
correct_algalu<- rf_errorM$table[1,1]
total_algalu<- sum(rf_errorM$table[1,])

# user's accuracy= (correct predictions/ total predictions)*100
ua_algal<- (correct_algalu/total_algalu)*100

# Producer's accuracy for agriculture
correct_agri<- rf_errorM$table[3,3]
total_agri<- sum(rf_errorM$table[,3])
pa_agri<- (correct_agri/total_agri)*100

# User's accuracy for agriculture
correct_agriu<- rf_errorM$table[3,3]
total_agriu<- sum(rf_errorM$table[3,])
ua_agri<- (correct_agriu/total_agriu)*100


# Neural Network Method

# Producer's accuracy for algal blooms
correct_algalnn<- nn_errorM$table[1,1]
total_algalnn<- sum(nn_errorM$table[,1])
pa_algalnn<- (correct_algalnn/total_algalnn)*100

# User's accuracy for algal blooms
correct_algalunn<- nn_errorM$table[1,1]
total_algalunn<- sum(nn_errorM$table[1,])
ua_algalnn<- (correct_algalunn/total_algalunn)*100

# Producer's accuracy for agriculture
correct_agrinn<- nn_errorM$table[3,3]
total_agrinn<- sum(nn_errorM$table[,3])
pa_agrinn<- (correct_agrinn/total_agrinn)*100

# User's accuracy for agriculture
correct_agriunn<- nn_errorM$table[3,3]
total_agriunn<- sum(nn_errorM$table[3,])
ua_agrinn<- (correct_agriunn/total_agriunn)*100

#### Answer 8 ####

# Calculate producer's and user's accuracy for all landclasses for both methods

rf_pa<- c() # Producer's accuarcy for RF
rf_ua<- c() # User's accuracy for RF
nn_pa<- c() # Producer's accuracy for NN
nn_ua<- c() # User's accuracy for NN


# Calculate using relevant formulae for Producer's accuracy and user's accuracy

# User's accuracy= (correct predictions/ total predictions)*100
for (i in 1:6){ 
  correct_algalrf<- rf_errorM$table[i,i]
  total_algalrf<- sum(rf_errorM$table[,i])
  pa_algalrf<- (correct_algalrf/total_algalrf)*100
  
  rf_pa<- c(rf_pa, pa_algalrf)
  
  correct_algalnn<- nn_errorM$table[i,i]
  total_algalnn<- sum(nn_errorM$table[,i])
  pa_algalnn<- (correct_algalnn/total_algalnn)*100
  
  nn_pa<- c(nn_pa, pa_algalnn)
  
  correct_algalrf<- rf_errorM$table[i,i]
  total_algalrf<- sum(rf_errorM$table[i,])
  ua_algalrf<- (correct_algalrf/total_algalrf)*100
  
  rf_ua<- c(rf_ua, ua_algalrf)
  
  correct_algalunn<- nn_errorM$table[i,i]
  total_algalunn<- sum(nn_errorM$table[i,])
  ua_algalnn<- (correct_algalunn/total_algalunn)*100
  
  nn_ua<- c(nn_ua, ua_algalnn)
}
