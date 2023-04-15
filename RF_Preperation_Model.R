########################################################################
#Import libraries
library(raster)
library(RStoolbox)
library(caret)
library(sf)

#set seed for reproducibility 
set.seed(1405)

#Set working directory for loading data
wd_in <- setwd("C:\\Users\\ellys\\Desktop\\R_Assignment\\input")

#Load Training Data 
training <- st_read("training\\validation_sal_urmia.shp")

#Load Landsat-Time-Series Images
landsat00 = brick("LandsatUrmai2000.tif")
landsat05 = brick("LandsatUrmai2005.tif")
landsat10 = brick("LandsatUrmai2010.tif")
landsat15 = brick("LandsatUrmai2015.tif")
landsat22 = brick("LandsatUrmai2022.tif")

#load aoi
urmia <- st_read("urmia.shp")
urmia <- st_transform(urmia, st_crs(landsat15))

# create list of landsat images for iteration
landsat_list <-  list(landsat00, landsat05, landsat10, landsat15, landsat22)

#string for renaming bands
bands <- c('B', 'G', 'R', 'NIR', 'SWIR1', 'SWIR2')

#For loop for masking image to lake basin area
#renaming bands 
#and apliying indices to all landsat images
#Errors might appear (Error in x$.self$finalize() : attempt to apply non-function) 
#but they can be ignored (can be resolved by uploading raster/Terra packages) or
#run part after this from second for loop
#may take a little while 
landsat_list2 <- list()
#i <- 0 #testing with one input
for(img in landsat_list) {
  # section for test run
  #  if (i > 0) {
  #    next
  #  }
  #  i <- i + 1
  img <- mask(img,urmia)
  names(img) <- bands
  
  img$ndvi <- ndvi(img, 3, 4)
  img$ndsi <- ndsi(img, 3, 4)
  img$ndii <- ndii(img, 4, 5)
  img$csri <- csri(img, 1, 2, 3, 4, 7)
  img$si_T <- si_T(img, 3, 4)
  img$si_1 <- si_1(img, 4, 5)
  img$si_2 <- si_2(img, 1, 3)
  img$si_3 <- si_3(img, 1, 2, 3)
  img$vssi <- vssi(img, 2, 3, 4)
  img$ndwi <- ndwi(img, 2, 5)
  #  img$water <- watermask(img, 7, 16)
  #  waterm <- (img$water)
  #  waterm[waterm != 0] <- NA
  #  img <- mask(img, waterm)
  
  img <- normImage(img)
  img <- rescaleImage(img, ymin = 0, ymax = 1)
  landsat_list2[[length(landsat_list2) +1]] <- img
}

#Select bands for classification by subsetting rasters
landsat_list3 <- list()

for (img2 in landsat_list2){
  img2 <-img2[[c(1:15)]]
  landsat_list3[[length(landsat_list3) +1]] <- img2
}

###############################################################################
###############################################################################
#Random forest classifier trained on 2015 to estimate salinity classes 
training <- st_transform(training, st_crs(landsat_list3[[4]]))

#create response variable for classifier
training$resp_var <- training$TEST

training_points <- list()
for(i in unique(training$resp_var)){
  message(paste0("Sampling points from polygons with resp_var=", i))
  
  # sample points for polygons of resp_var = i
  training_points[[i]] <- st_sample(
    x = training[training$resp_var == i,], 
    size = 100
  )
  training_points[[i]] <- st_as_sf(training_points[[i]])
  training_points[[i]]$resp_var <- i
}
training_points <- do.call(rbind, training_points)

# extract features from training image and label them with the variable!
unlabeled_features <- raster::extract(landsat_list3[[4]], training_points, df = T)
unlabeled_features <- unlabeled_features[,-1] # no ID column needed
training_features <- cbind(
  resp_var = training_points$resp_var,
  unlabeled_features
)

# check for dublicates in point samples and remove them
dupl <- duplicated(training_features)
training_features <- training_features[!dupl,]
#training_features <- na.omit(training_features)

# x = features
x <- training_features[,2:ncol(training_features)] # remove ID column
y <- as.factor(training_features$resp_var) #we want caret to treat this as categories, thus factor
levels(y) <- paste0("class_", levels(y))

# fit the model, here a ranodm Forest
model <- train(
  x = x,
  y = y, 
  trControl = trainControl(
    p = 0.75, # percentage of samples used for training, rest for validation
    method  = "cv", #cross validation
    number  = 5, # 5-fold
    verboseIter = TRUE, # progress update per iteration
    classProbs = TRUE # probabilities for each example
  ),
  #preProcess = c("center", "scale"), #center/scale if not done by you on the raster (see previous code rescl)
  method = "rf" # used algorithm
) 

# performance
model
confusionMatrix(model)

#predict raster's
landsat_list4 <- list()
#section for test run
#i <- 0
for (img3 in landsat_list3){
  #  if (i > 0) {
  #    next
  #  }
  #  i <- i + 1
  img3 <- predict(img3, model, type='raw')
  landsat_list4[[length(landsat_list4) +1]] <- img3
}

landsat_list4 <- setNames(landsat_list4, c("landsat00", "landsat05","landsat10", "landsat15", "landsat22"))

# working directory for saving rasters and writing of rasters in file
#setwd("C:\\Users\\ellys\\Desktop\\R_Assign\\output")
#writeRaster(stack(landsat_list4), names(landsat_list4), bylayer=TRUE, format='GTiff')
