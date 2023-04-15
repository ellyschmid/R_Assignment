##########################################################################
library(sf)
library(dplyr)
library(circlize)
library(rasterVis)
library(RColorBrewer)
#script for postprocessing and visualizations 
#list for class names of raster
classes <- list("low salinity", "low medium salinity", "medium salinity",
                "medium high salinity", "high salinity")

#list of years of rasters
year <- list("2000", "2005", "2010", "2015", "2022")


df_list <- data.frame(t(sapply(landsat_list4,c)))

#add Year, Class and Area of classes for each year
i = 1
df_list <- list()
for (l in landsat_list4){
  l <- as.data.frame(l) %>%
    group_by(value) %>%
    tally() %>%
    mutate(area = n * res(l)[1] * res(l)[2]) %>%
    na.omit(l)
  
  
  l$Areakm2 <- with(l, area/1000000)
  l$Class <- classes
  l$Year <- year[i] 
  
  i = i+1
  df_list[[length(df_list) +1]] <- l
}

#bind dataframes two one, for later use of e.g visualizations 
df_change <- cbind(df_list[[1]], df_list[[5]])
df_total <- rbind(df_list[[1]], df_list[[2]], df_list[[3]], df_list[[4]], df_list[[5]])

#create a tranistion raster to identify which classes switchted to another between 
#two time-steps by multiplying first year by 100 and adding last year
transition_raster <- (landsat_list4[[1]] * 100) + landsat_list4[[5]]

#create dataframe out of transition raster and calculate the area 
df_transition <- as.data.frame(transition_raster) %>% 
  na.omit(df_tranisition) %>%
  group_by(layer) %>%
  tally %>%
  mutate(area = n * res(landsat_list4[[1]])[1] * res(landsat_list4[[1]])[1]) 

#get area in km2  
df_transition$Areakm2 <- with(df_transition, area/1000000)

#chord diagramm to visualize the class transitions between the years
#preparing of the dataframe to get two colums of the transition number and renaming
#them corresponding to the class names of the classification
df_trans <- df_transition
for(row in 1:nrow(df_transition)){
  
  v <-as.character(df_transition$layer[[row]])
  
  l_first <- substr(v,1,1) 
  l_last <- substr(v,3,3)
  
  df_trans$first[row] <- l_first
  df_trans$last[row] <- l_last
  
  df_trans$first[df_trans$first == "1"] <- "low"
  df_trans$first[df_trans$first == "2"] <- "medium_low"
  df_trans$first[df_trans$first == "3"] <- "medium"
  df_trans$first[df_trans$first == "4"] <- "medium_high"
  df_trans$first[df_trans$first == "5"] <- "high"
  
  df_trans$last[df_trans$last == "1"] <- "low_salinity"
  df_trans$last[df_trans$last == "2"] <- "medium_low_salinity"
  df_trans$last[df_trans$last == "3"] <- "medium_salinity"
  df_trans$last[df_trans$last == "4"] <- "medium_high salinity"
  df_trans$last[df_trans$last == "5"] <- "high_salinity"
}

#select columns for chord
df_trans <- df_trans %>% select(first,last, Areakm2)

#chord diagramm 
grid.col = c(low = "green", medium_low = "yellow", medium = "orange",
             medium_high = "red", high = "red4")

#jpeg(file="C:\\Users\\ellys\\Desktop\\R_Assignment\\class_transitions.jpeg",
#     width=900, height=900, res = 200)
class_transitions <- chordDiagram(df_trans, grid.col = grid.col, scale = T)
#dev.off()

###############################################################################
#visualization of the classified rasters for each year 
#create stack of classified images from list
mystack <- stack(landsat_list4[[1]], landsat_list4[[2]], landsat_list4[[3]],
                 landsat_list4[[4]], landsat_list4[[5]])
names(mystack)
#set names of rasters in stack correspondingly to the year
mystack <- setNames(mystack, c("2000", "2005","2010", "2015", "2022"))
rasterNames  <- gsub("X","Year ", names(mystack))
#reprojection to lat/long
#mystack <- projectRaster(from = mystack, crs = "+init=epsg:4326")

#Palettes for visualization
myPalette <- colorRampPalette(rev(brewer.pal(5,"RdYlBu")))
myPalette2 <- colorRampPalette(rev(brewer.pal(6,"RdBu")))

#plot salinity time-series
#jpeg(file="C:\\Users\\ellys\\Desktop\\R_Assignment\\salinity_timeseries.jpeg",
#width=1200, height=800, res = 100)
levelplot(mystack,
          main="Salinity Change in Lake Urmia \nbetween 2005-2022",
          col.regions=myPalette,
          names.attr=rasterNames)
#dev.off()


#create change raster betwen first and last year of images
r_change <- landsat_list4[[5]] - landsat_list4[[1]]
r_change <- as.factor(r_change)
rat <- levels(r_change)[[1]]
rat[["change"]] <- c("-2","-1", "0","1","2","3","4")
levels(r_change) <- rat

#plot raster
#jpeg(file="C:\\Users\\ellys\\Desktop\\R_Assignment\\salinity_change.jpeg",
#     width=900, height=900, res = 200)
levelplot(r_change, main="Change in Salinity Classes \nbetween 2000 and 2022",
           col.regions=myPalette(7), xlab="", ylab="")
#dev.off()


