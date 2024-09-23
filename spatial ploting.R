#SPATIAL PLOTS______________________________

library(readxl)
library(ggplot2)


csv_files <- list.files(path = "C:/Users/user/Desktop/r term project", pattern = ".csv", full.names = TRUE)
allfiles <- list()

# Loop through the list of CSV files and read each file
for (csv_file in csv_files) {
  data <- read.csv(csv_file)
  data$latitude<- as.numeric(sub("^X([0-9]+\\.[0-9]+).*", "\\1", colnames(data)[2]))
  data$longitude<- as.numeric(sub("^X[0-9.]+\\.([0-9]+\\.[0-9]+)$", "\\1", colnames(data)[2]))
  colnames(data)[2]<- 'Rainfall'
  allfiles[[csv_file]] <- data
}

#mean
meanrain<- data.frame()
for (i in seq_along(allfiles)){
  meanR<- mean(allfiles[[i]]$Rainfall)
  meanra<-unique(data.frame(Mean= meanR, latitude=allfiles[[i]]$latitude, longitude= allfiles[[i]]$longitude))
  meanrain<- rbind(meanrain, meanra)
}


library(ggplot2)
library(sf)

# Sample data frame with latitude, longitude, and rainfall
# Create an sf data frame
sf_data <- st_as_sf(meanrain, coords = c("longitude", "latitude"), crs = 4326)



# Plot the spatial data using ggplot2
ggplot() +
  geom_sf(data = sf_data, aes(color = Mean), size = 2) +
  scale_color_gradient(name = "Mean Rainfall", low = "orange", high = "blue") +
  labs(title = "Spatial Plot of Mean Rainfall Data")


#for SD
sdrain<- data.frame()
for (i in seq_along(allfiles)){
  sdR<- sd(allfiles[[i]]$Rainfall)
  sdra<-unique(data.frame(SD= sdR, latitude=allfiles[[i]]$latitude, longitude= allfiles[[i]]$longitude))
  sdrain<- rbind(sdrain, sdra)
}

sf_data2 <- st_as_sf(sdrain, coords = c("longitude", "latitude"), crs = 4326)



# Plot the spatial data using ggplot2
ggplot() +
  geom_sf(data = sf_data2, aes(color = SD), size = 2) +
  scale_color_gradient(name = "SD Rainfall", low = "orange", high = "blue") +
  labs(title = "Spatial Plot of SD Rainfall Data")

#Skewness
library(e1071)
skrain<- data.frame()
for (i in seq_along(allfiles)){
  skR<- skewness(allfiles[[i]]$Rainfall)
  skra<-unique(data.frame(SK= skR, latitude=allfiles[[i]]$latitude, longitude= allfiles[[i]]$longitude))
  skrain<- rbind(skrain, skra)
}

sf_data3 <- st_as_sf(skrain, coords = c("longitude", "latitude"), crs = 4326)



# Plot the spatial data using ggplot2
ggplot() +
  geom_sf(data = sf_data3, aes(color = SK), size = 2) +
  scale_color_gradient(name = "Skewness Rainfall", low = "orange", high = "blue") +
  labs(title = "Spatial Plot of Skewness Rainfall Data")

#kurtosis
kurrain<- data.frame()
for (i in seq_along(allfiles)){
  kR<- kurtosis(allfiles[[i]]$Rainfall)
  kra<-unique(data.frame(KUR= kR, latitude=allfiles[[i]]$latitude, longitude= allfiles[[i]]$longitude))
  kurrain<- rbind(kurrain, kra)
}

sf_data4 <- st_as_sf(kurrain, coords = c("longitude", "latitude"), crs = 4326)



# Plot the spatial data using ggplot2
ggplot() +
  geom_sf(data = sf_data4, aes(color = KUR), size = 2) +
  scale_color_gradient(name = "Kurtosis Rainfall", low = "orange", high = "blue") +
  labs(title = "Spatial Plot of kurtosis Rainfall Data")


