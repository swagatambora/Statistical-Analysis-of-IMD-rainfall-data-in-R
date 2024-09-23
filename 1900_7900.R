library(readxl)
data<- read.csv("IMD_QuarterDegree_DailyRainfall_1900_7900.csv")
len<- length(data$DateTime)
len2<- len/2
data1<- data[1:len2,]
data2<- data[(len2+1):len,]
lend1<- length(data1$X19.0.79.0)
lend2<- length(data2$X19.0.79.0)

#statistical measures

#mean
avg1<- mean(data$X19.0.79.0)
avg2<- mean(data1$X19.0.79.0)
avg3<- mean(data2$X19.0.79.0)

#median
med1<- median(data$X19.0.79.0)
med2<- median(data1$X19.0.79.0)
med3<- median(data2$X19.0.79.0)

#mode
freq<- table(data$X19.0.79.0)
mode0<- as.numeric(names(freq[freq==max(freq)]))
freq1<- table(data1$X19.0.79.0)
mode1<- as.numeric(names(freq1[freq1==max(freq1)]))
freq2<- table(data2$X19.0.79.0)
mode2<- as.numeric(names(freq2[freq2==max(freq2)]))

#standard deviation
std1<- sd(data$X19.0.79.0)
std2<- sd(data1$X19.0.79.0)
std3<- sd(data2$X19.0.79.0)

#skewness
skew1<- sum((data$X19.0.79.0-avg1)^3)/((len-1)*std1^3)
skew2<- sum((data1$X19.0.79.0-avg2)^3)/((lend1-1)*std2^3)
skew3<- sum((data2$X19.0.79.0-avg3)^3)/((lend2-1)*std3^3)

#kurtosis
kur1<- sum((data$X19.0.79.0-avg1)^4)/((len-1)*std1^4)
kur2<- sum((data1$X19.0.79.0-avg2)^4)/((lend1-1)*std2^4)
kur3<- sum((data2$X19.0.79.0-avg3)^4)/((lend2-1)*std3^4)

my_table<-data.frame(MEAN= c(avg1, avg2, avg3), MEDIAN= c(med1, med2, med3), MODE= c(mode0, mode1, mode2), STANDARD_DEVIATION=c(std1, std2, std3), SKEWNESS= c(skew1, skew2, skew3), KURTOSIS= c(kur1, kur2, kur3))

#plot time series
library(dplyr)
data$DateTime<- as.Date(data$DateTime, "%d-%m-%Y")
monthly_data <- aggregate(X19.0.79.0~ format(data$DateTime, "%Y-%m"), data = data, sum)
names(monthly_data) <- c("Date", "Monthly_Rainfall")

yearly_data <- aggregate(X19.0.79.0~ format(data$DateTime, "%Y"), data = data, sum)
names(yearly_data) <- c("Date", "Yearly_Rainfall")

library(ggplot2)
library(zoo)
library(lubridate)
#daily plot
ggplot(data, aes(x= DateTime, y=X19.0.79.0, group =1))+geom_line(color='red')+geom_point(color='blue')+xlab("Time")+ylab("Daily rainfall")+ggtitle("Daily rainfall Time Series")

#monthly plot
monthly_data$Date<- as.Date(paste0(monthly_data$Date, '-01'))
monthly_data$Month <- month(monthly_data$Date)
ggplot(monthly_data, aes(x= Date, y=Monthly_Rainfall))+geom_line(color='green')+geom_point(color='yellow')+xlab("Time")+ylab("Monthly rainfall")+ggtitle("Monthly rainfall Time Series")
ggplot(monthly_data, aes(x= Month, y=Monthly_Rainfall, group= Month))+geom_boxplot()+ scale_x_continuous(breaks = seq(1,12,by=1))

#yearly plot
yearly_data$Date<- as.Date(paste0(yearly_data$Date, '-01-01'))
ggplot(yearly_data, aes(x= Date, y=Yearly_Rainfall))+geom_line(color='orange')+geom_point(color='magenta')+xlab("Time")+ylab("Yearly rainfall")+ggtitle("Yearly rainfall Time Series")


#seasonal data
library(zoo)
library(lubridate)
data$DateTime <- as.Date(data$DateTime, format = "%Y-%m-%d")
ts_data <- zoo(data$X19.0.79.0, data$DateTime)
seasonal_data<- data.frame()

#to get the months falling in different seasons
get_season <- function(month) {
  if (month %in% c(12, 1, 2)) {
    return("Winter")
  } else if (month %in% c(3, 4, 5)) {
    return("Spring")
  } else if (month %in% c(6, 7, 8)) {
    return("Summer")
  } else {
    return("Fall")
  }
}
data$Year <- year(data$DateTime)
data$Month <- month(data$DateTime)
data$Season <- sapply(data$Month, get_season)
seasons<- c('Winter', 'Spring', 'Summer', 'Fall')

for (year in unique(data$Year)) {
  for (season in seasons) {
    year_season_data <- subset(data, Year == year & Season == season)
    total_rainfall <- sum(year_season_data$X19.0.79.0, na.rm = TRUE)
    seasonal_data <- rbind(seasonal_data, data.frame(Year = year, Season = season, TotalRainfall = total_rainfall))
  }
}

#plotting seasonal data
ggplot(data = seasonal_data, aes(x = Year, y = TotalRainfall, color = Season)) + geom_point()+geom_line()+
  labs(title = "Seasonal Rainfall Time Series", x = "Year",   y = "Total Rainfall") +  scale_color_manual(values = c("Winter" = "blue", "Spring" = "green", "Summer" = "red", "Fall" = "orange")) +theme_minimal()

#annual daily maximum and second highest maximum
data$Year <- format(data$DateTime, "%Y")
yearly_data$year<- format(yearly_data$Date, '%Y')

# Define a custom function to get the second-highest maximum

second_max <- function(x) {
  sorted_x <- sort(x, decreasing = TRUE)
  if (length(sorted_x) >= 2) {
    return(sorted_x[2])
  
  }
}

# Use the aggregate function to calculate annual maximum and second-highest maximum
result <- aggregate(X19.0.79.0 ~ Year, data = data, FUN = function(x) c(Maximum = max(x), Second_Maximum = second_max(x)))

result$Year<-as.numeric(result$Year)


ggplot(result, aes(x = Year)) +  geom_line(aes(y =X19.0.79.0[,"Maximum"] , color = "Maximum",group=1), size = 1) +  geom_line(aes(y =  X19.0.79.0[,"Second_Maximum"], color = "Second Maximum", group=2), size = 1) +  scale_color_manual(values = c("Maximum" = "red", "Second Maximum" = "blue")) +  labs(x = "Year", y = "RAINFALL", color = "Legend") +  theme_minimal()+ scale_x_continuous(breaks = seq(1950,2021,by=10))
                                                                                                            
#probability of rainfall on any given day for each year

yearly_data$year<- format(yearly_data$Date, '%Y')
years<- yearly_data$year
yearly_probabilities <- data.frame()
yearly_probabilities1.25 <- data.frame()
yearly_probabilities2.5 <- data.frame()

for (year in years) {
  yearly_dataz <- data[data$Year == year, ]
  non_zero_rainfall_years <- sum(yearly_dataz$X19.0.79.0 > 0)
  total_years <- nrow(yearly_dataz)
  probability <- non_zero_rainfall_years / total_years
  yearly_probabilities <- rbind(yearly_probabilities, data.frame(Year=year, Probability= probability))
  
  #for 1.25
  rainfalldays <- sum(yearly_dataz$X19.0.79.0 > 12.5)
  probability1.25 <- rainfalldays / total_years
  yearly_probabilities1.25 <- rbind(yearly_probabilities1.25, data.frame(Year=year, Probability= probability1.25))
  
  #for 2.5
  rainydays <- sum(yearly_dataz$X19.0.79.0 > 25)
  probability2.5 <- rainydays / total_years
  yearly_probabilities2.5 <- rbind(yearly_probabilities2.5, data.frame(Year=year, Probability= probability2.5))
  
}

yearly_probabilities$Year<-as.numeric(yearly_probabilities$Year)
ggplot(yearly_probabilities, aes(x= Year, y=Probability, group=1))+geom_line(color='cyan')+geom_point(color='violet')+xlab("Time")+ylab("Probabilities")+  theme_minimal()+ scale_x_continuous(breaks = seq(1950,2021,by=10))

yearly_probabilities1.25$Year<-as.numeric(yearly_probabilities$Year)
ggplot(yearly_probabilities1.25, aes(x= Year, y=Probability, group=1))+geom_line(color='grey')+geom_point(color='black')+xlab("Time")+ylab("Probabilities")+  theme_minimal()+ scale_x_continuous(breaks = seq(1950,2021,by=10))


yearly_probabilities1.25$Year<-as.numeric(yearly_probabilities$Year)
ggplot(yearly_probabilities1.25, aes(x= Year, y=Probability, group=1))+geom_line(color='green')+geom_point(color='pink')+xlab("Time")+ylab("Probabilities")+  theme_minimal()+ scale_x_continuous(breaks = seq(1950,2021,by=10))

yearly_probabilities00 <- data.frame()
for (year in years) {
  yearly_datau <- data[data$Year == year, ]
  consecutive_rainy_days <- sum(yearly_datau$X19.0.79.0[-1] == 1 & yearly_datau$X19.0.79.0[-nrow(yearly_datau)] == 1)
  total_years <- nrow(yearly_data)
  probability_consecutive_rainy_days <- consecutive_rainy_days / total_years
  yearly_probabilities00 <-rbind(yearly_probabilities00, data.frame(Year=year, Probability= probability_consecutive_rainy_days))
}



