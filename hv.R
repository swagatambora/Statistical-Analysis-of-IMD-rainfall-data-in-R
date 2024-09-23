library(ggplot2)
library(readxl)
rainfall<-read.csv("IMD_QuarterDegree_DailyRainfall_1900_7900.csv")
rain<-as.data.frame(rainfall)
len<-length(rainfall$DateTime)
len2<-len/2
rain_1p<-rain[1:len2,]
rain_2p<-rain[(len2+1):len,]

#statistical measures
mean1<-mean(rain$X18.25.79.75)
mean2<-mean(rain_1p$X18.25.79.75)
mean3<-mean(rain_2p$X18.25.79.75)

#median
med1<-median(rain$X18.25.79.75)
med2<-median(rain_1p$X18.25.79.75)
med3<-median(rain_2p$X18.25.79.75)

#mode.
ft<-table(rain$X18.25.79.75)
mod1<-as.numeric(names(ft[ft==max(ft)]))


ft1<-table(rain_1p$X18.25.79.75)
mod2<-as.numeric(names(ft1[ft1==max(ft1)]))

ft2<-table(rain_2p$X18.25.79.75)
mod3<-as.numeric(names(ft2[ft2==max(ft2)]))

#SD
sd1<-sd(rain$X18.25.79.75)
sd2<-sd(rain_1p$X18.25.79.75)
sd3<-sd(rain_2p$X18.25.79.75)

#kurtosis
k1<- sum((rain$X18.25.79.75-mean1)^4)/((len-1)*(sd1^4))
k2<- sum((rain_1p$X18.25.79.75-mean2)^4)/((len2-1)*(sd2^4))
k3<- sum((rain_2p$X18.25.79.75-mean3)^4)/((len2-1)*(sd3^4))


#skew
skew1<-sum((rain$X18.25.79.75-mean1)^3)/((len-1)*(sd1^3))
skew2<-sum((rain_1p$X18.25.79.75-mean2)^3)/((len2-1)*(sd2^3))
skew3<-sum((rain_2p$X18.25.79.75-mean1)^3)/((len2-1)*(sd3^3))

#in tablular form
t<-data.frame(c(mean1,mean2,mean3),c(med1,med2,med3),c(mod1,mod2,mod3),c(sd1,sd2,sd3),c(k1,k2,k3),c(skew1,skew2,skew3))
colnames(t)<-c("mean","median","mode","Standard Dev","Kurtosis","Skewness")
rownames(t)<-c("Full","1st Half","2nd Half")


#Plotting Timeseries

library(dplyr)
rain$DateTime <- as.Date(rain$DateTime)
monthly_data <- aggregate(X19.0.79.0~ format(rain$DateTime, "%Y-%m"), data = rain, sum)
names(monthly_data) <- c("Date", "Monthly_Rainfall")

ggplot(monthly_data, aes(x = Date, y = Monthly_Rainfall)) +
  geom_line(color="blue") +
  geom_point() +
  xlab("Time") +
  ylab("Monthly Total Rainfall") +
  ggtitle("Monthly Total Rainfall Time Series")