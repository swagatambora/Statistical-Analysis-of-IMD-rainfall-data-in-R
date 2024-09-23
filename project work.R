library(readxl)
library(ggplot2)
allfiles<- list()
csv_files1 <- list.files(path = "C:/Users/user/Desktop/r term project/nizambad", pattern = ".csv", full.names = TRUE)
nizambad <- list()
csv_files2 <- list.files(path = "C:/Users/user/Desktop/r term project/jagtial", pattern = ".csv", full.names = TRUE)
jagtiyal <- list()
csv_files3 <- list.files(path = "C:/Users/user/Desktop/r term project/jayashankar", pattern = ".csv", full.names = TRUE)
jayashankar <- list()
csv_files4 <- list.files(path = "C:/Users/user/Desktop/r term project/karimnagar", pattern = ".csv", full.names = TRUE)
karimnagar <- list()
csv_files5 <- list.files(path = "C:/Users/user/Desktop/r term project/peddapalli", pattern = ".csv", full.names = TRUE)
pedapalli <- list()

# Loop through the list of CSV files and read each file

for (csv_file in csv_files1) {
    data <- read.csv(csv_file)
    nizambad[[csv_file]] <- data
}

for (csv_file in csv_files2) {
  data <- read.csv(csv_file)
  jagtiyal[[csv_file]] <- data
}

for (csv_file in csv_files3) {
  data <- read.csv(csv_file)
  jayashankar[[csv_file]] <- data
}

for (csv_file in csv_files4) {
  data <- read.csv(csv_file)
  karimnagar[[csv_file]] <- data
}

for (csv_file in csv_files5) {
  data <- read.csv(csv_file)
  pedapalli[[csv_file]] <- data
}

#to find average of all points of a district
GridSum<- function(csv_files){
  totdata<- data.frame(DateTime= character(),rainfall= numeric() )
  
  for (csv_file in csv_files) {
    data <- read.csv(csv_file)
    totdata<- merge(totdata, data, by = "DateTime", all = TRUE)
  }
  
  totdata$rainfall<- rowSums(totdata[3:ncol(totdata)])
  totdata$rainfall<- totdata$rainfall/length(csv_files)
  return(totdata[,1:2])
}

Nizambad<- as.data.frame(GridSum(csv_files1))
Jagtial<- as.data.frame(GridSum(csv_files2))
Jayashankar_Bhupalpally<- as.data.frame(GridSum(csv_files3))
Karimnagar<- as.data.frame(GridSum(csv_files4))
Pedapalli<- as.data.frame(GridSum(csv_files5))

#function to find statistical measures
statistics<- function(x){
  
  
  #statistical measures
  len<- length(x[,2])
  #mean
  avg1<- mean(x[,2])
  
  #median
  med1<- median(x[,2])
 
  #mode
  freq<- table(x[,2])
  mode0<- as.numeric(names(freq[freq==max(freq)]))
 
  #standard deviation
  std1<- sd(x[,2])
  
  #skewness
  skew1<- sum((x[,2]-avg1)^3)/((len-1)*std1^3)
 
  #kurtosis
  kur1<- sum((x[,2]-avg1)^4)/((len-1)*std1^4)
  
  #interquartile range
  iqr1= IQR(x[,2])
  
  #median absolute deviation
  mad_data<- abs(x[,2]-med1)
  sorted_mad_data<- sort(mad_data)
  if(len%%2==0)
  {
    mad1<- (sorted_mad_data[len/2]+sorted_mad_data[len/2+1])/2
  }else
  {
    mad1<- sorted_mad_data[(len+1)/2]
  }
  
  my_table<-data.frame( NAME=  deparse(substitute(x)), MEAN= avg1, MEDIAN= med1, MODE= mode0, STANDARD_DEVIATION=std1, SKEWNESS= skew1, KURTOSIS= kur1, IQR= iqr1, MAD=mad1)
  
}

tableNZ<- statistics(Nizambad)
tableJG<- statistics(Jagtial)
tableJB<- statistics(Jayashankar_Bhupalpally)
tableKA<- statistics(Karimnagar)
tablePD<- statistics(Pedapalli)
statTable<- rbind(tableNZ, tableJG, tableJB, tableKA, tablePD)


#function to change daily to monthly data
conMonthly<- function(x){
  
  x$year<- strftime(x$DateTime, '%Y')
  x$month<- strftime(x$DateTime, '%m')
  x$monyear<- strftime(x$DateTime, '%Y-%m')
  monthlydata<- aggregate(rainfall~month+year,x,FUN=sum)
  monthlydata$monthyear<- paste(monthlydata$year, monthlydata$month, sep = '-')
  return(monthlydata)
}

#function to change daily to yearly data
conYearly<- function(x){
  
  x$year<- strftime(x$DateTime, '%Y')
  yearlydata<- aggregate(rainfall~year,x,FUN=sum)
  return(yearlydata)
}

library(zoo)
library(lubridate)

#function to monthly to seasonal
conSeas<-function(data){
  data$DateTime <- as.Date(data$DateTime, format = "%Y-%m-%d")
  ts_data <- zoo(data$rainfall, data$DateTime)
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
      total_rainfall <- sum(year_season_data$rainfall, na.rm = TRUE)
      seasonal_data <- rbind(seasonal_data, data.frame(Year = year, Season = season, TotalRainfall = total_rainfall))
    }
  }
  return(seasonal_data)
}

#calculating the y,s,m values
#nizambad
monthNZ<- conMonthly(Nizambad)
yearlyNZ<-conYearly(Nizambad)
seasonalNZ<-conSeas(Nizambad)
#jagtiyal
monthJG<- conMonthly(Jagtial)
yearlyJG<- conYearly(Jagtial)
seasonalJG<-conSeas(Jagtial)
#jayashankar bhagampally
monthJB<- conMonthly(Jayashankar_Bhupalpally)
yearlyJB<- conYearly(Jayashankar_Bhupalpally)
seasonalJB<-conSeas(Jayashankar_Bhupalpally)
#karimangar
monthKN<- conMonthly(Karimnagar)
yearlyKN<- conYearly(Karimnagar)
seasonalKN<-conSeas(Karimnagar)
#pedapalli
monthPP<- conMonthly(Pedapalli)
yearlyPP<- conYearly(Pedapalli)
seasonalPP<-conSeas(Pedapalli)
#function for plotting
plots1<-function(data,monthly_data,yearly_data,seasonal_data){
#daily plot
d<-ggplot(data, aes(x= DateTime, y=rainfall, group =1))+geom_line(color='red')+geom_point(color='blue')+xlab("Time")+ylab("Daily rainfall")+ggtitle(paste("Daily rainfall Time Series",deparse(substitute(data))))

#monthly plot
m<-ggplot(monthly_data, aes(x= monthyear, y=rainfall,group=1))+geom_line(color='green')+geom_point(color='yellow')+xlab("Time")+ylab("Monthly rainfall")+ggtitle(paste("Monthly rainfall Time Series",deparse(substitute(data))))+geom_smooth()
mb<-ggplot(monthly_data, aes(x= month, y=rainfall, fill= month))+geom_boxplot(color= 'black')+ scale_x_discrete(breaks = seq(1,12,by=1))+scale_fill_brewer(palette = "Set3")+ggtitle(paste("Monthly Boxplot",deparse(substitute(data))))

#yearly plot
y<-ggplot(yearly_data, aes(x= year, y=rainfall,group=1))+geom_line(color='orange')+geom_point(color='magenta')+xlab("Time")+ylab("Yearly rainfall")+ggtitle(paste("Yearly rainfall Time Series", deparse(substitute(data))))+ geom_smooth(method= "lm")
#seasonal plot
s<-ggplot(data = seasonal_data, aes(x = Year, y = TotalRainfall, color= Season)) + geom_point()+geom_line()+facet_wrap(~ Season, scales = "free_y")+
labs(title = paste("Seasonal Rainfall Time Series",deparse(substitute(data))), x = "Year",   y = "Total Rainfall") +  scale_color_manual(values = c("Winter" = "blue", "Spring" = "green", "Summer" = "red", "Fall" = "orange")) +theme_minimal()

plotlist<-list(m,mb,y,s)
return(plotlist)
}


#plotting
plotNZ<-plots1(Nizambad,monthNZ,yearlyNZ,seasonalNZ)
plotJG<-plots1(Jagtial,monthJG,yearlyJG,seasonalJG)
plotJB<-plots1(Jayashankar_Bhupalpally,monthJB,yearlyJB,seasonalJB)
plotKN<-plots1(Karimnagar,monthKN,yearlyKN,seasonalKN)
plotPP<-plots1(Pedapalli,monthPP,yearlyPP,seasonalPP)
print(plotNZ)
print(plotJG)
print(plotJB)
print(plotKN)
print(plotPP)

#quantile plots
quantileplots<-function(monthlydata,yearlydata,dailydata){
  
  par(mfrow=c(1,1))
qm<-qqnorm(monthlydata$rainfall)
qqline(monthlydata$rainfall, col='red')
title(sub = paste( "Q-Q Plot of monthly" ,deparse(substitute(dailydata))))
qy<-qqnorm(yearlydata$rainfall)
qqline(yearlydata$rainfall, col='red')
title(sub = paste( "Q-Q Plot of yearly" ,deparse(substitute(dailydata))))
qd<-qqnorm(dailydata$rainfall)
qqline(dailydata$rainfall, col='red')
qlist<-list(qd,qm,qy)
title(sub = paste( "Q-Q Plot of daily" ,deparse(substitute(dailydata))))
return(qlist)
}
qNZ<-quantileplots(monthNZ,yearlyNZ,Nizambad)
qJG<-quantileplots(monthJG,yearlyJG,Jagtial)
qJB<-quantileplots(monthJB,yearlyJB,Jayashankar_Bhupalpally)
qKN<-quantileplots(monthKN,yearlyKN,Karimnagar)
qPP<-quantileplots(monthPP,yearlyPP,Pedapalli)


# Define a custom function to get the second-highest maximum

second_max <- function(x) {
  sorted_x <- sort(x, decreasing = TRUE)
  if (length(sorted_x) >= 2) {
    return(sorted_x[2])
    
  }
}


#find max 

tablemax<- function(data){
  data$year<- strftime(data$DateTime, '%Y')
result <- aggregate(rainfall ~ year, data = data, FUN = function(x) c(Maximum = max(x), Second_Maximum = second_max(x)))

result$year<-as.numeric(result$year)
return(result)
}

plotmax<- function(result){
ggplot(result, aes(x = year)) +  geom_line(aes(y =rainfall[,"Maximum"] , color = "Maximum",group=1), size = 1) +  geom_line(aes(y =  rainfall[,"Second_Maximum"], color = "Second Maximum", group=2), size = 1) +  scale_color_manual(values = c("Maximum" = "red", "Second Maximum" = "blue")) +  labs(x = "Year", y = "RAINFALL", color = "Legend") +  theme_minimal()+ scale_x_continuous(breaks = seq(1950,2021,by=10))+ggtitle(paste("First max and Second max",deparse(substitute(result))))

}

maxNZ<- tablemax(Nizambad)
pmaxNZ<- plotmax(maxNZ)
print(pmaxNZ)
maxJG<- tablemax(Jagtial)
pmaxJG<- plotmax(maxJG)
print(pmaxJG)
maxJB<- tablemax(Jayashankar_Bhupalpally)
pmaxJB<- plotmax(maxJB)
print(pmaxJB)
maxKN<- tablemax(Karimnagar)
pmaxKN<- plotmax(maxKN)
print(pmaxKN)
maxPP<- tablemax(Pedapalli)
pmaxPP<- plotmax(maxPP)
print(pmaxPP)

#correlation between first and second max.
corr<-function(x){
  c<-cor(x$rainfall[,"Maximum"], x$rainfall[,"Second_Maximum"])
  return(c)
}

#probability
calculate_yearly_rainfall_probability <- function(rainfall_data) {
  
  # Extract the year from the date
  rainfall_data$year <- strftime(rainfall_data$DateTime, "%Y")
  
  # Calculate the probability of rainfall for each year
  probability_by_year <- aggregate(rainfall_data$rainfall > 0, by = list(year = rainfall_data$year), FUN = mean)
  
  # Rename the columns for clarity
  colnames(probability_by_year) <- c("Year", "RainfallProbability")
  
  return(probability_by_year)
}

NZprob<- calculate_yearly_rainfall_probability(Nizambad)
ggplot(NZprob, aes(Year,RainfallProbability, group=1))+geom_line()+ geom_point()+ggtitle('Nizambad probability')
JGprob<- calculate_yearly_rainfall_probability(Jagtial)
ggplot(JGprob, aes(Year,RainfallProbability, group=1))+geom_line()+ geom_point()+ggtitle('Jagtiyal probability')
JBprob<- calculate_yearly_rainfall_probability(Jayashankar_Bhupalpally)
ggplot(JBprob, aes(Year,RainfallProbability, group=1))+geom_line()+ geom_point()+ggtitle('Jayashankar Bhupalpally probability')
KNprob<- calculate_yearly_rainfall_probability(Karimnagar)
ggplot(KNprob, aes(Year,RainfallProbability, group=1))+geom_line()+ geom_point()+ggtitle('Karimnagar probability')
PPprob<- calculate_yearly_rainfall_probability(Pedapalli)
ggplot(PPprob, aes(Year,RainfallProbability, group=1))+geom_line()+ geom_point()+ggtitle('Pedapalli probability')

#correlation and plotting between the districts

cor1to1<-function(x,y){
  one2one_dist<-cor(x$rainfall,y$rainfall)
  return(one2one_dist)
}
#Correlation Matrix of all dist.
library(corrplot)
combined_data<-as.data.frame(cbind(Nizambad$rainfall,Jagtial$rainfall,Jayashankar_Bhupalpally$rainfall,Karimnagar$rainfall,Pedapalli$rainfall))
colnames(combined_data)<-c("Nizambad",'Jagtial',"Jayashankar_Bhupalpally","Karimnagar","Pedapalli")
combinedcor<-cor(combined_data)
corrplot(combinedcor,method="number")


# Scatter plot with trend line for yearly data
ggplot(combined_data, aes(x = Nizambad, y = Jagtial)) + geom_point() + geom_smooth(method = "lm", color = "blue") +
  labs(title = "Scatter Plot and Trend Line")
ggplot(combined_data, aes(x = Nizambad, y = Jayashankar_Bhupalpally)) + geom_point() + geom_smooth(method = "lm", color = "blue") +
  labs(title = "Scatter Plot and Trend Line")
ggplot(combined_data, aes(x = Nizambad, y = Karimnagar)) + geom_point() + geom_smooth(method = "lm", color = "blue") +
  labs(title = "Scatter Plot and Trend Line")
ggplot(combined_data, aes(x = Nizambad, y = Pedapalli)) + geom_point() + geom_smooth(method = "lm", color = "blue") +
  labs(title = "Scatter Plot and Trend Line")


#plot riyal#########################################################


lmao<- data.frame()
lmao<- as.data.frame(cbind(yearlyJG$year, yearlyNZ$rainfall, yearlyJG$rainfall))
colnames(lmao)<- c('year', 'Nizambad', 'Jagtial')

# plot(lmao$Nizambad, lmao$Jagtial)
# fitlmao<- lm(Jagtial~Nizambad, lmao)
# abline(fitlmao, col='red')



#mk test__________________________________________
mk_test_results <- kendall.test(yearlyNZ$rainfall)
print(mk_test_results)

# Perform the Mann-Kendall test
mk_test_result <- cor.test(yearlyJG$rainfall, yearlyNZ$rainfall, method = "kendall")

# Print the test result
print(mk_test_result)

# Kendall's rank correlation tau
# 
# data:  yearlyJG$rainfall and yearlyNZ$rainfall
# z = 7.778, p-value = 7.368e-15
# alternative hypothesis: true tau is not equal to 0
# sample estimates:
#       tau 
# 0.6259781 

mk_test_result2 <- cor.test(yearlyPP$rainfall, yearlyKN$rainfall, method = "kendall")

# Print the test result
print(mk_test_result2)

# Kendall's rank correlation tau
# 
# data:  yearlyPP$rainfall and yearlyKN$rainfall
# z = 7.8461, p-value = 4.293e-15
# alternative hypothesis: true tau is not equal to 0
# sample estimates:
#       tau 
# 0.6314554 


# extraction of the monsoon or summer values
Summer<-function(data){
  summer_data <- data[data$Season == "Summer", c("Year", "TotalRainfall")]
  
  return(summer_data)
}

summerNz<-Summer(seasonalNZ)
summerJG<- Summer(seasonalJG)
summerJB<- Summer(seasonalJB)
summerKN<- Summer(seasonalKN)
summerPP<- Summer(seasonalPP)

#statsforseasons
statseas<-function(x){
  m<-statistics(x)
  m<-m[1,]
  m[1,1]<-deparse(substitute(x))
  return(m)
}
seas<-statseas(summerNz)

               #plotfunction
               monsoonplot<-function(x){
                 variable_name <- deparse(substitute(x))
                 ggplot(x, aes(x = Year, y = TotalRainfall)) +
                   geom_line(color = "red",size=1) +
                   geom_point(color = "blue", size = 1)+
                   ggtitle(bquote(paste("Rainfall Over the Years - ", .(variable_name))))+
                   xlab("Year") +
                   ylab("Rainfall (mm)")+scale_x_continuous(breaks = seq(1950,2021,by=5))
               }
print(monsoonplot(summerNz))
print(monsoonplot(summerJG))
print(monsoonplot(summerJB))
print(monsoonplot(summerKN))
print(monsoonplot(summerPP))



#regression________________________________________________________
regNZJG<- cbind(yearlyNZ, yearlyJG$rainfall)
colnames(regNZJG)<- c('year','rainNZ', 'rainJG')
modreg<-lm(rainJG~rainNZ,regNZJG)
ggplot(regNZJG, aes(rainNZ, rainJG))+geom_point(col='blue')+geom_smooth(method = 'lm', col='red')


conMonthlymean<- function(x){
  
  x$year<- strftime(x$DateTime, '%Y')
  x$month<- strftime(x$DateTime, '%m')
  x$monyear<- strftime(x$DateTime, '%Y-%m')
  monthlydataMean<- aggregate(rainfall~month+year,x,FUN=mean)
  monthlydataMean$monthyear<- paste(monthlydataMean$year, monthlydataMean$month, sep = '-')
  return(monthlydataMean)
}
meanMonthRainNZ<-conMonthlymean(Nizambad)

conMonthlySD<- function(x){
  
  x$yearSD<- strftime(x$DateTime, '%Y')
  x$monthSD<- strftime(x$DateTime, '%m')
  x$monyearSD<- strftime(x$DateTime, '%Y-%m')
  monthlydataSD<- aggregate(rainfall~monthSD+yearSD,x,FUN=sd)
  colnames(monthlydataSD)[3]<-'rainSD'
  monthlydataSD$monthyearSD<- paste(monthlydataSD$year, monthlydataSD$month, sep = '-')
  return(monthlydataSD)
}
SDMonthRainNZ<-conMonthlySD(Nizambad)

NZSDMN<- cbind(meanMonthRainNZ,SDMonthRainNZ)
ggplot(NZSDMN, aes(x = year)) +  geom_line(aes(y =rainfall, color='mean rain'  ,group=month), size = 1) +  geom_line(aes(y =  rainSD, color = "SD rain", group=monthSD), size = 1)  + scale_color_manual(values = c("mean rain" = "red", "SD rain" = "blue"))+  labs( y = "RAINFALL", color = "Legend") +  theme_minimal()

#auto correlation__________________________________________________________
install.packages('forecast')
library(forecast)
acf(yearlyNZ$rainfall,main= '"Autocorrelation Function (ACF) Nizambad"')
acf(yearlyJG$rainfall,main= '"Autocorrelation Function (ACF) Jagtiyal"')
acf(yearlyJB$rainfall,main= '"Autocorrelation Function (ACF) Jayashankar_Bhupalpally"')
acf(yearlyKN$rainfall,main= '"Autocorrelation Function (ACF) Karimnagar"')
acf(yearlyPP$rainfall,main= '"Autocorrelation Function (ACF) Pedapalli"')

#PCA_____________________________________________________________________________
combine_riyal<- as.data.frame(cbind(Date=Nizambad$DateTime, combined_data))
SD1<- scale(combine_riyal[,2:6], center = TRUE, scale=TRUE)
y<- cov.wt(SD1)
R <- princomp(combine_riyal[,2:6], scores = TRUE, covmat = y)
v<- ((R$sdev^2)/5)*100
barplot(v, ylim = c(0, 100), col='skyblue')
text(x = barplot(v, plot = FALSE), y = v + 1, label =round(v), pos = 3, cex = 0.8, col = "black")


library(dplyr)

classify_rainfall <- function(rainfall_rate) {
  if (rainfall_rate >= 0.25 & rainfall_rate < 2.5) {
    return("Light Rain")
  } else if (rainfall_rate >= 2.5 & rainfall_rate < 7.6) {
    return("Moderate Rain")
  } else if (rainfall_rate >= 7.6 & rainfall_rate < 25) {
    return("Heavy Rain")
  } else if (rainfall_rate >= 25 & rainfall_rate < 50) {
    return("Very Heavy Rain")
  } else if(rainfall_rate >=50){
    return("Extreme Rain")
  }else{
    return('No Rain')
  }
}

# Apply the classification function to the 'rainfall' column
rainfall_data <- Nizambad %>%
  mutate(Classification = sapply(rainfall, classify_rainfall))

ggplot(rainfall_data, aes(x = Classification, fill=Classification )) +
  geom_bar( color = "black") + scale_fill_brewer(palette = "Set3")+
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) +  
  labs(title = "Rainfall Classification Distribution for Nizambad",
       x = "Rainfall Classification",
       y = "Frequency") +
  theme_minimal()

rainfall_data2 <- Jagtial %>%
  mutate(Classification = sapply(rainfall, classify_rainfall))

ggplot(rainfall_data2, aes(x = Classification, fill=Classification )) +
  geom_bar( color = "black") + scale_fill_brewer(palette = "Set3")+
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) +  
  labs(title = "Rainfall Classification Distribution for Jagtial",
       x = "Rainfall Classification",
       y = "Frequency") +
  theme_minimal()

rainfall_data3 <- Jayashankar_Bhupalpally %>%
  mutate(Classification = sapply(rainfall, classify_rainfall))

ggplot(rainfall_data3, aes(x = Classification, fill=Classification )) +
  geom_bar( color = "black") + scale_fill_brewer(palette = "Set3")+
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) +  
  labs(title = "Rainfall Classification Distribution for Jayashankar_Bhupalpally", x = "Rainfall Classification", y = "Frequency") +
  theme_minimal()

rainfall_data4 <- Karimnagar %>%
  mutate(Classification = sapply(rainfall, classify_rainfall))

ggplot(rainfall_data4, aes(x = Classification, fill=Classification )) +
  geom_bar( color = "black") + scale_fill_brewer(palette = "Set3")+
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) +  
  labs(title = "Rainfall Classification Distribution for Karimnagar",
       x = "Rainfall Classification",
       y = "Frequency") +
  theme_minimal()

rainfall_data5 <- Pedapalli %>%
  mutate(Classification = sapply(rainfall, classify_rainfall))

ggplot(rainfall_data5, aes(x = Classification, fill=Classification )) +
  geom_bar( color = "black") + scale_fill_brewer(palette = "Set3")+
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) +  
  labs(title = "Rainfall Classification Distribution for Pedapalli",
       x = "Rainfall Classification",
       y = "Frequency") +
  theme_minimal()



hiss<- hist(Nizambad$rainfall)
plot(hiss,xaxt="n") 
axis(side="1",at=hiss$breaks,labels = hiss$breaks)
hiss$counts <- hiss$counts/sum(hiss$counts)
plot(hiss,freq=TRUE,ylab="Relative Frequency",xaxt="n", ylim= c(0,1))
axis(side =1,at=hiss$breaks,labels=hiss$breaks)

Rainfall_mm <- yearlyPP$rainfall

# Fit a normal distribution
fit <- fitdistr(your_variable, "normal")

# Print the estimated mean and standard deviation
cat("Estimated mean:", fit$estimate[1], "\n")
cat("Estimated standard deviation:", fit$estimate[2], "\n")

# Plot the histogram of the data and the fitted normal distribution
hist(Rainfall_mm, probability = TRUE, col = "lightblue", main = "Fitted Normal Distribution For Pedapalli")
curve(dnorm(x, mean = fit$estimate[1], sd = fit$estimate[2]), add = TRUE, col = "darkred", lwd = 2)

t_test_result <- t.test(yearlyNZ$rainfall, yearlyJB$rainfall)
print(t_test_result)

# Welch Two Sample t-test
# 
# data:  yearlyNZ$rainfall and yearlyJB$rainfall
# t = -5.3392, df = 136.97, p-value = 3.779e-07
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -331.2103 -152.1798
# sample estimates:
#   mean of x mean of y 
# 989.2699 1230.9650 

t_test_result <- t.test(yearlyNZ$rainfall, yearlyJG$rainfall)
print(t_test_result)

# Welch Two Sample t-test
# 
# data:  yearlyNZ$rainfall and yearlyJG$rainfall
# t = -1.0623, df = 141.76, p-value = 0.2899
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -126.34299   38.01817
# sample estimates:
#   mean of x mean of y 
# 989.2699 1033.4323 



# Extract only the columns with rainfall data
rainfall_cols <- combine_riyal[, 2:6]

# Perform PCA
pca_result <- prcomp(rainfall_cols, center = TRUE, scale. = TRUE)

# Visualize PCA results using ggplot2
fviz_pca_biplot(pca_result, col.ind = "blue", geom.ind = "point",
                col.var = "red", geom.var = c("text","arrow"), alpha.var = 0.5,
                addEllipses = TRUE, label = "var", repel = TRUE, label.var= colnames(rainfall_cols)) +
  theme_minimal() +
  ggtitle("PCA of Daily Rainfall Data for 5 Districts")
