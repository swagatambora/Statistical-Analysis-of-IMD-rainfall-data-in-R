library(readxl)
library(ggplot2)
data<- read.csv("IMD_QuarterDegree_DailyRainfall_1900_7900.csv")
colnames(data)[2]<- 'Rainfall(mm)' 

#split the data into n numbers
splitData<- function(x, n){
  splits <- vector("list", length = n)
  rows_per_split <- nrow(x) %/% n
  
  for (i in 1:n) {
    start <- (i - 1) * rows_per_split + 1
    end <- ifelse(i == n, nrow(x), i * rows_per_split)
    splits[[i]] <- x[start:end, ]
  }
  
  return(splits)
}

#function to change daily to monthly data
conMonthly<- function(x){
  
  x$year<- strftime(x$DateTime, '%Y')
  x$month<- strftime(x$DateTime, '%m')
  monthlydata<- aggregate( `Rainfall(mm)`~month+year,x,FUN=sum)
  return(monthlydata)
  
}

conYearly<- function(x){
  
  x$year<- strftime(x$DateTime, '%Y')
  monthlydata<- aggregate( `Rainfall(mm)`~year,x,FUN=sum)
  return(monthlydata)
  
}
  
datauu<- splitData(data,2)
data1<- datauu[[1]]
data2<- datauu[[2]]

monthly1<- conMonthly(data1)
monthly2<- conMonthly(data2)

#function for plotting monthly boxplot
plotmonthly<- function(x){
  plot<- ggplot(x, aes(x=month, y= `Rainfall(mm)`, group=month))+geom_boxplot(color= 'blue', fill= 'grey')
  return(plot)
}
second_max <- function(x) {
  sorted_x <- sort(x, decreasing = TRUE)
  if (length(sorted_x) >= 2) {
    return(sorted_x[2])
    
  }
}

result <- aggregate(`Rainfall(mm)` ~ year, data = data1, FUN = function(x) c(Maximum = max(x), Second_Maximum = second_max(x)))

result$Year<-as.numeric(result$Year)


ggplot(result, aes(x = Year)) +  geom_line(aes(y =`Rainfall(mm)`[,"Maximum"] , color = "Maximum",group=1), size = 1) +  geom_line(aes(y =  `Rainfall(mm)`[,"Second_Maximum"], color = "Second Maximum", group=2), size = 1) +  scale_color_manual(values = c("Maximum" = "red", "Second Maximum" = "blue")) +  labs(x = "Year", y = "RAINFALL", color = "Legend") +  theme_minimal()




print(plotmonthly(monthly1))
print(plotmonthly(monthly2))







