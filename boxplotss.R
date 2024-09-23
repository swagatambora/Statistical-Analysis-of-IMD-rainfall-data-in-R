library(readxl)
library(ggplot2)

csv_files <- list.files(path = "C:/Users/user/Desktop/r term project", pattern = ".csv", full.names = TRUE)
allfiles <- list()

# Loop through the list of CSV files and read each file
for (csv_file in csv_files) {
  data <- read.csv(csv_file)
  allfiles[[csv_file]] <- data
}
for (i in 1:length(allfiles)){
  
plots<- list()
for (i in 1:length(allfiles)){
    #taking files one by one from list 
    dailyrain<- data.frame()
    dailyrain<- allfiles[[i]]
    temp<- dailyrain
    colnames(temp)[2]<- 'Rainfall(mm)'  #change 2nd column name to rainfall
    
    #changing daily data to monthly
    temp$year<- strftime(temp$Date, '%Y')
    temp$month<- strftime(temp$Date, '%m')
    monthlydata<- aggregate(`Rainfall(mm)`~month+year,temp,FUN=sum)
    
    #plotting monthly boxplot
    plot<-ggplot(monthlydata, aes(x=month, y= `Rainfall(mm)`, fill=month))+geom_boxplot()+ ggtitle(colnames(dailyrain)[2])+scale_fill_brewer(palette = "Set3")#scale_fill_manual(values = c('red','magenta', 'pink','green','yellow','orange','violet', 'blue', 'skyblue','purple','darkblue', 'lightgreen'))
    plots[[i]]<- plot
}
}

# dd<-data.frame()
# for (i in 1:length((allfiles))){
#   dd<- allfiles[[i]]
#   #pdf(file= paste0("plot", colnames(dd)[2],'.pdf'))
#   print(plots[[i]])
#   dev.off()
# }













