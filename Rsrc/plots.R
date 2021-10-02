library(ggplot2)
library(readxl)
library(data.table)
data.all <- read_excel(path = "data/allDataProc_20012021.xlsx")
data.all <- data.table(data.all)
data.all$Area
data.all$year <- year(data.all$Date)
nYears <- length(unique(data.all$year))

#plot1 nNestMonthFemale
data.all$Date <- as.Date(data.all$Date)
data.all$month <- month(data.all$Date)
data.all$year <- year(data.all$Date)
hist(data.all$month)
meanNnestMonth <- data.all[,sum(Nest)/nYears,by=.(month)]
meanNnestMonth$monthID <- match(meanNnestMonth$month,c(7:12,1:6))
nNestMonth <- data.all[,sum(Nest),by=.(month,year)]
nNestMonth$monthID <- match(nNestMonth$month,c(7:12,1:6))

p <- ggplot(meanNnestMonth, aes(x=as.factor(monthID),y=V1)) + 
  geom_bar(stat="identity") + 
  geom_line(data=nNestMonth, aes(x=monthID,y=V1,col=as.factor(year))) + 
   scale_x_discrete(breaks=as.character(1:12),
    labels=c("Jul","Aug","Sep","Oct","Nov","Dec","Jan","Feb","Mar","Apr","May","Jun"))
p
