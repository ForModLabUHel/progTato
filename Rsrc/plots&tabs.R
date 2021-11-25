library(ggplot2)
library(readxl)
library(data.table)
library(ggpubr)

col_type = c(rep("text",2),"date",rep("text",5),rep("numeric",6),
             "text",rep("numeric",3),"text","numeric","numeric",
             "text",rep("numeric",6),"text","date","numeric")
data.all <- read_excel(path = "data/allDataProc_20012021.xlsx",col_types =col_type)

  data.all <- data.table(data.all)
# data.all$Area

###proc data
seasons <- c("S1415","S1516","S1617","S1718","S1819",
             "S1920","S2021")
data.all[Specie %in% c("lo","Lo")]$Specie <- "LO"
data.all[Specie %in% "Ei"]$Specie <- "EI"
data.all$CCL <- as.numeric(data.all$CCL)
data.all$TotalEggs <- as.numeric(data.all$TotalEggs)

data.all$Date <- as.Date(data.all$Date)
data.all$month <- month(data.all$Date)
data.all$year <- year(data.all$Date)
data.all <- data.all[Season %in% seasons]
nSeason <- length(unique(data.all$Season))



#plot1 nNestMonthFemale
# hist(data.all$month)
meanNnestMonth <- data.all[,sum(Nest)/nSeason,by=.(month)]
meanNnestMonth$monthID <- match(meanNnestMonth$month,c(7:12,1:6))
nNestMonth <- data.all[,sum(Nest),by=.(month,Season)]
nNestMonth$monthID <- match(nNestMonth$month,c(7:12,1:6))

p1 <- ggplot(meanNnestMonth, aes(x=as.factor(monthID),y=V1)) + 
  geom_bar(stat="identity") + 
  ylab("number of nests") + xlab("months") + 
  guides(color=guide_legend(title="Season")) +
  geom_line(data=nNestMonth, aes(x=monthID,y=V1,col=as.factor(Season))) + 
   scale_x_discrete(breaks=as.character(1:12),
    labels=c("Jul","Aug","Sep","Oct","Nov","Dec","Jan","Feb","Mar","Apr","May","Jun"))
p1




#plot2 nNestMonthBySpecies
meanNnestMonth <- data.all[,sum(Nest)/nSeason,by=.(month,Specie)]
meanNnestMonth$monthID <- match(meanNnestMonth$month,c(7:12,1:6))
nNestMonth <- data.all[,sum(Nest),by=.(month,Season,Specie)]
nNestMonth$monthID <- match(nNestMonth$month,c(7:12,1:6))
p2 <- list()
for(sp in c("CM","DC","EI","LO")){
p2[[sp]] <- ggplot(meanNnestMonth[Specie==sp], aes(x=monthID,y=V1)) + 
  geom_bar(stat="identity") + ggtitle(sp) + 
  ylab("number of nests") + xlab("months") + 
  guides(color=guide_legend(title="Season")) +
  geom_line(data=nNestMonth[Specie==sp], aes(x=monthID,y=V1,col=as.factor(Season))) +
  scale_x_continuous(breaks=1:12,
  labels=c("Jul","Aug","Sep","Oct","Nov","Dec","Jan","Feb","Mar","Apr","May","Jun")) +
  theme(axis.text.x = element_text(angle=90))
}
# p2$CM
ggarrange(p2[[1]],p2[[2]],p2[[3]],p2[[4]],common.legend = T)


###Clutch size
TEgs <- data.all[]
p3 <- list()
for(sp in c("CM","DC","EI","LO")){
p3[[sp]] <- ggplot(data.all[Nest==1 & Specie==sp], aes(x=TotalEggs)) + 
  geom_histogram() + ggtitle(sp)
}
ggarrange(p3[[1]],p3[[2]],p3[[3]],p3[[4]])

####tables
###CCL
CCLtab <- data.table()
CCLtab <- data.all[,min(CCL,na.rm=T),by=.(Season,Specie)]
setnames(CCLtab,"V1","min")
CCLtab$max <- data.all[,max(CCL,na.rm=T),by=.(Season,Specie)]$V1
CCLtab$mean <- data.all[,mean(CCL,na.rm=T),by=.(Season,Specie)]$V1
CCLtab$median <- data.all[,median(CCL,na.rm=T),by=.(Season,Specie)]$V1
CCLtab$sd <- data.all[,sd(CCL,na.rm=T),by=.(Season,Specie)]$V1
CCLtabAll <- data.table()
CCLtabAll <- data.all[,min(CCL,na.rm=T),by=.(Specie)]
setnames(CCLtabAll,"V1","min")
CCLtabAll$max <- data.all[,max(CCL,na.rm=T),by=.(Specie)]$V1
CCLtabAll$mean <- data.all[,mean(CCL,na.rm=T),by=.(Specie)]$V1
CCLtabAll$median <- data.all[,median(CCL,na.rm=T),by=.(Specie)]$V1
CCLtabAll$sd <- data.all[,sd(CCL,na.rm=T),by=.(Specie)]$V1


###Clutch size
Clutchtab <- data.table()
Clutchtab <- data.all[Nest==1,min(TotalEggs,na.rm=T),by=.(Season,Specie)]
setnames(Clutchtab,"V1","min")
Clutchtab$max <- data.all[Nest==1,max(TotalEggs,na.rm=T),by=.(Season,Specie)]$V1
Clutchtab$mean <- data.all[Nest==1,mean(TotalEggs,na.rm=T),by=.(Season,Specie)]$V1
Clutchtab$median <- data.all[Nest==1,median(TotalEggs,na.rm=T),by=.(Season,Specie)]$V1
Clutchtab$sd <- data.all[Nest==1,sd(TotalEggs,na.rm=T),by=.(Season,Specie)]$V1
ClutchtabAll <- data.table()
ClutchtabAll <- data.all[Nest==1,min(TotalEggs,na.rm=T),by=.(Specie)]
setnames(ClutchtabAll,"V1","min")
ClutchtabAll$max <- data.all[Nest==1,max(TotalEggs,na.rm=T),by=.(Specie)]$V1
ClutchtabAll$mean <- data.all[Nest==1,mean(TotalEggs,na.rm=T),by=.(Specie)]$V1
ClutchtabAll$median <- data.all[Nest==1,median(TotalEggs,na.rm=T),by=.(Specie)]$V1
ClutchtabAll$sd <- data.all[Nest==1,sd(TotalEggs,na.rm=T),by=.(Specie)]$V1
