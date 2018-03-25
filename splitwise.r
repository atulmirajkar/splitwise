library(reshape2)
library(lubridate)
library(ggplot2)

rm(list=ls())

setwd("C:/Users/atulm/RCode/Splitwise")


#import data
origDF = read.csv("province_export.csv",as.is = T)

#remove na
origDF<-origDF[!(is.na(origDF$Date)),]
origDF<-origDF[!(origDF$Description=="Total balance"),]

#structure
origDF$Date<-as.POSIXct(origDF$Date)
origDF$Category<-as.factor(origDF$Category)


#View rent / internet / netflix
View(origDF[origDF$Category=="Rent",])
View(origDF[grep("Netflix*",origDF$Description,ignore.case = TRUE),])
View(origDF[grep("*charter*",origDF$Description,ignore.case = TRUE),])

#myshare
myShareDF<-melt(data=origDF,id=c("Date","Description","Category","Cost","Currency"))

#drop currency
colnames(myShareDF)<-c("Date","Description","Category","Cost","Currency","User","Share")
myShareDF<-myShareDF[,!(names(myShareDF) %in% c("Currency","User"))]

#add month and year
myShareDF$year<-year(myShareDF$Date)
myShareDF$month<-month(myShareDF$Date)
myShareDF$day<-day(myShareDF$Date)

#remove total balance and cost as double
myShareDF<-myShareDF[myShareDF$User!="Aniket",]
myShareDF$Cost<-as.double(myShareDF$Cost)
myShareDF$Share<-as.double(myShareDF$Share)
myShareDF$Share[myShareDF$Share<0]<-myShareDF$Share[myShareDF$Share<0]*-1

#categoryShare
catShareDF<-aggregate(myShareDF$Share,by=list(myShareDF$Category),FUN=sum)
colnames(catShareDF)<-c("category","cost")
ggplot(data=catShareDF, aes(x="", y=cost, fill=factor(category))) +
  geom_bar(stat="identity",width=1)+
  coord_polar(theta = "y")

#aggregate on month and category
monthlyShareDF<-aggregate(myShareDF$Share,by=list(myShareDF$Category,myShareDF$month),FUN = sum)
colnames(monthlyShareDF)<-c("Category","Month","Share")
ggplot(data=monthlyShareDF, aes(x=Month, y=Share, fill=Category)) +
  geom_bar(colour="black", stat="identity")+
  scale_x_continuous(breaks = seq(1,12,by=1))



#quick
levels(origDF$Category)
str(origDF)
View(origDF)