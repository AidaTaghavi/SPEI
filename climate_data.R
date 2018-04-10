library("chron")
a<-read.csv("nowshahr.csv")
z <- read.zoo("nowshahr.csv", header = TRUE, sep = ",", FUN =as.chron)
bt <- read.zoo("Hydro.txt", FUN= as.chron)
########ggplot
#####before change style of date to uk. 2001-03-14
library(ggplot2)
library(splines2)
library(scales)

dataset = read.csv("hydro.csv", header=TRUE,sep=",",na.strings="")
colnames(dataset)<-c("date","prcp")
temp<-as.numeric(dataset$prcp)
temp
dataset$date
year<-as.numeric(substr(dataset$date,1,4))
year[1]
year[10]
head(dataset)
min.year <- min(year)
max.year <- max(year)
max.year
mon<-as.numeric(substr(dataset$date,6,7))
mon
day<-01
summary(mon)
date<-paste(year,mon,day,sep="/")
date
dataset$dates <- as.Date(date, "%Y/%m/%d")
min.date <- min(dataset$dates)
max.date <- max(dataset$dates)
head(dataset)
dataset$year <- as.numeric(format(dataset$dates,"%Y"))
dataset$month <- as.numeric(format(dataset$ndates,"%m"))
dataset$month <- as.numeric(format(dataset$dates,"%m"))
head(dataset)
temp<-as.numeric(dataset$prcp)
plot.title = 'Monthly Average Temperatures in Penang (1934-2013)'
plot.subtitle = 'Data source : GCHN v3 temperature data version ghcnm.tavg.v3.2.2.20140107'

p<-ggplot(data=dataset, aes(x=dates, y=temp)) +
  geom_line(colour="bisque3",size=1) +
  geom_point(colour="bisque4",size=1) +
  scale_y_continuous() +
  scale_x_date(date_breaks = "1 year",labels=date_format("%Y"))  +
  xlab("") + ylab(as.expression(expression( paste("Rainfall (","mm)") ))) +
  ggtitle(bquote(atop(.(plot.title), atop(italic(.(plot.subtitle)), "")))) + theme(plot.title = element_text(face = "bold",size = 16,colour="black")) +
  geom_smooth(aes(group = 1), method = "loess",span=0.75)
p
ggsave(p, file="D:/R-software/Penang_CISL_Data_Plot.png", width=10, height=5)
#######################NASHOD
c <- ggplot(dataset,aes(mon,temp)) + geom_boxplot(outlier.shape = NA,fill="paleturquoise", col="turquoise4")+ geom_smooth(aes(group = 1), method = "loess",span=0.75,colour="red")
c <- c + ggtitle(bquote(atop(.(boxplot.title), atop(italic(.(boxplot.subtitle)), "")))) + theme(plot.title = element_text(face = "bold",size = 16,colour="black"))
c <- c + ylab("Precipitation (mms)")+ xlab("Month") + stat_boxplot(geom ='errorbar',col="turquoise")
c <- c + theme_bw()
c
#####################SPEI
setwd("D:/Dr.Amiraslani/mirzaei-analysis")
x<-read.csv("speinoshahr.csv")
str(x)
th<-thornthwaite(x$tmed,36.39)
write.csv(x$PET,"pet-noushahr.csv")
spei1<-spei(x$pr-x$Pet,1)
plot(spei1)
plot(spei(ts(x$pr-x$Pet,12,start=c(1980,1),end =c(2014,12)),3), main='SPEI-3 with rectangular kernel')
plot(spei(ts(x$pr-x$Pet,12,start=c(1980,1),end =c(2014,12)),6), main='SPEI-6 with rectangular kernel')
plot(spei(ts(x$pr-x$Pet,12,start=c(1980,1),end =c(2014,12)),9), main='SPEI-9 with rectangular kernel')
plot(spei(ts(x$pr-x$Pet,12,start=c(1980,1),end =c(2014,12)),12), main='SPEI-12 with rectangular kernel')
plot(spei(ts(x$pr-x$Pet,12,start=c(1980,1),end =c(2014,12)),24), main='SPEI-24 with rectangular kernel')
####################

