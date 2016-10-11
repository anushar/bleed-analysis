rm(list = ls(all =TRUE))
setwd("C:/Users/c_indhiraA/Desktop/data extraction/modified_paramlist_hbv_13oct2010_2engines")
files_to_read = list.files(pattern = "*.csv")
files_data = lapply(files_to_read, read.csv)

data_test_flight = data.frame(NULL)
for (i in length(files_data)) {
  data_test_flight = rbind(data_test_flight,files_data[[i]])
  data_test_flight$ft_num = i
}
rm(files_data,files_to_read)
data_test_flight$time_sec = as.POSIXct(data_test_flight$Date.Time, format = "(%d-%b-%Y) %H:%M:%OS")


## GRAPHS ##

## HBV dmd signals ##
par(mfrow=c(2,2))
plot(data_test_flight$time_sec,factor(data_test_flight$HBV51OpenDmd),yaxt = "n",xlab="Time",ylab="hbv 51_1 valve demand signal",main = "HBV Graph")
axis(side = 2, 1:2, labels = levels(factor(data_test_flight$HBV51OpenDmd)))
plot(data_test_flight$time_sec,factor(data_test_flight$HBV52OpenDmd),col="red",lty= 3,pch=6,yaxt = "n",xlab="Time",
     ylab = "hbv 52_2 valve demand signal")
axis(side = 2, 1:2, labels = levels(factor(data_test_flight$HBV52OpenDmd)))
plot(data_test_flight$time_sec,factor(data_test_flight$HBV53OpenDmd),col="blue",pch="+",yaxt = "n",xlab="Time",
     ylab = "hbv 53_3 valve demand signal")
axis(side = 2, 1:2, labels = levels(factor(data_test_flight$HBV53OpenDmd)))
plot(data_test_flight$time_sec,factor(data_test_flight$HBV8OpenDmd),col="#E69F00",pch="6",yaxt = "n",xlab="Time",
     ylab = "hbv 52_2 valve demand signal")
axis(side = 2, 1:2, labels = levels(factor(data_test_flight$HBV8OpenDmd)))


## P30v and HBV dmd signals ##
##         and              ##
## delta P30v and HBV dmd signals ##
#layout(matrix(c(1,1,2,3,4,5),3,2,byrow = TRUE))
par(mfrow = c(5,1))
#plot(data_test_flight$time_sec,data_test_flight$P30V,xlab="Time",ylab="P30 valiTimed",main = "P30 and HBV valve comparision")
plot(data_test_flight$time_sec,data_test_flight$p30delta,xlab = "Time",ylab = "delta P30",main=" P30 delta and HBV signals")
#par(mfrow=c(4,1))
plot(data_test_flight$time_sec,factor(data_test_flight$HBV51OpenDmd),col="red",xlab = "Time", ylab="HBV 51_1 valve dmd signal",yaxt="n")
axis(side = 2, 1:2, labels = levels(factor(data_test_flight$HBV51OpenDmd)))
plot(data_test_flight$time_sec,factor(data_test_flight$HBV52OpenDmd),col="blue",xlab="Time",ylab="HBV 52_2 valve dmd signal",yaxt="n")
axis(side = 2, 1:2, labels = levels(factor(data_test_flight$HBV52OpenDmd)))
plot(data_test_flight$time_sec,factor(data_test_flight$HBV53OpenDmd),col="#E69F00",xlab="Time",ylab="HBV 53_3 valve dmd signal",yaxt="n")
axis(side = 2, 1:2, labels = levels(factor(data_test_flight$HBV53OpenDmd)))
plot(data_test_flight$time_sec,factor(data_test_flight$HBV8OpenDmd),col ="#56B4E9",xlab = "Time",ylab ="HBV 8_4 valve dmd signal",yaxt="n")
axis(side = 2, 1:2, labels = levels(factor(data_test_flight$HBV8OpenDmd)))

## P30v and P30 delta ##
par(mfrow=c(2,1))
plot(data_test_flight$time_sec,data_test_flight$P30V,xlab = "Time",ylab="P30 validated",main = "P30V and P30 delta comparision")
plot(data_test_flight$time_sec,data_test_flight$p30delta,xlab="Time",ylab="P30 delta")

## NHRTH20,NHRTH26 ##
par(mfrow=c(4,1))
plot(data_test_flight$time_sec,data_test_flight$P30V,xlab = "Time",ylab="P30 validated",main = "P30V Altitude and NHRTH20 comparision")
#plot(data_test_flight$time_sec,data_test_flight$p30delta,xlab="Time",ylab="P30 delta")
plot(data_test_flight$time_sec,data_test_flight$altitude_data,xlab="Time",ylab="altitude")
plot(data_test_flight$time_sec,data_test_flight$NHRTH20V,xlab="Time",ylab="n2 over root T20")
plot(data_test_flight$time_sec,data_test_flight$NHRTH26V,xlab = "Time",ylab = "n2 over root T26")

## P0v vs Time ##
plot(data_test_flight$time_sec,data_test_flight$altitude_data,xlab="Time",ylab="altitude")
plot(data_test_flight$time_sec,data_test_flight$acP0V,xlab="Time",ylab="aircraft P0v",main= "P0v and Time")
plot(data_test_flight$time_sec,data_test_flight$engP0V,xlab = "Time",ylab="Engine P0v")
plot(data_test_flight$time_sec,data_test_flight$P30V,xlab = "Time",ylab = "aircraft P30V")

## Fuelflow fuelindex ##
par(mfrow=c(2,1))
plot(data_test_flight$time_sec,data_test_flight$FuelFlowV,xlab = "Time",ylab = "Fuelflow",main = "Fuel flow and altitude")
plot(data_test_flight$time_sec,data_test_flight$altitude_data,xlab = "Time",ylab = "Altitude")

plot(data_test_flight$time_sec,factor(data_test_flight$fuelIndex),xlab = "Time",ylab="Fuel index",yaxt = "n")
axis(side = 2,1:9,labels = levels(factor(data_test_flight$fuelIndex)))


## two plots in same graph ##
## P30v and HBV dmd ##
plot(data_test_flight$time_sec, data_test_flight$P30V, ylim=c(-50, 350),ylab = "P30v",xlab="time",main="P30v and demand signals"); 
points(data_test_flight$time_sec, as.numeric(data_test_flight$HBV51OpenDmd) * 50 - 60, col="red")
points(data_test_flight$time_sec,as.numeric(data_test_flight$HBV52OpenDmd) * 40 - 55,col="blue")
points(data_test_flight$time_sec,as.numeric(data_test_flight$HBV53OpenDmd) * 45 - 50,col="#E69F00")
points(data_test_flight$time_sec,as.numeric(data_test_flight$HBV8OpenDmd) * 55 - 58, col= "#56B4E9")


## TGT , P30v and hbv ##
plot(data_test_flight$time_sec,data_test_flight$TGTV,ylab="TGTv",xlab="Time",main="TGTv and P30v",ylim=c(0,1200))
points(data_test_flight$time_sec,data_test_flight$P30V,col="blue")


## Fuelflow FuelSwitchON and Altitude ##
plot(data_test_flight$time_sec,data_test_flight$FuelFlowV,ylim=c(-60,5500),ylab="Fuelflow",xlab="Time",main="FuelFlow FuelSwitchON and Time")
points(data_test_flight$time_sec,data_test_flight$altitude_data,col="blue")
points(data_test_flight$time_sec,as.numeric(data_test_flight$FuelSwitchOnV) * 50 - 90,col="grey")
points(data_test_flight$time_sec,as.numeric(data_test_flight$SOVisOpenV) * 40 - 75,col="red")


## NHv NHmod_data and HBV rotation ##
#plot(data_test_flight$time_sec,data_test_flight$NHV,ylab="NHV",xlab="Time",main="NH speed",ylim = c(-50,100))
#points(data_test_flight$time_sec,data_test_flight$NHMinDem,col="red")
plot(data_test_flight$time_sec,data_test_flight$P30V,ylab="P30v",xlab="Time",main="P30v and HBV rotation ID",ylim=c(-50,400))
points(data_test_flight$time_sec,as.numeric(data_test_flight$HBVRotationID) * 50 - 60,col="grey")


## Comparing 2 Engine Performance ##
## subset left and right engine data ##
rt_data = data_test_flight[data_test_flight$isrightengine == "TRUE", ]
lt_data = data_test_flight[data_test_flight$isrightengine == "FALSE", ]




















#my_Time = strptime(as.character(data_test_flight$Time.Time),"(%d-%b-%Y) %H:%M:%OS3")
#table(data_test_flight$time_sec)
#library(lubriTime)
#mdy_hms(data_test_flight$Time.Time,format= "(%d-%b-%Y) %H:%M:%OS3")
#rm(my_Time)
#matplot(data_test_flight$time_sec,data_test_flight$HBV51OpenDmd,type = "p",axes=FALSE,frame.plot=FALSE,ylab="hbv 51 dmd signal",main="hbv dmd",xlab="time",
#        frame.plot=FALSE)
#axis(side = 1,14:20,labels = 14:20)