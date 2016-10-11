rm(list = ls(all = TRUE))
setwd("C:/Users/c_indhiraA/Desktop/data extraction/hbv_failopen_40flightdata_newparameters_30aug2011_fault")

## reading required files from the folder ##
file_list = list.files(pattern = "*_fullflight*.csv")
file_data = lapply(file_list, read.csv)

norm_data = data.frame(NULL)
for(i in 1:length(file_data)){
  norm_data = rbind(norm_data,file_data[[i]])  
}
norm_data$target = rep("normal",nrow(norm_data))
rm(file_list,file_data,i)

fault_data = read.csv("30aug2011_rtengine_faultdata.csv")
fault_data$target = rep("fault",nrow(fault_data))

##   Data Transformations #########################
fault_data$time_sec = as.POSIXct(as.character(fault_data$Time.of.Day.Last.Date.Time..2.), format = "%H:%M")
norm_data$time_sec = as.POSIXct(as.character(norm_data$Time.of.Day.Last.Date.Time..), format = "%H:%M")
norm_data = subset(norm_data,HBV51OpenDmd != "Unknown")
norm_data = droplevels(norm_data) #drop unknown levels
#norm_data$HBV51OpenDmd <- factor(norm_data$HBV51OpenDmd,levels = c(0,1),labels = c("false","true"))

####################################################
## check delta P30 values ##
for ( i in 1:length(norm_data)){
  deltap30c = abs(norm_data$p30mod_data) - abs(norm_data$P30V)
}
summary(deltap30c)
summary(norm_data$p30delta)

for (i in 1:length(fault_data)) {
  f_deltap30c = abs(fault_data$p30mod_data) - abs(fault_data$P30V)
}
summary(f_deltap30c)
summary(fault_data$p30delta)

## matching two lists function definition ##
diff_table <- function(x,y){
  union_c <- union(x,y)
  x_comm <- union_c %in% x
  y_comm <- union_c %in% y
  return(table(x_comm,y_comm))
}

diff_table(norm_data$p30delta,deltap30c)


####################################################
## find correlations between deltaP30 and HBV dmd signals ##

#cor(norm_data$p30delta,norm_data$HBV51OpenDmd)
summary(aov(p30delta ~ HBV51OpenDmd,data = norm_data))
print(aov(p30delta ~ HBV51OpenDmd,data = norm_data)$fitted)


##### PLOTS ########################################
plot(norm_data$time_sec,norm_data$p30delta)
points(norm_data$time_sec,norm_data$HBV51OpenDmd,col="red",lty=3,pch=6)

plot(fault_data$time_sec,fault_data$p30delta)
points(fault_data$time_sec,fault_data$HBV51OpenDmd,col="red",lty= 3,pch=6)


par(mfrow=c(1,2))
plot(norm_data$time_sec,norm_data$p30delta,xlab="time",ylab="delta P30",col = "black",main = "delta P30 value comparision")
#lines(norm_data$time_sec,deltap30c,col="blue",type="p")
#text(locator(),labels=c("P30delta","calculated p30delta"))
plot(norm_data$time_sec,deltap30c,xlab = "time",ylab = "calculated delta p30",col="red")

#par(mfrow = c(1,2))
plot(norm_data$time_sec,norm_data$p30mod_data,xlab = "time",ylab="P30 mod_data",col="black")
lines(norm_data$time_sec,norm_data$P30V,xlab="time",ylab="P30 validated",col="blue")


plot(norm_data$time_sec,norm_data$P30V,xlab="time",ylab="P30 validated",main= "P30v vs N2 over root T20")
points(norm_data$time_sec,norm_data$NHRTH20V,col="blue",pch=6)
#points(norm_data$time_sec,norm_data$nhrt20,col="red",pch= "+")


