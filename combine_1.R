rm(list = ls(all = T))
setwd("C:/Users/c_indhiraA/Desktop/data extraction/hbv_failopen_40flightdata_30aug2011_fault")

#### read all engine data ####
files = list.files(pattern = "*_Engine1*.csv")
files
files_data = lapply(files,read.csv)

norm_data = data.frame(NULL)
for (i in 1:length(files_data)){
 # assign(paste0("files_data",[[i]],sep =""),norm)
  norm_data = rbind(norm_data,files_data[[i]])
}
norm_data$target = rep("normal",nrow(norm_data))
rm(files,files_data)

fault_data = read.csv("30aug2011_Engine1_faultdata.csv")
fault_data$target = rep("fault",nrow(fault_data))
## duplicate fault data to number of rows of normal data ##
fault_data1 = rep(row.names(fault_data),(nrow(norm_data)-nrow(fault_data))) 

fault_data$time_sec = as.POSIXct(as.character(fault_data$Time.of.Day.Last.Date.Time..), format = "%H:%M:%S")
norm_data$time_sec = as.POSIXct(as.character(norm_data$Time.of.Day.Last.Date.Time..), format = "%H:%M:%S")
####### reducing data set ###########
norm_data = norm_data[1:nrow(fault_data),]

####### calculate correlations #######
summary(norm_data$P30V)
summary(fault_data$P30V)
#boxplot(norm_data$P30V,fault_data$P30V)
norm_cor = cor(norm_data[sapply(norm_data, is.numeric)],method = "pearson")
fault_cor = cor(fault_data[sapply(fault_data, is.numeric)],method = "pearson")
norm_cor[which.max(norm_cor)]
plot(norm_cor,na.rm = TRUE)
diag(norm_cor) = NA
diag(fault_cor) = NA
#max_ind = apply(norm_cor, 2, which.max)
#norm_max_cor = t(apply(norm_cor, 1, function(x){x[which.max(x)] <- NA;return(x)}))
norm_max_cor_names = t(apply(t(norm_cor),1, function(x){y<- names(sort(x,TRUE))[1:2];return(y)}))
norm_max_cor_num  = t(apply(t(norm_cor), 1, function(k){l<- sort(k,TRUE)[1:2];return(l)}))

fault_max_cor_names = t(apply(t(fault_cor), 1, function(x){y <- names(sort(x,TRUE))[1:2];return(y)}))
fault_max_cor_num = t(apply(t(fault_cor), 1, function(x){y <- sort(x,TRUE)[1:2];return(y)}))

####### plots ################
#install.packages("PerformanceAnalytics")
#library(PerformanceAnalytics)
#chart.Correlation(sapply(norm_data, is.numeric),histogram = TRUE, pch=19)
#install.packages("corrplot")
library(corrplot)
corrplot(norm_cor,type = "upper",tl.col = "black",tl.srt = 45)
# with correlation numbers
#corrplot(norm_cor,type = "upper",order = "hclust",tl.col = "black",tl.srt = 45,method = "number")
corrplot(fault_cor,type = "upper",tl.col = "black",tl.srt = 45)

#norm_max_cor_names %in% fault_max_cor_names
# setdiff(norm_max_cor_names,fault_max_cor_names)
"%!in%" <- function(x,table) match(x,table, nomatch = 0) == 0 
norm_max_cor_names <-as.data.frame(norm_max_cor_names)
fault_max_cor_names <- as.data.frame(fault_max_cor_names)
k <- 0
 for (i in which(norm_max_cor_names %!in% fault_max_cor_names)){
   cat("Not matching row number ",k,"\n")
   cat(row.names(norm_max_cor_names)[i+1],levels(norm_max_cor_names[,1])[norm_max_cor_names[i+1,1]],levels(norm_max_cor_names[,1])[norm_max_cor_names[i+1,1]],sep = " ", "\n")
   cat(row.names(fault_max_cor_names)[i+1],levels(fault_max_cor_names[,1])[fault_max_cor_names[i+1,1]],levels(fault_max_cor_names[,1])[fault_max_cor_names[i+1,1]],sep = " ", "\n") 
   k <- k+1
  }
rm(k)


#### delta P30 value computation ####
## threshold change is 5 pressure units ##
norm_data$deltaP30 <- NA
for (i in 1:(nrow(norm_data) - 1)){
  if((norm_data$P30V[i+1] - norm_data$P30V[i]) >= 5){
    norm_data$deltaP30[i] =  1
  }
  else if((norm_data$P30V[i+1] - norm_data$P30V[i]) < -5){
    norm_data$deltaP30[i] = -1
  }
  else{
    norm_data$deltaP30[i] = 0
  }
}
 
table(norm_data$deltaP30) 

plot(norm_data$time_sec,norm_data$HBV8OpenDmd,type = "b")
plot(norm_data$time_sec,norm_data$deltaP30)
