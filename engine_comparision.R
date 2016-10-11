rm(list = ls(all = TRUE))
setwd("C:/Users/c_indhiraA/Desktop/data extraction/modified_paramlist_hbv_13oct2010_2engines")

files_rt = list.files(pattern = "*_Engine2.csv")
files_rt =  files_rt[order(as.Date(files_rt,format = "%d%b%Y"))]
rt_files = lapply(files_rt, read.csv)
files_lt = list.files(pattern = "*_Engine1.csv")
files_lt =  files_lt[order(as.Date(files_lt,format = "%d%b%Y"))]
lt_files = lapply(files_lt,read.csv)
rt_engine_data = data.frame(NULL)
lt_engine_data = data.frame(NULL)

for (i in 1:length(rt_files)) {
  rt_files[[i]]$ft_num <- i
  rt_engine_data = rbind(rt_engine_data,rt_files[[i]])
}

for (i in 1:length(lt_files)) {
  lt_files[[i]]$ft_num <- i
  lt_engine_data = rbind(lt_engine_data,lt_files[[i]])
}
rm(files_rt,rt_files,files_lt,lt_files)

rt_engine_data$time_sec = as.POSIXct(rt_engine_data$Date.Time, format = "(%d-%b-%Y) %H:%M:%OS")
lt_engine_data$time_sec = as.POSIXct(lt_engine_data$Date.Time,format = "(%d-%b-%Y) %H:%M:%OS")

# To compare with in-service get 1hz data #
# For 1Hz Data subset every 4th element #
# converting 4hz to 1hz data #
rt_1hz_data = rt_engine_data[seq(1,nrow(rt_engine_data),4),]
lt_1hz_data = lt_engine_data[seq(1,nrow(lt_engine_data),4),]


plot(rt_engine_data$time_sec,rt_engine_data$altitude_data)
points(lt_engine_data$time_sec,lt_engine_data$altitude_data,col="blue")

summary(rt_engine_data)
summary(lt_engine_data)

# require(dplyr)
# anti_join(lt_engine_data,rt_engine_data)
# semi_join(lt_engine_data,rt_engine_data)
# 
# changes(lt_engine_data,rt_engine_data)
# 
# "%!in%" <- function(x,table) match(x,table, nomatch = 0) == 0 
# lt_engine_data %!in% rt_engine_data
# 
# setdiff(lt_engine_data,rt_engine_data)

require(dplyr)
gp_ft_num <- group_by(rt_engine_data,ft_num)
tgt_gp <- summarise(gp_ft_num,count = n(),mn = mean(TGTV,na.rm = TRUE),vr = var(TGTV,na.rm=TRUE))
p30_gp <- summarise(gp_ft_num,count = n(),mn = mean(P30V,na.rm = TRUE), vr = var(P30V,na.rm = TRUE))
nh_gp <- summarise(gp_ft_num,count = n(),mn = mean(NHV,na.rm = TRUE), vr = var(NHV,na.rm = TRUE))
alt_gp <- summarise(gp_ft_num,count = n(), mn = mean(altitude_data,na.rm = TRUE),vr =var(altitude_data,na.rm = TRUE))

require(ggplot2)
ggplot(tgt_gp,aes(mn,vr))+geom_point(aes(size = count),alpha =1/2)+geom_smooth()+scale_size_area()+ggtitle("TGTV mean variance")
  #geom_point(aes(color = count))+ scale_size_area()+geom_smooth()
ggplot(p30_gp,aes(mn,vr))+geom_point(aes(size = count))+geom_smooth()+scale_size_area()+ggtitle("P30v mean variance")
ggplot(nh_gp,aes(mn,vr))+geom_point(aes(size = count))+geom_smooth()+scale_size_area()+ggtitle("NHV mean variance")
ggplot(alt_gp,aes(mn,vr))+geom_point(aes(size = count))+geom_smooth()+scale_size_area()+ggtitle("Altitude Mean Variance")

plot(rt_engine_data$time_sec,rt_engine_data$altitude_data)

rm(tgt_gp,p30_gp,nh_gp,alt_gp)
rm(gp_ft_num)
### Moving averages for window size 10 ##
#rt_engine_data$week_num = strftime(rt_engine_data$time_sec,format = "%W")
#rt_engine_data$week_num = strftime(rt_engine_data$time_sec,format = "%Y-%m-%W")
#### trying to group hundred points for good graphs ##
k=1
for ( i in 1:nrow(rt_engine_data)){
  
  if(i%%100 != 0){
    rt_engine_data$cn[i] = k*100
  }
  else
  {
    rt_engine_data$cn[i] = i
    k=k+1
  }
    
}
rt_engine_data$week_num <- NULL

setdiff(names(rt_engine_data),names(lt_engine_data))
## grouping based on records ##
library(dplyr)
gp_rct_num <- group_by(rt_engine_data,cn)
gp_rct_tr <- subset(gp_rct_num,gp_rct_num$HBV51OpenDmd == TRUE)
gp_rct_fls <- subset(gp_rct_num,gp_rct_num$HBV51OpenDmd == FALSE)

tgt_mv_gp <- summarise(gp_rct_num,ft = median(ft_num) ,mn = mean(TGTV,na.rm = TRUE),vr = var(TGTV,na.rm=TRUE))
tgt_mv_tr <- summarise(gp_rct_tr,ft = median(ft_num), mn = mean(TGTV,na.rm = TRUE), vr = var(TGTV,na.rm = TRUE))
tgt_mv_fls <- summarise(gp_rct_fls,ft = median(ft_num), mn = mean(TGTV,na.rm = TRUE), vr = var(TGTV,na.rm = TRUE))

library(ggplot2)
ggplot(tgt_mv_gp,aes(mn,vr))+geom_point(aes(colour = factor(ft)),alpha =1/2)+geom_smooth()+scale_size_area()+
  ggtitle("TGTV mean variance")+scale_colour_brewer(palette = "Set1")+ylim(0,500)

##  rt_engine_data$cn <- NULL
## Bin altitude for flight states ##
plot(rt_engine_data$time_sec,rt_engine_data$altitude_data)
plot(lt_engine_data$time_sec,lt_engine_data$altitude_data)

a1=subset(rt_engine_data,ft_num == 9)
plot(a1$time_sec,a1$altitude_data)

## Binning altitude in lame way ###
## 1<altitude<14500 doesn't work ##
for(i in 1:nrow(rt_engine_data)){
  if(rt_engine_data$altitude_data[i] < 1){
    rt_engine_data$ft_state[i] = "taxi"
  }else if((1 < rt_engine_data$altitude_data[i]) && (rt_engine_data$altitude_data[i] < 14500)){
    rt_engine_data$ft_state[i] = "takeoff_descent"
  }else if((14500 < rt_engine_data$altitude_data[i]) && (rt_engine_data$altitude_data[i] < 30000)){
    rt_engine_data$ft_state[i] = "cruise"
  }else{
    rt_engine_data$ft_state[i] = "high_altitude_cr"
  }
}

rm(a1,alt_gp,nh_gp,p30_gp,tgt_gp)


rt_engine_allv_tr = subset(rt_engine_data, (HBV51OpenDmd == TRUE) & (HBV52OpenDmd == TRUE) &  (HBV53OpenDmd == TRUE) & (HBV8OpenDmd == TRUE))
#rt_engine_allv_tr = subset(rt_engine_data, (HBV51OpenDmd & HBV52OpenDmd & HBV53OpenDmd & HBV8OpenDmd) == TRUE)
#rt_engine_allv_fls = subset(rt_engine_data, (HBV51OpenDmd & HBV52OpenDmd & HBV53OpenDmd & HBV8OpenDmd) == FALSE)
rt_engine_allv_fls = subset(rt_engine_data, (HBV51OpenDmd == FALSE) & (HBV52OpenDmd == FALSE) & (HBV53OpenDmd == FALSE) & (HBV8OpenDmd == FALSE))


############# clustering ###########
library(stats)
mod1 = kmeans(rt_engine_data,4)

mod_data = rt_engine_data[,c("NHRTH20V","EPRActual","FuelFlowV","NHV","NLV","P20V","P30V","T20V","T30V","TGTV","altitude_data",
                             "acP0V")]


### finding gradient/slope for altitude attribute to get flight states ###
### lm ###
reg1= lm(altitude_data ~time_sec,data = rt_engine_data)
coef(reg1)

## altitude group by day ##
rt_engine_data$day_mon_y = strftime(rt_engine_data$time_sec,format = "%d-%b-%Y")
lt_engine_data$day_mon_y = strftime(lt_engine_data$time_sec,format = "%d-%b-%Y")

#setdiff(names(rt_engine_data),names(lt_engine_data))

#library(dplyr)
#rt_date_grp <- group_by(rt_engine_data,day_mon_y)
#tgt_mv_gp <- summarise(rt_date_grp,atl = function(x,y){if(y-x > 0){ "take-off";"descent"}})

for(i in 1:(nrow(rt_engine_data) - 5)){
  j=i+5 ## add plus 5
  if(rt_engine_data$day_mon_y[i] == rt_engine_data$day_mon_y[j]){
    if(rt_engine_data$altitude_data[i] > 0){
      if((rt_engine_data$altitude_data[j] - rt_engine_data$altitude_data[i]) > 0){
        rt_engine_data$ft_al_state[i] = "take-off"
        rt_engine_data$ft_al_state[j] = "take-off"
      }else if((rt_engine_data$altitude_data[j] - rt_engine_data$altitude_data[i]) < 0){
        rt_engine_data$ft_al_state[i] = "descent"
        rt_engine_data$ft_al_state[j] = "descent"
      }else if(((rt_engine_data$altitude_data[j] - rt_engine_data$altitude_data[i]) == 0) & (rt_engine_data$altitude_data[i] > 500)){
        rt_engine_data$ft_al_state[i] = "cruise"
        rt_engine_data$ft_al_state[j] = "cruise"
      }
    }else{
      rt_engine_data$ft_al_state[i] = "taxi" 
    }
  }
}


## taxi-out ##
for(i in 1:(nrow(rt_engine_data)-1)){
  if((rt_engine_data$day_mon_y[i] == rt_engine_data$day_mon_y[i+1]) & (rt_engine_data$ft_al_state[i] == "descent") & 
     (rt_engine_data$ft_al_state[i+1] == "taxi" ) ){    #assuming data is ordered
    rt_engine_data$ft_al_state[i+1] = "taxi-out"        
  }
}


## left engine data altitude states ##
for(i in 1:(nrow(lt_engine_data) - 5)){
  j=i+5 ## add plus 5
  if(lt_engine_data$day_mon_y[i] == lt_engine_data$day_mon_y[j]){
    if(lt_engine_data$altitude_data[i] > 0){
      if((lt_engine_data$altitude_data[j] - lt_engine_data$altitude_data[i]) > 0){
        lt_engine_data$ft_al_state[i] = "take-off"
        lt_engine_data$ft_al_state[j] = "take-off"
      }else if((lt_engine_data$altitude_data[j] - lt_engine_data$altitude_data[i]) < 0){
        lt_engine_data$ft_al_state[i] = "descent"
        lt_engine_data$ft_al_state[j] = "descent"
      }else if(((lt_engine_data$altitude_data[j] - lt_engine_data$altitude_data[i]) == 0) & (lt_engine_data$altitude_data[i] > 500)){
        lt_engine_data$ft_al_state[i] = "cruise"
        lt_engine_data$ft_al_state[j] = "cruise"
      }
    }else{
      lt_engine_data$ft_al_state[i] = "taxi" 
    }
  }
}


## taxi-out ##
for(i in 1:(nrow(lt_engine_data)-1)){
  if((lt_engine_data$day_mon_y[i] == lt_engine_data$day_mon_y[i+1]) & (lt_engine_data$ft_al_state[i] == "descent") & 
     (lt_engine_data$ft_al_state[i+1] == "taxi" ) ){    #assuming data is ordered
    lt_engine_data$ft_al_state[i+1] = "taxi-out"        
  }
}



# ## check ft_al_state ##
# plot(rt_engine_data$time_sec,rt_engine_data$ft_al_state)
# ggplot(rt_engine_data,aes(time_sec,altitude_data))+geom_point(aes(colour = factor(ft_al_state)),alpha =1/2)+scale_size_area()+
#   ggtitle("time vs altitude ")+scale_colour_brewer(palette = "Set1")+theme_bw()
# 
# a1 = subset(rt_engine_data, ft_al_state == c("taxi","taxi-out","cruise"))
# ggplot(a1,aes(time_sec,altitude_data))+geom_point(aes(colour = factor(ft_al_state)),alpha =1/2)+scale_size_area()+
#   ggtitle("time vs altitude ")+scale_colour_brewer(palette = "Set1")+theme_bw()

#######################################################################################

### demo function ###
gplot_al_state_demo <- function(dt,x,y,ft,tt,fn){
  g4 <-ggplot(dt,aes_string(x,y))+geom_point(aes(colour = factor(ft)),alpha =1/2)+scale_size_area()+
    ggtitle(paste(x," vs ",y,tt,sep=""))+scale_colour_brewer(palette = "Set1")+theme_bw()+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  #print ggplot if needed, x and y should be in quotes
  wd <- "C:/Users/c_indhiraA/Desktop/data extraction/modified_paramlist_hbv_13oct2010_2engines/al_plot/"
  ggsave(g4,filename = paste(wd,x,"_vs_",y,fn,sep = "")) 
}

gplot_al_state_demo(rt_engine_data,"p30delta","P30V",rt_engine_data$ft_al_state)

gplot_al_state_demo_ylim <- function(dt,x,y,ft,tt,fn,yl1,yl2){
  g4 <-ggplot(dt,aes_string(x,y))+geom_point(aes(colour = factor(ft)),alpha =1/2)+scale_size_area()+
    ggtitle(paste(x," vs ",y,tt,sep=""))+scale_colour_brewer(palette = "Set1")+ylim(yl1,yl2)+theme_bw()+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  #print ggplot if needed, x and y should be in quotes
  wd <- "C:/Users/c_indhiraA/Desktop/data extraction/modified_paramlist_hbv_13oct2010_2engines/al_plot/"
  ggsave(g4,filename = paste(wd,x,"_vs_",y,fn,sep = "")) 
}

###################### ALL FLIGHTS data ############################################
gplot_al_tr_fls_norm <- function(x,y){
  g1<-ggplot(rt_engine_allv_tr,aes_string(x,y))+geom_point(aes(colour = factor(ft_al_state)),alpha =1/2)+scale_size_area()+
    ggtitle(paste(x," vs ",y," all valve true",sep=""))+scale_colour_brewer(palette = "Set1")+theme_bw()+theme(axis.text = element_text(angle = 45, hjust = 1))
  g2<-ggplot(rt_engine_allv_fls,aes_string(x,y))+geom_point(aes(colour = factor(ft_al_state)),alpha =1/2)+scale_size_area()+
    ggtitle(paste(x," vs ",y," all valve false",sep=""))+scale_colour_brewer(palette = "Set1")+theme_bw()+theme(axis.text = element_text(angle = 45, hjust = 1))
  wd <- "C:/Users/c_indhiraA/Desktop/data extraction/modified_paramlist_hbv_13oct2010_2engines/al_plot_tr_fls/"
  ggsave(g1,filename = paste(wd,x,"_vs_",y,"_all_valve_true.jpeg",sep = "")) #,width = 40,height = 40)
  ggsave(g2,filename = paste(wd,x,"_vs_",y,"_all_valve_false.jpeg",sep = "")) 
  g3<-ggplot(rt_engine_data,aes_string(x,y))+geom_point(aes(colour = factor(ft_al_state)),alpha =1/2)+scale_size_area()+
    ggtitle(paste(x," vs ",y," all data",sep=""))+scale_colour_brewer(palette = "Set1")+theme_bw()+theme(axis.text = element_text(angle = 45, hjust = 1))
  wd1 <- "C:/Users/c_indhiraA/Desktop/data extraction/modified_paramlist_hbv_13oct2010_2engines/al_plot/"
  ggsave(g3,filename = paste(wd1,x,"_vs_",y,"_all_data.jpeg",sep = "")) 
  
}


#gplot_al_tr_fls_norm("p30delta","P30V")

#gplot_al_state_demo(subset(rt_engine_data,day_mon_y == "01-Oct-2010"),"time_sec","P30V",rt_engine_data$ft_al_state," 01-oct-2010 data")
gplot_al_tr_fls_norm("day_mon_y","P30V")


###### Method 1 ######
## P30V against time and hbv valves ##
gplot_al_tr_fls_norm("time_sec","P30V")
gplot_al_tr_fls_norm("day_mon_y","p30delta")
## p30 vs hbv ##
gplot_al_tr_fls_norm("HBV51OpenDmd","P30V")
gplot_al_tr_fls_norm("HBV52OpenDmd","P30V")
gplot_al_tr_fls_norm("HBV53OpenDmd","P30V")
gplot_al_tr_fls_norm("HBV8OpenDmd","P30V")

## p30delta vs hbv's ##
gplot_al_tr_fls_norm("HBV51OpenDmd","p30delta")
gplot_al_tr_fls_norm("HBV52OpenDmd","p30delta")
gplot_al_tr_fls_norm("HBV53OpenDmd","p30delta")
gplot_al_tr_fls_norm("HBV8OpenDmd","p30delta")

## P30v and NHV ##
gplot_al_tr_fls_norm("NHV","P30V")
## P30 delta NHV ##
gplot_al_tr_fls_norm("p30delta","NHV")
## NHRTH20 vs T20V ##
gplot_al_tr_fls_norm("NHRTH20V","T20V")


## Method 2 ##
## P30V vs p30delta ##
gplot_al_tr_fls_norm("p30delta","P30V")

## P30v NHRTH20V ##
gplot_al_tr_fls_norm("NHRTH20V","P30V")

## Engine start , NHV < 60% of Max(NHV) ##
## SAV command open and SAV open signals/variables are needed ##
rt_data_start = subset(rt_engine_data, ((NHV < 0.5 *max(NHV)) & (FuelSwitchOnV == TRUE) & (FuelFlowV > 0) & (SOVPosV == TRUE) & (ft_al_state == "taxi")))
                       #(NHV < 0.5 *max(NHV)) & (FuelSwitchOnV == TRUE) & (altitude_data == 0) & (FuelFlowV > 0) & (SOVPosV == TRUE))
gplot_al_state_demo(rt_data_start,"day_mon_y","P30V",rt_data_start$ft_al_state," at engine start(60% NHV) ","engine_start.jpeg")
gplot_al_state_demo(rt_data_start,"day_mon_y","P30V",rt_data_start$HBV51OpenDmd," hbv 51_1 at engine start(60% NHV) ","hbv51_1_engine_start.jpeg")
gplot_al_state_demo(rt_data_start,"day_mon_y","P30V",rt_data_start$HBV52OpenDmd," hbv 52_2 at engine start(60% NHV) ","hbv52_2_engine_start.jpeg")
gplot_al_state_demo(rt_data_start,"day_mon_y","P30V",rt_data_start$HBV53OpenDmd," hbv 53_3 at engine start(60% NHV) ","hbv53_3_engine_start.jpeg")
gplot_al_state_demo(rt_data_start,"day_mon_y","P30V",rt_data_start$HBV8OpenDmd," hbv 8_4 at engine start(60% NHV) ","hbv8_4_engine_start.jpeg")


rt_data_p_gr_350 = subset(rt_data_start, P30V > 350)
gplot_al_state_demo(rt_data_p_gr_350,"day_mon_y","P30V",rt_data_p_gr_350$ft_al_state," at engine start(60% NHV) P30 >350 ",
                    "engine_start_P30_gr_350.jpeg")
gplot_al_state_demo(rt_data_p_gr_350,"day_mon_y","P30V",rt_data_p_gr_350$HBV51OpenDmd," hbv 51_1 at engine start(60% NHV) P30 >350 ",
                    "hbv51_1_engine_start_P30_gr_350.jpeg")
rm(rt_data_start ,rt_data_p_gr_350)

## Cruise condiitons ##
rt_cruise = subset(rt_engine_data, ft_al_state == "cruise")
gplot_al_state_demo(rt_cruise,"day_mon_y","P30V",rt_cruise$HBV51OpenDmd," hbv 51_1 at cruise P30V",
                    "hbv51_1_cruise_P30.jpeg")

# EPRActual #
gplot_al_state_demo(rt_cruise,"day_mon_y","EPRActual",rt_cruise$HBV51OpenDmd," hbv 51_1 at cruise ",
                    "hbv51_1_cruise.jpeg")
# FuelFlowV #
gplot_al_state_demo(rt_cruise,"day_mon_y","FuelFlowV",rt_cruise$HBV51OpenDmd," hbv 51_1 at cruise ",
                    "hbv51_1_cruise.jpeg")

# Compression Efficiency #
compression_efficiency <- function(x){
  # gamma = 1.4
  gm <- 1.4
  ce <- (((x$P30V/x$P20V)^((gm-1)/gm)) - 1 )*100/(((x$T30V+273)/(x$T20V+273))-1)
  return(ce)
}

rt_cruise$ce <- compression_efficiency(rt_cruise)
gplot_al_state_demo(rt_cruise,"day_mon_y","ce",rt_cruise$HBV51OpenDmd," hbv 51_1 at cruise 
                    compression efifciency","hbv51_1_cruise.jpeg")


## engine to engine comparision ##
## check number of records for each date  on both engines##
table(lt_engine_data$day_mon_y)
table(rt_engine_data$day_mon_y)
## remove excess records compared to rt_engine_data corresponding to particular date ##


## find diff vales for both engines ##
nrow_mn =  (ifelse(nrow(rt_engine_data) > nrow(lt_engine_data),nrow(lt_engine_data),nrow(rt_engine_data)))
nrow_eng = 2487
eng_diff <- data.frame(day_mon_y = character(nrow_eng),ft_al_state = character(nrow_eng), P30V = numeric(nrow_eng),
                       p30delta = numeric(nrow_eng), EPRActual = numeric(nrow_eng), FuelFlowV = numeric(nrow_eng),
                       ce = numeric(nrow_eng))

#eng_diff <- data.frame(NULL)
lt_engine_data = lt_engine_data[1:nrow_mn,]
rt_engine_data = rt_engine_data[1:nrow_mn,]

rt_engine_data$ce = compression_efficiency(rt_engine_data)
lt_engine_data$ce = compression_efficiency(lt_engine_data)

eng_diff <- data.frame(day_mon_y = character(),ft_al_state = character(),P30V = numeric(),p30delta = numeric(),EPRActual = numeric(),
                       FuelFlowV = numeric(), ce = numeric(), NHV = numeric())

cnt = 0
# for(i in 1:nrow_mn){
#   if((rt_engine_data$day_mon_y[i] == lt_engine_data$day_mon_y[i]) & (rt_engine_data$ft_al_state[i] == lt_engine_data$ft_al_state[i])){
#     eng_diff <- rbind(eng_diff,data.frame(day_mon_y = rt_engine_data$day_mon_y[i],ft_al_state = rt_engine_data$ft_al_state[i],
#                                           P30V = (rt_engine_data$P30V[i] - lt_engine_data$P30V[i]),p30delta = (rt_engine_data$p30delta[i] - lt_engine_data$p30delta[i]),
#                                           EPRActual = (rt_engine_data$EPRActual[i] - lt_engine_data$EPRActual[i]),
#                                           FuelFlowV = (rt_engine_data$FuelFlowV[i] - lt_engine_data$FuelFlowV[i]),
#                                           ce = (rt_engine_data$ce[i] - lt_engine_data$ce[i]),NHV = (rt_engine_data$NHV[i] - lt_engine_data$NHV[i])))
#     cnt = cnt+1
#   }
# }



# engine comparision graphs #
#gplot_al_state_demo_ylim(eng_diff,"day_mon_y","P30V",eng_diff$ft_al_state," engine comparision","_eng_comparision.jpeg",-3,4)
#gplot_al_state_demo_ylim(eng_diff,"day_mon_y","p30delta",eng_diff$ft_al_state,"diff engine comparision","_eng_comparision.jpeg",-3,3)
#gplot_al_state_demo_ylim(eng_diff,"day_mon_y","EPRActual",eng_diff$ft_al_state,"diff engine comparision","_eng_comparision.jpeg",-5,6)
#gplot_al_state_demo_ylim(eng_diff,"day_mon_y","FuelFlowV",eng_diff$ft_al_state,"diff engine comparision","_eng_comparision.jpeg",-15,6)
#gplot_al_state_demo_ylim(eng_diff,"day_mon_y","ce",eng_diff$ft_al_state,"diff engine comparision","_eng_comparision.jpeg",-15,10)

gplot_al_state_demo(eng_diff,"day_mon_y","P30V",eng_diff$ft_al_state," engine comparision","_eng_comparision.jpeg")
gplot_al_state_demo(eng_diff,"day_mon_y","p30delta",eng_diff$ft_al_state,"diff engine comparision","_eng_comparision.jpeg")
gplot_al_state_demo(eng_diff,"day_mon_y","EPRActual",eng_diff$ft_al_state,"diff engine comparision","_eng_comparision.jpeg")
gplot_al_state_demo(eng_diff,"day_mon_y","FuelFlowV",eng_diff$ft_al_state,"diff engine comparision","_eng_comparision.jpeg")
gplot_al_state_demo(eng_diff,"day_mon_y","ce",eng_diff$ft_al_state,"diff engine comparision","_eng_comparision.jpeg")

gplot_al_state_demo(eng_diff,"p30delta","P30V",eng_diff$ft_al_state,"diff engine comparision","_diff_eng_comparision.jpeg")
gplot_al_state_demo(eng_diff,"p30delta","NHV",eng_diff$ft_al_state,"diff engine comparision","_diff_eng_comparision.jpeg")
gplot_al_state_demo(eng_diff,"EPRActual","FuelFlowV",eng_diff$ft_al_state,"diff engine comparision","_diff_eng_comparision.jpeg")


ggplot(eng_diff,aes_string("day_mon_y","P30V"))+geom_point(aes(colour = factor(eng_diff$ft_al_state)),alpha =1/2)+scale_size_area()+
  ggtitle(paste("day_mon_y vs P30V diff engine comparision",sep=""))+scale_colour_brewer(palette = "Set1")+
  theme_bw()+theme(axis.text.x = element_text(angle = 45, hjust = 1))


## Method 3 ##
gplot_al_tr_fls_norm("day_mon_y","TGTV")

# TGTV vs P30V #
gplot_al_tr_fls_norm("P30V","TGTV")

# TGTV vs p30delta #
gplot_al_tr_fls_norm("p30delta","TGTV")

# TGTV vs NHV #
gplot_al_tr_fls_norm("NHV","TGTV")


## Method 4 ##
gplot_al_tr_fls_norm("day_mon_y","FuelFlowV")

# fuelflow vs nhv #
gplot_al_tr_fls_norm("NHV","FuelFlowV")

# at steady state/cruise #
# fuelflow #
gplot_al_state_demo(rt_cruise,"day_mon_y","FuelFlowV",rt_cruise$HBV51OpenDmd," hbv 51_1 at cruise","hbv51_1_cruise.jpeg")

# fuelswitchon # 
gplot_al_state_demo(rt_cruise,"day_mon_y","FuelSwitchOnV",rt_cruise$HBV51OpenDmd," hbv 51_1 at cruise","hbv51_1_cruise.jpeg")

sov_open_data = subset(rt_engine_data, SOVPosV == "TRUE")
sov_open_cruise = subset(rt_cruise, SOVPosV == "TRUE")

gplot_al_state_demo(sov_open_data,"day_mon_y","FuelFlowV",sov_open_data$HBV51OpenDmd," hbv 51_1 at SOV open","hbv51_1_sovopen.jpeg")
gplot_al_state_demo(sov_open_data,"NHV","FuelFlowV",sov_open_data$HBV51OpenDmd," hbv 51_1 at SOV open","hbv51_1_sovopen.jpeg")
gplot_al_state_demo(sov_open_data,"NHV","p30delta",sov_open_data$HBV51OpenDmd," hbv 51_1 at SOV open","hbv51_1_sovopen.jpeg")


gplot_al_state_demo(sov_open_cruise,"day_mon_y","FuelFlowV",sov_open_cruise$HBV51OpenDmd," hbv 51_1 at SOV open cruise","hbv51_1_sovopen_cruise.jpeg")
gplot_al_state_demo(sov_open_cruise,"NHV","FuelFlowV",sov_open_cruise$HBV51OpenDmd," hbv 51_1 at SOV open cruise","hbv51_1_sovopen_cruise.jpeg")
gplot_al_state_demo(sov_open_cruise,"NHV","p30delta",sov_open_cruise$HBV51OpenDmd," hbv 51_1 at SOV open cruise","hbv51_1_sovopen_cruise.jpeg")


rm(sov_open_data,sov_open_cruise)



## rt axis ##
# use facet_grid(~.HBV51OpenDmd) or facet_grid(HBV51OpenDmd ~ HBV52OpenDmd) or facet_grid(HBV51OpenDmd+HBV52OpenDmd ~ HBV53OpenDmd) #

## model actual hbv signal ##
#install.packages("purrr")
#library(purrr)
library(dplyr)
gp_ft_rtengine <- rt_engine_data %>% split(.$ft_num) # %>% map(mean)
dly_51 = data.frame(delay = numeric(),time_sec = character(),act_hbv = character(),ft_num = numeric(),P30V = numeric(), NHV = numeric(),
                    hbvrotation = character(),acP0V = numeric())
dl_cnt =0
  
# for each flight
for ( j in 1:length(gp_ft_rtengine) ){
  for (i in 1:(nrow(gp_ft_rtengine[[j]]) - 1)) {
    if((gp_ft_rtengine[[j]]$HBV51OpenDmd[i] == TRUE) & (abs(gp_ft_rtengine[[j]]$p30delta[i]) > 0) ){
       #& ((abs(gp_ft_rtengine[[j]]$p30delta[i]) > mean(abs(gp_ft_rtengine[[j]]$p30delta))) | 
       # (abs(gp_ft_rtengine[[j]]$p30delta[i]) == max(abs(gp_ft_rtengine[[j]]$p30delta))))){
      dl_cnt = dl_cnt + 1
    
      if(abs(gp_ft_rtengine[[j]]$p30delta[i+1]) == 0){
        dly_51 <- rbind(dly_51,data.frame(delay = dl_cnt, time_sec = gp_ft_rtengine[[j]]$time_sec[i],act_hbv = TRUE,ft_num = gp_ft_rtengine[[j]]$ft_num[i],
                        P30V = gp_ft_rtengine[[j]]$P30V[i], NHV = gp_ft_rtengine[[j]]$NHV[i], hbvrotation = gp_ft_rtengine[[j]]$HBVRotationID[i],
                        acP0V = gp_ft_rtengine[[j]]$acP0V[i]))
        dlf <- dl_cnt
        dl_cnt = 0
      }
    }
    else if (gp_ft_rtengine[[j]]$HBV51OpenDmd[i] == FALSE)
      {
        dly_51 <- rbind(dly_51,data.frame(delay = 0,time_sec = (gp_ft_rtengine[[j]]$time_sec[i]+dlf),act_hbv = FALSE, ft_num = gp_ft_rtengine[[j]]$ft_num[i],
                        P30V = gp_ft_rtengine[[j]]$P30V[i], NHV = gp_ft_rtengine[[j]]$NHV[i], hbvrotation = gp_ft_rtengine[[j]]$HBVRotationID[i],
                        acP0V = gp_ft_rtengine[[j]]$acP0V[i]))
        }
    }
}  
rm(dlf,dl_cnt)
dly_52 = data.frame(delay = numeric(),time_sec = character(),act_hbv = character(),ft_num = numeric(),P30V = numeric(), NHV = numeric(),hbvrotation = character())
dl_cnt =0
for ( j in 1:length(gp_ft_rtengine) ){
  for (i in 1:(nrow(gp_ft_rtengine[[j]]) - 1)) {
    if((gp_ft_rtengine[[j]]$HBV52OpenDmd[i] == TRUE) & (abs(gp_ft_rtengine[[j]]$p30delta[i]) > 0) ){
      #& ((abs(gp_ft_rtengine[[j]]$p30delta[i]) > mean(abs(gp_ft_rtengine[[j]]$p30delta))) | 
      # (abs(gp_ft_rtengine[[j]]$p30delta[i]) == max(abs(gp_ft_rtengine[[j]]$p30delta))))){
      dl_cnt = dl_cnt + 1
      
      if(abs(gp_ft_rtengine[[j]]$p30delta[i+1]) == 0){
        dly_52 <- rbind(dly_52,data.frame(delay = dl_cnt, time_sec = gp_ft_rtengine[[j]]$time_sec[i],act_hbv = TRUE,ft_num = gp_ft_rtengine[[j]]$ft_num[i],
                        P30V = gp_ft_rtengine[[j]]$P30V[i], NHV = gp_ft_rtengine[[j]]$NHV[i], hbvrotation = gp_ft_rtengine[[j]]$HBVRotationID[i]))
        dlf <- dl_cnt
        dl_cnt = 0
      }
    }
      else if(gp_ft_rtengine[[j]]$HBV52OpenDmd[i] == FALSE){
        dly_52 <- rbind(dly_52,data.frame(delay = 0,time_sec = (gp_ft_rtengine[[j]]$time_sec[i]+dlf),act_hbv = FALSE, ft_num = gp_ft_rtengine[[j]]$ft_num[i],
                        P30V = gp_ft_rtengine[[j]]$P30V[i], NHV = gp_ft_rtengine[[j]]$NHV[i], hbvrotation = gp_ft_rtengine[[j]]$HBVRotationID[i]))
      }
  }
}  
rm(dlf,dl_cnt)
dly_53 = data.frame(delay = numeric(),time_sec = character(),act_hbv = character(),ft_num = numeric(),P30V = numeric(), NHV = numeric(),hbvrotation = character())
dl_cnt =0
for ( j in 1:length(gp_ft_rtengine) ){
  for (i in 1:(nrow(gp_ft_rtengine[[j]]) - 1)) {
    if((gp_ft_rtengine[[j]]$HBV53OpenDmd[i] == TRUE) & (abs(gp_ft_rtengine[[j]]$p30delta[i]) > 0) ){
      #& ((abs(gp_ft_rtengine[[j]]$p30delta[i]) > mean(abs(gp_ft_rtengine[[j]]$p30delta))) | 
      # (abs(gp_ft_rtengine[[j]]$p30delta[i]) == max(abs(gp_ft_rtengine[[j]]$p30delta))))){
      dl_cnt = dl_cnt + 1
      
      if(abs(gp_ft_rtengine[[j]]$p30delta[i+1]) == 0){
        dly_53 <- rbind(dly_53,data.frame(delay = dl_cnt, time_sec = gp_ft_rtengine[[j]]$time_sec[i],act_hbv = TRUE,ft_num = gp_ft_rtengine[[j]]$ft_num[i],
                        P30V = gp_ft_rtengine[[j]]$P30V[i], NHV = gp_ft_rtengine[[j]]$NHV[i], hbvrotation = gp_ft_rtengine[[j]]$HBVRotationID[i]))
        dlf <- dl_cnt
        dl_cnt = 0
      }
    }
      else if(gp_ft_rtengine[[j]]$HBV53OpenDmd[i] == FALSE){
        dly_53 <- rbind(dly_53,data.frame(delay = 0,time_sec = (gp_ft_rtengine[[j]]$time_sec[i]+dlf),act_hbv = FALSE, ft_num = gp_ft_rtengine[[j]]$ft_num[i],
                        P30V = gp_ft_rtengine[[j]]$P30V[i], NHV = gp_ft_rtengine[[j]]$NHV[i], hbvrotation = gp_ft_rtengine[[j]]$HBVRotationID[i]))
      }
  }
}  
rm(dl_cnt,dlf)
dly_8 = data.frame(delay = numeric(),time_sec = character(),act_hbv = character(),ft_num = numeric(),P30V = numeric(), NHV = numeric(),hbvrotation = character())
dl_cnt =0
for ( j in 1:length(gp_ft_rtengine) ){
  for (i in 1:(nrow(gp_ft_rtengine[[j]]) - 1)) {
    if((gp_ft_rtengine[[j]]$HBV8OpenDmd[i] == TRUE) & (abs(gp_ft_rtengine[[j]]$p30delta[i]) > 0) ){
      #& ((abs(gp_ft_rtengine[[j]]$p30delta[i]) > mean(abs(gp_ft_rtengine[[j]]$p30delta))) | 
      # (abs(gp_ft_rtengine[[j]]$p30delta[i]) == max(abs(gp_ft_rtengine[[j]]$p30delta))))){
      dl_cnt = dl_cnt + 1
      
      if(abs(gp_ft_rtengine[[j]]$p30delta[i+1]) == 0){
        dly_8 <- rbind(dly_8,data.frame(delay = dl_cnt, time_sec = gp_ft_rtengine[[j]]$time_sec[i],act_hbv = TRUE,ft_num = gp_ft_rtengine[[j]]$ft_num[i],
                       P30V = gp_ft_rtengine[[j]]$P30V[i], NHV = gp_ft_rtengine[[j]]$NHV[i], hbvrotation = gp_ft_rtengine[[j]]$HBVRotationID[i]))
        dlf <- dl_cnt
        dl_cnt = 0
      }
    }
    else if(gp_ft_rtengine[[j]]$HBV8OpenDmd[i] == FALSE)
      {
        dly_8 <- rbind(dly_8,data.frame(delay = dl_cnt,time_sec = (gp_ft_rtengine[[j]]$time_sec[i]+dlf),act_hbv = FALSE, ft_num = gp_ft_rtengine[[j]]$ft_num[i],
                       P30V = gp_ft_rtengine[[j]]$P30V[i], NHV = gp_ft_rtengine[[j]]$NHV[i], hbvrotation = gp_ft_rtengine[[j]]$HBVRotationID[i]))
      }
  }
}




ggplot(data = dly_51,aes(x=time_sec,y=act_hbv51)) + geom_point(aes(color = ft_num)) +theme_bw()+theme(axis.text = element_text(angle = 45, hjust = 1))
p1 <- ggplot(subset(dly_51,ft_num == 1))+geom_point(aes(time_sec,act_hbv51,color = "red"))
ggplot()+geom_point(data = subset(rt_engine_data,ft_num == 1),aes(time_sec,HBV51OpenDmd,color = "blue"))+scale_size_area()
db <- subset(rt_engine_data,ft_num == 1)

ggplot(data = dly_8,aes(x=time_sec,y=act_hbv8)) + geom_point(aes(color = ft_num)) +theme_bw()+theme(axis.text = element_text(angle = 45, hjust = 1))
ggplot(subset(dly_8,ft_num == 2))+geom_point(aes(time_sec,act_hbv8,color = "red"))
ggplot()+geom_point(data = subset(rt_engine_data,ft_num == 2),aes(time_sec,HBV8OpenDmd,color = "blue"))+scale_size_area()

## hbv 51 delay vs time ##
ggplot(data = dly_51,aes(x=time_sec,y=delay))+geom_point()+ylim(0,1000)+ggtitle("hbv 51 delay vs time")+theme_bw()+theme(axis.text = element_text(angle = 45, hjust = 1))
ggplot(subset(dly_51,ft_num == 1),aes(x=time_sec,y=delay))+geom_point()+ylim(0,1000)+ggtitle("hbv 51 flight 1 delay vs time")+theme_bw()+theme(axis.text = element_text(angle = 45, hjust = 1))

library(grid)
library(gtable)

p1 <-ggplot(subset(dly_51,ft_num == 2),aes(x=time_sec,y=delay))+geom_point()+ylim(0,100)+ggtitle("hbv 51 flight 2 delay vs time")+theme_bw()+theme(axis.text = element_text(angle = 45, hjust = 1))
p2 <- ggplot(subset(dly_51,ft_num == 2),aes(x=time_sec,y=act_hbv51))+geom_point()+theme_bw()+theme(axis.text = element_text(angle = 45, hjust = 1))
p3 <- ggplot(subset(rt_engine_data,ft_num == 2),aes(x=time_sec, y =HBV51OpenDmd))+geom_point()+theme_bw()+theme(axis.text = element_text(angle = 45, hjust = 1))
grid.newpage()
grid.draw(rbind(ggplotGrob(p1),ggplotGrob(p2),ggplotGrob(p3),size = "last"))



## hbv scheduling ##

# delay vs P30V #
gplot_al_state_demo(dly_51,"P30V","delay")

# delay vs NHV #



# Number of true valves vs NHV #
dly_53$tr_valve = dly_51$act_hbv51[1:39108]+dly_52$act_hbv[1:39108]+dly_53$act_hbv[1:39108]+dly_8$act_hbv8[1:39108]
gplot_al_state_demo(dly_53,"NHV","tr_valve",dly_53$ft_num,"number of true valve vs NHV","number_of_true_valve_vs_NHV.jpeg")



## step change - method 5 ##
## +ve,-ve,0 ##
for(i in 1:nrow(rt_engine_data)){
  if((HBV51OpenDmd == TRUE) & (P30V[i+1] - P30V[i] > 0)){
    rt_engine_data$stp_chg[i] = "+ve"
  }
}




ft_grp_rt <- split(rt_engine_data,rt_engine_data$ft_al_state)

hbv5x_gp <- rt_engine_data %>% group_by(ft_num)%>% summarise()#<-customised function inside))


# customised function
# cust_fn <- function(df){
#  for(i in 1:nrow(df)){
#    if((rt_engine_data$HBV51OpenDmd[i] == TRUE) & (abs(rt_engine_data$p30delta[i]) > 0) & 
#       ((abs(rt_engine_data$p30delta[i]) > mean(abs(rt_engine_data$p30delta))) | 
#        (abs(rt_engine_data$p30delta[i]) == max(abs(rt_engine_data$p30delta))))){
#      dl_cnt = dl_cnt + 1
#    }
#    if((rt_engine_data$HBV51OpenDmd[i+1] == FALSE) & (abs(rt_engine_data$p30delta[i+1]) == 0)){
#      dly_51$delay[i] = dl_cnt
#      dly_51$time_sec[i] = rt_engine_data$time_sec[i]
#      dly_51$act_hbv51[i] = TRUE
#      dly_51$ft_num[i] = rt_engine_data$ft_num[i]
#      dl_cnt = 0
#    }
#    else{
#      dly_51$act_hbv51[i] = FALSE
#      dly_51$time_sec[i] = rt_engine_data$time_sec[i]
#      dly_51$ft_num[i] = rt_engine_data$ft_num[i]
#    }
#  }
#}


# overlay rt and left engine data #
# P30V #
(p4 <- ggplot() + geom_point(data = rt_engine_data,aes(x=day_mon_y,y=P30V),color = 'blue') +
      geom_point(data = lt_engine_data,aes(x=day_mon_y,y=P30V),color = 'red',shape = 4)+theme_bw()+theme(axis.text = element_text(angle = 45, hjust = 1))+
      ggtitle("overlay plot right and left engine data"))
# NHV #
ggplot() + geom_point(data = rt_engine_data,aes(x=day_mon_y,y=NHV),color = 'blue') +
  geom_point(data = lt_engine_data,aes(x=day_mon_y,y=NHV),color = 'red',shape = 4)+theme_bw()+theme(axis.text = element_text(angle = 45, hjust = 1))+
  ggtitle("overlay plot right and left engine data NHV")
# P30V vs NHV overlay plot #
ggplot() + geom_point(data = rt_engine_data,aes(x=NHV,y=P30V),color = 'blue') +
  geom_point(data = lt_engine_data,aes(x=NHV,y=P30V),color = 'red',shape = 4)+theme_bw()+theme(axis.text = element_text(angle = 45, hjust = 1))+
  ggtitle("overlay plot right and left engine data P30V vs NHV")


# Method 6 #
#  how long hbv open each flight #
#library(dplyr)
gp_ft_hbv51 <- dly_51 %>% split(.$ft_num)
hbv_tr_cnt <- data.frame(cnt = numeric(),ft_num = numeric(),P30V = numeric(),P0V = numeric(),NHV = numeric())

state_change <- funtion(x){
  
}

for (j in 1:length(gp_ft_hbv51)) {
  hbv_tr_cnt <- rbind(hbv_tr_cnt,data.frame(cnt = sum(gp_ft_hbv51[[j]]$act_hbv), ft_num = mean(gp_ft_hbv51[[j]]$ft_num), P30V = mean(gp_ft_hbv51[[j]]$P30V),
                                            acP0V = mean(gp_ft_hbv51[[j]]$acP0V),NHV = mean(gp_ft_hbv51[[j]]$NHV)))  
    
} 

ggplot(data = hbv_tr_cnt,aes(ft_num,cnt))+geom_line()+theme_bw()+theme(axis.text = element_text(angle = 45, hjust = 1))+
  ggtitle("number of seconds hbv open for each flight")

ggplot(data = hbv_tr_cnt,aes(P30V,cnt))+geom_line()+theme_bw()+theme(axis.text = element_text(angle = 45, hjust = 1))+
  ggtitle("number of seconds hbv open vs P30V")

ggplot(data = hbv_tr_cnt,aes(NHV,cnt))+geom_line()+theme_bw()+theme(axis.text = element_text(angle = 45, hjust = 1))+
  ggtitle("number of seconds hbv open vs P30V")

#gplot_al_state_demo(hbv_tr_cnt,"NHV","P30V",hbv_tr_cnt$cnt,"P30V vs NHV hbv open time","_hbv_open_time.jpeg")
ggplot(data = hbv_tr_cnt,aes(NHV,P30V))+geom_line(aes(color = cnt))+theme_bw()+theme(axis.text = element_text(angle = 45, hjust = 1))+
  ggtitle("number of seconds hbv open NHV vs P30V")+scale_color_gradient(low = "red")




#h




































########### NHV P30v 4 valves #################

rt_engine_51_tr = subset(rt_engine_data,HBV51OpenDmd == TRUE)
rt_engine_51_fls = subset(rt_engine_data,HBV51OpenDmd == FALSE)


rm(rt_engine_allv_tr,rt_engine_allv_fls)
###########################################################
#

for(i in c("EPRActual","FuelFlowV","acairspeedv_data_S","altitude_data","NLV","NHV","P30V","T20V","p30delta","NHRTH20V","TGTV")){
  for(j in c("EPRActual","FuelFlowV","acairspeedv_data_S","altitude_data","NLV")){
    ggplot(rt_engine_data,aes(i,j))+geom_point(aes(colour = factor(ft_state)),alpha =1/2)+geom_smooth()+scale_size_area()+
      ggtitle(paste(i," vs ",j,sep = ""))+scale_colour_brewer(palette = "Set1")
    
  }
}

# p=(noquote(i))
# q=(noquote(j))
# ggplot(rt_engine_data,aes(p,q))+geom_point(aes(colour = factor(ft_state)),alpha =1/2)+geom_smooth()+scale_size_area()+
#   ggtitle(paste(i," vs ",j,sep = ""))+scale_colour_brewer(palette = "Set1")
