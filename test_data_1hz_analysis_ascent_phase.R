# 1hz test data analysis #
# # mostly concentrated at engine start or in ascent phase #
# altitude diff
plot(rt_1hz_data$time_sec,rt_1hz_data$altitude_data)
points(lt_1hz_data$time_sec,lt_1hz_data$altitude_data,col="blue")

rt_1hz_data$time_sec = as.POSIXct(rt_1hz_data$Date.Time, format = "(%d-%b-%Y) %H:%M:%OS")
lt_1hz_data$time_sec = as.POSIXct(lt_1hz_data$Date.Time,format = "(%d-%b-%Y) %H:%M:%OS")

rt_1hz_data$day_mon_y = strftime(rt_1hz_data$time_sec,format = "%d-%b-%Y")
lt_1hz_data$day_mon_y = strftime(lt_1hz_data$time_sec,format = "%d-%b-%Y")

rt_1hz_data$hr_min = as.numeric(format(rt_1hz_data$time_sec,"%H"))+as.numeric(format(rt_1hz_data$time_sec,"%M"))/60+
  as.numeric(format(rt_1hz_data$time_sec,"%S"))/3600

lt_1hz_data$hr_min = as.numeric(format(lt_1hz_data$time_sec,"%H"))+as.numeric(format(lt_1hz_data$time_sec,"%M"))/60+
  as.numeric(format(lt_1hz_data$time_sec,"%S"))/3600


# p30 threshold for initial spike at engine start
library(scales)
ggplot(data = rt_1hz_data)+geom_point(aes(hr_min,P30V,color = factor(ft_num)))+facet_grid(ft_num~.)+scale_x_continuous()


# altitude facet_grid plot
ggplot()+geom_point(data = rt_1hz_data,aes(hr_min,P30V,color = factor(ft_num)))+facet_grid(ft_num~.)+scale_x_continuous()#+
  geom_point(data = rt_test_data,aes(hr_min,altitude_data,color = factor(ft_num)))+theme_bw()+theme(axis.text = element_text(angle = 45, hjust = 1))


exp = subset(lt_1hz_data,ft_num == 10)
ggplot(data = exp)+geom_point(aes(hr_min,P30V,color = "P30"))+geom_point(aes(hr_min,altitude_data/100,color = "altitude/100"))+
  geom_point(aes(hr_min,NHV,color = "speed"))+ylab("")

table(exp$day_mon_y)

# density plot , y axis gives count in density
ggplot(rt_1hz_data,aes(P30V))+geom_density(aes(color=ft_num))+geom_density()

# Grouping / Ovarlay P30V for all flights
ggplot(data = rt_1hz_data,aes(x=hr_min,y=P30V,color = ft_num))+geom_line(aes(group = ft_num))+geom_point()+
  scale_color_gradientn(colours=rainbow(7))

p30_sp <- subset(rt_1hz_data, P30V >300)
ggplot(data = p30_sp,aes(x=hr_min,y=P30V,color = ft_num))+geom_line(aes(group = ft_num))+geom_point()+
  scale_color_gradientn(colours=rainbow(7))
table(p30_sp$ft_num)


cols_reqd = c("P30V","p30mod_data","p30delta","NHRTH20V","NHRTH26V","SOVPosV","EPRActual","FuelFlowV","NHV","NLV","P20V","P50V",
              "T20V","TGTV","T30V","acairspeedv_data_S","altitude_data","HBV51OpenDmd","HBV52OpenDmd","HBV53OpenDmd",
              "HBV8OpenDmd","FuelSwitchOnV","acP0V","CJCTemperature","TFuelV_S","NHMinDem","hr_min","ft_num")
p30_sp = p30_sp[,cols_reqd]
#p30_sp$p30a = p30_sp$P30V
# bin and find average of P30
#library(plyr)
#p30_bin <- ddply(p30_sp,.(cut(p30_sp$P30V,10)),colwise(mean))

ggplot(data = p30_bin,aes(x=hr_min))+geom_line(aes(y=p30a,col = "p30"))+geom_point(aes(y=p30a,col = "p30"))#+geom_point()


# first group by flight number and then by p30 bin
library(dplyr)
# 10 bins
p30_b <- p30_sp %>% group_by(p30_sp$'ft_num') %>% mutate(p30 = ntile(P30V,10)) %>% group_by(p30,ft_num) %>%
  summarise_each(funs(mean))
# 5 bins
p30_b5 <- p30_sp %>% group_by(p30_sp$'ft_num') %>% mutate(p30 = ntile(P30V,5)) %>% group_by(p30,ft_num) %>%
  summarise_each(funs(mean))

ggplot(data = p30_b5,aes(hr_min))+geom_line(aes(y=P30V,col="p30"))+geom_point(aes(y=P30V))+facet_grid(ft_num~.)+
  geom_line(aes(y=(HBV51OpenDmd*200),col="hbv51"))

ggplot(data= p30_b5,aes(P30V,y=TGTV,color=ft_num))+geom_line(aes(group= ft_num))+scale_color_gradientn(colours = rainbow(7))

ggplot(data = p30_b5,aes(NHV,FuelFlowV,color = p30))+geom_point(aes(group = p30))+scale_color_gradientn(colours = rainbow(5))+
  geom_line(aes(NHV,TGTV*5,color = p30))

ggplot(data = p30_b5,aes(NHV,P30V,color = factor(ft_num)))+geom_line(aes(group = factor(ft_num)))+scale_color_brewer(palette = "Set3")#+
  geom_line(aes(NHV,TGTV/3,color = p30))
  

# full data plots #
library(RColorBrewer)
colourCount = length(unique(rt_engine_data$ft_num))
getPalette = colorRampPalette(brewer.pal(12, "Set3"))

ggplot(data = rt_engine_data,aes(NHV,P30V,color = factor(ft_num)))+geom_line(aes(group = factor(ft_num)))+
  scale_fill_manual(values = getPalette(colourCount))

library(directlabels)
#ggplot(data = rt_engine_data,aes(NHV,P30V,color = factor(ft_num)))+geom_line(aes(group = factor(ft_num)))+scale_color_discrete(guide = 'none')+
#  scale_x_discrete(expand = c(0,1))+geom_dl(aes(label = ft_num),method = list(dl.combine("first.points","last.points"),cex = 0.8))

ggplot(data = p30_b5,aes(P30V, FuelFlowV,color = factor(ft_num)))+geom_line(aes(group = factor(ft_num)))+scale_color_discrete(guide = 'none')+
  scale_x_discrete(expand = c(0,1))+geom_dl(aes(label = ft_num),method = list(dl.combine("first.points","last.points"),cex = 0.8))


ggplot(data = p30_b5,aes(ft_num,P30V))+geom_boxplot(aes(group = p30))

# bvhp behaviour #
ggplot(data = rt_1hz_data)+geom_point(aes(hr_min,HBV51OpenDmd,col = "hbv51_dmd"))+
  geom_point(aes(hr_min,bvhp51drivestate,col = "bvhp_51"))


# altitude derivative flight mode #
dv_altitude <- function(x,y){
  alf <- ifelse(x>0 & y>0,"pos-gradient",ifelse(x<0 & y>0,"neg-gradient",ifelse(x==0 & y>1500,"zero-gradient","zero-alt")))
  return(alf)
}


#df <- rt_1hz_data %>% group_by(ft_num) %>% mutate(al_diff = altitude_data - lag(altitude_data,default = -1)) %>%
#  summarise(dv_ft = dv_altitude(al_diff))

df <- rt_1hz_data %>% group_by(ft_num) %>% mutate(al_diff = altitude_data - lag(altitude_data,default = -1)) %>%
  do(data.frame(dv_ft = dv_altitude(.$al_diff,.$altitude_data)))

rt_1hz_data = cbind(rt_1hz_data,ft_n1 = df$ft_num,dv_ft = df$dv_ft)
rt_1hz_data$ft_n1 <- NULL

ggplot(data = subset(rt_1hz_data,ft_num ==14 ),aes(hr_min,altitude_data,col = factor(dv_ft)))+geom_point()
  
ggplot(data = subset(rt_1hz_data,ft_num==2),aes(hr_min,HBV51OpenDmd,col = "hbv51"))+geom_point()+geom_point(aes(hr_min,dv_ft,col="derivative"))+
  geom_point(aes(hr_min,bvhp51drivestate,col="bvhp51"))+ylab("")


hbv_alldmd_tr = subset(rt_1hz_data, (HBV51OpenDmd & HBV52OpenDmd & HBV53OpenDmd & HBV8OpenDmd) == TRUE)
hbv_alldmd_fls = subset(rt_1hz_data, (HBV51OpenDmd == FALSE) & (HBV52OpenDmd == FALSE) & (HBV53OpenDmd == FALSE) & (HBV8OpenDmd == FALSE))


#ggplot(data = rt_1hz_data)+geom_point(aes(hr_min,as.numeric(HBV51OpenDmd),col="51"))+geom_point(aes(hr_min,as.numeric(HBV52OpenDmd)*2,col="52"))+
#  geom_point(aes(hr_min,as.numeric(HBV53OpenDmd)*3,col="53"))+geom_point(aes(hr_min,as.numeric(HBV8OpenDmd)*5,col="8"))
library(gridExtra)
p1 <- ggplot(rt_1hz_data,aes(hr_min,HBV51OpenDmd))+geom_point()
p2 <- ggplot(rt_1hz_data,aes(hr_min,HBV52OpenDmd))+geom_point()
p3 <- ggplot(rt_1hz_data,aes(hr_min,HBV53OpenDmd))+geom_point()
p4 <- ggplot(rt_1hz_data,aes(hr_min,HBV8OpenDmd))+geom_point()
grid.arrange(p1,p2,p3,p4, ncol =2,nrow =2)
rm(p1,p2,p3,p4)




## hbv true variation analysis ##(NHV,TGTV,P30V,FuelFlowV,EPRActual,TFuelV_S)
ggplot(hbv_alldmd_fls)+geom_point(aes(NHV,EPRActual,col =dv_ft))
ggplot(hbv_alldmd_tr)+geom_point(aes(P30V,TGTV,col =dv_ft))

ggplot(hbv_alldmd_tr,aes(dv_ft,P30V))+geom_boxplot(aes(col ="hbv true"))+geom_boxplot(data = hbv_alldmd_fls,aes(col = "hbv false"))

ggplot(rt_1hz_data)+geom_point(aes(ft_num,P30V,col = factor(HBV51OpenDmd)))#+geom_point(aes(ft_num,as.numeric(HBV51OpenDmd)*10))

# P30,hbv,flights facet-grid
ggplot()+geom_point(data = rt_1hz_data,aes(hr_min,P30V,col = "p30"))+geom_point(data = rt_1hz_data,aes(hr_min,as.numeric(HBV51OpenDmd)*100))+
  facet_grid(ft_num~.)+geom_point(data = rt_test_data,aes(hr_min,P30V,col = "p30"))+
  geom_point(data = rt_test_data,aes(hr_min,as.numeric(HBV51OpenDmd)*100))
# P30 derivative#
dv_p30 <- function(x){
    alf <- ifelse(x>0,"posg-p30",ifelse(x<0,"negg-p30","zerog-p30"))
    return(alf)
}
dv_p30thr <- function(x){
  return(ifelse(x>0.025,"pos-g",ifelse(x<0.025,"neg-g","zero-g")))
}

df <- rt_1hz_data %>% group_by(ft_num) %>% mutate(p30_diff = p30delta - lag(p30delta,default = 0)) %>%
  do(data.frame(dv_p30 = dv_p30(.$p30_diff), dv_p30thr = dv_p30thr(.$p30_diff) ))

rt_1hz_data <- cbind(rt_1hz_data,dv_p30 = df$dv_p30)
rt_1hz_data$`df$dv_p30` <- NULL
#which(colnames(rt_1hz_data) == "dv_ft")
#rt_1hz_data[,109] <- NULL

ggplot(subset(rt_1hz_data,ft_num == 1))+geom_point(aes(hr_min,as.numeric(dv_p30)*2,col="p30"))+geom_point(aes(hr_min,p30delta,col="P30"))

ggplot(df,aes(rt_1hz_data$hr_min,dv_p30thr))+geom_point()
# 0.05 is threshold can be cut off for derivative #

#Engine start#
ggplot(rt_1hz_data,aes(altitude_data,NHV))+geom_point()+xlim(-100,1500)
ggplot(rt_1hz_data,aes(altitude_data,P30V))+geom_point()+xlim(-100,1500)

ggplot(norm_1hz_data,aes(ADS_1_Pressure_Altitude,FADEC_R_Eng_n2_Speed))+geom_point()+xlim(-100,1500)
ggplot(norm_1hz_data,aes(ADS_1_Pressure_Altitude,FADEC_R_Eng_p30_Burner))+geom_point()+xlim(-100,1500)
# At engine start you can see maximum of P30V and NHV values #

rt_engine_start = subset(rt_1hz_data, ((altitude_data <= 100) & (FuelSwitchOnV == TRUE) & (FuelFlowV > 0) & (SOVPosV == TRUE)))

library(directlabels)
ggplot(data = rt_engine_start,aes(NHV, P30V,color = factor(ft_num)))+geom_line(aes(group = factor(ft_num)))+scale_color_discrete(guide = 'none')+
  #scale_x_discrete(expand = c(0,1))+
  geom_dl(aes(label = ft_num),method = list(dl.combine("first.points","last.points"),cex = 0.8))+
  ggtitle("Engine start P30V vs NHV")+theme_bw()+theme(axis.text = element_text(angle = 45, hjust = 1))

# check service data normal and flt behaviour#
#norm_engine_start = subset(norm_1hz_data,((ADS_1_Pressure_Altitude <= 100) & (FADEC_R_Eng_fuel_Flow > 0) & (FADEC_R_Eng_hpsov_Position == TRUE) &
#                                            (FADEC_R_Eng_fuel_Temp <= 40)))
norm_engine_start = subset(norm_1hz_data,AHTMS_Flight_Mode == 2)

ggplot(norm_1hz_data,aes(AHTMS_Time,FADEC_R_Eng_fuel_Temp))+geom_point()

ggplot(data = norm_engine_start,aes(FADEC_R_Eng_n2_Speed, FADEC_R_Eng_p30_Burner,color = factor(ft_num)))+geom_line(aes(group = factor(ft_num)))+scale_color_discrete(guide = 'none')+
  #scale_x_discrete(expand = c(0,1))+
  geom_dl(aes(label = ft_num),method = list(dl.combine("first.points","last.points"),cex = 0.8))+
  ggtitle("Engine start P30V vs NHV")+theme_bw()+theme(axis.text = element_text(angle = 45, hjust = 1))

# fault data #
ggplot(flt_data_1hz,aes(AHTMS_Time,FADEC_R_Eng_fuel_Temp))+geom_point()
#flt_engine_start = subset(flt_data_1hz,((ADS_1_Pressure_Altitude <= 100) & (FADEC_R_Eng_fuel_Flow > 0) & (FADEC_R_Eng_hpsov_Position == TRUE) &
#                                            (FADEC_R_Eng_fuel_Temp <= 40)))
flt_engine_start = subset(flt_data_1hz,AHTMS_Flight_Mode ==2)

ggplot(data = flt_engine_start,aes(FADEC_R_Eng_n2_Speed, FADEC_R_Eng_p30_Burner,color = factor(ft_num)))+geom_line(aes(group = factor(ft_num)))+scale_color_discrete(guide = 'none')+
  #scale_x_discrete(expand = c(0,1))+
  geom_dl(aes(label = ft_num),method = list(dl.combine("first.points","last.points"),cex = 0.8))+
  ggtitle("Engine start P30V vs NHV")+theme_bw()+theme(axis.text = element_text(angle = 45, hjust = 1))

# fault day data engine start #
#ggplot(flt_day_data,aes(AHTMS_Time,FADEC_R_Eng_fuel_Temp))+geom_point()
#flt_day_engine_start = subset(flt_day_data,((ADS_1_Pressure_Altitude <= 100) & (FADEC_R_Eng_fuel_Flow > 0) & (FADEC_R_Eng_hpsov_Position == TRUE) &
#                                          (FADEC_R_Eng_fuel_Temp <= 40)))

## 4 engine start in same graph#
ggplot()+#geom_line(data = rt_engine_start,aes(NHV,P30V,col = "test data"))+
  geom_line(data = norm_engine_start,aes(FADEC_R_Eng_n2_Speed, FADEC_R_Eng_p30_Burner,col = "norm data"))+
  geom_line(data = flt_engine_start,aes(FADEC_R_Eng_n2_Speed, FADEC_R_Eng_p30_Burner,col = "Fault data"))+
  geom_point(data = flt_day_data,aes(FADEC_R_Eng_n2_Speed, FADEC_R_Eng_p30_Burner,col = "Fault day"))+
  ggtitle(" Engine start Comparision Graph")+theme_bw()+theme(axis.text = element_text(angle = 45, hjust = 1))


# Engine start P30V variation
ggplot()+#geom_point(data = rt_engine_start,aes(hr_min,P30V,col = "test data"))+
  geom_line(data = norm_engine_start,aes(AHTMS_Time, FADEC_R_Eng_p30_Burner,col = "norm data"))+
  geom_line(data = flt_engine_start,aes(AHTMS_Time, FADEC_R_Eng_p30_Burner,col = "Fault data"))+
  geom_point(data = flt_day_data,aes(AHTMS_Time, FADEC_R_Eng_p30_Burner,col = "Fault day"))+
  ggtitle(" Engine start P30V Graph")+theme_bw()+theme(axis.text = element_text(angle = 45, hjust = 1))




## Monitor demand signals P30V #
# number of state changes of HBVDmd #
ggplot(data = subset(rt_1hz_data,ft_num ==5))+geom_point(aes(hr_min,as.numeric(HBV51OpenDmd)*0.6,col="51"))+
  geom_point(aes(hr_min,as.numeric(HBV52OpenDmd)*0.5,col="52"))+geom_point(aes(hr_min,as.numeric(HBV53OpenDmd)*0.4,col="53"))+
  geom_point(aes(hr_min,as.numeric(HBV8OpenDmd)*0.3,col="8"))+geom_line(aes(hr_min,(altitude_data/40000),col="alt"))+
  geom_line(aes(hr_min,p30delta))+ylim(-1,1)+ylab("")

hbv_any_true_test = subset(rt_1hz_data, (HBV51OpenDmd == TRUE | HBV52OpenDmd == TRUE | HBV53OpenDmd == TRUE |
                                           HBV8OpenDmd == TRUE)&(altitude_data > 100) == TRUE)

ggplot(data = hbv_any_true_test)+geom_point(aes(hr_min,as.numeric(HBV51OpenDmd)*0.6,col="51"))+
  geom_point(aes(hr_min,as.numeric(HBV52OpenDmd)*0.5,col="52"))+geom_point(aes(hr_min,as.numeric(HBV53OpenDmd)*0.4,col="53"))+
  geom_point(aes(hr_min,as.numeric(HBV8OpenDmd)*0.3,col="8"))+geom_point(aes(hr_min,P30V/100,group = ft_num))+
  ylab("")#+geom_line(aes(hr_min,TGTV/750,col = "tgt"))#+geom_line(aes(hr_min,(altitude_data/40000),col="alt"))

ggplot(data = hbv_any_true_test)+geom_point(aes(P30V,TGTV,col = HBV51OpenDmd))+geom_point(aes(P30V,TGTV,col = HBV52OpenDmd))+
  geom_point(aes(P30V,TGTV,col = HBV53OpenDmd))+geom_point(aes(P30V,TGTV,col = HBV8OpenDmd))+ggtitle("Any hbv true data")



hbv_52_8_tr = subset(hbv_any_true_test, (HBV52OpenDmd | HBV8OpenDmd ) == TRUE)
ggplot(data = hbv_52_8_tr)+geom_point(aes(hr_min,as.numeric(HBV52OpenDmd)*0.1,col="52"))+
  geom_point(aes(hr_min,as.numeric(HBV8OpenDmd)*0.05,col="8"))+geom_point(aes(hr_min,P30V/100,group = ft_num))+
  ylab("")+ylim(0,0.15)
summary(hbv_52_8_tr$P30V)
summary(hbv_52_8_tr$p30delta)
summary(hbv_52_8_tr$NHV)
summary(hbv_52_8_tr$TGTV)
summary(hbv_52_8_tr$FuelFlowV)

hbv_51_53_tr = subset(hbv_any_true_test, (HBV51OpenDmd | HBV53OpenDmd ) == TRUE)
summary(hbv_51_53_tr$P30V)
summary(hbv_51_53_tr$p30delta)
summary(hbv_51_53_tr$NHV)
summary(hbv_51_53_tr$TGTV)
summary(hbv_51_53_tr$FuelFlowV)


# P30,nhv and tgt range for different flight states #
# At cruise,altitude>1500 #
cruise_data = subset(rt_1hz_data, (altitude_data >= (max(altitude_data) - 10000)) & (altitude_data <= max(altitude_data)))
  
#ggplot(subset(rt_1hz_data,ft_num == 13))+geom_point(aes(hr_min,altitude_data))#+ylim(35000,45000)

ggplot(cruise_data)+geom_point(aes(hr_min,P30V/100,col="P30"))+
  geom_point(aes(hr_min,as.numeric(HBV51OpenDmd)*1.5,col = "51"))+geom_point(aes(hr_min,as.numeric(HBV52OpenDmd)*1.6,col = "52"))+
  geom_point(aes(hr_min,as.numeric(HBV53OpenDmd)*1.7,col = "53"))+geom_point(aes(hr_min,as.numeric(HBV8OpenDmd)*1.8,col = "8"))+
  ylab("")

summary(cruise_data$P30V)

#ggplot(subset(cruise_data,HBV51OpenDmd == TRUE))
ggplot(cruise_data)+geom_point(aes(TGTV,P30V,col = HBV51OpenDmd))

# Range where HBV transition is P30 42 to 137 #
hbv_trans = subset(rt_1hz_data, (P30V > 42) & (P30V <= 137))
ggplot(hbv_trans)+geom_point(aes(NHV,P30V,col = HBV51OpenDmd))
#ggplot(hbv_trans)
ggplot(subset(hbv_trans,ft_num == 5))+#geom_point(aes(hr_min,P30V,col = "p30"))+
  geom_point(aes(hr_min,as.numeric(HBV51OpenDmd)*101,col = "51"))+
  geom_point(aes(hr_min,as.numeric(HBV52OpenDmd)*102,col = "52"))+geom_point(aes(hr_min,as.numeric(HBV53OpenDmd)*104,col = "53"))+
  geom_point(aes(hr_min,as.numeric(HBV8OpenDmd)*103,col = "8"))+geom_line(aes(hr_min,(p30delta)*100,col = "deltap30"))+
  geom_line(aes(hr_min,(EPRActual)*10,col = "epr"))+ylab("")#+ylim(-500,350)#+
  #geom_point(aes(hr_min,p30mod_data/10,col = "p30mod"))

ggplot(subset(hbv_trans,ft_num == 10))+
  geom_point(aes(hr_min,as.numeric(HBV51OpenDmd)*101,col = "51"))+
  geom_point(aes(hr_min,as.numeric(HBV52OpenDmd)*102,col = "52"))+geom_point(aes(hr_min,as.numeric(HBV53OpenDmd)*104,col = "53"))+
  geom_point(aes(hr_min,as.numeric(HBV8OpenDmd)*103,col = "8"))+
  geom_line(aes(hr_min,(EPRActual)*50,col = "epr"))+ylab("")#+ylim()


cor(rt_1hz_data$p30delta,rt_1hz_data$p30mod_data,use = "complete.obs")


#which(flt_day_data$FADEC_R_Eng_p30_Burner > 300)
#flt_day_data[693,]
p300 =subset(rt_1hz_data,P30V > 300)
summary(p300$altitude_data)


# altitude gradient states #
zero_grad = subset(rt_1hz_data, dv_ft = "zero-gradient")
pos_grad = subset(rt_1hz_data,dv_ft == "pos-gradient")
neg_grad = subset(rt_1hz_data,dv_ft == "neg-gradient")

summary(neg_grad$FuelFlowV)

#ggplot(pos_grad)
ggplot(subset(pos_grad,ft_num ==3))+geom_line(aes(hr_min,p30delta,col ="p30delta"))+geom_point(aes(hr_min,as.numeric(HBV51OpenDmd)*0.5,col = "51"))+
  geom_line(aes(hr_min,P30V/300,col = "P30"))+geom_point(aes(hr_min,as.numeric(HBV52OpenDmd)*0.6,col = "52"))+
  geom_point(aes(hr_min,as.numeric(HBV53OpenDmd)*0.8,col = "53"))+geom_point(aes(hr_min,as.numeric(HBV8OpenDmd)*0.9,col = "8"))+
  ylab("")+geom_line(aes(hr_min,altitude_data/40000,col = "alt"))





## correlation ##
## one-way anova ##
library(heplots)
model_aov <- aov(HBV51OpenDmd ~ p30delta+HBV52OpenDmd, data = rt_1hz_data)
summary(model_aov)
p <-etasq(model_aov,partial = TRUE)
 cat("right engine data 1hz data 11flights HBV51 and P30 are ", sqrt(etasq(aov(P30V ~ HBV51OpenDmd, data = rt_1hz_data))$`Partial eta^2`[1])*100,"% correlated")
 cat("right engine data 1hz data 11flights HBV51 and p30delta are ", sqrt(etasq(aov(p30delta ~ HBV51OpenDmd, data = rt_1hz_data))$`Partial eta^2`[1])*100,"% correlated")
 cat("right engine data 1hz data 11flights HBV51 and TGT are ", sqrt(etasq(aov(TGTV ~ HBV51OpenDmd, data = rt_1hz_data))$`Partial eta^2`[1])*100,"% correlated")
 
 cat("right engine data 1hz data 11flights HBV52 and P30 are ", sqrt(etasq(aov(P30V ~ HBV52OpenDmd, data = rt_1hz_data))$`Partial eta^2`[1])*100,"% correlated")
 cat("right engine data 1hz data 11flights HBV52 and p30delta are ", sqrt(etasq(aov(p30delta ~ HBV52OpenDmd, data = rt_1hz_data))$`Partial eta^2`[1])*100,"% correlated")
 cat("right engine data 1hz data 11flights HBV52 and TGT are ", sqrt(etasq(aov(TGTV ~ HBV52OpenDmd, data = rt_1hz_data))$`Partial eta^2`[1])*100,"% correlated")
 
 cat("right engine data 1hz data 11flights HBV53 and P30 are ", sqrt(etasq(aov(P30V ~ HBV53OpenDmd, data = rt_1hz_data))$`Partial eta^2`[1])*100,"% correlated")
 cat("right engine data 1hz data 11flights HBV53 and p30delta are ", sqrt(etasq(aov(p30delta ~ HBV53OpenDmd, data = rt_1hz_data))$`Partial eta^2`[1])*100,"% correlated")
 cat("right engine data 1hz data 11flights HBV53 and TGT are ", sqrt(etasq(aov(TGTV ~ HBV53OpenDmd, data = rt_1hz_data))$`Partial eta^2`[1])*100,"% correlated")
 
 cat("right engine data 1hz data 11flights HBV8 and P30 are ", sqrt(etasq(aov(P30V ~ HBV8OpenDmd, data = rt_1hz_data))$`Partial eta^2`[1])*100,"% correlated")
 cat("right engine data 1hz data 11flights HBV8 and p30delta are ", sqrt(etasq(aov(p30delta ~ HBV8OpenDmd, data = rt_1hz_data))$`Partial eta^2`[1])*100,"% correlated")
 cat("right engine data 1hz data 11flights HBV8 and TGT are ", sqrt(etasq(aov(TGTV ~ HBV8OpenDmd, data = rt_1hz_data))$`Partial eta^2`[1])*100,"% correlated")
 
 
 
 ## Each flight number of hbv state changes,flight number vs mean P30 #
 gp_rt_1hz_data <- rt_1hz_data %>% group_by(rt_1hz_data$'ft_num') %>% mutate(cnt = n(), p30 = mean(P30V), n2_speed = mean(NHV),
                                                                             state_change_1 = sum(abs(diff(HBV51OpenDmd))), time_open_1 = sum(HBV51OpenDmd == 1),
                                                                             time_close_1 = sum(HBV51OpenDmd == 0), md_p30 = median(P30V),
                                                                             md_n2_speed = median(NHV),
                                                                             md_tgt = median(TGTV),
                                                                             md_ffl = median(FuelFlowV),
                                                                             tot_state_change = sum(abs(diff(HBV51OpenDmd)))+
                                                                               sum(abs(diff(HBV52OpenDmd)))+sum(abs(diff(HBV53OpenDmd)))+
                                                                             sum(abs(diff(HBV8OpenDmd))),
                                                                             tot_time_open = sum(HBV51OpenDmd == 1)+sum(HBV52OpenDmd == 1)+sum(HBV53OpenDmd == 1)+
                                                                               sum(HBV8OpenDmd == 1))

# remove data corresponding to 6,7,8 flights and change flight numbers above 9                                                                  
toremove <- which((gp_rt_1hz_data$ft_num == 6) | gp_rt_1hz_data$ft_num == 7 | gp_rt_1hz_data$ft_num == 8)
gp_rt_1hz_data <- gp_rt_1hz_data[-toremove,]
ft_num <- gp_rt_1hz_data$ft_num

gp_rt_1hz_data$ft_num[gp_rt_1hz_data$ft_num > 5] <- (gp_rt_1hz_data$ft_num[gp_rt_1hz_data$ft_num > 5] - 3)

#gp_rt_1hz_data <- subset(gp_rt_1hz_data,ft_num != c(6,7,8))
 
# hbv51 P30V
ggplot(data = gp_rt_1hz_data)+geom_line(aes(ft_num,tot_state_change,col="hbv_total"))+geom_line(aes(ft_num,state_change_1,col="51"))+
  geom_line(aes(ft_num,p30,col="p30"))+ylab("")+ggtitle("State_change P30V vs Flight number")+theme_bw()+
  theme(axis.text = element_text(angle = 45, hjust = 1))




#method 3
#norm_1hz_data$time_sec = as.POSIXct(paste(norm_1hz_data$AHTMS_Date,norm_1hz_data$AHTMS_Time,sep = " "), format = "%d-%b-%Y %H:%M:%OS")
#flt_data_1hz$time_sec = as.POSIXct(flt_data_1hz$AHTMS_Time,format = "%H:%M:%OS")
#flt_day_data$time_sec = as.POSIXct(paste(flt_day_data$AHTMS_Date,flt_day_data$AHTMS_Time,sep = " "),format = "%d-%b-%Y %H:%M:%OS")

# to overcome Posixct 'origin' must be supplied error
flt_day_data$hr_min = as.numeric(format(flt_day_data$time_sec,"%H"))+as.numeric(format(flt_day_data$time_sec,"%M"))/60+
  as.numeric(format(flt_day_data$time_sec,"%S"))/3600
norm_1hz_data$hr_min = as.numeric(format(norm_1hz_data$time_sec,"%H"))+as.numeric(format(norm_1hz_data$time_sec,"%M"))/60+
  as.numeric(format(norm_1hz_data$time_sec,"%S"))/3600
flt_data_1hz$hr_min = as.numeric(format(flt_data_1hz$time_sec,"%H"))+as.numeric(format(flt_data_1hz$time_sec,"%M"))/60+
  as.numeric(format(flt_data_1hz$time_sec,"%S"))/3600


library(directlabels)
ggplot(data = rt_1hz_data,aes(hr_min, TGTV,color = factor(ft_num)))+geom_line(aes(group = factor(ft_num)))+scale_color_discrete(guide = 'none')+
  geom_dl(aes(label = ft_num),method = list(dl.combine("first.points","last.points"),cex = 0.8))+
  ggtitle("TGT test data over flights")+theme_bw()+theme(axis.text = element_text(angle = 45, hjust = 1))#+
  
ggplot()+geom_line(data = norm_1hz_data,aes(hr_min,FADEC_R_Eng_tgt,col = "norm data",group = factor(ft_num)))+
  geom_line(data = flt_data_1hz,aes(hr_min,FADEC_R_Eng_tgt,col = "before fault day",group = factor(ft_num)))+
  geom_line(data = flt_day_data,aes(hr_min,FADEC_R_Eng_tgt,col = "fault day"))


# Method 4
rt_1hz_stdst <- subset(rt_1hz_data,dv_ft == "zero-gradient")
flt_stdst <- subset(flt_data_1hz,AHTMS_Flight_Mode == 6)
norm_stdst <- subset(norm_1hz_data,AHTMS_Flight_Mode == 6)


ggplot()+#geom_line(data = rt_1hz_stdst,aes(hr_min,FuelFlowV,col = "test data"))+
  geom_line(data = norm_stdst,aes(hr_min,FADEC_R_Eng_fuel_Flow,col = "norm data"))+
  geom_line(data = flt_stdst,aes(hr_min,FADEC_R_Eng_fuel_Flow,col = "before/after fault day"))+
  geom_line(data = flt_day_data,aes(hr_min,FADEC_R_Eng_fuel_Flow,col = "Fault day"))

rm(rt_1hz_stdst,flt_stdst,norm_stdst)

# Method 5
# 1hz eng_difference
df <- lt_1hz_data %>% group_by(ft_num) %>% mutate(al_diff = altitude_data - lag(altitude_data,default = -1)) %>%
  do(data.frame(dv_ft = dv_altitude(.$al_diff,.$altitude_data)))

lt_1hz_data = cbind(lt_1hz_data,dv_ft = df$dv_ft)
nrow_eng = min(nrow(rt_1hz_data),nrow(lt_1hz_data))
eng_diff <- data.frame(day_mon_y = character(),dv_ft = character(), P30V = numeric(),
                       p30delta = numeric(), EPRActual = numeric(), FuelFlowV = numeric(),
                       ce = numeric(), NHV = numeric(), hr_min = numeric())

rt_1hz_data$ce = compression_efficiency(rt_1hz_data)
lt_1hz_data$ce = compression_efficiency(lt_1hz_data)
cnt <- 0

for(i in 1:nrow_eng){
  if((rt_1hz_data$day_mon_y[i] == lt_1hz_data$day_mon_y[i]) & ( substr(rt_1hz_data$hr_min[i],0,4) == substr(lt_1hz_data$hr_min[i],0,4))){
    eng_diff <- rbind(eng_diff,data.frame(day_mon_y = rt_1hz_data$day_mon_y[i],dv_ft = rt_1hz_data$dv_ft[i],
                                          P30V = (rt_1hz_data$P30V[i] - lt_1hz_data$P30V[i]),p30delta = (rt_1hz_data$p30delta[i] - lt_1hz_data$p30delta[i]),
                                          EPRActual = (rt_1hz_data$EPRActual[i] - lt_1hz_data$EPRActual[i]),
                                          FuelFlowV = (rt_1hz_data$FuelFlowV[i] - lt_1hz_data$FuelFlowV[i]),
                                          ce = (rt_1hz_data$ce[i] - lt_1hz_data$ce[i]),NHV = (rt_1hz_data$NHV[i] - lt_1hz_data$NHV[i]),
                                          hr_min = rt_1hz_data$hr_min[i]),hbv51 = rt_1hz_data$HBV51OpenDmd[i],
                                          hbv52 = rt_1hz_data$HBV52OpenDmd,hbv53 = rt_1hz_data$HBV53OpenDmd,
                                          hbv8 = rt_1hz_data$HBV8OpenDmd)
    cnt = cnt+1
  }
}

# engine difference plots, hbv open /close
ggplot(data = eng_diff)+geom_point(aes(hr_min,P30V))
ggplot(data = eng_diff)+geom_point(aes(hr_min,p30delta))


# NHV p30delta hbv comparision for flight 1
ggplot(subset(rt_1hz_data,ft_num == 1))+geom_point(aes(hr_min,NHV/100,col="nhv"))+geom_line(aes(hr_min,p30delta,"dp30"))+
  geom_point(aes(hr_min,as.numeric(HBV51OpenDmd)*1.25,col="51"))+geom_point(aes(hr_min,as.numeric(HBV52OpenDmd)*1.2,col="52"))+
  geom_point(aes(hr_min,as.numeric(HBV53OpenDmd)*1.15,col="53"))+geom_point(aes(hr_min,as.numeric(HBV8OpenDmd)*1.1,col="8"))+
  theme_bw()+theme(axis.text = element_text(angle = 45, hjust = 1))+ggtitle("Flight 1")


# customised plots ##
## derivative of NHV steady state and transient state
## derivative taking 0.25 as threshold value
dv_nhv <- function(x,y){
  alf <- ifelse(x>0.01 | x< -0.01,"transient","steady")
  return(alf)
}

df <- rt_1hz_data %>% group_by(ft_num) %>% mutate(nhv_diff = NHV - lag(NHV,default = 0)) %>%
  do(data.frame(dv_speed = dv_nhv(.$nhv_diff)))

rt_1hz_data = cbind(rt_1hz_data,dv_nhv = df$dv_speed)

# note please do biining commnds below before running this dplyr command
rt_1hz_data$nhv_bins <- factor(rt_1hz_data$nhv_bins)
nhv_dp30_phase <- rt_1hz_data %>% group_by(ft_num,dv_ft) %>% summarise(dp30_mn_phase = mean(p30delta),dv_n2 = levels(dv_nhv)[which.max(unique(dv_nhv))],
                                                                       p30_mn_phase = mean(P30V), tgt_mn_phase = mean(TGTV),
                                                                       n2_bin = levels(nhv_bins)[which.max(unique(nhv_bins))])

any_hbv_tr_nhv_dp30_phase <- hbv_any_true_test %>% group_by(ft_num,dv_ft) %>% summarise(dp30_mn_phase = mean(p30delta),dv_n2 = levels(dv_nhv)[which.max(unique(dv_nhv))],
                                                                                  p30_mn_phase = mean(P30V), tgt_mn_phase = mean(TGTV),
                                                                                  n2_bin = levels(nhv_bins)[which.max(unique(nhv_bins))])

#rt_1hz_data = cbind(rt_1hz_data, dv_n2_phase = df$dv_n2, dp30_mn_phase = df$dp30_mn_phase)
ggplot()+geom_point(data = rt_1hz_data,aes(ft_num,p30delta,col = dv_nhv))+
  geom_point(data = rt_test_data,aes(ft_num,p30delta,col = dv_nhv))
ggplot(rt_1hz_data)+geom_point(aes(hr_min,p30delta,col = dv_nhv))

ggplot(rt_1hz_data)+geom_point(aes(ft_num,P30V,col = dv_nhv))

ggplot()+geom_point(data = nhv_dp30_phase,aes(ft_num,dp30_mn_phase,col = dv_n2))+
  geom_point(data = nhv_dp30_phase_test,aes(ft_num,dp30_mn_phase,col = dv_n2))
ggplot(nhv_dp30_phase)+geom_point(aes(ft_num,p30_mn_phase,col = dv_n2))

# Bin NHV based on speed range, <50, 50 -75, >75
rt_1hz_data$nhv_bins = ifelse(rt_1hz_data$NHV < 50,"l50",ifelse(rt_1hz_data$NHV > 75, "g75","b50-75"))
rt_1hz_data$nhv_bins = factor(rt_1hz_data$nhv_bins)

ggplot()+geom_point(data = rt_1hz_data,aes(P30V,TGTV,col = dv_nhv, shape = nhv_bins))+facet_grid(dv_ft~.)+
  geom_point(data = rt_test_data,aes(P30V,TGTV,col = dv_nhv,shape = nhv_bins))
ggplot()+geom_point(data = rt_1hz_data,aes(p30delta,TGTV,col = dv_nhv,shape = nhv_bins))+facet_grid(dv_ft~.)+
  geom_point(data = rt_test_data,aes(p30delta,TGTV,col = dv_nhv,shape = nhv_bins))

ggplot()+geom_point(data = rt_1hz_data,aes(P30V,TGTV,col = dv_nhv,shape = nhv_bins))+
  geom_point(data = rt_test_data,aes(P30V,TGTV,col=dv_nhv,shape = nhv_bins))
ggplot(hbv_any_true_test)+geom_point(aes(P30V,TGTV,col = dv_nhv,shape = nhv_bins))#+facet_grid(dv_ft~.)

ggplot(data = hbv_any_true_test)+geom_point(aes(P30V,TGTV,col = HBV51OpenDmd,shape = nhv_bins))+
  geom_point(aes(P30V,TGTV,col = HBV52OpenDmd, shape = nhv_bins))+
  geom_point(aes(P30V,TGTV,col = HBV53OpenDmd,shape = nhv_bins))+geom_point(aes(P30V,TGTV,col = HBV8OpenDmd,shape = nhv_bins))+
  ggtitle("Any hbv true data")

ggplot(any_hbv_tr_nhv_dp30_phase)+geom_point(aes(p30_mn_phase,tgt_mn_phase,col = dv_n2,shape = n2_bin))+
  ggtitle("any hbv true average p30 tgt per flight and phase")

# 0-1 state change of hbv
#st_chg_01 <- function(x){
#  x$
#}
#rt_1hz_data$st.chg.51.01 <- ifelse((rt_1hz_data$HBV51OpenDmd - lag(rt_1hz_data$HBV51OpenDmd)) == 1,"st_01","other")

rt_1hz_data <- rt_1hz_data %>% group_by(ft_num) %>% mutate(st_chg_51 = HBV51OpenDmd - lag(HBV51OpenDmd,default = 1),
                                                           st_chg_52 = HBV52OpenDmd - lag(HBV52OpenDmd,default = 1),
                                                           st_chg_53 = HBV53OpenDmd - lag(HBV53OpenDmd,default = 1),
                                                           st_chg_8 = HBV8OpenDmd - lag(HBV8OpenDmd,default = 1))
st_chg_51 <- subset(rt_1hz_data, st_chg_51 == 1 | st_chg_51 == -1)
st_chg_51 <- rbind(st_chg_51,subset(rt_test_data,st_chg_51 == 1 | st_chg_51 == -1))
st_chg_51$state <-ifelse(st_chg_51$st_chg_51 == 1,"0->1","1->0")
st_chg_51$st_dv_nhv <- paste0(st_chg_51$state,st_chg_51$dv_nhv,sep="")

ggplot(st_chg_51)+geom_point(aes(st_dv_nhv,P30V))+xlab("")+ggtitle("state change hbv51 P30V")+
  theme_bw()+theme(axis.text = element_text(angle = 45, hjust = 1))
ggplot(st_chg_51)+geom_point(aes(st_dv_nhv,P30V,col = ft_num))+xlab("")+ggtitle("state change hbv51 P30V")+
  theme_bw()+theme(axis.text = element_text(angle = 45, hjust = 1))+facet_grid(dv_ft~.)

st_chg_52 <- subset(rt_1hz_data, st_chg_52 == 1 | st_chg_52 == -1)
st_chg_52 <- rbind(st_chg_52,subset(rt_test_data,st_chg_52 == 1 | st_chg_52 == -1))
st_chg_52$state <-ifelse(st_chg_52$st_chg_52 == 1,"0->1","1->0")
st_chg_52$st_dv_nhv <- paste0(st_chg_52$state,st_chg_52$dv_nhv,sep="")

ggplot(st_chg_52)+geom_point(aes(st_dv_nhv,P30V))+xlab("")+ggtitle("state change hbv52 P30V")+
  theme_bw()+theme(axis.text = element_text(angle = 45, hjust = 1))
ggplot(st_chg_52)+geom_point(aes(st_dv_nhv,P30V,col = ft_num))+xlab("")+ggtitle("state change hbv52 p30")+
  theme_bw()+theme(axis.text = element_text(angle = 45, hjust = 1))+facet_grid(dv_ft~.)

st_chg_53 <- subset(rt_1hz_data, st_chg_53 == 1 | st_chg_53 == -1)
st_chg_53 <- rbind(st_chg_53,subset(rt_test_data,st_chg_53 == 1 | st_chg_53 == -1))
st_chg_53$state <-ifelse(st_chg_53$st_chg_53 == 1,"0->1","1->0")
st_chg_53$st_dv_nhv <- paste0(st_chg_53$state,st_chg_53$dv_nhv,sep="")
st_chg_8 <- subset(rt_1hz_data, st_chg_8 == 1 | st_chg_8 == -1)
st_chg_8 <- rbind(st_chg_8,subset(rt_test_data,st_chg_8 == 1 | st_chg_8 == -1))
st_chg_8$state <-ifelse(st_chg_8$st_chg_8 == 1,"0->1","1->0")
st_chg_8$st_dv_nhv <- paste0(st_chg_8$state,st_chg_8$dv_nhv,sep="")


## 4 hbv in same plot
ggplot()+geom_point(data = st_chg_51,aes(st_dv_nhv,p30delta,col ="51"))+geom_point(data = st_chg_52,aes(st_dv_nhv,p30delta,col = "52"))+
  geom_point(data = st_chg_53,aes(st_dv_nhv,p30delta,col = "53"))+geom_point(data = st_chg_8,aes(st_dv_nhv,p30delta,col = "8"))+
  theme_bw()+theme(axis.text = element_text(angle = 45, hjust = 1))#+theme(panel.grid.major = element_line(colour = "#808080"))


## xtra
ggplot(data = subset(rt_test_data,ft_num == 18))+geom_point(aes(hr_min,P30V,col = "p30"))+
     geom_point(aes(hr_min,as.numeric(HBV51OpenDmd)*100))+geom_point(aes(hr_min,NHV,col = "n2"))+
  geom_point(aes(hr_min,EPRActual*200,col = "epr"))+geom_point(aes(hr_min,p30delta*50,col = "dp30"))
ggplot(data = subset(rt_test_data,ft_num == 18))+geom_point(aes(hr_min,altitude_data,col = "alt"))+
  geom_point(aes(hr_min,P30V*100,col = "p30"))


