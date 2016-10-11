
setwd("C:/Users/c_indhiraA/Desktop/data extraction/actual 1hz hbv data/Ac6044 Aug 15 - HBV fault")
# hbv fault data aug 15 #
flt_data_1hz = read.csv("6044_20150811235157.csv",skip=7)

# colnames read from text #
#setwd("C:/Users/c_indhiraA/Desktop/data extraction/actual 1hz hbv data")
cl_names = read.table("mod_col_names_hbv.txt",header = FALSE)
colnames(flt_data_1hz) = as.character(cl_names$V1)
flt_data_1hz = na.omit(flt_data_1hz)

summary(flt_data_1hz)
plot(flt_data_1hz$AHTMS_Time,flt_data_1hz$FADEC_R_Eng_altitude_Computed)

plot(flt_data_1hz$AHTMS_Time,flt_data_1hz$FADEC_R_Eng_p30_Burner)
plot(flt_data_1hz$AHTMS_Time,flt_data_1hz$FADEC_R_Eng_n2_Speed)
plot(flt_data_1hz$FADEC_R_Eng_n2_Speed,flt_data_1hz$FADEC_R_Eng_p30_Burner)
plot(flt_data_1hz$AHTMS_Time,flt_data_1hz$AHTMS_Flight_Mode)

require(ggplot2)
gplot_fault_norm_data <- function(x,y){
  
  wd1 <- "C:/Users/c_indhiraA/Desktop/data extraction/modified_paramlist_hbv_13oct2010_2engines/al_plot_act_1hz/"
  g3<-ggplot(flt_data_1hz,aes_string(x,y))+geom_point(aes(colour = factor(AHTMS_Flight_Mode)),alpha =1/2)+scale_size_area()+
    ggtitle(paste(x," vs ",y," act 1hz fault data",sep=""))+scale_colour_brewer(palette = "Set1")+theme_bw()+theme(axis.text = element_text(angle = 45, hjust = 1))
  ggsave(g3,filename = paste(wd1,x,"_vs_",y,"_act_1hz_gplot_fault_data.jpeg",sep = "")) 
  g4 <- ggplot(norm_1hz_data,aes_string(x,y))+geom_point(aes(colour = factor(AHTMS_Flight_Mode)),alpha =1/2)+scale_size_area()+
    ggtitle(paste(x," vs ",y," act 1hz normal data",sep=""))+scale_colour_brewer(palette = "Set1")+theme_bw()+theme(axis.text = element_text(angle = 45, hjust = 1))
  ggsave(g4,filename = paste(wd1,x,"_vs_",y,"_act_1hz__norm_data.jpeg",sep = "")) 
  
}

gplot_custom_data <- function(dt,x,y,col_factor){
  wd3 <- "C:/Users/c_indhiraA/Desktop/data extraction/modified_paramlist_hbv_13oct2010_2engines/al_plot_act_1hz/"
  g6<-ggplot(dt,aes_string(x,y))+geom_point(aes(colour = factor(col_factor)),alpha =1/2)+scale_size_area()+
    ggtitle(paste(x," vs ",y," act 1hz fault data",sep=""))+scale_colour_brewer(palette = "Set1")+theme_bw()+theme(axis.text = element_text(angle = 45, hjust = 1))
  ggsave(g5,filename = paste(wd2,dt,"_",x,"_vs_",y,"_act_1hz_gplot_fault_data.jpeg",sep = "")) 
  
}


gplot_fault_data("AHTMS_Time","FADEC_R_Eng_p30_Burner")
gplot_fault_data("AHTMS_Time","FADEC_R_Eng_n2_Speed")
gplot_fault_data("FADEC_R_Eng_n2_Speed","FADEC_R_Eng_p30_Burner")
gplot_fault_data("FADEC_R_Eng_n2_Speed","FADEC_R_Eng_tgt")
gplot_fault_data("FADEC_R_Eng_n2_Speed","FADEC_R_Eng_t30")
flt_data_1hz$FADEC_R_Eng_bleed_Select_On = factor(flt_data_1hz$FADEC_R_Eng_bleed_Select_On)
gplot_fault_data("AHTMS_Time","FADEC_R_Eng_bleed_Select_On")
gplot_fault_data("AHTMS_Time","FADEC_R_Eng_altitude_Computed")

gplot_fault_data("FADEC_R_Eng_bleed_Select_On","FADEC_R_Eng_p30_Burner")

# Method 2 #
# At engine Start #
ggplot(subset(flt_data_1hz,AHTMS_Flight_Mode == 2),aes_string("AHTMS_Time","FADEC_R_Eng_p30_Burner"))+geom_point(aes(colour = factor(AHTMS_Flight_Mode)),alpha =1/2)+scale_size_area()+
  ggtitle("Flight mode 2 Altitude act 1hz fault data")+scale_colour_brewer(palette = "Set1")+theme_bw()+theme(axis.text = element_text(angle = 45, hjust = 1))
# cruise/steady state condition #
ggplot(subset(flt_data_1hz,AHTMS_Flight_Mode == 6),aes_string("AHTMS_Time","FADEC_R_Eng_fuel_Flow"))+geom_point(aes(colour = factor(FADEC_R_Eng_bleed_Select_On)),alpha =1/2)+scale_size_area()+
  ggtitle("Flight mode 6 Fuel Flow act 1hz fault data")+scale_colour_brewer(palette = "Set1")+theme_bw()+theme(axis.text = element_text(angle = 45, hjust = 1))

ggplot(subset(flt_data_1hz,AHTMS_Flight_Mode == 6),aes_string("AHTMS_Time","FADEC_R_Eng_p30_Burner"))+geom_point(aes(colour = factor(FADEC_R_Eng_bleed_Select_On)),alpha =1/2)+scale_size_area()+
  ggtitle("Flight mode 6 P30 act 1hz fault data")+scale_colour_brewer(palette = "Set1")+theme_bw()+theme(axis.text = element_text(angle = 45, hjust = 1))

# Compression Efficiency #
compression_efficiency <- function(x){
  # gamma = 1.4
  gm <- 1.4
  chg_cols = c("FADEC_R_Eng_p30_Burner","FADEC_R_Eng_p20_Selected","FADEC_R_Eng_t30","FADEC_R_Eng_t20_Validated","FADEC_L_ENG_p30_Burner","FADEC_L_ENG_p20_Selected","FADEC_L_ENG__t30","FADEC_L_ENG_t20_Validated")
  #x[,c(210,121,131,159)]
  x[chg_cols] = apply(x[chg_cols],2,as.numeric)
  rce <- (((x$FADEC_R_Eng_p30_Burner/x$FADEC_R_Eng_p20_Selected)^((gm-1)/gm)) - 1 )*100/(((x$FADEC_R_Eng_t30+273)/(x$FADEC_R_Eng_t20_Validated+273))-1)
  lce <- (((x$FADEC_L_ENG_p30_Burner/x$FADEC_L_ENG_p20_Selected)^((gm-1)/gm)) - 1 )*100/(((x$FADEC_L_ENG__t30+273)/(x$FADEC_L_ENG_t20_Validated+273))-1)
  ce <- list(rce,lce)
  return(ce)
}

#c(flt_data_1hz$rce,flt_data_1hz$lce) <- 
flt_data_1hz$rce <- (compression_efficiency(x=flt_data_1hz)[[1]])
flt_data_1hz$lce <- (compression_efficiency(flt_data_1hz)[[2]])


ggplot(subset(flt_data_1hz,AHTMS_Flight_Mode == 6),aes_string("AHTMS_Time","ce"))+geom_point(aes(colour = factor(FADEC_R_Eng_bleed_Select_On)),alpha =1/2)+scale_size_area()+
  ggtitle("Flight mode 6 ce act 1hz fault data")+scale_colour_brewer(palette = "Set1")+theme_bw()+theme(axis.text = element_text(angle = 45, hjust = 1))


# Engine to Engine comparisiom #
flt_eng_diff <- data.frame(AHTMS_Date = flt_data_1hz$AHTMS_Date,AHTMS_Flight_Mode = flt_data_1hz$AHTMS_Flight_Mode,p30 = (flt_data_1hz$FADEC_R_Eng_p30_Burner - flt_data_1hz$FADEC_L_ENG_p30_Burner),
                           EPRActual = (flt_data_1hz$FADEC_R_Eng_epr_Actual - flt_data_1hz$FADEC_L_ENG_epr_Actual),
                           FuelFlow = (flt_data_1hz$FADEC_R_Eng_fuel_Flow - flt_data_1hz$FADEC_L_ENG_fuel_Flow), ce = (flt_data_1hz$rce - flt_data_1hz$lce),
                           n2_speed = (flt_data_1hz$FADEC_R_Eng_n2_Speed - flt_data_1hz$FADEC_L_ENG_n2_Speed))
flt_eng_diff$AHTMS_Time <- flt_data_1hz$AHTMS_Time
ggplot(flt_eng_diff,aes(x=1:nrow(flt_eng_diff),p30))+geom_point(aes(colour = factor(AHTMS_Flight_Mode)),alpha =1/2)+scale_size_area()+
  ggtitle("p30 engine diff act 1hz fault data")+scale_colour_brewer(palette = "Set1")+theme_bw()+theme(axis.text = element_text(angle = 45, hjust = 1))
ggplot(flt_eng_diff,aes(AHTMS_Date,EPRActual))+geom_point(aes(colour = factor(AHTMS_Flight_Mode)),alpha =1/2)+scale_size_area()+
  ggtitle("EPRActual engine diff act 1hz fault data")+scale_colour_brewer(palette = "Set1")+theme_bw()+theme(axis.text = element_text(angle = 45, hjust = 1))
ggplot(flt_eng_diff,aes(AHTMS_Date,FuelFlow))+geom_point(aes(colour = factor(AHTMS_Flight_Mode)),alpha =1/2)+scale_size_area()+
  ggtitle("fuel flow engine diff act 1hz fault data")+scale_colour_brewer(palette = "Set1")+theme_bw()+theme(axis.text = element_text(angle = 45, hjust = 1))
ggplot(flt_eng_diff,aes(AHTMS_Date,ce))+geom_point(aes(colour = factor(AHTMS_Flight_Mode)),alpha =1/2)+scale_size_area()+
  ggtitle("compression efficiency engine diff act 1hz fault data")+scale_colour_brewer(palette = "Set1")+theme_bw()+theme(axis.text = element_text(angle = 45, hjust = 1))
ggplot(flt_eng_diff,aes(EPRActual,FuelFlow))+geom_point(aes(colour = factor(AHTMS_Flight_Mode)),alpha =1/2)+scale_size_area()+ylim(-500,1500)+
  ggtitle("epr vs fuel flow engine diff act 1hz fault data")+scale_colour_brewer(palette = "Set1")+theme_bw()+theme(axis.text = element_text(angle = 45, hjust = 1))

#plot(flt_data_1hz$IRS_3_ground_Speed,flt_eng_diff$n2_speed)
#plot(norm_1hz_data$IRS_3_ground_Speed,norm_eng_diff$n2_speed,col = factor(norm_1hz_data$AHTMS_Flight_Mode))
#legend(x="topright",legend = levels(factor(norm_1hz_data$AHTMS_Flight_Mode)),col = factor(norm_1hz_data$AHTMS_Flight_Mode))
qplot(norm_1hz_data$IRS_3_ground_Speed,norm_eng_diff$n2_speed,colour = factor(norm_1hz_data$AHTMS_Flight_Mode),main = "Normal Data")
qplot(flt_data_1hz$IRS_3_ground_Speed,flt_eng_diff$n2_speed,colour = factor(flt_data_1hz$AHTMS_Flight_Mode),main = "Fault Data")


# overlay plot #
ggplot(data = flt_data_1hz)+geom_point(aes(AHTMS_Time,FADEC_R_Eng_p30_Burner),color = "red")+geom_point(aes(AHTMS_Time,FADEC_L_ENG_p30_Burner),color = "blue")
ggplot(data = flt_data_1hz)+geom_point(aes(AHTMS_Time,FADEC_R_Eng_fuel_Flow),color = "red")+geom_point(aes(AHTMS_Time,FADEC_L_ENG_fuel_Flow),color = "blue")
ggplot(data = flt_data_1hz)+geom_point(aes(AHTMS_Time,rce),color = "red")+geom_point(aes(AHTMS_Time,lce),color = "blue")


# Method 3 #
gplot_fault_data("AHTMS_Time","FADEC_R_Eng_tgt")
gplot_fault_norm_data("FADEC_R_Eng_p30_Burner","FADEC_R_Eng_tgt")
gplot_fault_norm_data("AHTMS_Time","FADEC_R_Eng_fuel_Flow")
gplot_fault_norm_data("FADEC_R_Eng_n2_Speed","FADEC_R_Eng_fuel_Flow")
flt_hpsov_opn <- subset(flt_data_1hz,R_Eng_hpsov_Position == TRUE)
norm_hpsov_open <- subset(norm_1hz_data,R_Eng_hpsov_Position == TRUE)

#gplot_custom_data(flt_hpsov_opn,"AHTMS_Time","FADEC_R_Eng_fuel_Flow",flt_hpsov_opn$AHTMS_Flight_Mode)
ggplot(flt_hpsov_opn,aes_string("AHTMS_Time","FADEC_R_Eng_fuel_Flow"))+geom_point(aes(colour = factor(FADEC_R_Eng_bleed_Select_On)),alpha =1/2)+scale_size_area()+
  ggtitle("hpsovopen fuel flow act 1hz fault data")+scale_colour_brewer(palette = "Set1")+theme_bw()+theme(axis.text = element_text(angle = 45, hjust = 1))

ggplot(norm_hpsov_opn,aes("AHTMS_Time","FADEC_R_Eng_fuel_Flow"))+geom_point(aes(colour = factor(FADEC_R_Eng_bleed_Select_On)),alpha =1/2)+scale_size_area()+
  ggtitle("hpsovopen fuel flow act 1hz fault data")+scale_colour_brewer(palette = "Set1")+theme_bw()+theme(axis.text = element_text(angle = 45, hjust = 1))



# combine fault data set #
#  open jan 16 hbv set #
setwd("C:/Users/c_indhiraA/Desktop/data extraction/actual 1hz hbv data/AC6059 Jan 16 - HBV fault")
files_rt = list.files(pattern = "*_\\d+.csv")
cl_names = read.table("modified_colnames_sav_hbv.txt",header = FALSE)
flt_data_1hz$ft_num <- 1
#rt_files = lapply(files_rt, read.csv)
for (i in 1:length(files_rt)) {
  flt_data = read.csv(files_rt[i],skip=7)
  colnames(flt_data) = as.character(cl_names$V1)
  flt_data = na.omit(flt_data)
  flt_data$FADEC_L_ENG_p30_Burner = as.numeric(flt_data$FADEC_L_ENG_p30_Burner)
  flt_data$FADEC_L_ENG_p20_Selected = as.numeric(flt_data$FADEC_L_ENG_p20_Selected)
  flt_data$rce <- (compression_efficiency(flt_data)[[1]])
  flt_data$lce <- (compression_efficiency(flt_data)[[2]])
  flt_data$ft_num <- i+1
  flt_data <- flt_data[,!duplicated(colnames(flt_data))]
  flt_data_1hz = rbind(flt_data_1hz,flt_data)
  #setdiff(names(flt_data),names(flt_data_1hz))
}



# Method 6 #
# remove duplicate columns #
flt_data_1hz<- flt_data_1hz[,!duplicated(colnames(flt_data_1hz))]
require(dplyr)
flt_data_1hz$FADEC_R_Eng_bleed_Select_On = as.numeric(flt_data_1hz$FADEC_R_Eng_bleed_Select_On)
#ft_num = mean(ft_num), 
gp_flt_data <- flt_data_1hz %>% group_by(flt_data_1hz$'ft_num') %>% mutate(cnt = n(), p30 = mean(FADEC_R_Eng_p30_Burner), n2_speed = mean(FADEC_R_Eng_n2_Speed),
                                                                              state_change = sum(abs(diff(FADEC_R_Eng_bleed_Select_On))), time_open = sum(FADEC_R_Eng_bleed_Select_On == 1),
                                                                              time_close = sum(FADEC_R_Eng_bleed_Select_On == 0), md_p30 = median(FADEC_R_Eng_p30_Burner),
                                                                           lp30 = mean(FADEC_L_ENG_p30_Burner), l_md_p30 = median(FADEC_L_ENG_p30_Burner),
                                                                           md_n2_speed = median(FADEC_R_Eng_n2_Speed),
                                                                           md_tgt = median(FADEC_R_Eng_tgt),
                                                                           md_ffl = median(FADEC_R_Eng_fuel_Flow))

# flt and norm data #
# Time open or close vs p30 # 
ggplot()+geom_point(data = gp_flt_data,aes(p30,time_open,color = "flt_time_open"))+geom_point(data = gp_flt_data,aes(p30,time_close,color = "flt_time_close"))+
  geom_point(data = gp_norm_data,aes(p30,time_open,color="norm_time_open"))+geom_point(data = gp_norm_data,aes(p30,time_close,color="norm_time_close"))+
  scale_size_area()+theme_bw()+theme(axis.text = element_text(angle = 45, hjust = 1))

# state_change vs p30 #
ggplot()+geom_point(data = gp_flt_data,aes(p30,state_change,color = "flt_state_change_num"))+
  geom_point(data = gp_norm_data,aes(p30,state_change,color = "norm_state_change_num"))+scale_size_area()+theme_bw()+
  theme(axis.text = element_text(angle = 45, hjust = 1))

# median  p30 vs state_change#
ggplot()+geom_point(data = gp_flt_data,aes(md_p30,state_change,color = "flt_state_change_num"))+
  geom_point(data = gp_norm_data,aes(md_p30,state_change,color = "norm_state_change_num"))+scale_size_area()+theme_bw()+
  theme(axis.text = element_text(angle = 45, hjust = 1))

# median p30 vs Time open, time close # 
ggplot()+geom_point(data = gp_flt_data,aes(md_p30,time_open,color = "flt_time_open"))+geom_point(data = gp_flt_data,aes(md_p30,time_close,color = "flt_time_close"))+
  geom_point(data = gp_norm_data,aes(md_p30,time_open,color="norm_time_open"))+geom_point(data = gp_norm_data,aes(md_p30,time_close,color="norm_time_close"))+
  scale_size_area()+theme_bw()+theme(axis.text = element_text(angle = 45, hjust = 1))


# median p30 count #
ggplot()+geom_point(data = gp_flt_data,aes(md_p30,cnt,color = "flt_count"))+
  geom_point(data = gp_norm_data,aes(md_p30,cnt,color = "norm_count"))+scale_size_area()+theme_bw()+
  theme(axis.text = element_text(angle = 45, hjust = 1))

# n2 speed median p30 #
ggplot()+geom_point(data = gp_flt_data,aes(md_p30,md_n2_speed,color = "flt_n2_speed"))+
  geom_point(data = gp_norm_data,aes(md_p30,md_n2_speed,color = "norm_2_speed"))+scale_size_area()+theme_bw()+
  theme(axis.text = element_text(angle = 45, hjust = 1))
# left engine data #
ggplot()+geom_point(data = gp_flt_data,aes(l_md_p30,time_open,color = "flt_time_open"))+geom_point(data = gp_flt_data,aes(l_md_p30,time_close,color = "flt_time_close"))+
  geom_point(data = gp_norm_data,aes(l_md_p30,time_open,color="norm_time_open"))+geom_point(data = gp_norm_data,aes(l_md_p30,time_close,color="norm_time_close"))+
  scale_size_area()+theme_bw()+theme(axis.text = element_text(angle = 45, hjust = 1))

# all points #
ggplot()+geom_point(data = gp_flt_data,aes(FADEC_R_Eng_p30_Burner,time_close,color = "red"))+
  geom_point(data = gp_norm_data,aes(FADEC_R_Eng_p30_Burner,time_close),color = md_p30)+scale_size_area()+theme_bw()+
  theme(axis.text = element_text(angle = 45, hjust = 1))


# hbv vs p30 #
# fault data #
ggplot(gp_flt_data,aes(FADEC_R_Eng_p30_Burner,factor(FADEC_R_Eng_bleed_Select_On)))+geom_point()+facet_grid(ft_num~.)+
  scale_size_area()+theme_bw()+theme(axis.text = element_text(angle = 45, hjust = 1))+xlim(0,400)+ggtitle("Fault Data")
# normal data #
ggplot(gp_norm_data,aes(FADEC_R_Eng_p30_Burner,factor(FADEC_R_Eng_bleed_Select_On)))+geom_point()+facet_grid(ft_num~.)+
  scale_size_area()+theme_bw()+theme(axis.text = element_text(angle = 45, hjust = 1))+xlim(0,400)+ggtitle("Normal Data")


ggplot(gp_norm_data,aes(FADEC_R_Eng_n2_Corr_T26,FADEC_R_Eng_n2_Speed))+geom_point()
ggplot(gp_norm_data,aes(FADEC_R_Eng_n2_Corr_T26,factor(FADEC_R_Eng_bleed_Select_On)))+geom_point()+facet_grid(ft_num~.)+
  scale_size_area()+theme_bw()+theme(axis.text = element_text(angle = 45, hjust = 1))+ggtitle("Normal Data")
ggplot(gp_flt_data,aes(FADEC_R_Eng_n2_Corr_T26,factor(FADEC_R_Eng_bleed_Select_On)))+geom_point()+facet_grid(ft_num~.)+
  scale_size_area()+theme_bw()+theme(axis.text = element_text(angle = 45, hjust = 1))+ggtitle("Fault Data")








# actual 1hz data #
#  SAV Jan 16 ac6067 #
setwd("C:/Users/c_indhiraA/Desktop/data extraction/actual 1hz hbv data/AC6067 Jan 16 - SAV fault")
col_names_norm = read.table("modified_colnames_sav.txt",header = FALSE)


norm_1hz_data = read.csv("6067_20160101171246.csv",skip=6)
colnames(norm_1hz_data) = as.character(col_names_norm$V1)
norm_1hz_data$ft_num = 1
norm_1hz_data1 = read.csv("6067_20160109004632.csv",skip = 6)
colnames(norm_1hz_data1) = as.character(col_names_norm$V1)
norm_1hz_data1$ft_num = 2
norm_1hz_data2 = read.csv("6067_20160115225153.csv",skip = 6)
colnames(norm_1hz_data2) = as.character(col_names_norm$V1)
norm_1hz_data2$ft_num = 3
norm_1hz_data3 = read.csv("6067_20160118130700.csv",skip = 6)
colnames(norm_1hz_data3) = as.character(col_names_norm$V1)
norm_1hz_data3$ft_num = 4
norm_1hz_data4 = read.csv("6067_20160119220329.csv", skip = 6)
colnames(norm_1hz_data4) = as.character(col_names_norm$V1)
norm_1hz_data4$ft_num = 5
norm_1hz_data5 = read.csv("6067_20160120153148.csv",skip = 6)
colnames(norm_1hz_data5) = as.character(col_names_norm$V1)
norm_1hz_data5$ft_num = 6
norm_1hz_data = rbind(norm_1hz_data,norm_1hz_data1,norm_1hz_data2,norm_1hz_data3,norm_1hz_data4,norm_1hz_data5)
setwd("C:/Users/c_indhiraA/Desktop/data extraction/actual 1hz hbv data/AC6011 Feb 16 - FMU Fault")
norm_1hz_data6 = read.csv("6011_20160207140421.csv",skip=6)
colnames(norm_1hz_data6) = as.character(col_names_norm$V1)
norm_1hz_data6$ft_num = 7
norm_1hz_data6 <- norm_1hz_data6[,!duplicated(colnames(norm_1hz_data6))]
norm_1hz_data7 = read.csv("6011_20160207140527.csv",skip = 6)
colnames(norm_1hz_data7) = as.character(col_names_norm$V1)
norm_1hz_data7$ft_num = 8
norm_1hz_data7 <- norm_1hz_data7[,!duplicated(colnames(norm_1hz_data7))]
norm_1hz_data = rbind(norm_1hz_data,norm_1hz_data6,norm_1hz_data7)
rm(norm_1hz_data6,norm_1hz_data7)


norm_1hz_data = na.omit(norm_1hz_data)

rm(cl_names,col_names_norm,norm_1hz_data1,norm_1hz_data2,norm_1hz_data3,norm_1hz_data4,norm_1hz_data5)

gplot_norm_data <- function(x,y){
  wd1 <- "C:/Users/c_indhiraA/Desktop/data extraction/modified_paramlist_hbv_13oct2010_2engines/al_plot_act_1hz/"
  g4<-ggplot(norm_1hz_data,aes_string(x,y))+geom_point(aes(colour = factor(AHTMS_Flight_Mode)),alpha =1/2)+scale_size_area()+
    ggtitle(paste(x," vs ",y," act 1hz normal data",sep=""))+scale_colour_brewer(palette = "Set1")+theme_bw()+theme(axis.text = element_text(angle = 45, hjust = 1))
  ggsave(g4,filename = paste(wd1,x,"_vs_",y,"_act_1hz__norm_data.jpeg",sep = "")) 
  
}


norm_1hz_data$time_sec = as.POSIXct(paste(norm_1hz_data$AHTMS_Date,norm_1hz_data$AHTMS_Time,sep = " "), format = "%d-%b-%Y %H:%M:%OS")
flt_data_1hz$time_sec = as.POSIXct(flt_data_1hz$AHTMS_Time,format = "%H:%M:%OS")



gplot_norm_data("AHTMS_Time","FADEC_R_Eng_p30_Burner")
gplot_norm_data("AHTMS_Time","FADEC_R_Eng_n2_Speed")
gplot_norm_data("FADEC_R_Eng_n2_Speed","FADEC_R_Eng_p30_Burner")
gplot_norm_data("FADEC_R_Eng_n2_Speed","FADEC_R_Eng_tgt")
gplot_norm_data("FADEC_R_Eng_n2_Speed","FADEC_R_Eng_t30")
norm_1hz_data$FADEC_R_Eng_bleed_Select_On = factor(norm_1hz_data$FADEC_R_Eng_bleed_Select_On)
gplot_norm_data("AHTMS_Time","FADEC_R_Eng_bleed_Select_On")
gplot_norm_data("AHTMS_Time","FADEC_R_Eng_altitude_Computed")
gplot_norm_data("FADEC_R_Eng_altitude_Computed","R_Eng_n2_Corr_T26")
 
ggplot(data = subset(norm_1hz_data,ft_num == 1),aes(AHTMS_Time,FADEC_R_Eng_bleed_Select_On))+geom_point(aes(colour = factor(AHTMS_Flight_Mode)),alpha =1/2)+scale_size_area()+
  ggtitle("Flight 1 Bleed_select_on normal data")+scale_colour_brewer(palette = "Set1")+theme_bw()+theme(axis.text = element_text(angle = 45, hjust = 1))


gplot_norm_data("AHTMS_Date","FADEC_R_Eng_p30_Burner")
gplot_norm_data("FADEC_R_Eng_bleed_Select_On","FADEC_R_Eng_p30_Burner")

# Method 2 #
# At engine Start #
ggplot(subset(norm_1hz_data,AHTMS_Flight_Mode == 2),aes_string("AHTMS_Time","FADEC_R_Eng_p30_Burner"))+geom_point(aes(colour = factor(AHTMS_Flight_Mode)),alpha =1/2)+scale_size_area()+
  ggtitle("Flight mode 2 Altitude act 1hz normal data")+scale_colour_brewer(palette = "Set1")+theme_bw()+theme(axis.text = element_text(angle = 45, hjust = 1))

# cruise/steady state condition #
ggplot(subset(norm_1hz_data,AHTMS_Flight_Mode == 6),aes_string("AHTMS_Date","FADEC_R_Eng_fuel_Flow"))+geom_point(aes(colour = FADEC_R_Eng_bleed_Select_On),alpha =1/2)+scale_size_area()+
  ggtitle("Flight mode 6 Fuel flow act 1hz normal data")+scale_colour_brewer(palette = "Set1")+theme_bw()+theme(axis.text = element_text(angle = 45, hjust = 1))


# compression Efficiency #
norm_1hz_data$rce <- compression_efficiency(norm_1hz_data)[[1]]
norm_1hz_data$lce <- compression_efficiency(norm_1hz_data)[[2]]


ggplot(subset(norm_1hz_data,AHTMS_Flight_Mode == 6),aes_string("AHTMS_Date","ce"))+geom_point(aes(colour = FADEC_R_Eng_bleed_Select_On),alpha =1/2)+scale_size_area()+
  ggtitle("Flight mode 6 ce act 1hz normal data")+scale_colour_brewer(palette = "Set1")+theme_bw()+theme(axis.text = element_text(angle = 45, hjust = 1))


norm_eng_diff <- data.frame(AHTMS_Date = norm_1hz_data$AHTMS_Date,AHTMS_Flight_Mode = norm_1hz_data$AHTMS_Flight_Mode,p30 = (norm_1hz_data$FADEC_R_Eng_p30_Burner - norm_1hz_data$FADEC_L_ENG_p30_Burner),
                           EPRActual = (norm_1hz_data$FADEC_R_Eng_epr_Actual - norm_1hz_data$FADEC_L_ENG_epr_Actual),
                           FuelFlow = (norm_1hz_data$FADEC_R_Eng_fuel_Flow - norm_1hz_data$FADEC_L_ENG_fuel_Flow), ce = (norm_1hz_data$rce - norm_1hz_data$lce),
                           n2_speed = (norm_1hz_data$FADEC_R_Eng_n2_Speed - norm_1hz_data$FADEC_L_ENG_n2_Speed))
norm_eng_diff$AHTMS_Time <- norm_1hz_data$AHTMS_Time
ggplot(norm_eng_diff,aes(x=1:nrow(norm_eng_diff),y=p30))+geom_point(aes(colour = factor(AHTMS_Flight_Mode)),alpha =1/2)+scale_size_area()+
  ggtitle("p30 engine diff act 1hz normal data")+scale_colour_brewer(palette = "Set1")+theme_bw()+theme(axis.text = element_text(angle = 45, hjust = 1))
ggplot(norm_eng_diff,aes(AHTMS_Date,EPRActual))+geom_point(aes(colour = factor(AHTMS_Flight_Mode)),alpha =1/2)+scale_size_area()+
  ggtitle("EPRActual engine diff act 1hz normal data")+scale_colour_brewer(palette = "Set1")+theme_bw()+theme(axis.text = element_text(angle = 45, hjust = 1))
ggplot(norm_eng_diff,aes(AHTMS_Date,FuelFlow))+geom_point(aes(colour = factor(AHTMS_Flight_Mode)),alpha =1/2)+scale_size_area()+
  ggtitle("fuel flow engine diff act 1hz normal data")+scale_colour_brewer(palette = "Set1")+theme_bw()+theme(axis.text = element_text(angle = 45, hjust = 1))+ylim(0,2000)
ggplot(norm_eng_diff,aes(AHTMS_Date,ce))+geom_point(aes(colour = factor(AHTMS_Flight_Mode)),alpha =1/2)+scale_size_area()+
  ggtitle("compression efficiency engine diff act 1hz normal data")+scale_colour_brewer(palette = "Set1")+theme_bw()+theme(axis.text = element_text(angle = 45, hjust = 1))
ggplot(norm_eng_diff,aes(EPRActual,FuelFlow))+geom_point(aes(colour = factor(AHTMS_Flight_Mode)),alpha =1/2)+scale_size_area()+xlim(-0.2,0.2)+ylim(-1000,1500)+
  ggtitle("epr vs fuel flow engine diff act 1hz normal data")+scale_colour_brewer(palette = "Set1")+theme_bw()+theme(axis.text = element_text(angle = 45, hjust = 1))


# overlay plots #
ggplot(data= norm_1hz_data)+geom_point(aes(AHTMS_Time,rce),color="red")+geom_point(aes(AHTMS_Time,lce),color="blue")


# Method 3 #
gplot_norm_data("FADEC_R_Eng_p30_Burner","FADEC_R_Eng_tgt")


# Method 6 #
# remove duplicate columns #
norm_1hz_data <- norm_1hz_data[,!duplicated(colnames(norm_1hz_data))]



require(dplyr)
norm_1hz_data$FADEC_R_Eng_bleed_Select_On = as.numeric(norm_1hz_data$FADEC_R_Eng_bleed_Select_On)
#ft_num = mean(ft_num), 
gp_norm_data <- norm_1hz_data %>% group_by(norm_1hz_data$'ft_num') %>% mutate(cnt = n(), p30 = mean(FADEC_R_Eng_p30_Burner), n2_speed = mean(FADEC_R_Eng_n2_Speed),
                                                                   state_change = sum(abs(diff(FADEC_R_Eng_bleed_Select_On))), time_open = sum(FADEC_R_Eng_bleed_Select_On == 1),
                                                                   time_close = sum(FADEC_R_Eng_bleed_Select_On == 0), md_p30 = median(FADEC_R_Eng_p30_Burner),
                                                                   lp30 = mean(FADEC_L_ENG_p30_Burner), l_md_p30 = median(FADEC_L_ENG_p30_Burner),
                                                                   md_n2_speed = median(FADEC_R_Eng_n2_Speed),
                                                                   md_tgt = median(FADEC_R_Eng_tgt),
                                                                   md_ffl = median(FADEC_R_Eng_fuel_Flow))




#hbv_tr_cnt <- data.frame(cnt = numeric(),ft_num = numeric(),P30V = numeric(),P0V = numeric(),NHV = numeric())
#for (j in 1:length(gp_ft_hbv51)) {
#  hbv_tr_cnt <- rbind(hbv_tr_cnt,data.frame(cnt = sum(gp_ft_hbv51[[j]]$act_hbv), ft_num = mean(gp_ft_hbv51[[j]]$ft_num), P30V = mean(gp_ft_hbv51[[j]]$P30V),
#                                            acP0V = mean(gp_ft_hbv51[[j]]$acP0V),NHV = mean(gp_ft_hbv51[[j]]$NHV)))  
#  #
#} 


# P30 vs TGT method 3 #
ggplot(norm_1hz_data,aes(FADEC_R_Eng_p30_Burner,FADEC_R_Eng_tgt))+geom_point()+
  geom_point(data = flt_data_1hz,aes(FADEC_R_Eng_p30_Burner,FADEC_R_Eng_tgt,shape = "+",color = "red"))

ggplot(norm_1hz_data,aes(x=(1:nrow(norm_1hz_data)),FADEC_R_Eng_tgt))+geom_point()+
  geom_point(data = flt_data_1hz,aes(x=(1:nrow(flt_data_1hz)),FADEC_R_Eng_tgt,color = "red"),shape = 100)

ggplot(gp_norm_data,aes(md_p30,md_tgt))+geom_point()+geom_point(data = gp_flt_data,aes(md_p30,md_tgt),color="red")
ggplot(gp_norm_data,aes(md_n2_speed,md_tgt))+geom_point()+geom_point(data = gp_flt_data,aes(md_n2_speed,md_tgt),color= "red")+
  ggtitle("n2_speed vs tgt")

Mode_v <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

gplot_overlay_data <- function(d1,a1,a2,d2,a3,a4) {
  ggplot(d1,aes_string(a1,a2))+geom_point()+geom_point(data = d2,aes_string(a1,a2),color= "red")+
    scale_size_area()+ggtitle(paste(a1," vs ",a2,sep=""))+scale_colour_brewer(palette = "Set1")+
    theme_bw()+theme(axis.text = element_text(angle = 45, hjust = 1))
} 

# Fuelflow Method 4#
ggplot(norm_1hz_data,aes(x=(1:nrow(norm_1hz_data)),FADEC_R_Eng_fuel_Flow))+geom_point()+
  geom_point(data = flt_data_1hz,aes(x=(1:nrow(flt_data_1hz)),FADEC_R_Eng_fuel_Flow,color = "red"),shape = 100)

gplot_overlay_data(gp_norm_data,"md_n2_speed","md_ffl",gp_flt_data)
gplot_overlay_data(gp_norm_data,"md_p30","md_ffl",gp_flt_data)

# at steady state condition flight mode = 6#
norm_stdst = subset(norm_1hz_data,AHTMS_Flight_Mode == 6)
flt_stdst = subset(flt_data_1hz, AHTMS_Flight_Mode == 6)
ggplot(norm_stdst,aes(x=(1:nrow(norm_stdst)),FADEC_R_Eng_fuel_Flow))+geom_point()+
  geom_point(data = flt_stdst,aes(x=(1:nrow(flt_stdst)),FADEC_R_Eng_fuel_Flow,color = "red"),shape = 100)


# Method 5 #
#  bleed select on vs n2_speed #
gplot_overlay_data(norm_1hz_data,"FADEC_R_Eng_n2_Speed","factor(FADEC_R_Eng_bleed_Select_On)",flt_data_1hz)
ggplot(norm_1hz_data,aes(x=FADEC_R_Eng_n2_Speed,factor(FADEC_R_Eng_bleed_Select_On),color="normal"))+geom_point()+
  geom_point(data = flt_data_1hz,aes(x=FADEC_R_Eng_n2_Speed,factor(FADEC_R_Eng_bleed_Select_On),color = "fault"),shape = 100)


# Method 6 #
#  time open vs flight num #
gplot_overlay_data(gp_norm_data,"ft_num","time_open",gp_flt_data)
gplot_overlay_data(gp_norm_data,"md_n2_speed","time_open",gp_flt_data)
ggplot(gp_norm_data,aes(x=md_p30,time_open,color="normal"))+geom_line()+
  geom_line(data = gp_flt_data,aes(x=md_p30,time_open,color = "fault"))

# cumulative number of times open/close #
norm_1hz_data$total_open = sum(norm_1hz_data$FADEC_R_Eng_bleed_Select_On == 1)
norm_1hz_data$total_close = sum(norm_1hz_data$FADEC_R_Eng_bleed_Select_On == 0)
flt_data_1hz$total_open = sum(flt_data_1hz$FADEC_R_Eng_bleed_Select_On == 1)
flt_data_1hz$total_close = sum(flt_data_1hz$FADEC_R_Eng_bleed_Select_On == 0)

ggplot(norm_1hz_data,aes(median(FADEC_R_Eng_p30_Burner),total_open,color = "Normal open"))+geom_point()+
  geom_point(data = norm_1hz_data,aes(median(FADEC_R_Eng_p30_Burner),total_close,color = "Normal close"))+
  geom_point(data = flt_data_1hz,aes(median(FADEC_R_Eng_p30_Burner),total_open,color = "Fault open"))+
  geom_point(data = flt_data_1hz,aes(median(FADEC_R_Eng_p30_Burner),total_close,color = "Fault close"))

flt_data_1hz[69665:79406,] <- NA
cor(norm_1hz_data$FADEC_R_Eng_p30_Burner,flt_data_1hz$FADEC_R_Eng_p30_Burner,
    use='complete.obs')
flt_data_1hz = na.omit(flt_data_1hz)
norm_1hz_data[79407:558080,] <- NA
cor(norm_1hz_data$FADEC_R_Eng_p30_Burner,rt_engine_data$P30V,use = 'complete.obs')


# Fault Day Data #
setwd("C:/Users/c_indhiraA/Desktop/data extraction/actual 1hz hbv data/AC6059 Jan 16 - HBV fault")
cl_names = read.table("modified_colnames_sav_hbv.txt",header = FALSE)
flt_day_data = read.csv("6059_20160110214307_not_complete_10jan_1833_to_1838.csv",skip=7)
colnames(flt_day_data) = as.character(cl_names$V1)
flt_day_data1 = read.csv("6059_20160112220959_not_complete_10jan_2059_to_2107.csv",skip=7)
colnames(flt_day_data1) = as.character(cl_names$V1)
flt_day_data2 = read.csv("6059_20160110223057_not_complete_10jan2142_to_2143.csv",skip=7)
colnames(flt_day_data2) = as.character(cl_names$V1)
flt_day_data3 = read.csv("6059_20160112220906_not_complete_10jan_2143to45.csv",skip=7)
colnames(flt_day_data3) = as.character(cl_names$V1)
flt_day_data <- rbind(flt_day_data,flt_day_data1,flt_day_data2,flt_day_data3)
rm(flt_day_data1,flt_day_data2,flt_day_data3)
flt_day_data = na.omit(flt_day_data)

flt_day_data$FADEC_L_ENG_p30_Burner = as.numeric(flt_day_data$FADEC_L_ENG_p30_Burner)
flt_day_data$FADEC_L_ENG_p20_Selected = as.numeric(flt_day_data$FADEC_L_ENG_p20_Selected)
flt_day_data$rce <- (compression_efficiency(flt_day_data)[[1]])
flt_day_data$lce <- (compression_efficiency(flt_day_data)[[2]])
#flt_day_data$ft_num <- i+1
flt_day_data <- flt_day_data[,!duplicated(colnames(flt_day_data))]

