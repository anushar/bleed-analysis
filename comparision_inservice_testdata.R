#comparision with flight test data and In-Service Data #
gp_rt_1hz_data <- rt_1hz_data %>% group_by(rt_1hz_data$'ft_num') %>% mutate(cnt = n(), p30 = mean(P30V), n2_speed = mean(NHV),
                                                                              state_change_1 = sum(abs(diff(HBV51OpenDmd))), time_open_1 = sum(HBV51OpenDmd == 1),
                                                                              time_close_1 = sum(HBV51OpenDmd == 0), md_p30 = median(P30V),
                                                                              md_n2_speed = median(NHV),
                                                                              md_tgt = median(TGTV),
                                                                              md_ffl = median(FuelFlowV))

ggplot()+geom_point(data = gp_flt_data,aes(md_p30,cnt,color = "flt_count"))+
  geom_point(data = gp_norm_data,aes(md_p30,cnt,color = "norm_count"))+geom_point(data = gp_rt_1hz_data,aes(md_p30,cnt,color = "Test_count"))
  scale_size_area()+theme_bw()+
  theme(axis.text = element_text(angle = 45, hjust = 1))

  ggplot()+geom_point(data = gp_flt_data,aes(md_p30,md_n2_speed,color = "flt_count"))+
    geom_point(data = gp_norm_data,aes(md_p30,md_n2_speed,color = "norm_count"))+geom_point(data = gp_rt_1hz_data,
                                                                                            aes(md_p30,md_n2_speed,color = "Test_count"))+
    scale_size_area()+theme_bw()+theme(axis.text = element_text(angle = 45, hjust = 1))

ggplot()+geom_point(data = gp_flt_data,aes(md_p30,p30,color = "flt_data"))+
  geom_point(data = gp_norm_data,aes(md_p30,p30,color = "norm_data"))+geom_point(data = gp_rt_1hz_data,
                                                                                          aes(md_p30,p30,color = "Test_data"))+
  scale_size_area()+theme_bw()+theme(axis.text = element_text(angle = 45, hjust = 1))

 
#method 3 #
ggplot()+geom_point(data = gp_flt_data,aes(md_p30,md_tgt,color = "flt_data"))+
  geom_point(data = gp_norm_data,aes(md_p30,md_tgt,color = "norm_data"))+geom_point(data = gp_rt_1hz_data,
                                                                                 aes(md_p30,md_tgt,color = "Test_data"))+
  scale_size_area()+theme_bw()+theme(axis.text = element_text(angle = 45, hjust = 1))

ggplot()+geom_point(data = flt_data_1hz,aes(FADEC_R_Eng_p30_Burner,FADEC_R_Eng_tgt,color = "Fault Data"))+
  geom_point(data = norm_1hz_data,aes(FADEC_R_Eng_p30_Burner,FADEC_R_Eng_tgt, color = "Normal Data"))+
  geom_point(data = rt_1hz_data,aes(P30V,TGTV,color = "Test_Data"))

#method 4#
gplot_3data_sets <- function(fd,a1,a2,nd,td,a3,a4){
  ggplot()+geom_point(data = fd,aes_string(a1,a2),color = "red")+
    geom_point(data = nd,aes_string(a1,a2), color = "green")+
    geom_point(data = td,aes_string(a3,a4),color = "blue")+scale_size_area()+theme_bw()+
    theme(axis.text = element_text(angle = 45, hjust = 1))
  
}

rt_1hz_stdst = subset(rt_1hz_data,ft_state == "cruise")

ggplot()+geom_point(data = flt_stdst,aes(1:nrow(flt_stdst),FADEC_R_Eng_fuel_Flow,color = "Fault Data"))+
  geom_point(data = norm_stdst,aes(1:nrow(norm_stdst),FADEC_R_Eng_fuel_Flow, color = "Normal Data"))+
  geom_point(data = rt_1hz_stdst,aes(1:nrow(rt_1hz_stdst),FuelFlowV,color = "Test_Data"))

gplot_3data_sets(gp_flt_data,"md_n2_speed","md_ffl",gp_norm_data,gp_rt_1hz_data,"md_n2_speed","md_ffl")
gplot_3data_sets(gp_flt_data,"md_p30","md_ffl",gp_norm_data,gp_rt_1hz_data,"md_p30","md_ffl")

gplot_3data_sets(flt_data_1hz,"FADEC_R_Eng_n2_Speed","factor(FADEC_R_Eng_bleed_Select_On)",norm_1hz_data,norm_1hz_data,
                 "FADEC_R_Eng_n2_Speed","FADEC_R_Eng_customervBleedvFlow")

ggplot()+geom_point(data = flt_data_1hz,aes(FADEC_R_Eng_n2_Speed,factor(FADEC_R_Eng_bleed_Select_On),color = "Fault Data"))+
  geom_point(data = norm_1hz_data,aes(FADEC_R_Eng_n2_Speed,factor(FADEC_R_Eng_bleed_Select_On), color = "Normal Data"))+
  geom_point(data = rt_1hz_data,aes(NHV,factor(HBV8OpenDmd),color = "Test_Data"))



# method 6 #
ggplot()+geom_line(data = gp_flt_data,aes(state_change,md_p30,color = "Fault data"))+
  geom_line(data = gp_norm_data,aes(state_change,md_p30,color = "Normal data"))+
  geom_line(data = gp_rt_1hz_data,aes(state_change_1,md_p30,color = "Test data"))





# experiments #

ggplot()+geom_point(data =norm_1hz_data,aes(time_sec,FADEC_R_Eng_customervBleedvFlow,color = "normal"))
# compare customer bleed to hbv and p30  and bleed pressure #
ggplot()+geom_point(data = subset(norm_1hz_data,ft_num == 1),aes(time_sec,(FADEC_R_Eng_customervBleedvFlow *100),color = "customer_bleed_normal"))+
  geom_point(data = subset(norm_1hz_data,ft_num == 1),aes(time_sec,(FADEC_R_Eng_bleed_Select_On*100),color = "bleed_normal"))+
  geom_point(data = subset(norm_1hz_data,ft_num == 1),aes(time_sec,FADEC_R_Eng_p30_Burner,color = "p30"))+
  geom_point(data = subset(norm_1hz_data,ft_num == 1),aes(time_sec,R_BAC_bleed_Air_Pressure,color = "bleed_pressure"))


# compare bleed pressure to faulty data #
ggplot()+geom_point(data = norm_1hz_data,aes(time_sec,R_BAC_bleed_Air_Pressure))

#stats::deriv(rt_1hz_data$altitude_data)
library(pspline)
dal <- (diff(predict(sm.spline(rt_1hz_data$time_sec,rt_1hz_data$altitude_data)$ysmth)))
dal$time_sec <- rt_1hz_data$time_sec
dal$ft_num <- rt_1hz_data$ft_num
dal$mod_deriv <- sapply(dal$V1,function(x){ifelse(x>0,"ascent",ifelse(x<0,"descent","cruise"))}) 
zero_altitude_phase <- function(x,y){
  if(x == 0){
    y = "zero-alt"
  }
  else{
    y = y
  }
}

dal$mod_deriv <- mapply(zero_altitude_phase,rt_1hz_data$altitude_data,dal$mod_deriv)


ggplot()+geom_point(data = subset(rt_1hz_data,ft_num == 9),aes(time_sec,altitude_data,color = "altitude"))+
  geom_point(data = subset(dal,ft_num == 9),aes(time_sec,V1,color = "derivative"))+
  geom_point(data = subset(dal,ft_num == 9),aes(time_sec,as.numeric(factor(mod_deriv)),color = "mod-derivative"))+ylim(-10,10)+
  geom_point(data = subset(rt_1hz_data,ft_num == 9),aes(time_sec,as.numeric(factor(ft_al_state)),color = "rt_al_state"))



ggplot()+geom_point(data = subset(rt_1hz_data, ft_num == 1),aes(time_sec,altitude_data))



