# Processed Data #
# br725 service data 20 flights 4 engines 5flight each #
setwd("C:/Users/c_indhiraA/Desktop/data extraction/actual 1hz hbv data/processed br725 data")
files_rt = list.files(pattern = "6049_*_\\d+.csv")
files_rt =  files_rt[order(as.Date(files_rt,format = "%Y%m%d%H%M%S"))]
cl_names = read.table("modified_colnames_sav_hbv.txt",header = FALSE)
proc_eng_6049 = data.frame(NULL)
for (i in 1:length(files_rt)) {
  temp_proc = read.csv(files_rt[i],skip=7)
  colnames(temp_proc) = as.character(cl_names$V1)
  temp_proc = na.omit(temp_proc)
  temp_proc$FADEC_L_ENG_p30_Burner = as.numeric(temp_proc$FADEC_L_ENG_p30_Burner)
  temp_proc$FADEC_L_ENG_p20_Selected = as.numeric(temp_proc$FADEC_L_ENG_p20_Selected)
  temp_proc$rce <- (compression_efficiency(temp_proc)[[1]])
  temp_proc$lce <- (compression_efficiency(temp_proc)[[2]])
  temp_proc$ft_num <- i
  temp_proc <- temp_proc[,!duplicated(colnames(temp_proc))]
  proc_eng_6049 = rbind(proc_eng_6049,temp_proc)
  #setdiff(names(temp_proc),names(temp_proc_1hz))
}

## rbind norm_1z_data with processed normal data
## norm_1hz_data -initial row number 558080
norm_1hz_data$total_open <- NULL
norm_1hz_data$total_close <- NULL
norm_1hz_data$time_sec <- NULL
setdiff(colnames(norm_1hz_data),colnames(proc_eng_6049))

norm_1hz_data = rbind(norm_1hz_data,proc_eng_6011,proc_eng_6021,proc_eng_6024,proc_eng_6049)


## 50 flights test data extraction 
setwd("C:/Users/c_indhiraA/Desktop/data extraction/test_data_hbv_params_50flights")

#files_rt = list.files(pattern = ("*_Engine2*.csv"))
files_rt = list.files(pattern = ("*_Engine2.*\\.csv"))
files_rt =  files_rt[order(as.Date(files_rt,format = "%d%b%Y"))]
rt_files = lapply(files_rt, read.csv)
files_lt = list.files(pattern = "*_Engine1.*\\.csv")
files_lt =  files_lt[order(as.Date(files_lt,format = "%d%b%Y"))]
lt_files = lapply(files_lt,read.csv)
rt_test_data = data.frame(NULL)
lt_test_data = data.frame(NULL)

for (i in 1:length(rt_files)) {
  rt_files[[i]]$ft_num <- i+14
  rt_test_data = rbind(rt_test_data,rt_files[[i]])
}

for (i in 1:length(lt_files)) {
  lt_files[[i]]$ft_num <- i+14
  lt_test_data = rbind(lt_test_data,lt_files[[i]])
}
rm(files_rt,rt_files,files_lt,lt_files)

# get 1hz data
rt_test_data = rt_test_data[seq(1,nrow(rt_test_data),4),]
lt_test_data = lt_test_data[seq(1,nrow(lt_test_data),4),]

# data transformation
rt_test_data$X <- NULL
rt_test_data = na.omit(rt_test_data)
summary(rt_test_data)

rt_test_data$time_sec = as.POSIXct(rt_test_data$Date.Time, format = "(%d-%b-%Y) %H:%M:%OS")
lt_test_data$time_sec = as.POSIXct(lt_test_data$Date.Time,format = "(%d-%b-%Y) %H:%M:%OS")

setdiff(colnames(rt_1hz_data),colnames(rt_test_data))
rt_1hz_data$X <- NULL
rt_test_data$day_mon_y = strftime(rt_test_data$time_sec,format = "%d-%b-%Y")
rt_test_data$hr_min = as.numeric(format(rt_test_data$time_sec,"%H"))+as.numeric(format(rt_test_data$time_sec,"%M"))/60+
  as.numeric(format(rt_test_data
                    $time_sec,"%S"))/3600
rt_test_data$ce = compression_efficiency(rt_test_data)
rt_1hz_data$nhv_bins <- NULL
library(dplyr)
df <- rt_test_data %>% group_by(ft_num) %>% mutate(al_diff = altitude_data - lag(altitude_data,default = -1)) %>%
  do(data.frame(dv_ft = dv_altitude(.$al_diff,.$altitude_data)))

rt_test_data = cbind(rt_test_data,dv_ft = df$dv_ft)
rm(df)
rt_1hz_data$dv_p30 <- NULL
df <- rt_test_data %>% group_by(ft_num) %>% mutate(nhv_diff = NHV - lag(NHV,default = 0)) %>%
  do(data.frame(dv_speed = dv_nhv(.$nhv_diff)))

rt_test_data = cbind(rt_test_data,dv_nhv = df$dv_speed)
rt_test_data <- rt_test_data %>% group_by(ft_num) %>% mutate(st_chg_51 = HBV51OpenDmd - lag(HBV51OpenDmd,default = 1),
                                                           st_chg_52 = HBV52OpenDmd - lag(HBV52OpenDmd,default = 1),
                                                           st_chg_53 = HBV53OpenDmd - lag(HBV53OpenDmd,default = 1),
                                                           st_chg_8 = HBV8OpenDmd - lag(HBV8OpenDmd,default = 1))

rt_test_data$nhv_bins = ifelse(rt_test_data$NHV < 50,"l50",ifelse(rt_test_data$NHV > 75, "g75","b50-75"))
hbv_any_true_test = rbind(hbv_any_true_test,subset(rt_test_data, (HBV51OpenDmd == TRUE | HBV52OpenDmd == TRUE | HBV53OpenDmd == TRUE |
                                           HBV8OpenDmd == TRUE)&(altitude_data > 100) == TRUE))
rt_test_data$nhv_bins <- factor(rt_test_data$nhv_bins)
nhv_dp30_phase_test <- rt_test_data %>% group_by(ft_num,dv_ft) %>% summarise(dp30_mn_phase = mean(p30delta),dv_n2 = levels(dv_nhv)[which.max(unique(dv_nhv))],
                                                                       p30_mn_phase = mean(P30V), tgt_mn_phase = mean(TGTV),
                                                                       n2_bin = levels(nhv_bins)[which.max(unique(nhv_bins))])


# stitch flight number 12 and 13 data
for (i in 1:nrow(rt_1hz_data)) {
  if(rt_1hz_data$ft_num[i] == 13 & rt_1hz_data$hr_min[i] <5){
    rt_1hz_data$ft_num[i] = 12
    rt_1hz_data$hr_min[i] = 24+rt_1hz_data$hr_min[i]
  }else if(rt_1hz_data$ft_num[i] == 14 & rt_1hz_data$hr_min[i] < 5){
    rt_1hz_data$ft_num[i] = 13
    rt_1hz_data$hr_min[i] = 24+rt_1hz_data$hr_min[i]
  }
}

# stictch flight number 26 in test data
for(i in 1:nrow(rt_test_data)){
  if(rt_test_data$ft_num[i] == 27 & rt_test_data$hr_min[i] < 10){
    rt_test_data$ft_num[i] = 26
    rt_test_data$hr_min[i] = 24+rt_test_data$hr_min[i]
  }
}

