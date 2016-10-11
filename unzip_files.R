rm(list = ls(all = T))
setwd("C:/Users/c_indhiraA/Desktop/data extraction/actual 1hz hbv data/Ac6044 Aug 15 - HBV fault")
files_to_unzip = list.files(pattern = "*.zip")

#for(i in dir(pattern = "/.zip$")){
for (i in files_to_unzip) {
  unzip(i)
}
