library(data.table)
library(tidyr)
library(dplyr)
library(lubridate)

setwd("D:/OneDrive - g.ntu.edu.tw/Documents/108-I/中研院環變中心實習/Diurnal")

#Data檢查
PP01_check = function(data) {
  temp = c()
  check = T
  for (k in data$PP01) {
    temp = c(temp, k)
  }
  check = -9998 %in% temp
  
  if (check == F) {
    print(paste(read, "ERROR!"))
  }
}

month = sprintf("%02d", seq(1, 12, 1))

for (i in 2000:2016) {
  for (j in month) {
    read = paste0("Raw_all/", i, j, "_cwb_hr.txt")
    write = paste0("Done/", i, j, "_cwb_PP01.csv")
    
    data = fread(read, skip = "# stno", header = F)
    data = data[,c("V1", "V2", "V16")] #(stno, time, PP01)
    colnames(data) = c("stno", "yyyymmddhh", "PP01")
    data$yyyymmddhh = ymd_h(data$yyyymmddhh)
    
    PP01_check(data)
    
    data.D = spread(data, yyyymmddhh, PP01)
    write.csv(data.D, write, row.names = F)
  }
}

#Draft
data = fread("Raw/201810_cwb_hr.txt", skip = "# stno", header = F)
#colnames(data) = c("stno", "yyyymmddhh", "PS01", "PS02", "TX01", "TX04", "TX05", "RH01", "RH02", "WD01", "WD02", "WD03", "WD04", "WD05", "WD06", "PP01", "PP02" ,"SS01", "SS02", "VS01", "CD01", "CD02", "CD03", "CD04", "CD05", "CD06", "CD07", "CD08", "CD09", "CD10", "CD11", "TS01", "TS02", "TS03", "TS04", "TS05", "ST01", "ST02", "ST03", "ST04", "ST05", "ST06", "ST07", "ST08", "ST09", "ST10", "ST11", "ST12")
data$V2 = ymd_h(data$V2)

#PP01，降水量(mm)
#一天24小時，局屬測站30(一月少4677990)
data = data[,c("V1", "V2", "V12")] #(stno, time, PP01(V16))
colnames(data) = c("stno", "yyyymmddhh", "PP01")

#展開spreadsheet，測站/時間
data1 = spread(data, yyyymmddhh, PP01)
write.csv(data1, "test.csv", row.names = F)
test = read.csv("test.csv")