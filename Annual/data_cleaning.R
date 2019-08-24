library(data.table)
library(tidyr)
library(dplyr)
library(lubridate)

setwd("D:/OneDrive - g.ntu.edu.tw/Documents/108-I/中研院環變中心實習/Annual")

### Main
#雨量資料檢查，出現-9998(雨跡)代表所選欄位正確
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

for (i in 1980:2018) {
  for (j in month) {
    read = paste0("Raw/", i, j, "_cwb_dy.txt")
    write = paste0("Done/WD02/", i, j, "_cwb_WD02.csv")
    
    data = fread(read, skip = "# stno", header = F)
    data = data[,c("V1", "V2", "V37")] #(stno, time, x)
    colnames(data) = c("stno", "yyyymmdd", "WD02")
    data$yyyymmdd = ymd(data$yyyymmdd)
    
    #PP01_check(data)
    
    data.D = spread(data, yyyymmdd, WD02)
    write.csv(data.D, write, row.names = F)
  }
}


### Draft
data = fread("Raw/201810_cwb_dy.txt", skip = "# st", header = F)
#colnames(data) = c("stno", "yyyymmdd", "PS01", "PS02", "PS03", "PS04", "PS05", "PS06", "PS07", "PS08", 
#                   "PS09", "PS10", "TX01", "TX02", "TX03", "TX04", "TX05", "TX06", "TD01", "TD02",
#                   "TD03", "TD04", "TX07", "TX08", "TX09", "VP01", "VP02", "VP03", "VP04", "VP05",
#                   "RH01", "RH02", "RH03", "RH04", "RH05", "WD01", "WD02", "WD03", "WD04", "WD05",
#                   "WD06", "WD07", "WD08", "WD09", "PP01", "PP02", "PP03", "PP04", "PP05", "PP06",
#                   "SS01", "SS02", "GR01", "GR02", "GR03", "VS01", "CD01", "SD01", "ST02", "ST03",
#                   "ST04", "ST05", "ST06", "ST07", "ST08", "ST09", "ST10", "ST11", "ST12", "EP01",
#                   "EP02", "EP03", "TG01", "TS01", "TS02", "TS03", "TS04", "TS05", "TS06", "TS07", 
#                   "TS08", "TS09", "TS10")

#PP01(降水量，mm，V45)
#PS01(平均測站氣壓，V3/78)
#TX01(平均氣溫，V13/74)
#WD01(平均風速，V36/57)
#WD02(平均風風向，V37/72)

data = data[,c("V1", "V2", "V78")]
colnames(data) = c("stno", "yyyymmdd", "WD01")
data$yyyymmdd = ymd(data$yyyymmdd)

#展開資料表，測站/時間
data = spread(data, yyyymmdd, WD01)
write.csv(data, "test.csv", row.names = F)
test = read.csv("test.csv")