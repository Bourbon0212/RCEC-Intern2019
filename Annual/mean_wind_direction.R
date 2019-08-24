setwd("D:/OneDrive - g.ntu.edu.tw/Documents/108-I/中研院環變中心實習/Annual")

### Draft

WD01 = read.csv("Done/WD01/198001_cwb_WD01.csv") #風速
WD02 = read.csv("Done/WD02/198001_cwb_WD02.csv") #風向

u.res = WD01 #計算後東西風向(sin(wind))的資料表
v.res = WD01 #計算後南北風向(cos(wind))的資料表

speed = WD01[2:ncol(WD01)]
direct = WD02[2:ncol(WD02)]/180*pi

u.res[2:ncol(u.res)] = round(speed * cos(direct), 4)
v.res[2:ncol(v.res)] = round(speed * sin(direct), 4)

### Main
month = sprintf("%02d", seq(1, 12, 1)) #產出01,02,...12的數列

for (i in 1980:2018) {
  for (j in month) {
    WD01 = read.csv(paste0("Done/WD01/", i, j, "_cwb_WD01.csv")) #風速
    WD02 = read.csv(paste0("Done/WD02/", i, j, "_cwb_WD02.csv")) #風向
    print(paste(i, j))
    
    #儀器故障待修，不明原因或故障無資料，未觀測
    WD01[WD01 == -9991] = NA; WD01[WD01 == -9997] = NA; WD01[WD01 == -9999] = NA
    WD02[WD02 == -9991] = NA; WD02[WD02 == -9997] = NA; WD02[WD02 == -9999] = NA
    
    u.res = WD01 #計算後東西風向(sin(wind))的資料表
    v.res = WD01 #計算後南北風向(cos(wind))的資料表
    
    speed = WD01[2:ncol(WD01)]
    direct = WD02[2:ncol(WD02)]/180*pi
    
    u.res[2:ncol(u.res)] = round(speed * cos(direct), 4)
    v.res[2:ncol(v.res)] = round(speed * sin(direct), 4)
    
    write.csv(u.res, paste0("Done/WD_u/", i, j, "_cwb_WD_u.csv"), row.names = F)
    write.csv(v.res, paste0("Done/WD_v/", i, j, "_cwb_WD_v.csv"), row.names = F)
  }
}
