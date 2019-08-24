library(dplyr)

setwd("D:/OneDrive - g.ntu.edu.tw/Documents/108-I/中研院環變中心實習/Annual")

### Draft
#依雨量的空間分群結果
cluster_1 = factor(c(466900, 466950, 466990, 467080, 467610, 467620, 467660)) #East
cluster_2 = factor(c(466910, 466930, 466940)) #North
cluster_3 = factor(c(467300, 467350, 467480, 467490, 467770)) #West
cluster_4 = factor(c(467440, 467530, 467540, 467550, 467590, 467650)) #Center & South
cluster_All = factor(c(as.numeric(as.character(cluster_1)), as.numeric(as.character(cluster_2)),
                       as.numeric(as.character(cluster_3)), as.numeric(as.character(cluster_4))))

#篩選出該分群
data = read.csv("Done/PP01/201701_cwb_PP01.csv")
data.f = filter(data, data$stno %in% cluster_1)

#異常值處理
data.f[data.f == -9991] = NA #儀器故障待修
data.f[data.f == -9997] = NA #不明原因或故障無資料
data.f[data.f == -9998] = 0  #雨跡(<0.1 mm)
data.f[data.f == -9999] = NA #未觀測

#逐列計算平均，得出該日該分群中的測站平均值
mean = apply(data.f[,2:ncol(data.f)], 2, function (x) round(mean(x, na.rm = T), 2))

### Main
month = sprintf("%02d", seq(1, 12, 1)) #產出01,02,...12的數列
month.d = c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31) #每個月分的天數，忽略閏年29號

ret = data.frame(day = c(1:365)) #生成dataframe容納產出的逐日資料

for (i in 1980:2018) {
  result = c()
  for (j in 1:length(month)) {
    read = paste0("Done/WD_v/", i, month[j], "_cwb_WD_v.csv")
    print(read)
    data = read.csv(read)
    
    data.f = filter(data, data$stno %in% cluster_4) #篩選分群
    data.f[data.f == -9991] = NA #儀器故障待修
    data.f[data.f == -9997] = NA #不明原因或故障無資料
    data.f[data.f == -9998] = 0  #雨跡(<0.1 mm)
    data.f[data.f == -9999] = NA #未觀測
    
    #逐日平均，註：註月平均再一層mean()
    mean = apply(data.f[,2:(month.d[j] + 1)], 2, function (x) round(mean(x, na.rm = T), 2))
    result = c(result, mean)
  }
  print(as.character(i))
  ret[as.character(i)] = result
}
write.csv(ret, "Daily_cwb_cs_WD_v.csv", row.names = F)
print(result)