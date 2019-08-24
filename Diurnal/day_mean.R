library(data.table)

setwd("D:/OneDrive - g.ntu.edu.tw/Documents/108-I/中研院環變中心實習/Diurnal")

hr = sprintf("%02d", seq(1, 24, 1))
month = sprintf("%02d", seq(1, 12, 1))
#month = c("01", "02", "03")

status = T
station = read.csv("station.csv")

for (i in month) {
  for (j in hr) {
    read = paste0("Day/", i, j, "_cwb_PP01.csv")
    
    data = fread(read, header = T)
    data[data == -9991] = NA #儀器故障待修
    data[data == -9997] = NA #不明原因或故障無資料
    data[data == -9998] = 0  #雨跡(<0.1 mm)
    data[data == -9999] = NA #未觀測
    index = c(2:ncol(data))
    
    if (status == T) {
      result = as.data.frame(data[,1])
      mean = apply(data[,..index], 1, function (x) round(mean(x, na.rm = T), 2))
      status = F
    } else {
      mean = apply(data[,..index], 1, function (x) round(mean(x, na.rm = T), 2))
    }
    result = cbind(result, as.data.frame(mean))
  }
  colnames(result) = c("stno", hr)
  final = merge(result, station, by = "stno")
  colnames(final) = c("stno", hr, "name", "Lon", "Lat")
  write = paste0("Month/", i, "_cwb_PP01.csv")
  write.csv(final, write, row.names = F)
  status = T
}

#Draft
read = paste0("Day/0101_cwb_PP01.csv")
data = fread(read, header = T)

#異常值處理
data1 = data
data1[data1 == -9991] = NA #儀器故障待修
data1[data1 == -9997] = NA #不明原因或故障無資料
data1[data1 == -9998] = 0  #雨跡(<0.1 mm)
data1[data1 == -9999] = NA #未觀測

#data check
data.f = apply(data, 2, function (x) as.factor(as.character(x)))
summary(data.f)

#mean calculation
index = c(2:ncol(data))
mean = apply(data1[,..index], 1, function (x) round(mean(x, na.rm = T), 2))

test = result[1, 2:25]
test.t = gather(test, 1)
colnames(test.t) = c("time", "PP01")
ggplot(data = test.t) +
  geom_col(aes(x = time, y = PP01))

Jan = read.csv("Month/01_cwb_PP01.csv")
tt = merge(Jan, station, by = "stno")