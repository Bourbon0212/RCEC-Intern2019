library(data.table)

setwd("D:/OneDrive - g.ntu.edu.tw/Documents/108-I/中研院環變中心實習/Diurnal")

hr = sprintf("%02d", seq(1, 24, 1))

#製造變數
#一個月有31天
var.31 = c()
for (i in (1:length(hr))) {
  assign(paste0("M31", hr[i]), c(1, seq(i+1, 745, 24)))
  var.31 = c(var.31, paste0("M31", hr[i]))
}
#一個月有30天
var.30 = c()
for (i in (1:length(hr))) {
  assign(paste0("M30", hr[i]), c(1, seq(i+1, 721, 24)))
  var.30 = c(var.30, paste0("M30", hr[i]))
}
#一個月有29天
var.29 = c()
for (i in (1:length(hr))) {
  assign(paste0("M29", hr[i]), c(1, seq(i+1, 697, 24)))
  var.29 = c(var.29, paste0("M29", hr[i]))
}
#一個月有28天
var.28 = c()
for (i in (1:length(hr))) {
  assign(paste0("M28", hr[i]), c(1, seq(i+1, 673, 24)))
  var.28 = c(var.28, paste0("M28", hr[i]))
}

#主方法
paste_month = function(year1, year2, month, hr, name) {
  status = T
  for (i in year1:year2) {
    if (status == T) { #只抓stno，所以輸入年份1要 - 1
      read = "Done/201812_cwb_PP01.csv"
      head = fread(read, header = T)[,1] 
      status = F
    } else {
      read = paste0("Done/", i, month, "_cwb_PP01.csv")
      print(read)
      tail = fread(read, header = T)[,..hr] #..data.table用法
      head = merge(head, tail, by = "stno")
    }
  }
  write.csv(head, paste0("Day/", month, substring(name, 4, 5), "_cwb_PP01.csv"), row.names = F)
}

#回圈套
for (i in var.30) {
  paste_month(1979, 2018, "11", get(i), i)
}

#Draft
read = paste0("Done/", "1980", "01", "_cwb_PP01.csv")
data = fread(read, header = T)

read1 = paste0("Done/", "2017", "02", "_cwb_PP01.csv")
data1 = fread(read1, header = T)

bind = data[,c(1,2)]
bind = merge(bind, data1[,c(1,2)], by = "stno", all = T)