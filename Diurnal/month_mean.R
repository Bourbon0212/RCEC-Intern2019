
setwd("D:/OneDrive - g.ntu.edu.tw/Documents/108-I/中研院環變中心實習/Diurnal")

#Draft
read = paste0("Month/01_cwb_PP01.csv")
data = read.csv(read, header = T)

head = data[,c(1, 26:28)]

tail = as.data.frame(apply(data[,2:25], 1, sum))

head["JAN"] = apply(data[,2:25], 1, sum)

month = sprintf("%02d", seq(1, 12, 1))

##
status = T

for (i in 1:length(month)) {
  read = paste0("Month/", month[i], "_cwb_PP01.csv")
  print(read)
  data = read.csv(read, header = T)
  head = data[,c(1, 26:28)]
  if (status == T) {
    result = head
    result[month.abb[i]] = apply(data[,2:25], 1, sum)
    status = F
    print(result)
  } else {
    result[month.abb[i]] = apply(data[,2:25], 1, sum)
  }
}
write.csv(result, "Monthly_cwb_PP01.csv", row.names = T)
