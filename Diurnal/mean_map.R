library(ggplot2)
library(ggmap)
library(RColorBrewer)
library(tidyr)
library(dplyr)
library(RColorBrewer)

setwd("D:/OneDrive - g.ntu.edu.tw/Documents/108-I/中研院環變中心實習/Diurnal")

data = read.csv("Month/08_cwb_PP01.csv")

tw <- c(118.2, 21.5, 122.5, 26.4)
Map <- get_stamenmap(tw, zoom = 8, source = "stamen", maptype = "toner-lite")

#迴圈畫圖
hr = sprintf("X%02d", seq(1, 24, 1))
sn = c("DJF", "MAM", "JJA", "SON")

month = month.abb
for (j in 1:length(month)) {
  data = read.csv(paste0("Result/Monthly_cwb_PP01.csv"))
    
  mean_map = ggmap(Map, darken = c(0.5, "white")) +
               stat_summary_2d(data = data, aes_string(x = "Lon", y = "Lat", z = month[j]),
                               fun = function(x) mean(log10(x)), alpha = 0.95) +
               scale_fill_gradientn(name = 'log(PP01)', colours = new_col(20),
                                    space = 'Lab', limits = c(-0.3, 1.5)) +
               geom_point(data = data, aes(x = Lon, y = Lat), col = "#00000033", pch = 20) +
               ggtitle(paste(month[j], "log(PP01)"))
  
  name = paste0("Result/png/", month[j], "_log(PP01).png")
  ggsave(filename=name, plot=mean_map)
}

#Colours
new_col <- colorRampPalette(c("#FFFFFF", "#00BBFF", "#008800", "#FFFF00", "#FF5511", "#CC0000", "#990099", "#EE82EE"))
munsell::plot_hex(new_col(20))

#Draft
ggmap(Map, darken = c(0.5, "white")) +
  stat_summary_2d(data = data, aes_string(x = "Lon", y = "Lat", z = "Jan"),
                  fun = function(x) mean(log10(x)), alpha = 0.8) +
  scale_fill_gradientn(name = 'log(PP01)', colours = new_col(20),
                       space = 'Lab', limits = c(-0.3, 1.5)) +
  geom_point(data = data, aes(x = Lon, y = Lat), col = "#00000044")

#Distribution
ggplot() +
  geom_bar(aes(x = data[, 2])) +
  scale_x_log10()

#Limits
data <- apply(data[,6:17], 2, function(x) max(log10(x)))
