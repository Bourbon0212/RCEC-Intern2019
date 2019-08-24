library(ggplot2)
library(ggfortify)
library(ggrepel)
library(factoextra)
library(gridExtra)

setwd("D:/OneDrive - g.ntu.edu.tw/Documents/108-I/中研院環變中心實習/Annual")

### Draft
#逐列(年)雨量標準化
standardize = function(x) {
  return((x - mean(x, na.rm = T))/sd(x, na.rm = T))
}

#Preparation
all = read.csv("Result/PP01_cluster/Monthly_cwb_all_PP01.csv")
ce = read.csv("Result/PP01_cluster/Monthly_cwb_ce_PP01.csv")
cn = read.csv("Result/PP01_cluster/Monthly_cwb_cn_PP01.csv")
cw = read.csv("Result/PP01_cluster/Monthly_cwb_cw_PP01.csv")
cs = read.csv("Result/PP01_cluster/Monthly_cwb_cs_PP01.csv")

all$month = as.factor(all$month)
all["mean"] = apply(all[,2:40], 1, function (x) round(mean(x, na.rm = T), 3))
all["upper"] = apply(all[,2:40], 1, function (x) max(x, na.rm = T))
all["lower"] = apply(all[,2:40], 1, function (x) min(x, na.rm = T))

ce$month = as.factor(ce$month)
ce["mean"] = apply(ce[,2:40], 1, function (x) round(mean(x, na.rm = T), 3))
ce["upper"] = apply(ce[,2:40], 1, function (x) max(x, na.rm = T))
ce["lower"] = apply(ce[,2:40], 1, function (x) min(x, na.rm = T))

cn$month = as.factor(cn$month)
cn["mean"] = apply(cn[,2:40], 1, function (x) round(mean(x, na.rm = T), 3))
cn["upper"] = apply(cn[,2:40], 1, function (x) max(x, na.rm = T))
cn["lower"] = apply(cn[,2:40], 1, function (x) min(x, na.rm = T))

cw$month = as.factor(cw$month)
cw["mean"] = apply(cw[,2:40], 1, function (x) round(mean(x, na.rm = T), 3))
cw["upper"] = apply(cw[,2:40], 1, function (x) max(x, na.rm = T))
cw["lower"] = apply(cw[,2:40], 1, function (x) min(x, na.rm = T))

cs$month = as.factor(cs$month)
cs["mean"] = apply(cs[,2:40], 1, function (x) round(mean(x, na.rm = T), 3))
cs["upper"] = apply(cs[,2:40], 1, function (x) max(x, na.rm = T))
cs["lower"] = apply(cs[,2:40], 1, function (x) min(x, na.rm = T))

#K-means
set.seed(1)
all.k = kmeans(all[2:40], centers = 2, iter.max = 100)
all["cluster"] = factor(all.k[["cluster"]])

set.seed(1)
ce.k = kmeans(ce[2:40], centers = 2, iter.max = 100)
ce["cluster"] = factor(ce.k[["cluster"]])

set.seed(1)
cn.k = kmeans(cn[2:40], centers = 2, iter.max = 100)
cn["cluster"] = factor(cn.k[["cluster"]])

set.seed(1)
cw.k = kmeans(cw[2:40], centers = 2, iter.max = 100)
cw["cluster"] = factor(cw.k[["cluster"]])

set.seed(1)
cs.k = kmeans(cs[2:40], centers = 2, iter.max = 100)
cs["cluster"] = factor(cs.k[["cluster"]])

#Plot
vis.vali = function(data, figName) {
  ret = ggplot(data = data) +
    geom_col(aes_string(x = "month", y = "mean", fill = "cluster")) +
    geom_linerange(aes(x = month, ymin = lower, ymax = upper), size = 2, col = "#00000033") +
    scale_x_discrete(labels = month.abb) +
    ylab("mean(mm)") +
    theme_minimal() +
    theme(axis.title.x = element_blank(), legend.position = "none") + 
    ggtitle(figName)
  return(ret)
}

#plot1 = vis.vali(all, "Taiwan")
plot1 = vis.vali(ce, "East")
plot2 = vis.vali(cn, "North")
plot3 = vis.vali(cw, "West")
plot4 = vis.vali(cs, "South")

grid1 = grid.arrange(plot1, plot2, plot3, plot4, ncol = 2)

grid.arrange(plot5, grid1, ncol = 2)
