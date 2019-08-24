library(ggplot2)

sddt0 = read.csv("Result/all_all_cluster/sdDaily_cwb_all_all.csv")
sddt1 = read.csv("Result/ce_all_cluster/sdDaily_cwb_ce_all.csv")
sddt2 = read.csv("Result/cn_all_cluster/sdDaily_cwb_cn_all.csv")
sddt3 = read.csv("Result/cw_all_cluster/sdDaily_cwb_cw_all.csv")
sddt4 = read.csv("Result/cs_all_cluster/sdDaily_cwb_cs_all.csv")

set.seed(1)
data.k0 = kmeans(sddt0[2:6], centers = 4, iter.max = 100)
set.seed(1)
data.k1 = kmeans(sddt1[2:6], centers = 4, iter.max = 100)
set.seed(1)
data.k2 = kmeans(sddt2[2:6], centers = 4, iter.max = 100)
set.seed(1)
data.k3 = kmeans(sddt3[2:6], centers = 4, iter.max = 100)
set.seed(1)
data.k4 = kmeans(sddt4[2:6], centers = 4, iter.max = 100)

sddt0["cluster"] = factor(data.k0[["cluster"]])
sddt1["cluster"] = factor(data.k1[["cluster"]])
sddt2["cluster"] = factor(data.k2[["cluster"]])
sddt3["cluster"] = factor(data.k3[["cluster"]])
sddt4["cluster"] = factor(data.k4[["cluster"]])

ggplot() +
  scale_y_continuous(limits = c(-1, 1)) +
  geom_point(data = sddt0, aes(x = day, col = cluster), y = 1, pch = 15, size = 3) +
  geom_point(data = sddt1, aes(x = day, col = cluster), y = 0.5, pch = 15, size = 3) +
  geom_point(data = sddt2, aes(x = day, col = cluster), y = 0, pch = 15, size = 3) +
  geom_point(data = sddt3, aes(x = day, col = cluster), y = -0.5, pch = 15, size = 3) +
  geom_point(data = sddt4, aes(x = day, col = cluster), y = -1, pch = 15, size = 3) +
  theme_minimal() + xlab("Day") +
  theme(axis.text.y = element_blank())