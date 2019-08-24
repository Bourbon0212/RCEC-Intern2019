library(gridExtra)
library(factoextra)
library(ggfortify)

setwd("D:/OneDrive - g.ntu.edu.tw/Documents/108-I/中研院環變中心實習/Annual")

### Standardize
dt = read.csv("Daily_cwb_cs_all.csv")

dt["PP01"] = (dt$PP01 - mean(dt$PP01)) / sd(dt$PP01)
dt["PS01"] = (dt$PS01 - mean(dt$PS01)) / sd(dt$PS01)
dt["TX01"] = (dt$TX01 - mean(dt$TX01)) / sd(dt$TX01)
dt["WD_u"] = (dt$WD_u - mean(dt$WD_u)) / sd(dt$WD_u)
dt["WD_v"] = (dt$WD_v - mean(dt$WD_v)) / sd(dt$WD_v)

write.csv(dt, "sdDaily_cwb_cs_all.csv", row.names = F)

### Main
sddt = read.csv("Result/all_all_cluster/sdDaily_cwb_all_all.csv")
sddt["day"] = as.Date(sddt$day, origin = as.Date("2017-12-31"))

#PCA & K-means Cluster
data.pca = prcomp(sddt[2:6])
set.seed(1)
data.k = kmeans(sddt[2:6], centers = 5, iter.max = 100)
sddt["cluster"] = factor(data.k[["cluster"]])

#Optimal Cluster Plot
plot1 = fviz_nbclust(sddt[2:6], 
                     FUNcluster = kmeans,# K-Means
                     method = "wss",     # total within sum of square
        ) + geom_vline(xintercept = 5, linetype = 2) + theme_minimal()

#Scree Plot
plot2 = fviz_eig(data.pca)

grid.arrange(plot1, plot2, ncol=2)

#Bi Plot
plot3 = fviz_pca_biplot(data.pca, geom.ind = "point")

#Cluster Plot
plot4 = autoplot(data.k, data = sddt[2:6], frame = T) +
        theme_minimal()

grid.arrange(plot3, plot4, ncol=2)

#Visual
plot5 = ggplot(data = sddt) +
        geom_col(aes(x = day, y = PP01, fill = "PP01(mm)")) +
        geom_point(aes(x = day, col = cluster), y = 0, pch = 15, size = 1.5) +
        scale_color_manual(name = "", values = c("#F8766D", "#A3A500", "#00BF7D", "#00B0F6", "#E76BF3"), breaks = "", labels = "") +
        scale_fill_manual(name = "", values = c("PP01(mm)"="#ADD8E666")) +
        scale_x_date(date_breaks = "1 month", date_labels = "%B", expand = c(0,0)) +
        ylab("z-score") + ggtitle("Daily Clustering by K-means") +
        theme_minimal() +
        theme(axis.title.x = element_blank(),
              axis.text.x = element_blank())

plot6 = ggplot(data = sddt) +
        geom_col(aes(x = day, y = PS01, fill = "PS01(hPa)")) +
        geom_col(aes(x = day, y = TX01, fill = "TX01(°C)")) +
        geom_point(aes(x = day, col = cluster), y = 0, pch = 15, size = 1.5) +
        scale_color_manual(name = "", values = c("#F8766D", "#A3A500", "#00BF7D", "#00B0F6", "#E76BF3"), breaks = "", labels = "") +
        scale_fill_manual(name = "", values = c("PS01(hPa)"="#ADFF2F44", "TX01(°C)"="#FF8C0044")) +
        scale_x_date(date_breaks = "1 month", date_labels = "%B", expand = c(0,0)) +
        ylab("z-score") +
        theme_minimal() +
        theme(axis.title.x = element_blank(),
              axis.text.x = element_blank())

plot7 = ggplot(data = sddt) +
        geom_col(aes(x = day, y = WD_u, fill = "E - W(m/s)")) +
        geom_col(aes(x = day, y = WD_v, fill = "N - S(m/s)")) +
        geom_point(aes(x = day, col = cluster), y = 0, pch = 15, size = 1.5) +
        scale_color_manual(name = "", values = c("#F8766D", "#A3A500", "#00BF7D", "#00B0F6", "#E76BF3"), breaks = "", labels = "") +
        scale_fill_manual(name = "", values = c("E - W(m/s)"="#DAA52044", "N - S(m/s)"="#EE82EE44")) +
        scale_x_date(date_breaks = "1 month", date_labels = "%B", expand = c(0,0)) +
        ylab("z-score") + xlab("Month") +
        theme_minimal()

grid.arrange(plot5, plot6, plot7, nrow=3)
