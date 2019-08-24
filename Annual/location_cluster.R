library(ggmap)
library(ggplot2)
library(ggfortify)
library(ggrepel)
library(gridExtra)
library(factoextra)

setwd("D:/OneDrive - g.ntu.edu.tw/Documents/108-I/中研院環變中心實習/Annual")
data = read.csv("Result/PP01_cluster/Monthly_cwb_loc_PP01.csv")

#PCA & K-means Cluster
data.pca = prcomp(data[5:16])
set.seed(1)
data.k = kmeans(data[5:16], centers = 4)
data["cluster"] = factor(data.k[["cluster"]])

#Optimal Cluster Plot
plot1 = fviz_nbclust(data[5:16], 
                     FUNcluster = kmeans,# K-Means
                     method = "wss",     # total within sum of square
) + geom_vline(xintercept = 6, linetype = 2) + theme_minimal()

#Scree Plot
plot2 = fviz_eig(data.pca)

grid.arrange(plot1, plot2, ncol=2)

#Bi Plot
plot3 = fviz_pca_biplot(data.pca, geom.ind = "point")

#Cluster Plot
plot4 = autoplot(data.k, data = data[5:16], frame = T) +
        theme_minimal()

grid.arrange(plot3, plot4, ncol=2)

#Map
tw <- c(118.2, 21.5, 122.5, 26.4)
Map <- get_stamenmap(tw, zoom = 8, source = "stamen", maptype = "toner-lite")

plot5 = ggmap(Map, darken = c(0.5, "white")) +
          geom_point(data = data, aes(x = Lon, y = Lat, col = cluster), size = 3) +
          geom_label_repel(data = data, aes(x = Lon, y = Lat, label = name),
                    box.padding   = 0.35, point.padding = 0.5, size = 3)
