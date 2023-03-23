list.of.packages <- c("tidyverse", "cluster", "factoextra", "gridExtra", "caret", "sqldf")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos="http://cran.rstudio.com/")

library(tidyverse)# Data Processing
library(cluster)# Clustering
library(factoextra)# Cluster Visualization
library(gridExtra)# Cluster Visualization
library(caret)
library(sqldf)

Path<-("C:/Users/Arhit Roy Chowdhury/Desktop/RStudio Class/Case Studies/Clustering")
setwd(Path)
getwd()

data_main = read.csv("Mall_Customers.csv")
data0 = data_main

summary(data0)
as.data.frame(colSums(is.na(data0)))

data1 = data0[,-1]
summary(data1)
data2 = data1
data2$Gender = as.factor(data1$Gender)
summary(data2)
data3 = data2[,-1]

PreProc = preProcess(data3)
data_norm = predict(PreProc, data3)
summary(data_norm)

data_distance = dist(data_norm, method = "euclidean")
data_hclust = hclust(data_distance, method = "ward.D")
plot(data_hclust)

data_cluster = cutree(data_hclust, k = 6)
table(data_cluster)

aggregate(data3, list(data_cluster), mean)

data_hc = data.frame(data0, data_cluster)
write.csv(data_hc, "Hierarchial Cluster.csv", row.names = FALSE)
colnames(data_hc)

sqldf("select Gender, data_cluster as Data_Cluster,
      avg(Age) as Age, 
      round(avg(`Annual.Income..k..`), 2) as Average_Income, 
      round(avg(`Spending.Score..1.100.`), 2) as Spending_Score
      from data_hc 
      group by Gender, data_cluster
      order by data_cluster")

set.seed(123)
fviz_nbclust(data_norm, kmeans, method = "wss")
gap_stat <- clusGap(data_norm, FUN = kmeans, nstart = 30, K.max = 24, B = 50)
fviz_gap_stat(gap_stat)

set.seed(185)
data_kmeans = kmeans(data_norm, centers = 7, iter.max = 1000)
data_kmeans

table(data_kmeans$cluster)
data_kmeans$centers
fviz_cluster(data_kmeans, data = data3)

data_km = data.frame(data0, data_kmeans$cluster)
write.csv(data_km, "K-Means.csv", row.names = FALSE)
colnames(data_km)

sqldf("select Gender, `data_kmeans.cluster` as Data_Cluster,
      avg(Age) as Age, 
      round(avg(`Annual.Income..k..`), 2) as Average_Income, 
      round(avg(`Spending.Score..1.100.`), 2) as Spending_Score
      from data_kMeans 
      group by Gender, `data_kmeans.cluster`
      order by `data_kmeans.cluster`")

