library(dplyr)
library(ggplot2)
library(tidyverse)
library(caret)
library (skimr)
library(scales)
library(factoextra) 
library(cluster)
library(fmsb) 
library(NbClust) 
library(RColorBrewer) 
library(gridExtra)
library(FactoMineR)
library(stringr)
library(ggdendro)
library(ggpubr)
library(ggplot2)
library(gridExtra)

# pre-processing clustering
df_water_potability <- read.csv("C:/Users/ramad/Downloads/water_quality/water_potability.csv")

df_water_potability$Potability <- as.factor(df_water_potability$Potability)

# fill missing value
# fill NA values with Median
df_water_potability <- df_water_potability %>% mutate(across(where(is.numeric), ~replace_na(., median(., na.rm=TRUE))))

# drop column Potability
df_water_potability <- subset(df_water_potability, select = -c(Potability))

view(df_water_potability)

#scaling
df_water_potability_clust[, 1:9] = scale(df_water_potability[, 1:9])

view(df_water_potability_clust)
# 3276 row and 9 column
# check PCA analysis
water_pca <- PCA(df_water_potability_clust, graph = F)
fviz_screeplot(water_pca)
fviz_pca_biplot(water_pca)

#check PCA contributions
loadings <- as.data.frame(water_pca$var$contrib[,1:5])
loadings$Symbol <- row.names(loadings)
loadings <- gather(loadings, 'Component', 'Weight', -Symbol)
ggplot(loadings, aes(x=Symbol, y=Weight)) +
  geom_bar(stat='identity') +
  facet_grid(Component ~ ., scales='free_y') +
  theme(axis.text.x = element_text(angle = 90))


#hierachical clustering
# choose method
find_method <- c( "average", "single", "complete", "ward")
names(find_method) <- c( "average", "single", "complete", "ward")

#function to compute agglomerative coefficient
ac <- function(x) {
  agnes(df_water_potability_clust, method = x)$ac
}

#calculate agglomerative coefficient for each clustering linkage method
sapply(find_method, ac)

#find k optimal
# Elbow method
fviz_nbclust(df_water_potability_clust, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")

# Silhouette method
fviz_nbclust(df_water_potability_clust, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

# Gap statistic
set.seed(123)
fviz_nbclust(df_water_potability_clust, kmeans, nstart = 25,  method = "gap_stat", nboot = 50)+
  labs(subtitle = "Gap statistic method")

# Set cluster number
n_clust <- 2

# Set cluster name
c1 <- "cluster1"
c2 <- "cluster2"

# try to clust
cluster <- df_water_potability_clust %>% 
  dist(method = "euclidean") %>% 
  hclust(method="ward.D")

# visualize   
fviz_dend(cluster, k = n_clust, show_labels = F, rect = T)

water_hc_clust <- cutree(cluster, k = n_clust)

fviz_cluster(list(data=df_water_potability, clusters=water_hc_clust))

# get each data with cluster
groups <- cutree(cluster, k=n_clust) # cut tree

databind <- cbind(df_water_potability, Cluster = groups)  # Bind data with respective clusters
tail(databind)

#visualize each fitur with cluster
#preparation
ncol <- ncol(df_water_potability)

# Store variable names
var <- list()
for(i in 1:ncol){
  var[[i]] <- names(df_water_potability)[i]
}
names(df_water_potability)[1:ncol] <- paste("var", 1:ncol, sep="")

xy <- data.frame(cmdscale(dist(df_water_potability)), factor(groups))
names(xy) <- c("x", "y", "cluster")
xy$model <- rownames(xy)

for(i in 2:ncol){
  for(j in i: ncol){
    p <- ggplot(xy, aes(df_water_potability[[i-1]], df_water_potability[[j]])) + geom_point(aes(colour=cluster)) + labs(x = var[[i-1]], y = var[[j]])         
    print(p)
  }
}