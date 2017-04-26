# Loading data
data<-iris[,-c(5)]

# To standarize the variables 
data = scale(data) 

# Assessing cluster tendency
if(!require(clustertend)) install.packages("clustertend")
library(clustertend)
# Compute Hopkins statistic for the dataset
set.seed(123)
hopkins(data, n = nrow(data)-1)
#Since the H value = 0.1815 which is far below the threshold 0.5, it is highly clusterable

###########################################################################
####################### K Means clustering ################################
###########################################################################

# K-mean - Determining optimal number of clusters
# NbClust Package : 30 indices to determine the number of clusters in a dataset
# If index = 'all' - run 30 indices to determine the optimal no. of clusters
# If index = "silhouette" - It is a measure to estimate the dissimilarity between clusters.
# A higher silhouette width is preferred to determine the optimal number of clusters

if(!require(NbClust)) install.packages("NbClust")
nb <- NbClust(data,  distance = "euclidean", min.nc=2, max.nc=15, method = "kmeans",
              index = "silhouette")
nb$All.index
nb$Best.nc

#Method II : Same Silhouette Width analysis with fpc package
library(fpc)
pamkClus <- pamk(data, krange = 2:15, criterion="multiasw", ns=2, critout=TRUE)
pamkClus$nc
cat("number of clusters estimated by optimum average silhouette width:", pamkClus$nc, "\n")

#Method III : Scree plot to determine the number of clusters
#wss <- (nrow(data)-1)*sum(apply(data,2,var))
for (i in 2:15) {
  wss[i] <- (kmeans(data,centers=i)$betweenss)
} 
plot(1:15, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")

# K-Means Cluster Analysis
fit <- kmeans(data,pamkClus$nc)

# get cluster means
aggregate(data,by=list(fit$cluster),FUN=mean)

# append cluster assignment
data <- data.frame(data, clusterid=fit$cluster)

###########################################################################
####################### Hierarchical clustering############################
###########################################################################

# Hierarchical clustering - Determining optimal number of clusters
library(NbClust)
res<-NbClust(data, diss=NULL, distance = "euclidean", min.nc=2, max.nc=6,
             method = "ward.D2", index = "kl")
res$All.index
res$Best.nc

# Ward Hierarchical Clustering
d <- dist(data, method = "euclidean")
fit <- hclust(d, method="ward.D2")
plot(fit) # display dendogram

# cluster assignment (members)
groups <- cutree(fit, k=2)
data = cbind(data,groups)

# draw dendogram with red borders around the 2 clusters
rect.hclust(fit, k=2, border="red")


################ Clustering validation #################################
#Load data
data = iris[,-c(5)]
data = scale(data)

#Loading desired package
install.packages("clValid")
library(clValid)

# Internal Validation
clmethods <- c("hierarchical","kmeans","pam")
internval <- clValid(data, nClust = 2:5, clMethods = clmethods, validation = "internal")

# Summary
summary(internval)
optimalScores(internval)

# Hierarchical clustering with two clusters was found the best clustering algorithm in each case (i.e., for connectivity, Dunn and Silhouette measures)
plot(internval)

# Stability measure Validation
clmethods <- c("hierarchical","kmeans","pam")
stabval <- clValid(data, nClust = 2:6, clMethods = clmethods,
                   validation = "stability")

# Display only optimal Scores
summary(stabval)
optimalScores(stab)

# External Clustering Validation
library(fpc)

# K-Means Cluster Analysis
fit <- kmeans(data,2)

# Compute cluster stats
species <- as.numeric(iris$Species)
clust_stats <- cluster.stats(d = dist(data), species, fit$cluster)

# Corrected Rand index and VI Score
# Rand Index should be maximized and VI score should be minimized
clust_stats$corrected.rand
clust_stats$vi

# k means Cluster Analysis
fit <- kmeans(data,2)

# Compute cluster stats
species <- as.numeric(iris$Species)
clust_stats <- cluster.stats(d = dist(data), species, fit$cluster)

# Corrected Rand index and VI Score
# Rand Index should be maximized and VI score should be minimized
clust_stats$corrected.rand
clust_stats$vi

# Same analysis for Ward Hierarchical Clustering
d2 <- dist(data, method = "euclidean")
fit2 <- hclust(d2, method="ward.D2")

# cluster assignment (members)
groups <- cutree(fit2, k=2)

# Compute cluster stats
clust_stats2 <- cluster.stats(d = dist(data), species, groups)

# Corrected Rand index and VI Score
# Rand Index should be maximized and VI score should be minimized
clust_stats2$corrected.rand
clust_stats2$vi