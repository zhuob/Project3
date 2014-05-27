getwd()
leaf<-read.csv("/Users/feifeilei/Box Documents/OSU/14Spring/ST599/Project3/Data/leaf.csv")
leaf_std<-scale(leaf) #stardardize variables

#Determine number of clusters
wss <- (nrow(leaf_std)-1)*sum(apply(leaf_std,2,var))
for (i in 2:40) wss[i] <- sum(kmeans(leaf_std, 
                                     centers=i)$withinss)
plot(1:40, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

# K-Means Cluster Analysis
fit <- kmeans(leaf_std, 15) # 15 cluster solution
# get cluster means 
aggregate(leaf_std,by=list(fit$cluster),FUN=mean)
# append cluster assignment
leaf_std <- data.frame(leaf_std, fit$cluster)
head(leaf_std)

#Partitioning Around Medoids with estimation of number of clusters
library(fpc)
pk<-pamk(leaf,krange=1:5,criterion="asw",usepam=TRUE, scaling=TRUE, critout=TRUE)

