setwd("/home/zhuob/Project3/Data")


leaf <- read.csv("leaf.csv", header=T)
mydata <- leaf[, c(-1,-2)]
mydata <- scale(mydata)

# seed is set to use the same training and 
# validation datasets and make the two methods comparable.
set.seed(100)
id <- sample(1:dim(mydata)[1], 300)
training <- mydata[id, ]  # training data
validation <- mydata[-id,] # validation data


# K-Means Cluster Analysis

# Determine number of clusters
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:20) wss[i] <- sum(kmeans(mydata, 
                                     centers=i)$withinss)
plot(1:20, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")


fit <- kmeans(mydata, 12) # 5 cluster solution
# get cluster means 
aggregate(mydata,by=list(fit$cluster),FUN=mean)
# append cluster assignment
 mydata <- data.frame(mydata, fit$cluster)

library(cclust)
mod1 <- cclust(training, centers= 8, iter.max = 50,
               dist = "manhattan",method="kmeans")

km.pred <- predict(mod1, validation)
names(km.pred)
km.pred$cluster

# Cluster Plot against 1st 2 principal components

# vary parameters for most readable graph
library(cluster) 
clusplot(mydata, fit$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)
# Centroid Plot against 1st 2 discriminant functions
library(fpc)
plotcluster(mydata, fit$cluster)






## model based clustering  Mclust

leaf <- read.csv("leaf.csv", header=T)
mydata1 <- leaf[, c(-1,-2)]
mydata1 <- scale(mydata1)

# seed is set to use the same training and 
# validation datasets and make the two methods comparable.
# set.seed(100)
# id <- sample(1:dim(mydata)[1], 300)
# training <- mydata1[id, ]  # training data
# validation <- mydata1[-id,] # validation data


library(mclust)

fit <- Mclust(mydata1, G= 1:18)
names(fit)
fit$G # optimal number of clustering
head(fit$data)
fit$classification

result <- data.frame(leaf[, 1:2], classification = fit$classification)

head(result, 30)
aa <- t(table(result[,3], result[,1]))

# to find the max number of observations of a particular species belonging to
# the new cluster. for example, species 1 has 11  out of 12 falling into group 2
predict <- max.col(aa, ties.method= "first")
percentage <- round(apply(aa, 1, max)/rowSums(aa),2)
result.table <- cbind(aa, predict, percentage)
dim(result.table)
colnames(result.table) <- c("cluster1", "cluster2", "cluster3", "cluster4", "cluster5", 
                              "cluster6", "membership", "percentage")
species <- row.names(result.table)
result.table <- data.frame(species, result.table)

write.csv(result.table,"mclust.result.csv")

## prediction
# pre <- predict(fit, validation)
# pre$classification
plot(fit, what="classification" )


## how to visualize the results?


##  Hierarchical Clustering
# examples from http://www.stat.berkeley.edu/classes/s133/Cluster2a.html
library(protoclust)
hc <- protoclust(dist(training))
names(hc)

plot(hc, main = "default from hclust")

k <- 8
group.8 <- protocut(hc, k) # cut the tree into 8 groups
h <- hc$height[dim(training)[1]-k]

# plot dendrogram and show cut
plotwithprototypes(hc, imerge=group.8$imerge, col=2)
abline(h=h, lty=2)
predict(hc, validation)




































# generate some data:
set.seed(1)
n <- 100
p <- 2
x <- matrix(rnorm(n * p), n, p)
rownames(x) <- paste("A", 1:n, sep="")
d <- dist(x)
# perform minimax linkage clustering:
hc <- protoclust(d)
# cut the tree to yield a 10-cluster clustering:
k <- 10 # number of clusters
cut <- protocut(hc, k=k)
h <- hc$height[n - k]
# plot dendrogram (and show cut):
plotwithprototypes(hc, imerge=cut$imerge, col=2)
abline(h=h, lty=2)
# get the prototype assigned to each point:
pr <- cut$protos[cut$cl]
# find point farthest from its prototype:
dmat <- as.matrix(d)
ifar <- which.max(dmat[cbind(1:n, pr[1:n])])
# note that this distance is exactly h:
stopifnot(dmat[ifar, pr[ifar]] == h)
# since this is a 2d example, make 2d display:
plot(x, type="n")
points(x, pch=20, col="lightblue")
lines(rbind(x[ifar, ], x[pr[ifar], ]), col=3)
points(x[cut$protos, ], pch=20, col="red")
text(x[cut$protos, ], labels=hc$labels[cut$protos], pch=19)
tt <- seq(0, 2 * pi, length=100)
for (i in cut$protos) {
  lines(x[i, 1] + h * cos(tt), x[i, 2] + h * sin(tt))
}
