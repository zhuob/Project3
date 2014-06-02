dat <- read.csv("C:/Users/Addison/Documents/Addison/Courses/ST599 (Big Data)/leaf.csv")
dat_s=scale(dat)
#round(runif(1,0,999999999))
#THE SEED IS: 28742798
set.seed(28742798)
dat.cl <- kmeans(dat_s[,3:16],30)
dat$cluster <- dat.cl$cluster

#############################################################
#                                                           #
# This section attemps to identify species based on cluster #
#                                                           #
#############################################################

a<-as.matrix(table(dat$cluster, dat$class))
key <- data.frame(class2=NA,cluster=1:30)
for(i in 1:30){
  if(length(as.numeric(names(which(a[i,]==max(a[i,])))))==1){
    key$class2[i] <- as.numeric(names(which(a[i,]==max(a[i,]))))
  }
}
dups <- key$class2[duplicated(key$class2)]
bad<-key$class2==27 | key$class2==23 | key$class2==8 | key$class2==34 | key$class2==5 | is.na(key$class2)
key$class2[bad]=0
dat2 <- merge(dat,key,by="cluster")
sum(dat2$class==dat2$class2)
#138 correct species clusters
mean(dat2$class==dat2$class2)
#0.41 prop of correct cluster/species
(length(unique(key$class2))-1)/30
#63% of clusters identified as species

#############################################################
#                                                           #
# This section attemps to identify cluster based on species #
#                                                           #
#############################################################

key <- data.frame(class=unique(dat$class),cluster2=NA)
for(i in 1:30){
  if(length(as.numeric(names(which(a[,i]==max(a[,i])))))==1){
    key$cluster2[i] <- as.numeric(names(which(a[,i]==max(a[,i]))))
  }
}
dups <- key$cluster2[duplicated(key$cluster2)]
bad<-key$cluster2==17 | key$cluster2==15 | key$cluster2==26 | key$cluster2==20 |  is.na(key$cluster2)
key$cluster2[bad]=0
dat2 <- merge(dat,key,by="class")
sum(dat2$cluster==dat2$cluster2)
#126 correct species clusters
mean(dat2$cluster==dat2$cluster2)
#0.37 prop of correct cluster/species
(length(unique(key$cluster2))-1)/30
#57% of species identified by clusters

