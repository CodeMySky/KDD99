library('lattice')
train.data <- data.set$training
train.data[,1:41] <- data.matrix(train.data[,1:41])
pca <- prcomp(train.data[,c(2,3,4,5,6,12,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41)], 
              retx=TRUE, center=TRUE, scale=TRUE)
new.train.data = predict(pca, train.data[,c(2,3,4,5,6,12,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41)])
km = kmeans(new.train.data, centers=10)
cluster = km$cluster
new.train.data = cbind(new.train.data, cluster)
new.train.data = as.data.frame(new.train.data)
xyplot(PC1 ~ PC2, group=cluster, data=new.train.data)