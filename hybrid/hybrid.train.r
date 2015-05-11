# Set the seed to make partition reproductible
set.seed(1)
is.debug = TRUE
# Prepare, split train and test
if (is.debug == TRUE)  {
  train.size <- floor(0.7 * nrow(data))
  train.index <- sample(seq_len(nrow(data)), size = train.size)
  train.data <- data[train.index, ]
  test.data <- data[-train.index, ]
  rm(train.index)
} else {
  train.data = data
}

#feature.selection = c(2,3,4,5,6,12,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41)
feature.selection = 1:41
# First layer, use decision tree to classify is.attack
println('Training normal/abnormal data classifier...')
## Scale does not help to improve accuracy
# train.data[1:41] = scale(data.matrix(train.data[1:41]))
# real.test.data[1:41] = scale(data.matrix(real.test.data[1:41]))
# test.data[1:41] = scale(data.matrix(test.data[1:41]))

train.dos = train.data[train.data$attack.type=='dos', feature.selection]
train.normal = train.data[train.data$attack.type=='normal', feature.selection]
train.probe = train.data[train.data$attack.type=='probe', feature.selection]
train.r2l = train.data[train.data$attack.type=='r2l', feature.selection]
train.u2r = train.data[train.data$attack.type=='u2r', feature.selection]

train.dos = data.matrix(train.dos)
train.normal = data.matrix(train.normal)
train.probe = data.matrix(train.probe)
train.r2l = data.matrix(train.r2l)
train.u2r = data.matrix(train.u2r)
test.data = test.data[test.data$label != 'normal',]
real.test.data = real.test.data[real.test.data$attack.type != 'normal',]


for (i in c(5)) {
  
  dos.centers = kmeans(train.dos, centers = i*6 ,iter.max = 10)$centers
#   normal.centers = kmeans(train.normal, centers = i*3,iter.max = 10)$centers
  probe.centers = kmeans(train.probe, centers = i,iter.max = 10)$centers
  r2l.centers = kmeans(train.r2l, centers = i,iter.max = 10)$centers
  u2r.centers = kmeans(train.u2r, centers = i,iter.max = 10)$centers
  y.labels = c(rep('dos',dim(dos.centers)[1]),
#                 rep('normal',dim(normal.centers)[1]),
                   rep('probe',dim(probe.centers)[1]),
                       rep('r2l',dim(r2l.centers)[1]),
                           rep('u2r',dim(u2r.centers)[1]))
  x.centers = rbind(dos.centers,  probe.centers, r2l.centers, u2r.centers)
  
  
  y.hat = knn(x.centers, data.matrix(test.data[,feature.selection]), y.labels, k=1)
  result1 = confusionMatrix(as.character(y.hat), as.character(test.data$attack.type))
  
  y.hat = knn(x.centers, data.matrix(real.test.data[,feature.selection]), y.labels, k=1)
  result2 = confusionMatrix(as.character(y.hat), as.character(real.test.data$attack.type))
  print(c(i, result1$overall[['Accuracy']], result2$overall[['Accuracy']]))
}


# Second Layer
# println('Training abnormal data classifier...')
# abnormal.data = data[data$is.attack == TRUE,]
# 
# if (is.debug == TRUE) {
#   train.size <- floor(0.3 * nrow(abnormal.data))
#   train.index <- sample(seq_len(nrow(abnormal.data)), size = train.size)
#   train.data <- abnormal.data[train.index, ]
#   test.data <- abnormal.data[-train.index, ]
# } else {
#   train.data = abnormal.data
# }
# 
# 
# nb.model <- naiveBayes(attack.type ~ ., data = train.data[c(feature.selection, 44)])

# random.forest.model = randomForest(attack.type ~ ., data=train.data[c(feature.selection, 44)], importance=TRUE, ntree=2000)

# if (is.debug == TRUE) {
#   y.hat = predict(nb.model, test.data[feature.selection])
#   print(confusionMatrix(y.hat, test.data[,44]))
# }

# println('Training finished')
