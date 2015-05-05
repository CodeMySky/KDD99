# Set the seed to make partition reproductible
set.seed(1)
is.debug = TRUE
# Prepare, split train and test
if (is.debug == TRUE)  {
  train.size <- floor(0.8 * nrow(data))
  train.index <- sample(seq_len(nrow(data)), size = train.size)
  train.data <- data[train.index, ]
  test.data <- data[-train.index, ]
  rm(train.index)
} else {
  train.data = data
}

feature.selection = c(2,3,4,5,6,12,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41)
#feature.selection = 1:41
# First layer, use decision tree to classify is.attack
println('Training normal/abnormal data classifier...')
## Scale does not help to improve accuracy
# train.data[1:41] = scale(data.matrix(train.data[1:41]))
# real.test.data[1:41] = scale(data.matrix(real.test.data[1:41]))
# test.data[1:41] = scale(data.matrix(test.data[1:41]))

train.normal = train.data[train.data$is.attack == FALSE, feature.selection]
train.attack = train.data[train.data$is.attack == TRUE, feature.selection]
train.normal = data.matrix(train.normal)
train.attack = data.matrix(train.attack)

for (i in c(2,3,4,5,6,7,8,9,10,15,20,50,100,200,500,1000, 1500, 2000)) {
  normal.centers = kmeans(train.normal, centers = i,iter.max = 10)$centers
  attack.centers = kmeans(train.attack, centers = 3 * i,iter.max = 10)$centers
  y.labels = c(rep(FALSE,dim(normal.centers)[1]), rep(TRUE,dim(attack.centers)[1]))
  x.centers = rbind(normal.centers, attack.centers)
  
  
  y.hat = knn(x.centers, data.matrix(test.data[,feature.selection]), y.labels, k=1)
  result1 = confusionMatrix(y.hat, test.data$is.attack)
  
  y.hat = knn(x.centers, data.matrix(real.test.data[,feature.selection]), y.labels, k=1)
  result2 = confusionMatrix(y.hat, real.test.data$is.attack)
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
