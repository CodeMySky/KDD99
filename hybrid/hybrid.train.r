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

train.normal = train.data[train.data$is.attack == FALSE, feature.selection]
train.attack = train.data[train.data$is.attack == TRUE, feature.selection]
train.normal = data.matrix(train.normal)
train.attack = data.matrix(train.attack)

normal.centers = kmeans(train.normal, centers = 1000)$centers
attack.centers = kmeans(train.attack, centers = 5000)$centers
y.labels = c(rep(FALSE,dim(normal.centers)[1]), rep(TRUE,dim(attack.centers)[1]))
x.centers = rbind(normal.centers, attack.centers)


y.hat = knn(x.centers, data.matrix(test.data[,feature.selection]), y.labels, k=1)
print(confusionMatrix(y.hat, test.data$is.attack))


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
# if (is.debug == TRUE) {
#   y.hat = predict(nb.model, test.data[feature.selection])
#   print(confusionMatrix(y.hat, test.data[,44]))
# }
println('Training finished')