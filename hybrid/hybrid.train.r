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
nTree = c(10, 30, 50, 100, 200, 500, 1000, 1500, 2000)
for (i in nTree) {
  random.forest.model = randomForest(as.factor(is.attack) ~ ., data=train.data[c(feature.selection, 43)],
                                     importance=TRUE, ntree=i)
  if (is.debug == TRUE) {
    y.hat = predict(random.forest.model, test.data[,feature.selection])
    result1 = confusionMatrix(y.hat, test.data$is.attack)
    y.hat = predict(random.forest.model, real.test.data[,feature.selection])
    result2 = confusionMatrix(y.hat, real.test.data$is.attack)
    println(c(i,result1$overall[['Accuracy']],result2$overall[['Accuracy']]))
  }
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