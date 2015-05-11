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

# train.data = train.data[train.data$label != 'normal', ]
test.data = test.data[test.data$label != 'normal', ]
real.test.data = real.test.data[real.test.data$label != 'normal', ]
random.forest.model = randomForest(attack.type ~ ., data=train.data[c(feature.selection, 45)], importance=TRUE, ntree=200)

if (is.debug == TRUE) {
  y.hat = predict(random.forest.model, test.data[feature.selection])
  print(confusionMatrix(y.hat, test.data$attack.type))
  y.hat = predict(random.forest.model, real.test.data[feature.selection])
  print(confusionMatrix(y.hat, real.test.data$attack.type))
}
println('Training finished')
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

println('Training normal/abnormal data classifier...')
## Scale does not help to improve accuracy
# train.data[1:41] = scale(data.matrix(train.data[1:41]))
# real.test.data[1:41] = scale(data.matrix(real.test.data[1:41]))
# test.data[1:41] = scale(data.matrix(test.data[1:41]))

train.normal = train.data[train.data$is.attack == FALSE, feature.selection]
train.attack = train.data[train.data$is.attack == TRUE, feature.selection]
train.normal = data.matrix(train.normal)
train.attack = data.matrix(train.attack)


  dt.model = rpart(is.attack ~ ., data=train.data[,c(feature.selection,43)])
  y.hat = predict(dt.model, test.data[,feature.selection])
    
  result1 = confusionMatrix(round(y.hat), as.numeric(test.data$is.attack))
  
  y.hat =predict(dt.model, real.test.data[,feature.selection])
  result2 = confusionMatrix(round(y.hat), as.numeric(real.test.data$is.attack))
  print(c(i,result1$overall[['Accuracy']],result2$overall[['Accuracy']]))



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
