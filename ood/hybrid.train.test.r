hybrid.train.test <- function(data.set) {
  #feature.selection = c(2,3,4,5,6,12,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41)
  feature.selection = 1:41
  
  train.data = data.set[['training']]
  test.data = data.set[['testing']]
  # First layer, use decision tree to classify is.attack
  println('Training normal/abnormal data classifier...')
  type = 'random.forest'
  binary.classifier <-
    switch(type,
           decision.tree = {
             # Train a decision tree
             binary.classifier = rpart(is.attack ~ ., data=train.data[,c(feature.selection,43)])
             y.hat = predict(binary.classifier, test.data[,feature.selection])
             binary.result = confusionMatrix(round(y.hat), as.numeric(test.data$is.attack))
           },
           random.forest = {
             # Train a random forest
             binary.classifier = randomForest(is.attack ~ ., data=train.data[c(feature.selection, 43)],
                                              importance=TRUE, ntree=200, type="class")
             y.hat = predict(binary.classifier, test.data[feature.selection], type="class")
             binary.result = confusionMatrix(y.hat, test.data$is.attack)
           })
  print(type)
  print(binary.result)
}