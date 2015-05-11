hybrid.train.test <- function(data.set) {
  special.feature.selection = c(2,3,4,5,6,12,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41)
  feature.selection = 1:41
  
  train.data = data.set[['training']]
  test.data = data.set[['testing']]
  # First layer, use decision tree to classify is.attack
  println('Training normal/abnormal data classifier...')
  binary.type = 'kmeans'
  binary.classifier <-
    switch(binary.type,
           decision.tree = {
             # Train a decision tree, Accuracy : 0.925  
             binary.classifier = rpart(is.attack ~ ., data=train.data[,c(feature.selection,43)])
             y.hat = predict(binary.classifier, test.data[,feature.selection])
             binary.result = confusionMatrix(round(y.hat), as.numeric(test.data$is.attack))
           },
           random.forest = {
             # Train a random forest，Accuracy : 0.9258 
             train.data.matrix  <-  train.data
             train.data.matrix[1:41] <- data.matrix(train.data[1:41])
             train.data.matrix$is.attack <- as.factor(train.data.matrix$is.attack)
             test.data.matrix = test.data
             test.data.matrix[1:41] = data.matrix(test.data[1:41])
             test.data.matrix$is.attack <- as.factor(test.data.matrix$is.attack)
             binary.classifier = randomForest(is.attack ~ ., data=train.data.matrix[,c(feature.selection, 43)],
                                              importance=TRUE, ntree=10, type="class")
             y.hat = predict(binary.classifier, test.data.matrix[,feature.selection], type="class")
             binary.result = confusionMatrix(y.hat, test.data$is.attack)
           },
           kmeans = {
             # Create template and use KNN to classify data, Accuracy : 0.9228
             # Accuracy can be improved by adjusting number of centers!
             train.normal = data.matrix(train.data[train.data$label == 'normal', 
                                                   special.feature.selection])
             train.attack = data.matrix(train.data[train.data$label != 'normal', 
                                                   special.feature.selection])
             normal.centers = kmeans(train.normal, centers = 100,iter.max = 10)$centers
             attack.centers = kmeans(train.attack, centers = 300,iter.max = 10)$centers
             x.centers = rbind(normal.centers,  attack.centers)
             y.labels = c(rep(FALSE,dim(normal.centers)[1]),
                          rep(TRUE,dim(attack.centers)[1]))
             y.hat = knn(x.centers, data.matrix(test.data[,special.feature.selection]), y.labels, k=1)
             binary.result = confusionMatrix(y.hat, test.data$is.attack)
           })
  print(binary.type)
  print(binary.result)
  
  
  
  # Begin multi-class classification
  
}