hybrid.train.test <- function(data.set) {
  special.feature.selection <- c(2,3,4,5,6,12,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41)
  feature.selection <- 1:41
  
  train.data <- data.set[['training']]
  test.data <- data.set[['testing']]
  # First layer, use decision tree to classify is.attack
  println('Training normal/abnormal data classifier...')
  binary.type <- 'na'
  binary.classifier <-
    switch(binary.type,
           decision.tree = {
             # Train a decision tree, Accuracy : 0.925  
             binary.classifier <- rpart(is.attack ~ ., data=train.data[,c(feature.selection,43)])
             y.hat <- predict(binary.classifier, test.data[,feature.selection])
             binary.result <- confusionMatrix(round(y.hat), as.numeric(test.data$is.attack))
           },
           random.forest = {
             # Train a random forest，Accuracy : 0.9258 
             train.data.matrix  <-  train.data
             train.data.matrix[1:41] <- data.matrix(train.data[1:41])
             train.data.matrix$is.attack <- as.factor(train.data.matrix$is.attack)
             test.data.matrix <- test.data
             test.data.matrix[1:41] <- data.matrix(test.data[1:41])
             test.data.matrix$is.attack <- as.factor(test.data.matrix$is.attack)
             binary.classifier <- randomForest(is.attack ~ ., data=train.data.matrix[,c(feature.selection, 43)],
                                              importance=TRUE, ntree=10, type="class")
             y.hat <- predict(binary.classifier, test.data.matrix[,feature.selection], type="class")
             binary.result <- confusionMatrix(y.hat, test.data$is.attack)
           },
           kmeans = {
             # Create template and use KNN to classify data, Accuracy : 0.9228
             # Accuracy can be improved by adjusting number of centers!
             train.normal <- data.matrix(train.data[train.data$label == 'normal', 
                                                   special.feature.selection])
             train.attack <- data.matrix(train.data[train.data$label != 'normal', 
                                                   special.feature.selection])
             normal.centers <- kmeans(train.normal, centers = 100,iter.max = 10)$centers
             attack.centers <- kmeans(train.attack, centers = 300,iter.max = 10)$centers
             x.centers <- rbind(normal.centers,  attack.centers)
             y.labels <- c(rep(FALSE,dim(normal.centers)[1]),
                          rep(TRUE,dim(attack.centers)[1]))
             y.hat <- knn(x.centers, data.matrix(test.data[,special.feature.selection]), y.labels, k=1)
             binary.result <- confusionMatrix(y.hat, test.data$is.attack)
           })
  print(binary.type)
  if (exists("binary.result"))
    print(binary.result)
  
  
  
  # Begin multi-class classification
  println('Training attack type classifier...')
  multiclass.type <- 'random.forest'
  train.attack = train.data[train.data$label != 'normal',]
  test.attack = test.data[test.data$label != 'normal',]
  multiclass.classifier <-
    switch(multiclass.type,
           decision.tree = {
             # Train a decision tree,  Accuracy : 0.9302  
             multiclass.classifier <- rpart(attack.type ~ ., data=train.attack[,c(feature.selection,45)])
             y.hat <- predict(multiclass.classifier, test.attack[,feature.selection],type='class')
             multiclass.result <- confusionMatrix(y.hat, test.attack$attack.type)
           },
           random.forest = {
             # Train a random forest, please notice the confusion matrix, Accuracy : 0.9178  
             train.data.matrix  <-  train.attack
             train.data.matrix[1:41] <- data.matrix(train.data[1:41])
             test.data.matrix <- test.attack
             test.data.matrix[1:41] <- data.matrix(test.data[1:41])
             train.data.matrix$attack.type <- factor(train.data.matrix$attack.type, levels = c("dos","probe", "r2l", "u2r"))
             multiclass.classifier <- randomForest(attack.type ~ ., data=train.data.matrix[,c(feature.selection, 45)],
                                               importance=TRUE, ntree=10, type="class")
             y.hat <- predict(multiclass.classifier, test.data.matrix[,feature.selection], type="class")
             y.hat <- factor(y.hat, levels = c("dos","normal","probe", "r2l", "u2r"))
             multiclass.result <- confusionMatrix(y.hat, test.attack$attack.type)
           },
           kmeans = {
             # Create template and use KNN to classify data, Accuracy : 0.7711 
             # Accuracy can be improved by adjusting number of centers!
             train.dos = train.data[train.attack$attack.type=='dos', feature.selection]
             train.normal = train.data[train.attack$attack.type=='normal', feature.selection]
             train.probe = train.data[train.attack$attack.type=='probe', feature.selection]
             train.r2l = train.data[train.attack$attack.type=='r2l', feature.selection]
             train.u2r = train.data[train.attack$attack.type=='u2r', feature.selection]
             train.dos = data.matrix(train.dos)
             train.normal = data.matrix(train.normal)
             train.probe = data.matrix(train.probe)
             train.r2l = data.matrix(train.r2l)
             train.u2r = data.matrix(train.u2r)
             
             dos.centers = kmeans(train.dos, centers = 60 ,iter.max = 10)$centers
             #normal.centers = kmeans(train.normal, centers = i*3,iter.max = 10)$centers
             probe.centers = kmeans(train.probe, centers = 10,iter.max = 10)$centers
             r2l.centers = kmeans(train.r2l, centers = 10,iter.max = 10)$centers
             u2r.centers = kmeans(train.u2r, centers = 10,iter.max = 10)$centers
             y.labels = c(rep('dos',dim(dos.centers)[1]),
                          #rep('normal',dim(normal.centers)[1]),
                          rep('probe',dim(probe.centers)[1]),
                          rep('r2l',dim(r2l.centers)[1]),
                          rep('u2r',dim(u2r.centers)[1]))
             x.centers = rbind(dos.centers,
                               probe.centers,
                               r2l.centers,
                               u2r.centers)
             y.hat <- knn(x.centers, data.matrix(test.attack[,feature.selection]), y.labels, k=1)
             y.hat <- factor(y.hat, levels = c("dos","normal","probe", "r2l", "u2r"))
             multiclass.result <- confusionMatrix(y.hat, test.attack$attack.type)
           })
  print(multiclass.type)
  print(multiclass.result)
}