# Binary Classification, normal vs. abnormal
# Precision: 87%
# need to install caret, e1071
# if not kmeans first, there will be too many ties
library('caret')
library('e1071')
library('rpart')
x_test_n = data.matrix(x_test)
x_train_n = data.matrix(x_train)
train_normal = x_train_n[y_train == 'normal',]
train_abnormal = x_train_n[y_train == 'abnormal',]
normal_centers = kmeans(train_normal, centers = 5)$centers
abnormal_centers = kmeans(train_abnormal, centers = 5)$centers
y_centers = c(rep('normal',5), rep('abnormal',10))
x_centers = rbind(normal_centers,abnormal_centers)
y_hat = knn(x_centers, x_test_n, y_centers, use.all = FALSE, k=1)
confusionMatrix(y_hat, y_test)
#             Reference
# Prediction abnormal normal
# abnormal    27739   4456
# normal         24   2363
# 
# Accuracy : 0.8705
# Kappa : 0.4579
rm(list=c('normal_centers','abnormal_centers'))