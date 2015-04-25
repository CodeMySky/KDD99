library('MASS')
x_train_n = data.matrix(x_train)
x_test_n = data.matrix(x_test)
# u [44461 * 41]
u = svd(x_train_n)$u
# p_c = [20 * 41]
prominent_component = u[1:20,]
# invp [41 * 20]
invp = ginv(prominent_component)
# x_train_n = [44461 * 41]
x_train_n_pca = x_train_n %*% invp
x_test_n_pca =  x_test_n %*% invp
y_hat = knn(x_train_n_pca, x_test_n_pca, y_train, use.all = FALSE, k=1)