data("iris")
data = iris # Just to be able to use the name data and change the dataset.
n = nrow(data)

# Simple hold out validation
n_train = 0.8*n
ind = sample(1:n, n_train)
train_set = data[ind, ]

# Some linear models to start
lin_fit_1 = lm(Petal.Length ~ Petal.Width, data = train_set)
lin_fit_2 = lm(Petal.Length ~ poly(Petal.Width, degree = 2), data = train_set)

test_set = data[-ind, ]

# Make predictions
prediction1 = predict(lin_fit_1, newdata = test_set)
prediction2 = predict(lin_fit_2, newdata = test_set)

# Obtain MSE (mean((estimate - truth)^2))
mse1 = mean((prediction1 - test_set$Petal.Length)^2)
mse2 = mean((prediction2 - test_set$Petal.Length)^2)


# Cross validation
fit_n = 2
MSE_test = matrix(0, nrow = n, ncol = fit_n) # To store results

for (i in 1:n) {
  train_set = data[-i, ]

  lin_fit_1 = lm(Petal.Length ~ Petal.Width, data = train_set)
  lin_fit_2 = lm(Petal.Length ~ poly(Petal.Width, degree = 2), data = train_set)
  
  test_set = data[i, ]
  
  prediction1 = predict(lin_fit_1, newdata = test_set)
  prediction2 = predict(lin_fit_2, newdata = test_set)
  
  MSE_test[i, 1] = mean((prediction1 - test_set$Petal.Length)^2)
  MSE_test[i, 2] = mean((prediction2 - test_set$Petal.Length)^2)
}

apply(MSE_test, 2, mean) # Apply to columns
  

# k-fold cross validation
k = 10
ind = rep(1:k, n/k)
class_vector = sample(ind)
MSE_test = matrix(0, nrow = k, ncol = fit_n)
for (i in 1:k) {
  train_set = data[class_vector != i, ]
  
  lin_fit_1 = lm(Petal.Length ~ Petal.Width, data = train_set)
  lin_fit_2 = lm(Petal.Length ~ poly(Petal.Width, degree = 2), data = train_set)
  
  test_set = data[class_vector == i, ]
  
  prediction1 = predict(lin_fit_1, newdata = test_set)
  prediction2 = predict(lin_fit_2, newdata = test_set)
  
  MSE_test[i, 1] = mean((prediction1 - test_set$Petal.Length)^2)
  MSE_test[i, 2] = mean((prediction2 - test_set$Petal.Length)^2)
}

apply(MSE_test, 2, mean) # Apply to columns