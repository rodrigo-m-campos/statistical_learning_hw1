#install.packages("Ecdat")
library(Ecdat)
library(ggplot2)
library(GGally)
library(e1071)
#data(DoctorContacts)
data = DoctorContacts

# Check for NAs
# hist(rowMeans(is.na(data)))
barplot(colMeans(is.na(data)), las=2)

any(is.na(data))

# No NAs :)

summary(data)

# Have male as 0 and female as 1
data$sex = factor(data$sex, levels = c('male', 'female'), labels = c(0, 1))

levels(data$health)
# They are correctly ordered!

# Outliers
boxplot(data[, sapply(data, is.numeric)], 
        main = "Outliers",
        ylab = "Values",
        col = "red")

# Look at correlation
# We need numerical values ONLY
data_num = data[,-c(3, 6, 8, 13, 14, 15)]
R = cor(data_num)
ggcorr(data_num, label = T)
# It seems there are no strong correlations with mdu. Biggest one is ndisiease.

featurePlot(x = data_num[-1], y = data_num$mdu,
            plot = "scatter")
# No clear correlation looking at this either. Maybe with linc?


# Linear models
n = nrow(data)
# Simple hold out validation
n_train = 0.8*n
ind = sample(1:n, n_train)
train_set = data[ind, ]

lin_fit_1 = lm(mdu ~ ndisease, data = data)
# It seems closer to degree 2
lin_fit_2 = lm(mdu ~ poly(linc, degree = 2), data = data)
# A big one
lin_fit_3 = lm(mdu ~ ., data=data)

test_set = data[-ind, ]

# Make predictions
prediction1 = predict(lin_fit_1, newdata = test_set)
prediction2 = predict(lin_fit_2, newdata = test_set)
prediction3 = predict(lin_fit_3, newdata = test_set)

# Obtain MSE (mean((estimate - truth)^2))
mse1 = mean((prediction1 - test_set$mdu)^2)
mse2 = mean((prediction2 - test_set$mdu)^2)
mse3 = mean((prediction3 - test_set$mdu)^2)
# Pretty, pretty, pretty, pretty bad...

# Let us try with k-fold:
k = 10
fit_n = 3
ind = rep(1:k, n/k)
class_vector = sample(ind)
MSE_test = matrix(0, nrow = k, ncol = fit_n)
for (i in 1:k) {
  train_set = data[class_vector != i, ]
  
  lin_fit_1 = lm(mdu ~ ndisease, data = data)
  lin_fit_2 = lm(mdu ~ poly(linc, degree = 2), data = data)
  lin_fit_3 = lm(mdu ~ ., data=data)
  
  test_set = data[class_vector == i, ]
  
  # Make predictions
  prediction1 = predict(lin_fit_1, newdata = test_set)
  prediction2 = predict(lin_fit_2, newdata = test_set)
  prediction3 = predict(lin_fit_3, newdata = test_set)
  
  MSE_test[i, 1] = mean((prediction1 - test_set$mdu)^2)
  MSE_test[i, 2] = mean((prediction2 - test_set$mdu)^2)
  MSE_test[i, 3] = mean((prediction3 - test_set$mdu)^2)
}

apply(MSE_test, 2, mean) # Apply to columns


# Bootstrap?
