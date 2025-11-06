#install.packages("Ecdat")
library(Ecdat)
library(ggplot2)
library(GGally)
library(factoextra)
library(e1071)
library(boot)
library(bootstrap)
#data(DoctorContacts)
data = DoctorContacts

# What are the variables?
# lc:  log(coinsurance rate + 1), where coinsurance rate ranges from 0 to 100 (the percentage the patient pays).
# idp:  Binary indicator for whether the person has an individual deductible plan. (Has to pay for going to the doctor).
# lpi:  log(annual participation incentive payment + 1) (or 0 if no payment). (Paid to go).
# fmde:  log(max(medical deductible expenditure)) if IDP=1 and MDE > 1, else 0. How much someone must spend before insurance kicks in.
# physlim:  Binary variable indicating if the person has a physical limitation.
# ndisease:  Number of chronic diseases.
# health:  Categorical self-rated health: excellent, good, fair, poor.
# linc:  log(annual family income).
# lfam:  log(family size).
# educdec:  Years of schooling of the household head.
# age:  Exact age in years.
# sex:  sex (male/female).
# child:  Binary (TRUE if age < 18, else FALSE).
# black:  Binary (TRUE if household head is Black, else FALSE).


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
# It seems there are no strong correlations with mdu. Biggest one is ndisease.

# Let us look at all variable relations (mostly with mdu)
pairs(data)
# Doesn't give many ideas. We can try looking at just the numerical values.

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


# Bootstrap
# Does the number of visits depend on the number of diseases?
B = 1000
MeanDiff.boot = replicate(B, mean(sample(data$mdu, replace=TRUE)) - mean(sample(data$ndisease, replace=TRUE)))
hist(MeanDiff.boot)
sd(MeanDiff.boot)
# 95% Percentile CI
quantile(MedianDiff.boot, c(.025, .975))
# Doesn't seem significant

# PCA
# We remove mdu
data_num_pca = data_num[-1]
pca = prcomp(data_num_pca, scale=T)
summary(pca)

# EIGENVALUES!
eigen(R)

# How many components do we take?
screeplot(pca,main="Screeplot",col="blue",type="barplot", pch=19)

# Same thing with the other package
fviz_screeplot(pca, addlabels = TRUE)

# First component
barplot(pca$rotation[,1], las=2, col="darkblue")

# How much does each variable contribute?
fviz_contrib(pca, choice = "var", axes = 1)

# First PC
plot_data_1 = data.frame(PC1 = pca$x[,1], Target = data$mdu)
ggplot(plot_data_1, aes(x = PC1, y = Target)) +
  geom_point() +
  labs(x = "Principal Component 1",
       y = "mdu") +
  theme_minimal()

# Second PC
plot_data_2 = data.frame(PC2 = pca$x[,2], Target = data$mdu)
ggplot(plot_data_2, aes(x = PC2, y = Target)) +
  geom_point() +
  labs(x = "Principal Component 2",
       y = "mdu") +
  theme_minimal()

# Third PC
plot_data_3 = data.frame(PC3 = pca$x[,3], Target = data$mdu)
ggplot(plot_data_3, aes(x = PC3, y = Target)) +
  geom_point() +
  labs(x = "Principal Component 3",
       y = "mdu") +
  theme_minimal()


# Scale
data_num_scaled = scale(data_num)
cor(data_num_scaled)
data_num_fa = data_num_scaled[,-1]
# Not too correlated, so factor analysis might not work that well, but we still try it!
# We start simple, just 2 factors (Bartlett should yield weighted least-squares)
fa_2 = factanal(data_num_fa, 2, rotation = "varimax", scores = "Bartlett")
fa_2
loadings(fa_2)
# 0.4 cumulative, not much...
# We try 3 factors
fa_3 = factanal(data_num_fa, 3, rotation = "varimax", scores = "Bartlett")
fa_3
loadings(fa_3)
# Still not much... How about 4?
fa_4 = factanal(data_num_fa, 4, rotation = "varimax", scores = "Bartlett")
fa_4
loadings(fa_4)
# Not a big difference, we try sticking with 3

# Observe the 3 factors
par(mfrow=c(3,1))
barplot(fa_3$loadings[,1], names=F, las=2, col="darkblue", ylim = c(-1, 1))
barplot(fa_3$loadings[,2], names=F, las=2, col="darkblue", ylim = c(-1, 1))
barplot(fa_3$loadings[,3], las=2, col="darkblue", ylim = c(-1, 1))

# FACTOR INTERPRETATION
# 

